#!/usr/bin/env python3

import clize
import os
import os.path
import subprocess
import sys

# clize turns this into a CLI where program is a required argument and prefix is a required option
def benchmark(program, *, prefix):
    """
    This script runs an executable 5 times and collects the running times it
    reports. Names of the .txt files in the data/ directory are provivided to the
    program on the command line. Expected output of the program is tab-separated
    times (with trailing tab) per run in nanoseconds on stdout, and the number of
    matches on stderr (only once, not per run). The output should be one line per
    input file.

    Example:
    ./benchmark.py ./naive.py --prefix python

    :param program: What to benchmark.
    :param prefix: Used for generating output files $prefix.{stats,results}
    """

    # # Disable automatic CPU frequency scaling to get lower variance measurements.
    with open('/sys/devices/system/cpu/cpu1/cpufreq/scaling_governor', 'r') as f:
        scaling_governor = next(f)

    if scaling_governor != 'performance\n':
        print('Please run the following to clock CPU frequency to its maximum:')
        print('echo performance | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor')
        sys.exit(1)

    input_file_names = []
    for f in os.listdir('data'):
        file_name = f'data/{f}'
        if os.path.isfile(file_name) and file_name.endswith('.txt'):
            input_file_names.append(os.path.abspath(file_name))

    print(f'Found {len(input_file_names)} files to benchmark.')

    # Run the program to benchmark under taskset to lock it to CPU core 1. This
    # avoids variance due to CPU migrations.
    cmdline = ['taskset', '-c', '1', program] + input_file_names

    times = []
    results = None 

    num_rounds = 5
    for i in range(0, num_rounds):
        print(f'Round {i + 1} of {num_rounds}.')
        p = subprocess.run(cmdline, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        
        if p.returncode != 0:
            print(f'Running {cmdline[:6]}... failed.')
            print(p.stdout.decode('utf-8'))
            print(p.stderr.decode('utf-8'))
            sys.exit(1)

        assert results is None or results == p.stderr, (
            'Program should have consistent output.\n'
            f'{results!r} != {p.stderr!r}'
        )
        results = p.stderr

        # Paste outputs of the different runs together, like GNU Paste.
        if len(times) == 0:
            times = p.stdout.splitlines()
        else:
            times = [acc + new for acc, new in zip(times, p.stdout.splitlines())]

    with open(f'{prefix}.stats', 'wb') as f:
        f.writelines(ts + b'\n' for ts in times) 

    with open(f'{prefix}.results', 'wb') as f:
        f.write(results)

    print(f'Results written to {prefix}.{{stats,results}}.')

if __name__ == '__main__':
    clize.run(benchmark)