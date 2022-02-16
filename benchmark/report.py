#!/usr/bin/env python3

"""
Report the results of a benchmark run recorded by benchmark.py.

Usage:
    ./report.py PROGRAM0.stats PROGRAM1.stats ...
"""

import sys
import numpy as np

def report_file(file_name: str) -> np.array:
    # When running the benchmark on a single file, using ndmin ensures that we actually
    # get a 2-dimensional array.
    data_ns = np.loadtxt(file_name, ndmin=2, dtype=int)
    num_files, num_iterations = data_ns.shape

    data_secs = data_ns / 1e9
    mean_times_by_file = np.mean(data_secs, axis=1)
    min_times_by_file = np.min(data_secs, axis=1)
    variance_by_file = np.var(data_secs, axis=1)

    total_mean_secs = np.sum(mean_times_by_file) 
    total_min_secs = np.sum(min_times_by_file) 
    total_variance = np.sum(variance_by_file) 
    total_stdev = np.sqrt(total_variance)

    print(f'{file_name}:')
    print(f'  mean time: {total_mean_secs:0.3f} ± {total_stdev:0.3f} seconds')
    print(f'   min time: {total_min_secs:0.3f} seconds')


for file_name in sys.argv[1:]:
    report_file(file_name)
    print()
