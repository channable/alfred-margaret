#!/usr/bin/python3

"""
Report the results of a benchmark run recorded by benchmark.py.

Usage:
    ./report.py PROGRAM0.stats PROGRAM1.stats ...
"""

import sys
import numpy as np
import matplotlib.pyplot as plt

def report_file(file_name: str) -> np.array:
    data_ns = np.genfromtxt(file_name, dtype=int)
    num_files, num_cols = data_ns.shape

    # I made a mess of the data format: it is one line of tab-separated numbers
    # per input. The first number is the number of needles, then it's
    # Aho-Corasick time in ns and regex time in ns interleaved. There are 6
    # iterations of this (so 13 numbers). And then that is repeated for 2
    # rounds, so the 14th number is again the number of needles, and then again
    # Aho-Corasick and regex times interleaved. I am sorry about this hack.
    aho_ns = data_ns[:, (1, 3, 5, 7, 9, 11, 14, 16, 18, 20, 22, 24)]
    reg_ns = data_ns[:, (2, 4, 6, 8, 10, 12, 15, 17, 19, 21, 23, 25)]
    num_needles = data_ns[:, 0]
    num_needles_alt = data_ns[:, 13]
    assert np.all(num_needles == num_needles_alt)
    assert aho_ns.shape == reg_ns.shape

    aho_secs_by_file = np.mean(aho_ns, axis=1) / 1e9
    reg_secs_by_file = np.mean(reg_ns, axis=1) / 1e9
    aho_err_by_file = np.var(aho_ns, axis=1) / 1e18 / aho_secs_by_file
    reg_err_by_file = np.var(reg_ns, axis=1) / 1e18 / reg_secs_by_file

    ratio = reg_secs_by_file / aho_secs_by_file
    error = np.sqrt(ratio * (aho_err_by_file + reg_err_by_file))

    fig = plt.figure()
    ax = fig.add_subplot(1, 1, 1)
    ax.errorbar(num_needles, ratio, error, fmt='o', capsize=2, markersize=3)
    ax.set_yscale('log')
    ax.set_xscale('log')
    ax.set_xlim(0.5, np.max(num_needles) * 1.5)
    ax.axhline(y=1.0, color='red')
    ax.set_xlabel('Number of needles')
    ax.set_ylabel('time regex / time Aho-Corasick')
    plt.show()


for file_name in sys.argv[1:]:
    report_file(file_name)
