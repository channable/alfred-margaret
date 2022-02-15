#!/usr/bin/env python3

import sys
import time

for file_name in sys.argv[1:]:
    with open(file_name, 'r', encoding='utf-16le') as f:
        needles = []

        for line in f:
            if line == '\n':
                break
            needles.append(line[:-1])

        haystack = f.read()
        num_matches = 0

        # Measure every input five times.
        for i in range(0, 5):
            # In Python 3.7 we would be able to use time.monotonic_ns ...
            epoch_ns = int(time.monotonic() * 1e9)

            for needle in needles:
                start = 0
                while True:
                    n = haystack.find(needle, start)
                    if n != -1:
                        start = n + 1
                        num_matches += 1
                    else:
                        break

            duration_ns = int(time.monotonic() * 1e9) - epoch_ns
            print(f'{duration_ns}\t', end='')

            # Print the number of matches once per file to have reference output.
            if i == 0:
                print(num_matches, file=sys.stderr)

        # Print a newline, so we have one line of timings per input file.
        print()
