#!/usr/bin/env python

import re
import os
from datetime import datetime

# Regular expressions
header_re = re.compile(r'(?:\s)(?:[A-Z][A-Z]+)(?::)([a-z0-9][a-z0-9]*)')

# Setup
today = datetime.today().__format__('%y-%m-%d')
time = datetime.today().today().__format__('%H-%M')
default_output_dir = 'results/' + today + '/'
default_extension = '.dat'

print('Post-processing...')

files = []

read_file = open('saida.dat', 'r').read().split('SUCCESSFULLY WROTE TO FILE')
for block in read_file:
    copy_header = header_re.findall(block)
    block = header_re.sub('', block).lstrip('\n')
    if copy_header:
        name = copy_header[0]

        files.append(name)
        print('File found:' + name)

        output = open(name + default_extension, 'w')
        output.write(block)
        output.close()

if not os.path.exists(default_output_dir):
    print('Expected directory: ' + default_output_dir)
    print('Creating...')
    os.makedirs(default_output_dir, exist_ok=True)

if files:
    for file in files:
        if os.path.exists(default_output_dir + file + default_extension):
            i = 1
            while True:
                new_file = file + '(' + str(i) + ')'
                print('File already exist, trying: ' + new_file)
                if os.path.exists(default_output_dir + new_file + default_extension):
                    i += 1
                    continue
                else:
                    print('Moving ' + new_file + default_extension, 'to ' + default_output_dir + new_file + default_extension)
                    os.replace(file + default_extension, default_output_dir + new_file + default_extension)
                    break

        else:
            print('Moving ' + file + default_extension, 'to ' + default_output_dir + file + default_extension)
            os.replace(file + default_extension, default_output_dir + file + default_extension)
else:
    print('No files found!')

print('Finished!')
