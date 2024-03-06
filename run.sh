#!/bin/bash

set -x

swipl -s main.pl -g 'run_all_tests,halt.'

