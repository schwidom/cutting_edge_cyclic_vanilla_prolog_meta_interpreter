#!/bin/bash

set -x

swipl -s main.pl -g 'run_all_tests_with_depth_300,halt.'

