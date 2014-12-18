#!/bin/bash
sbcl --dynamic-space-size 4096 --script test.lisp $*
##
