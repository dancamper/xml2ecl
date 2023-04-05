#!/usr/bin/env bash

#############################################
# Uses buildapp utility to create a binary
#
# https://www.xach.com/lisp/buildapp/
#############################################

set -euo pipefail

mkdir -p bin

buildapp --output bin/xml2ecl \
  --eval "(let* ((home-dir (user-homedir-pathname)) (setup (or (probe-file (merge-pathnames \"quicklisp/setup.lisp\" home-dir)) (probe-file (merge-pathnames \"portacle/all/quicklisp/setup.lisp\" home-dir))))) (load setup))" \
  --eval "(require :asdf)" \
  --eval "(ql:quickload '(:adopt :flexi-streams :fxml :with-user-abort) :silent t)" \
  --asdf-path . \
  --load-system xml2ecl \
  --entry xml2ecl:toplevel \
  --compress-core
