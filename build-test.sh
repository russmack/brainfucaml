#!/bin/bash

ocamlfind ocamlc -o test -package ounit2 -linkpkg -g config.ml trace.ml brainfucaml.ml test.ml

