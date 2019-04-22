#!/bin/bash

ocamlfind ocamlc -o test -package ounit -linkpkg -g brainfucaml.ml test.ml

