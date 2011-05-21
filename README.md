# Swim-scheme

## General view

This is an **in-progress** attempt to follow and expand on Abdoulaziz Ghuloum in his paper "An Incremental Approach to Compiler Construction". It currently generated (badly) x86 Windows code. I am planning to potentially add some ATmega328 (Arduino) code.

I will just use it as a starting point to experiment on some things and potentially expand on something more meaningful (to me).

## Quick doc

Right now the file 'compiler.scm' (in racket scheme) generates x86 Windows code wherever specified from a given scheme expression. This assembly code can be embedded as is (and compiled w/ NASM) in Visual Studio.

A cpp project has been added that contains a "driver" executable that links w/ the generated assembly code and calls it.

## Reasoning and perspectives

Part of the things I have been "working" on during my spare time are programming language structure in general, byte-code interpretation,
efficient interpretation, Forth related threaded-code techniques, etc. Ideally I would love to see all this stuff combined in one project
(if possible).

This is a long term goal. What I have in mind would be a Forth like interpreter in a subset of Scheme as close as possible to R5RS (which is
already a daunting task).
