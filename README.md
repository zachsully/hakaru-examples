# HKC Testing Framework #

HKC compiles Hakaru programs to C. Programs stored in "src" directory are
starting points for tests. In the future, output programs can be stored there
as well.

## Running HKC-Test on your machine

You will have to make sure these variables are set on your machine. They are at
top of the Makefile.

```
GCC=gcc
GCC_DIR=build/gcc

CLANG=clang
CLANG_DIR=build/clang

C_FLAGS=-std=c99 -pedantic -lm -g

HKC=~/hakaru/dist/build/hkc/hkc
HKC_FLAGS=-O

HAKARU=~/hakaru/dist/build/hakaru/hakaru
```

Then you can run the test pipeline:

```
make sea -j; make binaries -j; make output -j; make timed;
```

NOTE:

I use `-j` flag to parallelize where appropriate. The timed tests are not
appropriate because we need to have all cores available to ensure that the
parallel versions of the programs are being evaluated correctly.



## Test Types ##

### Testing Compiler Coverage ###

Testing the HKC compiler coverage involves putting Hakaru programs into it, if
they produce valid C code then they pass the test. Portability is a vital
concern, therefore each program is compiler with clang, gcc, and
gcc-arm-none-eabi. The compiler flags used are `-std=c99 -pedantic` again for
portability.

In order to test if HKC can even produce C at all:
```
make sea -j;
```
This will produce a directory "build/c" that will contain all the C code
generated.


To check if the generated C code compiles:
```
make binaries -j;
```
This will produce the directories "build/gcc/bin", "build/clang/bin",
"build/gcc/pbin", and "build/clang/pbin", where the directories prefixed with
"p" correspond to the parallel versions of the programs.


### Testing Correctness of Computations ###

Hakaru programs that have compute a value have type: Nat, Int, Real, Prob, or
some datum. The output of these programs can be tests against known answers.


### Testing Measures ###

Measure compile to programs that sample from distributions. These could be
tested by plotting the density?

### Testing Functions ###

Hakaru functions compile to C functions, there for in order to test these
programs, they must be embedded in some other C program.
