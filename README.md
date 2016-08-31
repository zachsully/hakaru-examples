# HKC Testing Framework #

HKC compiles Hakaru programs to C. Programs stored in "src" directory are
starting points for tests. In the future, output programs can be stored there
as well.



## Test Types ##

### Testing Compiler Coverage ###

Testing the HKC compiler coverage involves putting Hakaru programs into it, if
they produce valid C code then they pass the test. Portability is a vital
concern, therefore each program is compiler with clang, gcc, and
gcc-arm-none-eabi. The compiler flags used are "-std=c99 -pedantic" again for
portability.

### Testing Computations ###

Hakaru programs that have compute a value have type: Nat, Int, Real, Prob, or
some datum. The output of these programs can be tests against known answers.

### Testing Measures ###

Measure compile to programs that sample from distributions. These could be
tested by plotting the density?

### Testing Functions ###

Hakaru functions compile to C functions, there for in order to test these
programs, they must be embedded in some other C program.
