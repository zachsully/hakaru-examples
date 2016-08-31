CC=gcc
CFLAGS=-std=c99 -pedantic -lm

HKC=~/hakaru/dist/build/hkc/hkc
HKCFLAGS=-O

TESTS= true.c \
       false.c \
       addInt1.c \
       addInt2.c \
       addProb1.c \
       addProb2.c \
       normal.c \
       uniform.c \
       dirac.c \
       matchBool.c \
       matchNested.c \
       array.c \
       arrayLit.c \
       arraySize.c \
       arrayIndex.c

test : $(TESTS)

%.c : src/hakaru/%.hk buildDirC
	$(HKC) $(HKCFLAGS) $< -o build/c/$@

buildDirC :
	mkdir -p build/c

clean :
	rm -rf build
