GCC=gcc
GCC_DIR=build/gcc

CLANG=clang
CLANG_DIR=build/clang

C_FLAGS=-std=c99 -pedantic -lm

HKC=~/hakaru/dist/build/hkc/hkc
HKC_FLAGS=-O

###################
##  TESTS
###################


HK_TO_C = true.c \
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
          # arrayCoersion.c


C_TO_EXE = true.bin \
           false.bin \
           addInt1.bin \
           addInt2.bin \
           addProb1.bin \
           addProb2.bin \
           normal.bin \
           uniform.bin \
           dirac.bin \
           matchBool.bin \
           matchNested.bin \
           array.bin \
           arrayLit.bin \
           arraySize.bin \
           arrayIndex.bin
           # arrayCoersion.bin


testAll : testCoverage $(C_TO_EXE)
testCoverage : $(HK_TO_C)
# checkCG : $(HK_TO_C)
# 	diff build/c/$^ src/c/$^

####################
## Hakaru to C
####################

%.c : src/hakaru/%.hk buildDirC
	$(HKC) $(HKC_FLAGS) $< -o build/c/$@

######################
## C to Binaries
######################

%.bin : build/c/%.c buildDirBin
	$(GCC) $(C_FLAGS) $< -o $(GCC_DIR)/$@
	$(CLANG) $(C_FLAGS) $< -o $(CLANG_DIR)/$@

####################

buildDirC :
	mkdir -p build/c

buildDirBin :
	mkdir -p $(GCC_DIR)
	mkdir -p $(CLANG_DIR)

clean :
	rm -rf build
