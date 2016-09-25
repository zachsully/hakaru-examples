GCC=gcc
GCC_DIR=build/gcc

CLANG=clang
CLANG_DIR=build/clang

C_FLAGS=-std=c99 -pedantic -lm -g

HKC=~/hakaru/dist/build/hkc/hkc
HKC_FLAGS=-O

HAKARU=~/hakaru/dist/build/hakaru/hakaru

###################
##  TESTS
###################

# Hakaru programs converted to C via HKC
HK_TO_C = true.c \
          false.c \
          boolEq.c \
          binaryAnd.c \
          binaryAnd2.c \
          matchBool.c \
          matchNested.c \
	  addInt1.c \
          addInt2.c \
          addProb1.c \
          addProb2.c \
	  summate.c \
          product.c \
          array.c \
          arrayLit.c \
          arraySize.c \
          arrayIndex.c \
          arrayCoercion.c \
          measureNormal.c \
          measureUniform.c \
          measureDirac.c \
	  superpose.c \
	  superpose2.c \
          superpose3.c \
          superpose5.c \
          superposeRec.c \
          superposeNormals.c \
          plateDirac.c \
          plateUniform.c \
          plateNormal.c \
          lam.c \
          lam2.c \
          lamMeasure.c \
          simp_hello2.c \
          simp_nbg.c


# C programs compiled to binary executables
C_TO_EXE = true.bin \
           false.bin \
           boolEq.bin \
           binaryAnd.bin \
           binaryAnd2.bin \
           matchBool.bin \
           matchNested.bin \
           addInt1.bin \
           addInt2.bin \
           addProb1.bin \
           addProb2.bin \
           summate.bin \
           product.bin \
           array.bin \
           arrayLit.bin \
           arraySize.bin \
           arrayIndex.bin \
           arrayCoercion.bin \
           measureNormal.bin \
           measureUniform.bin \
           measureDirac.bin \
           superpose.bin \
	   superpose2.bin \
           superpose3.bin \
           superpose5.bin \
           superposeRec.bin \
           superposeNormals.bin \
           plateDirac.bin \
           plateUniform.bin \
           plateNormal.bin

# C function compiled to object files
C_TO_OBJ = lam.o \
           lam2.o \
           lamMeasure.o \
	   simp_hello2.o \
           simp_nbg.o

# Executables ran to produce output
OUTPUT  = true.out \
          false.out \
	  binaryAnd.out \
          binaryAnd2.out \
          matchBool.out \
          matchNested.out \
	  addInt1.out \
          addInt2.out \
          addProb1.out \
          addProb2.out \
	  summate.out \
          product.out \
          array.out \
          arrayLit.out \
          arraySize.out \
          arrayIndex.out \
          arrayCoercion.out \

# Timed files generated when testing the time
TIMED = summate.time \
        product.time \
        array.time


# build tests
sea         : $(HK_TO_C)
binaries    : $(C_TO_EXE)
output      : $(OUTPUT)
timed       : $(TIMED)

####################
## Hakaru to C
####################

%.c : src/hakaru/%.hk buildDirC
	$(HKC) $(HKC_FLAGS) $< -o build/c/$@


######################
## C to Binaries
######################

%.bin : build/c/%.c buildDirBin
	$(GCC) $(C_FLAGS) $< -o $(GCC_DIR)/bin/$@
	$(CLANG) $(C_FLAGS) $< -o $(CLANG_DIR)/bin/$@

%.o : build/c/%.c buildDirBin
	$(GCC) -c $(C_FLAGS) $< -o $(GCC_DIR)/bin/$@
	$(CLANG) -c $(C_FLAGS) $< -o $(CLANG_DIR)/bin/$@


#####################
## Program Output
#####################

%.out : $(GCC_DIR)/bin/%.bin $(CLANG_DIR)/bin/%.bin buildDirOut
	$< > $(GCC_DIR)/out/$@
	$(word 2,$^) > $(CLANG_DIR)/out/$@


%.time : $(GCC_DIR)/bin/%.bin $(CLANG_DIR)/bin/%.bin src/hakaru/%.hk buildDirTime
	{ time $<; } > /dev/null 2> $(GCC_DIR)/time/$@
	{ time $(word 2,$^); } > /dev/null 2> $(CLANG_DIR)/time/$@
	{ time $(HAKARU) $(word 3,$^); } > /dev/null 2> build/hakaru/time/$@

####################

buildDirC :
	mkdir -p build/c

buildDirBin :
	mkdir -p $(GCC_DIR)/bin
	mkdir -p $(CLANG_DIR)/bin

buildDirOut :
	mkdir -p $(GCC_DIR)/out
	mkdir -p $(CLANG_DIR)/out

buildDirTime :
	mkdir -p $(GCC_DIR)/time
	mkdir -p $(CLANG_DIR)/time
	mkdir -p build/hakaru/time


####################

clean :
	rm -rf build
