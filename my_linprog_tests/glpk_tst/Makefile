## BEGIN HEADER ##
tt1 = glpk_tst
## END HEADER ##

## BEGIN OTHERTAGS ##
## END OTHERTAGS ##

## BEGIN COMPILERS ##
COMPILER_C = gcc
COMPILER_CPP = g++
LINKER_C = gcc
LINKER_CPP = g++
## END COMPILERS ##

## BEGIN FLAGS ##
FLAGS = -Winline -Wall -g -O3 #-std=gnu99
FLAGSL = -lm -lglpk
## END FLAGS ##

## BEGIN OBJECTS ##
tt1obj = $(tt1).o
## END OBJECTS ##

## BEGIN OUTPUTS ##
tt1out = ./glpk_tst
## END OUTPUTS ##

## BEGIN DEPENDENCIES ##
tt1deps = $(tt1).c 
## END DEPENDENCIES ##

## BEGIN PHONYS ##
.PHONY : all clean ctags tt1

all : $(tt1out)
## END PHONYS ##

## BEGIN BODY tt1 ##
tt1 : $(tt1out)

$(tt1out) : $(tt1obj)
	$(LINKER_C) -o $(tt1out) $(tt1obj) $(FLAGSL)

$(tt1).o : $(tt1deps)
	$(COMPILER_C) -c $(FLAGS) $(tt1).c
## END BODY tt1 ##

## BEGIN CLEAN ##
clean :
	rm -f ctags *~ core.* $(tt1obj)
## END CLEAN ##

## BEGIN TAGS ##
ctags : tags

tags : $(tt1deps)
	ctags -R
## END TAGS ##

## BEGIN USERDEFINED ##
## END USERDEFINED ##

