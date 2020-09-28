FC = ifort -fpp -traceback
FCFLAGS=-g -check all -warn all
#FCFLAGS=-g -warn all
#FC = gfortran -cpp
#FCFLAGS=-Wall -g

all: spginfo

spginfo: constants.o lsqutil.o arrayutil.o strutil.o math_util.o error.o fileutil.o fract.o program.o symm_table.o spginfo.o main.o 
	$(FC) constants.o arrayutil.o strutil.o lsqutil.o math_util.o error.o fileutil.o fract.o program.o symm_table.o spginfo.o main.o -o spginfo


constants.o: constants.f90
	$(FC) $(FCFLAGS) -c constants.f90 

lsqutil.o: lsqutil.f90 constants.f90
	$(FC) $(FCFLAGS) -c lsqutil.f90 

arrayutil.o: arrayutil.f90
	$(FC) $(FCFLAGS) -c arrayutil.f90 

strutil.o: strutil.f90 arrayutil.f90
	$(FC) $(FCFLAGS) -c strutil.f90 

math_util.o: math_util.f90 strutil.f90
	$(FC) $(FCFLAGS) -c math_util.f90 

error.o: error.f90 strutil.f90
	$(FC) $(FCFLAGS) -c error.f90

fract.o: fract.f90 strutil.f90
	$(FC) $(FCFLAGS) -c fract.f90 

symm_table.o: symm_table.f90
	$(FC) $(FCFLAGS) -c symm_table.f90 

program.o: program.f90 strutil.f90
	$(FC) $(FCFLAGS) -c program.f90 

fileutil.o: fileutil.f90 strutil.f90
	$(FC) $(FCFLAGS) -c fileutil.f90 

spginfo.o: spginfo.f90 strutil.f90 math_util.f90 lsqutil.f90
	$(FC) $(FCFLAGS) -c spginfo.f90 


main.o: main.f90 spginfo.f90 program.f90 fract.f90 symm_table.f90 fileutil.f90 error.f90 math_util.f90 lsqutil.f90 strutil.f90 constants.f90 arrayutil.f90
	$(FC) $(FCFLAGS) -c main.f90

clean: 
	rm -rf *.o *.mod spginfo

dist:
	cp *.f90 ./distrib/spginfo
	cp Makefile ./distrib/spginfo
	cp spginfo ./distrib/spginfo
	cp syminfo.lib ./distrib/spginfo

install:
	make
	cp spginfo ~/bin
	cp syminfo.lib ~/bin
