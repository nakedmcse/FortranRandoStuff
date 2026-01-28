all: api module program

api: regex_api.c
	gcc -o regex_api.o -c regex_api.c

module: regex.f90
	gfortran -o regex.o -c regex.f90

program: regex_test.f90
	gfortran -o regex_test regex_api.o regex.o regex_test.f90

clean:
	rm -f *.o
	rm -f *.mod
	rm -f regex_test
