all: datagen

datagen: datagen.f90 datagen_mt.f90
	gfortran -O3 -o datagen datagen.f90
	gfortran -O3 -o datagen_mt datagen_mt.f90 -fopenmp

test: omp_test.f90 hashmap_play.f90
	gfortran -O3 -o omp_test omp_test.f90 -fopenmp
	gfortran -O3 -o hashmap_play hashmap_play.f90 -I/usr/local/include/fortran_stdlib/GNU-14.1.0 -L/usr/local/lib -lfortran_stdlib

clean:
	rm -f datagen
	rm -f datagen_mt
	rm -f omp_test
	rm -f hashmap_play