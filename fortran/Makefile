FC = gfortran
FFLAGS = -fPIC

all: ndcg.out

qsort.o: qsort.f90
	$(FC) $(FFLAGS) -c qsort.f90
	$(FC) $(FFLAGS) -shared -o qsort.so qsort.o

ndcg.o: ndcg.f90 qsort.o
	$(FC) $(FFLAGS) -c ndcg.f90
	$(FC) $(FFLAGS) -shared -o ndcg.so ndcg.o

ndcg.out: qsort.o ndcg.o
	$(FC) $(FFLAGS) test_ndcg.f90 ndcg.o qsort.o -o test_ndcg.out

clean:
	rm -f *.o
	rm -f *.so
	rm -f *.mod
	rm -f *.out
