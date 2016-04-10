FC = gfortran

all: ndcg.so ndcg.out

ndcg.so: ndcg.f90
	$(FC) $(FFLAGS) -c ndcg.f90
	$(FC) $(FFLAGS) -shared -o ndcg.so ndcg.o

ndcg.out: ndcg.o
	$(FC) $(FFLAGS) test_ndcg.f90 ndcg.o -o test_ndcg.out

clean:
	rm -f *.o
	rm -f *.so
	rm -f *.mod
	rm -f *.out
