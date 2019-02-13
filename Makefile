FC = gfortran
#FC = g95

#FFLAGS = -O3 -fopenmp
#FFLAGS = -O3 -mcmodel=medium
#FFLAGS = -O0 -ggdb

#LFLAGS = -O3 -fopenmp
#LFLAGS = -O3 -mcmodel=medium
#LFLAGS = -O0 -ggdb

#LIBS = -lgomp /usr/lib/liblapack.a /usr/lib/libblas.a
LIBS = -lgomp C:/cygwin64/lib/liblapack.a C:/cygwin64/lib/libblas.a
#LIBS = 

#DEBUG = -fsanitize=address

#OBJECTS = interface.o silicene2d.o tightb.o cherndet.o deter.o
OBJECTS = ogpf.o interface.o zigzaghand.o

MODULES = ogpf.mod interface.mod

DATA = 

.PHONY: clean

zzhand.dat: zigzaghand.exe
	./zigzaghand.exe > saida.dat

zigzaghand.exe: $(OBJECTS)
	$(FC) $(LFLAGS) $(OBJECTS) $(DEBUG) -o zigzaghand.exe $(LIBS)

%.mod : %.f90
	$(FC) $(FFLAGS) -c $<

%.o : %.f90
	$(FC) $(FFLAGS) -c $<

clean:
	rm -f $(OBJECTS) $(MODULES) $(DATA) $(FIGURES) zigzaghand.exe

help:
	@echo "Valid targets:"
	@echo "  zigzaghand.exe"
	@echo "  zigzaghand.o"
	@echo "  clean:  removes .o, .dat, .ps, and .exe files"
