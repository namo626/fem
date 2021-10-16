FC = gfortran
mods = global.o material.o prep.o quadrature.o testFunctions.o 

main: code1.o 
	$(FC) $< $(mods) -o $@

code1.o: code1.f90 $(mods)
	$(FC) -c $<

global.o: global.f90
	$(FC) -c $<

material.o: material.f90
	$(FC) -c $<

prep.o: prep.f90
	$(FC) -c $<

quadrature.o: quadrature.f90
	$(FC) -c $<

testFunctions.o: testFunctions.f90
	$(FC) -c $<

clean: 
	rm -rf *.o *.mod main
