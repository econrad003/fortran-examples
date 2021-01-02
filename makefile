all: hello aas asa sas sss triangle

hello: hello.f90
	gfortran -o hello hello.f90

aas: aas.f90
	gfortran -o aas aas.f90

asa: asa.f90
	gfortran -o asa asa.f90

sas: sas.f90
	gfortran -o sas sas.f90

sss: sss.f90
	gfortran -o sss sss.f90

triangle: triangle.f90
	gfortran -o triangle triangle.f90
