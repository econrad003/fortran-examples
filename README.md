# fortran-examples
Simple examples of Fortran code

These don't represent the best coding practices, but they should compile with most Fortran-90 compilers.  I've tested them using the gfortran (Gnu Fortran) compiler in Cygwin.

+ *makefile* - a simple makefile
+ Solving triangles (compute the sides, angles, area \[using Heron's Formula\] and semiperimeter of a triangle given certain information):
  + *aas.f90* - solve a triangle (using the Law of Sines) given two angles and the side opposite the first angle.
  + *asa.f90* - solve a triangle (using the Law of Sines) given two angles and the side subtended by these two angles.
  + *ass.f90* - solve a triangle (using the Law of Sines) given an angle, the side opposite that angle, and another side -- there can be no solutions, a unique solution, or 2 distinct solutions -- and there is a good deal to check in the process
  + *sas.f90* - solve a triangle (using the Law of Cosines) given two sides and the angle contained by these two sides.
  + *sss.f90* - solve a triangle (using the Law of Cosines) given the lengths of its three sides.
+ *hello.f90* - a simple hello world program -- nothing special
+ *triangle.f90* - Heron's area formula - a precursor of *sss.f90*

A runtime note for read and write statements:  gfortran assigns *stdin* to device 5 and *stdout* to device 6.  I should not hard-code these values in read and write statements, but be reminded that these are not intended as examples of best practice.

The triangle solvers could be greatly improved (from the standpoint of best coding practices) by incorporating subroutines.

FYI: by default in Fortran, variables are defined by simply referencing them.  Variable names that start with the letters i through n are assumed to represent integer values, while variables starting with other letters are assumed to represent floating point values.  This doesn't reflect best coding practices.  (These defaults can be changed using the declaration "IMPLICIT NONE" and using "INTEGER" and "REAL" declarations.  I haven't done so here, at least not yet.)
