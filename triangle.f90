! Area of a triangle - Heron's formula
!    input - the lengths of the three sides
!    output -
!        the semiperimeter and the area
!        type of triangle

      program triangle
      print*,"Enter the lengths of the three sides:"
      read(*,*) a, b, c
      if(a.le.0.0 .or. b.le.0.0 .or. c.le.0.0) stop 1
      write(6,*) "sides:", a, b, c

! output semiperimeter and area
      s = (a + b + c) / 2.0
      area = sqrt(s * (s-a) * (s-b) * (s-c))
      write(6,*) "semiperimeter:", s

! triangle inequality
      if (max(a,b,c) .ge. s) then
          print*,"Error: max(a,b,c) >= s (Triangle)"
      end if

      write(6,*) "area:", area

! Nesbitt's inequality
      symsum = a/(b+c) + b/(a+c) + c/(a+b)
      print*,"Nesbitt's symmetric sum:", symsum
      if (symsum.lt.1.5) then
          print*,"   Error: sum < 1.5"
      end if
      if (symsum.ge.2) then
          print*,"   Error: sum > 2"
      end if

! acute, right or obtuse
      if (c.ge.a .and. c.ge.b) then
          x = a
          y = b
          z = c
      else if (a.ge.b .and. a.ge.c) then
          x = b
          y = c
          z = a
      else
          x = c
          y = a
          z = b
      end if
      print*, "longest side:", z
      sumsq = z*z - x*x - y*y
      print*, "Pythagorean sum:", sumsq
      if(sumsq.lt.0.0) then
          print*,"Acute angle opposite", z
      else if(sumsq.gt.0.0) then
          print*,"Obtuse angle opposite", z
      else
          print*,"Right angle opposite", z
      end if

      end program
