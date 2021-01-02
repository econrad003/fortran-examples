! triangle - SSS configuration
!   input - the lengths of the three sides
!   output -
!       the angles opposite each side
!       the semiperimeter and the area
!       type of triangle

      program triangle
      print*,"SSS Congruence"
      print*,"Enter the lengths of the three sides:"
      read(*,*) a, b, c

! Error conditions
!   (1) the sides must be positive in length
      if(a.le.0.0 .or. b.le.0.0 .or. c.le.0.0) then
          print*,"ERROR: The sides must be positive in length"
          stop 1
      end if
!   (2) The triangle inequality must be strictly satisfied.
!           a + b < c; b + c < a; c + a < c
!       A sufficient and necessary condition is that no side may
!       exceed the semiperimeter.
      s = (a + b + c) / 2.0     ! semiperimeter
      if (max(a,b,c) .ge. s) then
          print*,"ERROR: (triangle inequality) max(a,b,c) < s"
          stop 1
      end if

      write(6,*) "sides:", a, b, c

! law of cosines
      alpha = acos((b*b + c*c - a*a) / (2*b*c))
      beta = acos((a*a + c*c - b*b) / (2*a*c))
      gamm = acos((a*a + b*b - c*c) / (2*a*b))

! angles in degrees
      pi = 4*atan(1.d0)
      dalpha = 180 * alpha / pi
      dbeta = 180 * beta / pi
      dgamma = 180 * gamm / pi

! output solution of triangle
      print*,"------- SOLUTION ------------------------------------"
      write(6,*) "a =", a, "  (given)"
      write(6,*) "b =", b, "  (given)"
      write(6,*) "c =", c, "  (given)"
      write(6,*) "A =", alpha, "degrees:", dalpha
      write(6,*) "B =", beta, "degrees:", dbeta
      write(6,*) "C =", gamm, "degrees:", dgamma
      print*,"-----------------------------------------------------"

! output semiperimeter and area
      write(6,*) "semiperimeter:", s
      area = sqrt(s * (s-a) * (s-b) * (s-c))    ! Heron's formula
      write(6,*) "area:", area

! angle sum
      sum1 = alpha + beta + gamm
      err1 = sum1 - pi
      sum2 = dalpha + dbeta + dgamma
      err2 = sum2 - 180.0
      write(6,*) "angle sum (radians):", sum1, "   error:", err1
      write(6,*) "angle sum (degrees):", sum2, "   error:", err2

! Nesbitt's inequality
      symsum = a/(b+c) + b/(a+c) + c/(a+b)
      print*,"Nesbitt's symmetric sum:", symsum
      if (symsum.lt.1.5) then
          print*,"   Error: sum < 1.5"
      end if
      if (symsum.ge.2) then
          print*,"   Error: sum > 2"
      end if

      end program
