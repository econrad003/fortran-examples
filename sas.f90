! triangle - SAS configuration
!   input - the lengths of two sides and the measure of the subtended angle
!   output -
!       the sides and angles
!       the semiperimeter and the area
!       type of triangle

      program triangle
      print*,"SAS Congruence"

! Notes:
!   100 grad = 90 degrees = pi/2 (rad) = 1 rt angle = 1/4 rev
!   200 grad = 180 degrees = pi (rad) = 2 rt angles = 1/2 rev

      print*,"Angle measure units (1-radian, 2-degree, 3-grad, 4-rev):"
      read(*,*) imeas
      if(imeas .lt. 1 .or. imeas .gt. 4) then
          print*,"ERROR: unsupported angle measure"
          stop 1
      end if

      print*,"Enter the lengths of the two given sides (a and b):"
      read(*,*) a, b
      print*,"Enter the measure subtended angle (C):"
      read(*,*) gamm

! Error conditions
!   (1) the sides must be positive in length
      if(a.le.0.0 .or. b.le.0.0) then
          print*,"ERROR: The sides must be positive in length"
          stop 1
      end if
!   (2) the measure of the angle should be between 0 and pi
      pi = 4*atan(1.d0)
      if(imeas.eq.1) then       ! input in radians
          dgamma = 180 * gamm / pi
          ggamma = 200 * gamm / pi
          rgamma = gamm / (2 * pi)
      else if(imeas.eq.2) then  ! input in degrees
          dgamma = gamm
          gamm = pi * dgamma / 180
          ggamma = 200 * dgamma / 180
          rgamma = dgamma / 360
      else if(imeas.eq.3) then  ! input in grads
          ggamma = gamm
          gamm = pi * ggamma / 200
          dgamma = 180 * ggamma / 200
          rgamma = dgamma / 400
      else                      ! input in revolutions
          rgamma = gamm
          gamm = 2 * pi * ggamma
          dgamma = 360 * ggamma
          ggamma = 400 * rgamma
      end if
      if(gamm.le.0 .or. gamm .ge.pi) then
          print*, "ERROR: the given angle must be between 0 and 1/2 rev"
          stop 1
      end if


! finding the side opposite the given angle gives SSS and SSA
      c = sqrt(a*a + b*b - 2*a*b*cos(gamm))

! we could use law of sines but arcsin gives incorrect results if
! A or B is obtuse -- law of cosines has other numerical issues...
      alpha = acos((c*c + b*b - a*a) / (2*c*b))
      beta = acos((c*c + a*a - b*b) / (2*c*a))

! angles in degrees
      dalpha = 180 * alpha / pi
      dbeta = 180 * beta / pi

! angles in grads
      galpha = 200 * alpha / pi
      gbeta = 200 * beta / pi

! angles in revolutions
      ralpha = alpha / (2 * pi)
      rbeta = beta / (2 * pi)

! output solution of triangle
      print*,"------- SOLUTION ------------------------------------"
      write(6,*) "a =", a, "  (given)"
      write(6,*) "b =", b, "  (given)"
      write(6,*) "c =", c
      write(6,*) "A =", alpha, "degrees:", dalpha
      write(6,*) "B =", beta, "degrees:", dbeta
      write(6,*) "C =", gamm, "degrees:", dgamma, "  (given)"
      print*,"-----------------------------------------------------"
      print*,"Angles in grads:"
      write(6,*) "     ", galpha, gbeta, ggamma
      write(6,*) "     sum:", galpha + gbeta + ggamma, "  (exp: 200)"
      print*,"Angles in revolutions:"
      write(6,*) "     ", ralpha, rbeta, rgamma
      write(6,*) "     sum:", ralpha + rbeta + rgamma, "  (exp: 0.5)"
      print*,"-----------------------------------------------------"

! output semiperimeter and area
      s = (a + b + c) / 2
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

! triangle inequality check:
!           a + b < c; b + c < a; c + a < c
! A sufficient and necessary condition is that no side may
! exceed the semiperimeter.
      if (max(a,b,c) .ge. s) then
          print*,"ERROR: (triangle inequality) max(a,b,c) < s"
      end if

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
