! triangle - ASA configuration
!   input - the measures of two angles and the length of the side
!       that they subtend
!   output -
!       the sides and angles
!       the semiperimeter and the area
!       type of triangle

      program triangle
      print*,"ASA Congruence"

! Notes:
!   100 grad = 90 degrees = pi/2 (rad) = 1 rt angle = 1/4 rev
!   200 grad = 180 degrees = pi (rad) = 2 rt angles = 1/2 rev

      print*,"Angle measure units (1-radian, 2-degree, 3-grad, 4-rev):"
      read(*,*) imeas
      if(imeas .lt. 1 .or. imeas .gt. 4) then
          print*,"ERROR: unsupported angle measure"
          stop 1
      end if

      print*,"Enter the measures of the two given angles (A and B):"
      read(*,*) alpha, beta
      print*,"Enter the length of the subtended side (c):"
      read(*,*) c

! First error condition:
! the given side must be positive in length
      if(c.le.0.0) then
          print*,"ERROR: The given side must be positive in length"
          stop 1
      end if

! Convert the angle measures
      pi = 4*atan(1.d0)
      if(imeas.eq.1) then       ! input in radians
          dalpha = 180 * alpha / pi
          galpha = 200 * alpha / pi
          ralpha = alpha / (2 * pi)
          dbeta = 180 * beta / pi
          gbeta = 200 * beta / pi
          rbeta = beta / (2 * pi)
      else if(imeas.eq.2) then  ! input in degrees
          dalpha = alpha
          alpha = pi * dalpha / 180
          galpha = 200 * dalpha / 180
          ralpha = dalpha / 360
          dbeta = beta
          beta = pi * dbeta / 180
          gbeta = 200 * dbeta / 180
          rbeta = dbeta / 360
      else if(imeas.eq.3) then  ! input in grads
          galpha = alpha
          alpha = pi * galpha / 200
          dalpha = 180 * galpha / 200
          ralpha = dalpha / 400
          gbeta = beta
          beta = pi * gbeta / 200
          dbeta = 180 * gbeta / 200
          rbeta = dbeta / 400
      else                      ! input in revolutions
          ralpha = alpha
          alpha = 2 * pi * galpha
          dalpha = 360 * galpha
          galpha = 400 * ralpha
          rbeta = beta
          beta = 2 * pi * gbeta
          dbeta = 360 * gbeta
          gbeta = 400 * rbeta
      end if

! Remaining error checks:
!   (1) the given angles should positive
      if(alpha.le.0 .or. beta.le.0) then
          print*, "ERROR: the given angles must be positive"
          stop 1
      end if
!   (2) the given angles must sum to less than a straight angle
      if((alpha + beta).ge.pi) then
          print*, "ERROR: the given angles must sum less" // &
                "than a straight angle"
          stop 1
      end if

! law of sines
! it's safe for finding sides (apart from numerical errors)
!     - no problems with arcsin and obtuse angles, but potential for
!       numerical errors when an angle is close to zero

      gamm = pi - alpha - beta          ! third angle
      dgamma = 180 * gamm / pi
      ggamma = 200 * gamm / pi
      rgamma = gamm / (2 * pi)

      a = sin(alpha) * c / sin(gamm)
      b = sin(beta) * c / sin(gamm)

! output solution of triangle
      print*,"------- SOLUTION ------------------------------------"
      write(6,*) "a =", a
      write(6,*) "b =", b
      write(6,*) "c =", c, "  (given)"
      write(6,*) "A =", alpha, "degrees:", dalpha, "  (given)"
      write(6,*) "B =", beta, "degrees:", dbeta, "  (given)"
      write(6,*) "C =", gamm, "degrees:", dgamma
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
