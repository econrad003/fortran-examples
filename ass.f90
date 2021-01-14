! triangle - ASS configuration
! Copyright 2021 by Eric Conrad.
! License - MIT License - see: https://opensource.org/licenses/MIT
!
!   input - the measure of an angle the length of a pair of sides
!       that don't contain the angle (NB: one of the two sides will
!       be opposite the given angle)
!   remarks - there may be 0, 1 or 2 solutions
!   output -
!       the sides and angles
!       the semiperimeter and the area
!       type of triangle
!
! Corrections:
!  14 Jan 2021 -
!		- added copyright and license statement
!		- corrected text of second error message under "Remaining error checks",
!		  changing "given angles" (plural) to "given angle" (singular).

! When I took geometry in high school, this was called the ASS
! configuration.  In today's more puritannical times, it's called
! the SSA configuration.

      program triangle
      print*,"ASS Configuration"

! Notes:
!   100 grad = 90 degrees = pi/2 (rad) = 1 rt angle = 1/4 rev
!   200 grad = 180 degrees = pi (rad) = 2 rt angles = 1/2 rev

      print*,"Angle measure units (1-radian, 2-degree, 3-grad, 4-rev):"
      read(*,*) imeas
      if(imeas .lt. 1 .or. imeas .gt. 4) then
          print*,"ERROR: unsupported angle measure"
          stop 1
      end if

      print*,"Enter the measure of the given angle (A):"
      read(*,*) alpha
      print*,"Enter the length of the side opposite A (a):"
      read(*,*) a
      print*,"Enter the length of a side adjacent to A (b):"
      read(*,*) b

! First error condition:
! the given sides must be positive in length
      if(a.le.0.0 .or. b.le.0.0) then
          print*,"ERROR: The given sides must be positive in length"
          stop 1
      end if

! Convert the angle measures
      pi = 4*atan(1.d0)
      if(imeas.eq.1) then       ! input in radians
          dalpha = 180 * alpha / pi
          galpha = 200 * alpha / pi
          ralpha = alpha / (2 * pi)
      else if(imeas.eq.2) then  ! input in degrees
          dalpha = alpha
          alpha = pi * dalpha / 180
          galpha = 200 * dalpha / 180
          ralpha = dalpha / 360
      else if(imeas.eq.3) then  ! input in grads
          galpha = alpha
          alpha = pi * galpha / 200
          dalpha = 180 * galpha / 200
          ralpha = dalpha / 400
      else                      ! input in revolutions
          ralpha = alpha
          alpha = 2 * pi * galpha
          dalpha = 360 * galpha
          galpha = 400 * ralpha
      end if

! Remaining error checks:
!   (1) the given angle should positive
      if(alpha.le.0) then
          print*, "ERROR: the given angle must be positive"
          stop 1
      end if
!   (2) the given angle must be less than a straight angle
      if(alpha.ge.pi) then
          print*, "ERROR: the given angle must be less" // &
                "than a straight angle"
          stop 1
      end if

! law of sines
! it's safe for finding sides (apart from numerical errors), but
! must be used carefully when used to find angles...
! there will be either no solutions or two solutions (the principal
! acute angle and its obtuse counterpart) or a double solution (a
! right angle)...

! Step 1) Find angle B using the law of sines
!     sin(B) / B = sin(A) / A

      sinB = b * sin(alpha) / a
      print*, "sin(B) =", sinB
      if(sinB .gt. 1) then
          print*, "There is no triangle satifying these constraints."
          stop 1
      end if

! we have reduced the confirguration to AAS (A, B, a), but there
! are two values of B to consider... and there are still some
! landmines...

! the principal solution
      print*, "Principal solution"
      beta = asin(sinB)
      print*, "a =", a, ", b =", b
      print*, "A =", alpha, ", B =", beta
      if((alpha + beta) .ge. pi) then
          print*, "The two known angles exceed a straight angle."
          print*, "No solution."
          stop 1
      end if

      dbeta = 180 * beta / pi
      gbeta = 200 * beta / pi
      rbeta = beta / (2 * pi)

! the principal solution B gives a real triangle...

      gamm = pi - alpha - beta          ! third angle
      dgamma = 180 * gamm / pi
      ggamma = 200 * gamm / pi
      rgamma = gamm / (2 * pi)

      c = sin(gamm) * a / sin(alpha)

! output solution of triangle
      print*, "------- PRINCIPAL SOLUTION --------------------------"
      print*, "a =", a, "  (given)"
      print*, "b =", b, "  (given)"
      print*, "c =", c
      print*, "A =", alpha, "degrees:", dalpha, "  (given)"
      print*, "B =", beta, "degrees:", dbeta, "  (principal)"
      print*, "C =", gamm, "degrees:", dgamma
      print*, "-----------------------------------------------------"
      print*, "Angles in grads:"
      print*, "     ", galpha, gbeta, ggamma
      print*, "     sum:", galpha + gbeta + ggamma, "  (exp: 200)"
      print*, "Angles in revolutions:"
      print*, "     ", ralpha, rbeta, rgamma
      print*, "     sum:", ralpha + rbeta + rgamma, "  (exp: 0.5)"
      print*, "-----------------------------------------------------"

! output semiperimeter and area
      s = (a + b + c) / 2
      print*, "semiperimeter:", s
      area = sqrt(s * (s-a) * (s-b) * (s-c))    ! Heron's formula
      print*, "area:", area

! angle sum
      sum1 = alpha + beta + gamm
      err1 = sum1 - pi
      sum2 = dalpha + dbeta + dgamma
      err2 = sum2 - 180.0
      print*, "angle sum (radians):", sum1, "   error:", err1
      print*, "angle sum (degrees):", sum2, "   error:", err2

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

! the secondary solution
      print*, "====================================================="
      print*, "Secondary solution"
      beta2 = pi - beta
      print*, "a =", a, ", b =", b
      print*, "A =", alpha, ", B =", beta2
      if(beta2 .eq. beta) then
          print*, "Angle B is a right angle."
          print*, "The solution is unique."
          stop 0
      end if
      beta = beta2
      if((alpha + beta) .ge. pi) then
          print*, "The two known angles exceed a straight angle."
          print*, "No secondary solution."
          stop 0
      end if

      dbeta = 180 * beta / pi
      gbeta = 200 * beta / pi
      rbeta = beta / (2 * pi)

! the secondary (obtuse) solution B gives a real triangle...

      gamm = pi - alpha - beta          ! third angle
      dgamma = 180 * gamm / pi
      ggamma = 200 * gamm / pi
      rgamma = gamm / (2 * pi)

      c = sin(gamm) * a / sin(alpha)

! output solution of triangle
      print*, "------- SECONDARY SOLUTION --------------------------"
      print*, "a =", a, "  (given)"
      print*, "b =", b, "  (given)"
      print*, "c =", c
      print*, "A =", alpha, "degrees:", dalpha, "  (given)"
      print*, "B =", beta, "degrees:", dbeta, "  (secondary)"
      print*, "C =", gamm, "degrees:", dgamma
      print*, "-----------------------------------------------------"
      print*, "Angles in grads:"
      print*, "     ", galpha, gbeta, ggamma
      print*, "     sum:", galpha + gbeta + ggamma, "  (exp: 200)"
      print*, "Angles in revolutions:"
      print*, "     ", ralpha, rbeta, rgamma
      print*, "     sum:", ralpha + rbeta + rgamma, "  (exp: 0.5)"
      print*, "-----------------------------------------------------"

! output semiperimeter and area
      s = (a + b + c) / 2
      print*, "semiperimeter:", s
      area = sqrt(s * (s-a) * (s-b) * (s-c))    ! Heron's formula
      print*, "area:", area

! angle sum
      sum1 = alpha + beta + gamm
      err1 = sum1 - pi
      sum2 = dalpha + dbeta + dgamma
      err2 = sum2 - 180.0
      print*, "angle sum (radians):", sum1, "   error:", err1
      print*, "angle sum (degrees):", sum2, "   error:", err2

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
