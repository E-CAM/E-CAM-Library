      SUBROUTINE histout (time)

!**********************************************************************
!
!     write out history data file for trajectory and localized data
!
!     copyright stfc daresbury laboratory
!     author - m. a. seaton july 2015
!     adapted to use SIONlib: s. chiacchiera November 2017
!**********************************************************************

      INTEGER :: i
      REAL(KIND=dp) :: time

!     write out data

!      WRITE (nhist) time, REAL (nbeads, KIND=dp), dimx, dimy, dimz, shrdx, shrdy, shrdz
      nelem=8
      size=8
      buffer_r (1:8) = (/ time, REAL (nbeads, KIND=dp), dimx, dimy, dimz, shrdx, shrdy, shrdz /)
      call fsion_write(buffer_r,size,nelem,sid,sierr)

      SELECT CASE (keytrj)
      CASE (0)
        ! positions
        DO i = 1, nbeads
!          WRITE (nhist) REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz
          nelem=4
          size=8
          buffer_r (1:4) = (/ REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz /)
          call fsion_write(buffer_r,size,nelem,sid,sierr)
        END DO
      CASE (1)
        ! positions and velocities
        DO i = 1, nbeads
!          WRITE (nhist) REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
!               &vxx (i), vyy (i), vzz (i)
          nelem=7
          size=8
          buffer_r (1:7) = (/ REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
               &vxx (i), vyy (i), vzz (i) /)
          call fsion_write(buffer_r,size,nelem,sid,sierr)
        END DO
      CASE (2)
        ! positions, velocities and forces
        IF (itype==1) THEN
          DO i = 1, nbeads
!            WRITE (nhist) REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
!                 &vxx (i), vyy (i), vzz (i), (fxx(i)+fvx(i)), (fyy(i)+fvy(i)), (fzz(i)+fvz(i))
            nelem=10
            size=8
            buffer_r (1:10) = (/ REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
                 &vxx (i), vyy (i), vzz (i), (fxx(i)+fvx(i)), (fyy(i)+fvy(i)), (fzz(i)+fvz(i))  /)
            call fsion_write(buffer_r,size,nelem,sid,sierr)
          END DO
        ELSE
          DO i = 1, nbeads
!            WRITE (nhist) REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
!                 &vxx (i), vyy (i), vzz (i), fxx (i), fyy (i), fzz (i)
            nelem=10
            size=8
            buffer_r (1:10) = (/ REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
                         &vxx (i), vyy (i), vzz (i), fxx (i), fyy (i), fzz (i)  /)
            call fsion_write(buffer_r,size,nelem,sid,sierr)
         END DO
        END IF
      END SELECT

!     clear buffers in case of job failure

      ENDFILE (nhist)
      BACKSPACE (nhist)

      RETURN
      END SUBROUTINE histout
