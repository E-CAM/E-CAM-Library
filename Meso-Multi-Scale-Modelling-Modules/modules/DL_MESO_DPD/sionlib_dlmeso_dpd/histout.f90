      SUBROUTINE histout (time)

!**********************************************************************
!
!     write out history data file for trajectory and localized data
!
!     copyright stfc daresbury laboratory
!     author - m. a. seaton july 2015
!     adapted to use SIONlib: s. chiacchiera March 2018
!**********************************************************************

      INTEGER :: i
      REAL(KIND=dp) :: time

!     write out data
#ifdef STDTRAJ
      WRITE (nhist) time, REAL (nbeads, KIND=dp), dimx, dimy, dimz, shrdx, shrdy, shrdz
#endif
!!! SIONlib 2k: write into SION file
      nelem=8
      size=8
      buffer_r (1:8) = (/ time, REAL (nbeads, KIND=dp), dimx, dimy, dimz, shrdx, shrdy, shrdz /)
      call fsion_write(buffer_r,size,nelem,sid,sierr)
#ifdef DEBUG
      IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
      WRITE (6,*) "k written in sionfile on node=",idnode ,"; # elements",sierr
#endif
!!!             

      SELECT CASE (keytrj)
      CASE (0)
        ! positions
        DO i = 1, nbeads
#ifdef STDTRAJ
           WRITE (nhist) REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz
#endif
!!! SIONlib 2l: write into SION file
          nelem=4
          size=8
          buffer_r (1:4) = (/ REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz /)
          call fsion_write(buffer_r,size,nelem,sid,sierr)
#ifdef DEBUG
          IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
          WRITE (6,*) "l written in sionfile on node=",idnode ,"; # elements",sierr
#endif
!!!             
        END DO
      CASE (1)
        ! positions and velocities
        DO i = 1, nbeads
#ifdef STDTRAJ
           WRITE (nhist) REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
               &vxx (i), vyy (i), vzz (i)
#endif
!!! SIONlib 2m: write into SION file
          nelem=7
          size=8
          buffer_r (1:7) = (/ REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
               &vxx (i), vyy (i), vzz (i) /)
          call fsion_write(buffer_r,size,nelem,sid,sierr)
#ifdef DEBUG
          IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
          WRITE (6,*) "m written in sionfile on node=",idnode ,"; # elements",sierr
#endif
!!!          
        END DO
      CASE (2)
        ! positions, velocities and forces
        IF (itype==1) THEN
          DO i = 1, nbeads
#ifdef STDTRAJ
             WRITE (nhist) REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
                 &vxx (i), vyy (i), vzz (i), (fxx(i)+fvx(i)), (fyy(i)+fvy(i)), (fzz(i)+fvz(i))
#endif
!!! SIONlib 2n: write into SION file
            nelem=10
            size=8
            buffer_r (1:10) = (/ REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
                 &vxx (i), vyy (i), vzz (i), (fxx(i)+fvx(i)), (fyy(i)+fvy(i)), (fzz(i)+fvz(i))  /)
            call fsion_write(buffer_r,size,nelem,sid,sierr)
#ifdef DEBUG
            IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
            WRITE (6,*) "n written in sionfile on node=",idnode ,"; # elements",sierr
#endif
!!!                         
          END DO
        ELSE
          DO i = 1, nbeads
#ifdef STDTRAJ
             WRITE (nhist) REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
                  &vxx (i), vyy (i), vzz (i), fxx (i), fyy (i), fzz (i)
#endif
!!! SIONlib 2p: write into SION file
            nelem=10
            size=8
            buffer_r (1:10) = (/ REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
                         &vxx (i), vyy (i), vzz (i), fxx (i), fyy (i), fzz (i)  /)
            call fsion_write(buffer_r,size,nelem,sid,sierr)
#ifdef DEBUG
            IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
            WRITE (6,*) "p written in sionfile on node=",idnode ,"; # elements",sierr
#endif
!!!                         
         END DO
        END IF
      END SELECT

!     clear buffers in case of job failure
#ifdef STDTRAJ
      ENDFILE (nhist)
      BACKSPACE (nhist)
#endif
      RETURN
      END SUBROUTINE histout
