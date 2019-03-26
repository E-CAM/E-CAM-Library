!***********************************************************************
!
!     patch for the subroutine start (set up starting configuration)
!
!     copyright stfc daresbury laboratory
!     authors - w. smith & m. a. seaton july 2015
!     adapted to use SIONlib: s. chiacchiera March 2018
!***********************************************************************
!     first part is unchanged
  
!     start data saving file and include species/molecule data

      IF (ltraj) THEN

        IF (nstep>0) THEN
           
!!! SIONlib 1a: give error for resart
           CALL error (idnode, 1500, 1)
!!!
#ifdef STDTRAJ           
           IF (nodes>1) THEN
              OPEN (nhist, file='HISTORY'//chan, access = 'sequential', form = 'unformatted', status = 'unknown',&
                   & position = 'append')
           ELSE
              OPEN (nhist, file='HISTORY', access = 'sequential', form = 'unformatted', status = 'unknown',&
                   & position = 'append')
           END IF
#endif          
        ELSE

!!! SIONlib 1b: define and open
           gcomm = MPI_COMM_WORLD
           lcomm = MPI_COMM_WORLD 
           fsblksize = -1
           chunksize = 100
           nfiles = 1
           call fsion_paropen_mpi(trim(filename),'bw',nfiles, gComm,lComm, &
                chunksize,fsblksize,idnode,newfname,sid)
#ifdef DEBUG
           WRITE (6,*) "opened sionfile on node=",idnode ,"; sid=",sid
           WRITE (6,*) "input chunksize (if needed, will be internally corrected)=", chunksize, "; fsblksize=", fsblksize
#endif
!!!
#ifdef STDTRAJ           
          IF (nodes>1) THEN
            OPEN (nhist, file='HISTORY'//chan, access = 'sequential', form = 'unformatted', status = 'unknown')
          ELSE
            OPEN (nhist, file='HISTORY', access = 'sequential', form = 'unformatted', status = 'unknown')
          END IF
#endif          
          IF (lgbnd .AND. idnode>0) THEN
#ifdef STDTRAJ
             WRITE (nhist) nspe, nmoldef, nusyst, nsyst, nbeads, 0
#endif
!!! SIONlib 2a: write into SION file
            nelem=6
            size=4
            buffer_i (1:6) = (/ nspe, nmoldef, nusyst, nsyst, nbeads, 0 /)
            call fsion_write(buffer_i,size,nelem,sid,sierr)
#ifdef DEBUG
            IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
            WRITE (6,*) "a written in sionfile on node=",idnode ,"; # elements",sierr
#endif
!!!            
          ELSE
#ifdef STDTRAJ
             WRITE (nhist) nspe, nmoldef, nusyst, nsyst, nbeads, nbonds
#endif
!!! SIONlib 2b: write into SION file
            nelem=6
            size=4
            buffer_i (1:6) = (/ nspe, nmoldef, nusyst, nsyst, nbeads, nbonds /)
            call fsion_write(buffer_i,size,nelem,sid,sierr)
#ifdef DEBUG
            IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
            WRITE (6,*) "b written in sionfile on node=",idnode ,"; # elements",sierr
#endif
!!!            
         END IF
#ifdef STDTRAJ
         WRITE (nhist) dimx, dimy, dimz, volm
#endif
!!! SIONlib 2c: write into SION file
         nelem=4
         size=8
         buffer_r (1:4) = (/ dimx, dimy, dimz, volm /)
         call fsion_write(buffer_r,size,nelem,sid,sierr)
#ifdef DEBUG
         IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
         WRITE (6,*) "c written in sionfile on node=",idnode ,"; # elements",sierr
#endif
!!!
#ifdef STDTRAJ
         WRITE (nhist) keytrj, srftype*srfx, srftype*srfy, srftype*srfz
#endif
!!! SIONlib 2d: write into SION file
         nelem=4
         size=4
         buffer_i (1:4) = (/ keytrj, srftype*srfx, srftype*srfy, srftype*srfz /)
         call fsion_write(buffer_i,size,nelem,sid,sierr)
#ifdef DEBUG
         IF (sierr.ne.nelem)  CALL error (idnode, 1501, INT (sierr - nelem))
         WRITE (6,*) "d written in sionfile on node=",idnode ,"; # elements",sierr
#endif
!!!
       
!      write species information
          DO i = 1, nspe
            k = (i * (i + 1)) / 2
            SELECT CASE (ktype (k))
            CASE (0:2)
#ifdef STDTRAJ
               WRITE (nhist) namspe (i), amass (i), vvv (2, k), lfrzn (i)
#endif
!!! SIONlib 2e: write into SION file
              nelem=1
              size=8
              buffer_c = namspe (i)
              call fsion_write(buffer_c,size,nelem,sid,sierr)
#ifdef DEBUG
              IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
              WRITE (6,*) "e1 written in sionfile on node=",idnode ,"; # elements",sierr
#endif
              nelem=2
              size=8
              buffer_r (1:2) = (/ amass (i), vvv (2, k) /)              
              call fsion_write(buffer_r,size,nelem,sid,sierr)              
#ifdef DEBUG
              IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
              WRITE (6,*) "e2 written in sionfile on node=",idnode ,"; # elements",sierr
#endif              
              nelem=1
              size=4
              buffer_i (1) = lfrzn (i)
              call fsion_write(buffer_i,size,nelem,sid,sierr)
#ifdef DEBUG
              IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
              WRITE (6,*) "e3 written in sionfile on node=",idnode ,"; # elements",sierr
#endif
!!!                        
           CASE (3)
#ifdef STDTRAJ
              WRITE (nhist) namspe (i), amass (i), vvv (6, k), lfrzn (i)
#endif
!!! SIONlib 2f: write into SION file
              nelem=1
              size=8
              buffer_c = namspe (i)
              call fsion_write(buffer_c,size,nelem,sid,sierr)
#ifdef DEBUG
              IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
              WRITE (6,*) "f1 written in sionfile on node=",idnode ,"; # elements",sierr
#endif              
              nelem=2
              size=8
              buffer_r (1:2) = (/ amass (i), vvv (6, k) /)              
              call fsion_write(buffer_r,size,nelem,sid,sierr)              
#ifdef DEBUG
              IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
              WRITE (6,*) "f2 written in sionfile on node=",idnode ,"; # elements",sierr
#endif              
              nelem=1
              size=4
              buffer_i (1) = lfrzn (i)
              call fsion_write(buffer_i,size,nelem,sid,sierr)
#ifdef DEBUG
              IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
              WRITE (6,*) "f3 written in sionfile on node=",idnode ,"; # elements",sierr
#endif
!!!                        
           END SELECT
          END DO

!      write molecule names
          IF (nmoldef>0) THEN
            DO i = 1, nmoldef
#ifdef STDTRAJ
               WRITE (nhist) nammol (i)
#endif
!!! SIONlib 2g: write into SION file
              nelem=1
              size=8
              buffer_c = nammol (i)
              call fsion_write(buffer_c,size,nelem,sid,sierr)
#ifdef DEBUG
              IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
              WRITE (6,*) "g written in sionfile on node=",idnode ,"; # elements",sierr
#endif
!!!
           END DO
          END IF

!      write name of calculation
#ifdef STDTRAJ
          WRITE (nhist) text
#endif
!!! SIONlib 2h: write into SION file
              nelem=1
              size=80
              call fsion_write(text,size,nelem,sid,sierr)
#ifdef DEBUG
              IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
              WRITE (6,*) "h written in sionfile on node=",idnode ,"; # elements",sierr
#endif              
!!!
          
!      create map of local bead numbers to molecule numbers
          ALLOCATE (localmolmap (nbeads), STAT=fail(1))
          IF (fail (1)/=0) CALL error (idnode, 1011, 5)
          CALL create_local_id_mol_map(nbeads, lab, molstart, localmolmap)

!      write bead information (including molecule numbers)
          DO i = 1, nbeads
            imol = localmolmap(i)
#ifdef STDTRAJ
            WRITE (nhist) lab (i), ltp (i), ltm (i), imol
#endif
!!! SIONlib 2i: write into SION file
            nelem=4
            size=4
            buffer_i (1:4) = (/ lab (i), ltp (i), ltm (i), imol /)
            call fsion_write(buffer_i,size,nelem,sid,sierr)
#ifdef DEBUG
            IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
            WRITE (6,*) "i written in sionfile on node=",idnode ,"; # elements",sierr
#endif
!!!            
         END DO

          DEALLOCATE (localmolmap, STAT=fail(1))
          IF (fail(1)/=0) CALL error (idnode, 1012, 2)

!      write bonds between beads
          IF (nbonds>0 .AND. ((.NOT. lgbnd) .OR. idnode==0)) THEN
            DO j = 1, nbonds
#ifdef STDTRAJ
               WRITE (nhist) bndtbl (j, 1), bndtbl (j, 2)
#endif
!!! SIONlib 2j: write into SION file
            nelem=2
            size=4
            buffer_i (1:2) = (/ bndtbl (j, 1), bndtbl (j, 2) /)
            call fsion_write(buffer_i,size,nelem,sid,sierr)
#ifdef DEBUG
            IF (sierr.ne.nelem) CALL error (idnode, 1501, INT (sierr - nelem))
            WRITE (6,*) "j written in sionfile on node=",idnode ,"; # elements",sierr
#endif
!!!            
           END DO
          END IF

        END IF

      END IF

      RETURN
      END SUBROUTINE start
