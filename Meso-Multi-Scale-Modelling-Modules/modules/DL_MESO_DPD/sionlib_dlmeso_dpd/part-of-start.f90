!***********************************************************************
!
!     patch for the subroutine start (set up starting configuration)
!
!     copyright stfc daresbury laboratory
!     authors - w. smith & m. a. seaton july 2015
!     adapted to use SIONlib: s. chiacchiera November 2017
!***********************************************************************
!     first part is unchanged
  
!     start data saving file and include species/molecule data

      IF (ltraj) THEN

        IF (nstep>0) THEN

           WRITE (*,*) "Error: this version (using sionlib) does not support restart"
           STOP

        ELSE

! SIONlib: define and open
           gcomm=MPI_COMM_WORLD
           lcomm=MPI_COMM_WORLD
           fsblksize=-1
           chunksize=1 
           nfiles=1
           call fsion_paropen_mpi(trim(filename),'bw',nfiles, gComm,lComm, &
                chunksize,fsblksize,idnode,newfname,sid)
           
          IF (lgbnd .AND. idnode>0) THEN
!            WRITE (nhist) nspe, nmoldef, nusyst, nsyst, nbeads, 0
            nelem=6
            size=4
            buffer_i (1:6) = (/ nspe, nmoldef, nusyst, nsyst, nbeads, 0 /)
            call fsion_write(buffer_i,size,nelem,sid,sierr)
          ELSE
!            WRITE (nhist) nspe, nmoldef, nusyst, nsyst, nbeads, nbonds
            nelem=6
            size=4
            buffer_i (1:6) = (/ nspe, nmoldef, nusyst, nsyst, nbeads, nbonds /)
            call fsion_write(buffer_i,size,nelem,sid,sierr)
         END IF

!         WRITE (nhist) dimx, dimy, dimz, volm
         nelem=4
         size=8
         buffer_r (1:4) = (/ dimx, dimy, dimz, volm /)
         call fsion_write(buffer_r,size,nelem,sid,sierr)
!         WRITE (nhist) keytrj, srftype*srfx, srftype*srfy, srftype*srfz
         nelem=4
         size=4
         buffer_i (1:4) = (/ keytrj, srftype*srfx, srftype*srfy, srftype*srfz /)
         call fsion_write(buffer_i,size,nelem,sid,sierr)
       
!      write species information
          DO i = 1, nspe
            k = (i * (i + 1)) / 2
            SELECT CASE (ktype (k))
            CASE (0:2)
!              WRITE (nhist) namspe (i), amass (i), vvv (2, k), lfrzn (i)
              nelem=1
              size=8
              buffer_c = namspe (i) 
              call fsion_write(buffer_c,size,nelem,sid,sierr)
              
              nelem=2
              size=8
              buffer_r (1:2) = (/ amass (i), vvv (2, k) /)              
              call fsion_write(buffer_r,size,nelem,sid,sierr)              
              
              nelem=1
              size=4
              buffer_i (1) = lfrzn (i)
              call fsion_write(buffer_i,size,nelem,sid,sierr)

           CASE (3)
!              WRITE (nhist) namspe (i), amass (i), vvv (6, k), lfrzn (i)
              nelem=1
              size=8
              buffer_c = namspe (i) 
              call fsion_write(buffer_c,size,nelem,sid,sierr)

              nelem=2
              size=8
              buffer_r (1:2) = (/ amass (i), vvv (6, k) /)              
              call fsion_write(buffer_r,size,nelem,sid,sierr)              
              
              nelem=1
              size=4
              buffer_i (1) = lfrzn (i)
              call fsion_write(buffer_i,size,nelem,sid,sierr)

           END SELECT
          END DO

!      write molecule names
          IF (nmoldef>0) THEN
            DO i = 1, nmoldef
!              WRITE (nhist) nammol (i)
              nelem=1
              size=8 
              buffer_c = nammol (i) 
              call fsion_write(buffer_c,size,nelem,sid,sierr)
           END DO
          END IF

!      write name of calculation
!          WRITE (nhist) text
              nelem=1
              size=80 
              call fsion_write(text,size,nelem,sid,sierr)
          
!      create map of local bead numbers to molecule numbers
          ALLOCATE (localmolmap (nbeads), STAT=fail(1))
          IF (fail (1)/=0) CALL error (idnode, 1011, 5)
          CALL create_local_id_mol_map(nbeads, lab, molstart, localmolmap)

!      write bead information (including molecule numbers)
          DO i = 1, nbeads
            imol = localmolmap(i)
!            WRITE (nhist) lab (i), ltp (i), ltm (i), imol
            nelem=4
            size=4
            buffer_i (1:4) = (/ lab (i), ltp (i), ltm (i), imol /)
            call fsion_write(buffer_i,size,nelem,sid,sierr)
         END DO

          DEALLOCATE (localmolmap, STAT=fail(1))
          IF (fail(1)/=0) CALL error (idnode, 1012, 2)

!      write bonds between beads
          IF (nbonds>0 .AND. ((.NOT. lgbnd) .OR. idnode==0)) THEN
            DO j = 1, nbonds
!              WRITE (nhist) bndtbl (j, 1), bndtbl (j, 2)
            nelem=2
            size=4
            buffer_i (1:2) = (/ bndtbl (j, 1), bndtbl (j, 2) /)
            call fsion_write(buffer_i,size,nelem,sid,sierr)
           END DO
          END IF

        END IF

      END IF

      RETURN
      END SUBROUTINE start
