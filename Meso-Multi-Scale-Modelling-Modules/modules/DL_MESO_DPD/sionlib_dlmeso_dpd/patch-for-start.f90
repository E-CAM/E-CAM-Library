diff --git a/start_module.f90 b/start_module.f90
index a7f0233..affd0c9 100644
--- a/start_module.f90
+++ b/start_module.f90
@@ -95,6 +95,9 @@ CONTAINS
       INTEGER :: fail (4)
       INTEGER, ALLOCATABLE :: localmolmap(:)
 
+!     SIONlib 0: set sionlib filename
+      filename = 'test_sionfile.sion'
+
 !     set restart filename
 
       WRITE (chan, '(i6.6)') idnode
@@ -296,6 +299,11 @@ CONTAINS
 
         IF (nstep>0) THEN
 
+!!! SIONlib 1a: give error for resart
+           WRITE (*,*) "Error: this version (using sionlib) does not support restart"
+           STOP
+!!!
+           
           IF (nodes>1) THEN
             OPEN (nhist, file='HISTORY'//chan, access = 'sequential', form = 'unformatted', status = 'unknown',&
                        & position = 'append')
@@ -306,21 +314,59 @@ CONTAINS
 
         ELSE
 
+!!! SIONlib 1b: define and open
+           gcomm=MPI_COMM_WORLD
+           lcomm=MPI_COMM_WORLD !added by SC
+           fsblksize=-1
+           chunksize=1 !2000000! too small?
+           nfiles=1
+           call fsion_paropen_mpi(trim(filename),'bw',nfiles, gComm,lComm, &
+                chunksize,fsblksize,idnode,newfname,sid)
+!           WRITE (6,*) "opened sionfile on node=",idnode ,"; sid=",sid
+!!!
+           
           IF (nodes>1) THEN
             OPEN (nhist, file='HISTORY'//chan, access = 'sequential', form = 'unformatted', status = 'unknown')
           ELSE
             OPEN (nhist, file='HISTORY', access = 'sequential', form = 'unformatted', status = 'unknown')
           END IF
 
-
           IF (lgbnd .AND. idnode>0) THEN
             WRITE (nhist) nspe, nmoldef, nusyst, nsyst, nbeads, 0
+!!! SIONlib 2a: write into SION file
+            nelem=6
+            size=4
+            buffer_i (1:6) = (/ nspe, nmoldef, nusyst, nsyst, nbeads, 0 /)
+            call fsion_write(buffer_i,size,nelem,sid,sierr)
+!            WRITE (6,*) "a written in sionfile on node=",idnode ,"; # elements",sierr
+!!!            
           ELSE
             WRITE (nhist) nspe, nmoldef, nusyst, nsyst, nbeads, nbonds
+!!! SIONlib 2b: write into SION file
+            nelem=6
+            size=4
+            buffer_i (1:6) = (/ nspe, nmoldef, nusyst, nsyst, nbeads, nbonds /)
+            call fsion_write(buffer_i,size,nelem,sid,sierr)
+!            WRITE (6,*) "b written in sionfile on node=",idnode ,"; # elements",sierr
+!!!            
          END IF
 
          WRITE (nhist) dimx, dimy, dimz, volm
+!!! SIONlib 2c: write into SION file
+         nelem=4
+         size=8
+         buffer_r (1:4) = (/ dimx, dimy, dimz, volm /)
+         call fsion_write(buffer_r,size,nelem,sid,sierr)
+!         WRITE (6,*) "c written in sionfile on node=",idnode ,"; # elements",sierr
+!!!            
          WRITE (nhist) keytrj, srftype*srfx, srftype*srfy, srftype*srfz
+!!! SIONlib 2d: write into SION file
+         nelem=4
+         size=4
+         buffer_i (1:4) = (/ keytrj, srftype*srfx, srftype*srfy, srftype*srfz /)
+         call fsion_write(buffer_i,size,nelem,sid,sierr)
+!         WRITE (6,*) "d written in sionfile on node=",idnode ,"; # elements",sierr
+!!!
        
 !      write species information
           DO i = 1, nspe
@@ -328,8 +374,46 @@ CONTAINS
             SELECT CASE (ktype (k))
             CASE (0:2)
               WRITE (nhist) namspe (i), amass (i), vvv (2, k), lfrzn (i)
+!!! SIONlib 2e: write into SION file
+              nelem=1
+              size=8
+              buffer_c = namspe (i) !could skip this line and use namspe into write
+              call fsion_write(buffer_c,size,nelem,sid,sierr)
+!              WRITE (6,*) "e1 written in sionfile on node=",idnode ,"; # elements",sierr
+              
+              nelem=2
+              size=8
+              buffer_r (1:2) = (/ amass (i), vvv (2, k) /)              
+              call fsion_write(buffer_r,size,nelem,sid,sierr)              
+!              WRITE (6,*) "e2 written in sionfile on node=",idnode ,"; # elements",sierr
+              
+              nelem=1
+              size=4
+              buffer_i (1) = lfrzn (i)
+              call fsion_write(buffer_i,size,nelem,sid,sierr)
+!              WRITE (6,*) "e3 written in sionfile on node=",idnode ,"; # elements",sierr
+!!!                        
            CASE (3)
               WRITE (nhist) namspe (i), amass (i), vvv (6, k), lfrzn (i)
+!!! SIONlib 2f: write into SION file
+              nelem=1
+              size=8
+              buffer_c = namspe (i) !could skip this line and use namspe into write
+              call fsion_write(buffer_c,size,nelem,sid,sierr)
+!              WRITE (6,*) "f1 written in sionfile on node=",idnode ,"; # elements",sierr
+              
+              nelem=2
+              size=8
+              buffer_r (1:2) = (/ amass (i), vvv (6, k) /)              
+              call fsion_write(buffer_r,size,nelem,sid,sierr)              
+!              WRITE (6,*) "f2 written in sionfile on node=",idnode ,"; # elements",sierr
+              
+              nelem=1
+              size=4
+              buffer_i (1) = lfrzn (i)
+              call fsion_write(buffer_i,size,nelem,sid,sierr)
+!              WRITE (6,*) "f3 written in sionfile on node=",idnode ,"; # elements",sierr
+!!!                        
            END SELECT
           END DO
 
@@ -337,11 +421,24 @@ CONTAINS
           IF (nmoldef>0) THEN
             DO i = 1, nmoldef
               WRITE (nhist) nammol (i)
+!!! SIONlib 2g: write into SION file
+              nelem=1
+              size=8
+              buffer_c = nammol (i) !could skip this line and use namspe into write
+              call fsion_write(buffer_c,size,nelem,sid,sierr)
+!              WRITE (6,*) "g written in sionfile on node=",idnode ,"; # elements",sierr
+!!!
            END DO
           END IF
 
 !      write name of calculation
           WRITE (nhist) text
+!!! SIONlib 2h: write into SION file
+              nelem=1
+              size=80
+              call fsion_write(text,size,nelem,sid,sierr)
+!              WRITE (6,*) "h written in sionfile on node=",idnode ,"; # elements",sierr
+!!!
           
 !      create map of local bead numbers to molecule numbers
           ALLOCATE (localmolmap (nbeads), STAT=fail(1))
@@ -352,6 +449,13 @@ CONTAINS
           DO i = 1, nbeads
             imol = localmolmap(i)
             WRITE (nhist) lab (i), ltp (i), ltm (i), imol
+!!! SIONlib 2i: write into SION file
+            nelem=4
+            size=4
+            buffer_i (1:4) = (/ lab (i), ltp (i), ltm (i), imol /)
+            call fsion_write(buffer_i,size,nelem,sid,sierr)
+!            WRITE (6,*) "i written in sionfile on node=",idnode ,"; # elements",sierr
+!!!            
          END DO
 
           DEALLOCATE (localmolmap, STAT=fail(1))
@@ -361,6 +465,13 @@ CONTAINS
           IF (nbonds>0 .AND. ((.NOT. lgbnd) .OR. idnode==0)) THEN
             DO j = 1, nbonds
               WRITE (nhist) bndtbl (j, 1), bndtbl (j, 2)
+!!! SIONlib 2j: write into SION file
+            nelem=2
+            size=4
+            buffer_i (1:2) = (/ bndtbl (j, 1), bndtbl (j, 2) /)
+            call fsion_write(buffer_i,size,nelem,sid,sierr)
+!            WRITE (6,*) "j written in sionfile on node=",idnode ,"; # elements",sierr
+!!!            
            END DO
           END IF
