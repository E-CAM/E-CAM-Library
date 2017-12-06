diff --git a/statistics_module.f90 b/statistics_module.f90
index e2d5e79..c5d337e 100644
--- a/statistics_module.f90
+++ b/statistics_module.f90
@@ -623,18 +623,40 @@ CONTAINS
 !     write out data
 
       WRITE (nhist) time, REAL (nbeads, KIND=dp), dimx, dimy, dimz, shrdx, shrdy, shrdz
+!!! SIONlib 2k: write into SION file
+      nelem=8
+      size=8
+      buffer_r (1:8) = (/ time, REAL (nbeads, KIND=dp), dimx, dimy, dimz, shrdx, shrdy, shrdz /)
+      call fsion_write(buffer_r,size,nelem,sid,sierr)
+!      WRITE (6,*) "k written in sionfile on node=",idnode ,"; # elements",sierr
+!!!             
 
       SELECT CASE (keytrj)
       CASE (0)
         ! positions
         DO i = 1, nbeads
           WRITE (nhist) REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz
+!!! SIONlib 2l: write into SION file
+          nelem=4
+          size=8
+          buffer_r (1:4) = (/ REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz /)
+          call fsion_write(buffer_r,size,nelem,sid,sierr)
+!          WRITE (6,*) "l written in sionfile on node=",idnode ,"; # elements",sierr
+!!!             
         END DO
       CASE (1)
         ! positions and velocities
         DO i = 1, nbeads
           WRITE (nhist) REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
                &vxx (i), vyy (i), vzz (i)
+!!! SIONlib 2m: write into SION file
+          nelem=7
+          size=8
+          buffer_r (1:7) = (/ REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
+               &vxx (i), vyy (i), vzz (i) /)
+          call fsion_write(buffer_r,size,nelem,sid,sierr)
+!          WRITE (6,*) "m written in sionfile on node=",idnode ,"; # elements",sierr
+!!!          
         END DO
       CASE (2)
         ! positions, velocities and forces
@@ -642,11 +664,27 @@ CONTAINS
           DO i = 1, nbeads
             WRITE (nhist) REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
                  &vxx (i), vyy (i), vzz (i), (fxx(i)+fvx(i)), (fyy(i)+fvy(i)), (fzz(i)+fvz(i))
+!!! SIONlib 2n: write into SION file
+            nelem=10
+            size=8
+            buffer_r (1:10) = (/ REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
+                 &vxx (i), vyy (i), vzz (i), (fxx(i)+fvx(i)), (fyy(i)+fvy(i)), (fzz(i)+fvz(i))  /)
+            call fsion_write(buffer_r,size,nelem,sid,sierr)
+!            WRITE (6,*) "n written in sionfile on node=",idnode ,"; # elements",sierr
+!!!                         
           END DO
         ELSE
           DO i = 1, nbeads
             WRITE (nhist) REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
                  &vxx (i), vyy (i), vzz (i), fxx (i), fyy (i), fzz (i)
+!!! SIONlib 2p: write into SION file
+            nelem=10
+            size=8
+            buffer_r (1:10) = (/ REAL (lab(i), KIND=dp), xxx (i) + delx, yyy (i) + dely, zzz (i) + delz, &
+                         &vxx (i), vyy (i), vzz (i), fxx (i), fyy (i), fzz (i)  /)
+            call fsion_write(buffer_r,size,nelem,sid,sierr)
+!            WRITE (6,*) "p written in sionfile on node=",idnode ,"; # elements",sierr
+!!!                         
          END DO
         END IF
       END SELECT
