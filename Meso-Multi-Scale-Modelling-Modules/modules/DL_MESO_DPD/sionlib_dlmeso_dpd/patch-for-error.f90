diff --git a/error_module.f90 b/error_module.f90
index 8da34eb..21274b7 100644
--- a/error_module.f90
+++ b/error_module.f90
@@ -608,6 +608,9 @@ CONTAINS
 
         IF (idnode==0) CLOSE (nprint)
         CLOSE (nhist)
+!!! SIONlib: close file
+        IF (ltraj) call fsion_parclose_mpi(sid,sierr)
+!!!
         IF (idnode==0) CLOSE (nsave)
 
 !     shut down MPI and stop program
