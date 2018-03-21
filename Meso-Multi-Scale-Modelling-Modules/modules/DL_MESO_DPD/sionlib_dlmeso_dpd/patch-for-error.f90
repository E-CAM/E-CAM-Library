diff --git a/error_module.f90 b/error_module.f90
index cb19b28..f8c3c3b 100644
--- a/error_module.f90
+++ b/error_module.f90
@@ -589,6 +589,11 @@ CONTAINS
         CASE (1198)
           WRITE (nprint,"(/,1x,'error: deallocation failure in field_module -> plcfor_stoyanov')")
 
+!      sionlib          
+       CASE (1500) 
+          WRITE (nprint,"(/,1x,'error: this version (using sionlib) does not support restart')")
+       CASE (1501)
+          WRITE (nprint,"(/,1x,'error: problem in writing SIONfile, mismatched number of items',i10)") value
           
         CASE DEFAULT
           WRITE (nprint,"(/,1x,'error: undefined error code found')") 
@@ -605,7 +610,12 @@ CONTAINS
 !     close all i/o channels
 
         IF (idnode==0) CLOSE (nprint)
+#ifdef STDTRAJ
         CLOSE (nhist)
+#endif
+!!! SIONlib: close file
+        IF (ltraj) call fsion_parclose_mpi(sid,sierr)
+!!!
         IF (idnode==0) CLOSE (nsave)
 
 !     shut down MPI and stop program
