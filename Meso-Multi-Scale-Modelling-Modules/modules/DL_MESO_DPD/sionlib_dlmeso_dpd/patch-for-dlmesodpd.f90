diff --git a/dlmesodpd.f90 b/dlmesodpd.f90
index 062c26c..59b7a8b 100644
--- a/dlmesodpd.f90
+++ b/dlmesodpd.f90
@@ -191,6 +191,10 @@ PROGRAM dlmesodpd
 !     close files, deallocate arrays and close down MPI
 
       IF (ltraj) CLOSE (nhist)
+!!! SIONlib 3: close SIONlib file  
+      IF (ltraj) call fsion_parclose_mpi(sid,sierr)
+      WRITE (nprint, *) "sierr=", sierr , "on idnode=", idnode
+!!!
       CALL free_memory
       IF (.NOT. l_scr) CLOSE (nprint)
       CALL exitcomms ()
