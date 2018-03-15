diff --git a/dlmesodpd.f90 b/dlmesodpd.f90
index 062c26c..90600f2 100644
--- a/dlmesodpd.f90
+++ b/dlmesodpd.f90
@@ -189,8 +189,15 @@ PROGRAM dlmesodpd
       END IF
 
 !     close files, deallocate arrays and close down MPI
-
+#ifdef STDTRAJ
       IF (ltraj) CLOSE (nhist)
+#endif
+!!! SIONlib 3: close SIONlib file  
+      IF (ltraj) call fsion_parclose_mpi(sid,sierr)
+#ifdef DEBUG
+      WRITE (nprint, *) "sierr=", sierr , "on idnode=", idnode
+#endif
+!!!
       CALL free_memory
       IF (.NOT. l_scr) CLOSE (nprint)
       CALL exitcomms ()
