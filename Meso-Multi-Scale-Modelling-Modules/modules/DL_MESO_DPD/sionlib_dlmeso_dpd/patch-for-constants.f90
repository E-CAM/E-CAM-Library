diff --git a/constants.f90 b/constants.f90
index 65bbee4..82c2641 100644
--- a/constants.f90
+++ b/constants.f90
@@ -13,5 +13,7 @@ MODULE constants
       REAL(KIND=dp), PARAMETER :: fkt=2.0_dp/3.0_dp
       REAL(KIND=dp), PARAMETER :: rt12=3.464101615377546_dp
       REAL(KIND=dp), PARAMETER :: langepsilon=1.0e-6_dp
+!     for SIONlib
+      INTEGER, PARAMETER :: nsion = 13
       
 END MODULE
