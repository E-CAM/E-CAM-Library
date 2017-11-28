!     variables needed by SIONlib 
      INTEGER*8 sierr
      CHARACTER(len=255) :: filename 
      CHARACTER(len=255) :: newfname
      INTEGER:: nfiles
      INTEGER:: gComm, lComm, sid
      INTEGER:: fsblksize
      INTEGER*8 :: chunksize
      INTEGER*8 :: size, nelem
      INTEGER :: seof 
      INTEGER :: buffer_i(6)
      REAL(KIND=dp) :: buffer_r(10)
      CHARACTER(LEN=8) :: buffer_c
