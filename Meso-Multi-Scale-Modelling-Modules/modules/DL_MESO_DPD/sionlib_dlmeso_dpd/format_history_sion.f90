PROGRAM format_history_sion
!***********************************************************************************
!
! module to format dl_meso HISTORY files written using SIONlib library
!
! authors - m. a. seaton & s. chiacchiera, february 2017
! adapted to use SIONlib: march 2018
!**********************************************************************************
  
IMPLICIT none
INCLUDE "mpif.h"
  
      INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND (15, 307)
      INTEGER, PARAMETER :: ntraj=10,nuser=5

      CHARACTER(80) :: text
      CHARACTER(8), ALLOCATABLE :: namspe (:), nammol (:)
      CHARACTER(6) :: chan
      
      INTEGER, ALLOCATABLE :: ltp (:), ltm (:), mole (:),  beads (:), bonds (:), bndtbl (:,:)
      INTEGER, ALLOCATABLE :: nbdmol (:), nbomol (:)
      INTEGER :: chain, imol, ioerror, i, k, j, nmoldef, ibond
      INTEGER :: nspe, nbeads, nusyst, nsyst, nbonds, global, species, molecule, numnodes, numbond
      INTEGER :: nummol, lfrzn, rnmol, keytrj, srfx, srfy, srfz
      INTEGER :: bead1, bead2
      INTEGER :: nform
      
      REAL(KIND=dp), ALLOCATABLE :: nmol (:)
      REAL(KIND=dp) :: volm, dimx, dimy, dimz, shrdx, shrdy, shrdz
      REAL(KIND=dp) :: amass, rcii
      REAL(KIND=dp) :: time, mbeads, mglobal, x, y, z, vx, vy, vz, fx, fy, fz
      
      LOGICAL :: eof, lcomm, lmcheck
!     for SIONlib
      CHARACTER*(*) :: fname, file_mode
      INTEGER :: numfiles, ntasks, fsblksize, sid
      INTEGER, ALLOCATABLE :: globalranks (:)
      INTEGER*8, ALLOCATABLE :: chunksizes (:)
      INTEGER*8 :: chunksize_input
      INTEGER*8 :: sierr
      INTEGER :: nformsion
      INTEGER*8 :: size, nelem
      INTEGER :: buffer_i(6)
      REAL(KIND=dp) :: buffer_r(10)
      CHARACTER(LEN=8) :: buffer_c
      INTEGER :: rank, chunknum
      INTEGER*8 :: posinchunk
      INTEGER*8, ALLOCATABLE :: pos_d (:)
      INTEGER, ALLOCATABLE :: chun_d (:)
      INTEGER :: seof
      PARAMETER (fname = 'history.sion')
      PARAMETER (file_mode= 'br')
      
      ! Switches for commenting and checking molecules
      
      lcomm =   .TRUE.
      lmcheck = .TRUE.
      
      ! Get number of nodes 

      WRITE (*,*) "Number of nodes used in calculations ?"
      READ (*,*) numnodes

      ! Get chunksize used to write
      WRITE (*,*) "Chunksize used to write history.sion?"
      READ (*,*) chunksize_input
      
      ALLOCATE (beads (numnodes), bonds (numnodes))

! SIONlib: Determine if history.sion file exists
      INQUIRE (file = fname, EXIST = eof)
      IF (.NOT. eof) THEN
         WRITE (*,*) "ERROR: cannot find history.sion file"
         STOP
      END IF

! SIONlib: serial open
      numfiles = 1
      fsblksize = -1
      ALLOCATE (chunksizes (numnodes), globalranks (numnodes)) 
      chunksizes (:) = -1
      globalranks (:) = -1
      call FSION_OPEN (fname, file_mode, ntasks, numfiles, &
           chunksizes, fsblksize, globalranks, sid)
      IF(ntasks.ne.numnodes) THEN
         WRITE (6,*) "Number of tasks used to write is different from given! -", ntasks   
         STOP
      END IF
!      WRITE(6,*) "chunksizes=", chunksizes !not read as it should. Why?
      WRITE(6,*) "fsblksize=", fsblksize
!      WRITE(6,*) "globalranks=",globalranks !not read as it should. Why?
      WRITE(6,*) "sid=", sid
      ! Set *by hand* the values of chunksizes and globalranks
      DO j = 1, ntasks
         globalranks (j) = j-1
         chunksizes (j) = 0
         DO WHILE (chunksizes (j) < chunksize_input) 
            chunksizes (j) = chunksizes (j) + fsblksize
         END DO
      END DO
      WRITE(6,*) "(set by hand) chunksizes=", chunksizes
      WRITE(6,*) "(set by hand) globalranks=", globalranks

!     variables to track positions within the .sion file      
      ALLOCATE (pos_d (numnodes), chun_d (numnodes))      
      
      ! Open the output files
      nform = ntraj + numnodes
      nformsion = nform + numnodes       
      DO j = 1, numnodes
         IF (numnodes>1)THEN
            WRITE (chan, '(i6.6)') j-1
            OPEN (nformsion+j-1, file = 'sion'//chan//'-F', status = 'replace')
         ELSE
            OPEN (nformsion+j-1, file = 'sion-F', status = 'replace')
         END IF
      END DO
      
! SIONlib:  reading the header of history.sion
      ! Here the number of beads, molecules and bonds are determined
      ! Arrays are filled with names of particles and molecules

      numbond = 0

      DO j = 1, numnodes
         seof = 0
         call fsion_feof (sid, seof)
         IF (seof /= 0) THEN
#ifdef DEBUG
            WRITE (6,*) "rank ", j-1, ": End of file !"
#endif
            CYCLE
         END IF
         
         rank = j - 1
         chunknum = 0
         posinchunk = 0
         CALL FSION_SEEK (sid, rank, chunknum, posinchunk, sierr)
         
!     lines a and b
         nelem=6
         size=4
         buffer_i (1:6) = 0
         CALL FSION_READ(buffer_i,size,nelem,sid,sierr)
#ifdef DEBUG
         WRITE (6,*) "(a/b) in sion file, rank ", rank, ": buffer_i=",buffer_i
         CALL READ_CHECK (sierr, nelem)
#endif
         CALL DETERMINE_POS (rank, nelem*size, chunknum, posinchunk)
         IF (j==1) THEN
            nspe = buffer_i (1)
            nmoldef = buffer_i (2)
            nusyst = buffer_i (3)
            nsyst = buffer_i (4)
         END IF
         nbeads = buffer_i (5)
         nbonds = buffer_i (6)
         !     line c
         nelem=4
         size=8
         buffer_r (1:4) = 0
         CALL FSION_READ(buffer_r,size,nelem,sid,sierr)
#ifdef DEBUG
         WRITE (6,*) "(c) in sion file, rank ", rank, ": buffer_r(1:4)=",buffer_r(1:4)
         CALL READ_CHECK (sierr, nelem)
#endif
         CALL DETERMINE_POS (rank, nelem*size, chunknum, posinchunk)
         IF (j==1) THEN
            dimx = buffer_r (1)
            dimy = buffer_r (2)
            dimz = buffer_r (3)
            volm = buffer_r (4)
         END IF
         !     line d
         nelem=4
         size=4
         buffer_i (1:4) = 0
         CALL FSION_READ(buffer_i,size,nelem,sid,sierr)
#ifdef DEBUG
         WRITE (6,*) "(d) in sion file, rank ", rank, ": buffer_i(1:4)=",buffer_i(1:4)
         CALL READ_CHECK (sierr, nelem)
#endif
         CALL DETERMINE_POS (rank, nelem*size, chunknum, posinchunk) 
         IF (j==1) THEN
            keytrj = buffer_i (1)
            srfx = buffer_i (2)
            srfy = buffer_i (3)
            srfz = buffer_i (4)
         END IF
         beads (j) = nbeads
         bonds (j) = nbonds
         numbond = numbond + nbonds
         
         IF (lcomm) WRITE (nformsion+j-1,*) "# nspe, nmoldef, nusyst, nsyst, nbeads, nbonds"
         WRITE (nformsion+j-1,*) nspe, nmoldef, nusyst, nsyst, nbeads, nbonds
         IF (lcomm) WRITE (nformsion+j-1,*) "# dimx, dimy, dimz, volm"
         WRITE (nformsion+j-1,97) dimx, dimy, dimz, volm
         IF (lcomm) WRITE (nformsion+j-1,*) "# keytrj, srfx, srfy, srfz"
         WRITE (nformsion+j-1,*) keytrj, srfx, srfy, srfz

         chun_d (j) = chunknum
         pos_d (j) = posinchunk
      END DO ! end of loop over nodes
!!!
      ALLOCATE (namspe (nspe), nammol (nmoldef))
      IF (lmcheck) THEN 
         ALLOCATE (ltp (1:nsyst), ltm (1:nsyst), mole (1:nsyst))
         ALLOCATE (nmol (1:nmoldef), nbdmol (1:nmoldef), nbomol (1:nmoldef))
         ALLOCATE (bndtbl (numbond, 2))
      ENDIF
      
      DO j = 1, numnodes
         rank = j - 1
         chunknum = chun_d (j)
         posinchunk = pos_d (j)
         CALL FSION_SEEK (sid, rank, chunknum, posinchunk, sierr)
!!! 
         IF (lcomm) WRITE (nformsion+j-1,*) "# SPECIES:"
         IF (lcomm) WRITE (nformsion+j-1,*) "# namspe, amass, rcii, lfrzn"
         
         DO i = 1, nspe
            !     line e
            nelem = 1
            size = 8
            buffer_c = '        '
            CALL FSION_READ(buffer_c,size,nelem,sid,sierr)
#ifdef DEBUG
            WRITE (6,*) "(e1) in sion file, rank ", rank, ": buffer_c=",buffer_c
            CALL READ_CHECK (sierr, nelem)
#endif               
            CALL DETERMINE_POS (rank, nelem*size, chunknum, posinchunk) 
            IF (j==1) THEN
               namspe (i) = buffer_c
            END IF
               
            nelem=2
            size=8
            buffer_r (1:2) = 0
            CALL FSION_READ(buffer_r,size,nelem,sid,sierr)
#ifdef DEBUG
            WRITE (6,*) "(e2) in sion file, rank ", rank, ": buffer_r (1:2)=",buffer_r (1:2)
            CALL READ_CHECK (sierr, nelem)
#endif
            CALL DETERMINE_POS (rank, nelem*size, chunknum, posinchunk) 
            amass = buffer_r (1)
            rcii = buffer_r (2)
            
            nelem=1
            size=4
            buffer_i (1) = 0
            CALL FSION_READ(buffer_i,size,nelem,sid,sierr)
#ifdef DEBUG
            WRITE (6,*) "(e3) in sion file, rank ", rank, ": buffer_i (1)=",buffer_i (1)
            CALL READ_CHECK (sierr, nelem)
#endif
            CALL DETERMINE_POS (rank, nelem*size, chunknum, posinchunk) 
            lfrzn = buffer_i (1)
            WRITE (nformsion+j-1,96) namspe (i), amass, rcii, lfrzn
         END DO
         
         IF (nmoldef>0) THEN
            IF (lcomm) WRITE (nformsion+j-1,*) "# MOLECULES:"
            IF (lcomm) WRITE (nformsion+j-1,*) "# nammol"

            DO i = 1, nmoldef
               !     line g
               nelem=1
               size=8
               buffer_c = '        '
               CALL FSION_READ(buffer_c,size,nelem,sid,sierr)
               IF (j==1) THEN
                  nammol (i) = buffer_c
               END IF
#ifdef DEBUG
               WRITE (6,*) "(g) in sion file, rank ", rank, ": buffer_c=",buffer_c
               CALL READ_CHECK (sierr, nelem)
#endif
               CALL DETERMINE_POS (rank, nelem*size, chunknum, posinchunk) 
               WRITE (nformsion+j-1,*) nammol (i)
            END DO
         END IF

         !     line h
         nelem=1
         size=80
         text = '                                                                                '
         CALL FSION_READ(text,size,nelem,sid,sierr)
#ifdef DEBUG
         WRITE (6,*) "(h) in sion file, rank ", rank, ": text=", text
         CALL READ_CHECK (sierr, nelem)
#endif
         CALL DETERMINE_POS (rank, nelem*size, chunknum, posinchunk) 
         
         IF (lcomm) WRITE (nformsion+j-1,*) "# Simulation name:"
         WRITE (nformsion+j-1,*) text

         IF (j==1) THEN
            nummol = 0 !counter for number of molecules      
            ibond = 0  !counter for bonds
         END IF

         ! Here one could close and open again the loop over nodes, as in the std 
         ! version of the utility: in case, pos_d and chunk_d must be updated too
         
      !     fill in arrays for beads and bonds
         rank = j - 1

         IF (lcomm) WRITE (nformsion+j-1,*) "# BEADS:"
         IF (lcomm) WRITE (nformsion+j-1,*) "# global, species, molecule, chain"

         IF (lmcheck) THEN
            !Build ltp, ltm, mole
            DO i = 1, beads (j)
               !     line i
               nelem = 4
               size=4
               buffer_i (1:4) = 0
               CALL FSION_READ(buffer_i,size,nelem,sid,sierr)
#ifdef DEBUG
               WRITE (6,*) "(i) in sion file, rank ", rank, ": buffer_i(1:4)=",buffer_i(1:4)
               CALL READ_CHECK (sierr, nelem)
#endif 
               CALL DETERMINE_POS (rank, nelem*size, chunknum, posinchunk) 

               global = buffer_i (1)
               species = buffer_i (2)
               molecule = buffer_i (3)
               chain = buffer_i (4)

               ltp (global) = species
               ltm (global) = molecule
               mole (global) = chain
               nummol = MAX (nummol, chain)
               WRITE (nformsion+j-1,*) global, species, molecule, chain 
            END DO
         ELSE
            DO i = 1, beads (j)
               !     line i
               nelem = 4
               size=4
               buffer_i (1:4) = 0
               CALL FSION_READ(buffer_i,size,nelem,sid,sierr)
#ifdef DEBUG
               WRITE (6,*) "(i) in sion file, rank ", rank, ": buffer_i(1:4)=",buffer_i(1:4)
               CALL READ_CHECK (sierr, nelem)
#endif
               CALL DETERMINE_POS (rank, nelem*size, chunknum, posinchunk) 
               global = buffer_i (1)
               species = buffer_i (2)
               molecule = buffer_i (3)
               chain = buffer_i (4)
               WRITE (nformsion+j-1,*) global, species, molecule, chain 
            END DO
         ENDIF
         
         IF (bonds (j)>0) THEN
            IF (lcomm) WRITE (nformsion+j-1,*) "# BONDS:"
            IF (lcomm) WRITE (nformsion+j-1,*) "# extremes of the bond"

            IF (lmcheck) THEN
               ! Build bndtbl
               DO i = 1, bonds (j)
                  ibond = ibond + 1
                  !     line j
                  nelem=2
                  size=4
                  buffer_i (1:2) = 0 
                  CALL FSION_READ(buffer_i,size,nelem,sid,sierr)
#ifdef DEBUG
                  WRITE (6,*) "(j) in sion file, rank ", rank, ": buffer_i(1:2)=",buffer_i(1:2)
                  CALL READ_CHECK (sierr, nelem)
#endif
                  CALL DETERMINE_POS (rank, nelem*size, chunknum, posinchunk) 
                  bead1 = buffer_i (1)
                  bead2 = buffer_i (2)
                  bndtbl (ibond, 1) = bead1
                  bndtbl (ibond, 2) = bead2
                  WRITE (nformsion+j-1,*) bead1, bead2
               END DO
            ELSE
               DO i = 1, bonds (j)
                  !     line j
                  nelem=2
                  size=4
                  buffer_i (1:2) = 0 
                  CALL FSION_READ(buffer_i,size,nelem,sid,sierr)
#ifdef DEBUG
                  WRITE (6,*) "(j) in sion file, rank ", rank, ": buffer_i(1:2)=",buffer_i(1:2)
                  CALL READ_CHECK (sierr, nelem)
#endif
                  CALL DETERMINE_POS (rank, nelem*size, chunknum, posinchunk) 
                  bead1 = buffer_i (1)
                  bead2 = buffer_i (2)
                  WRITE (nformsion+j-1,*) bead1, bead2
               END DO
            END IF
         END IF
         chun_d (j) = chunknum
         pos_d (j) = posinchunk
         
      END DO ! over nodes

      IF (lmcheck) THEN
      ! determine numbers of molecules, beads and bonds per molecule type
         nmol = 0.0_dp
         nbdmol = 0
         nbomol = 0 
         chain = 0
         imol = 0 !necessary to avoid out of bounds
         
         DO i = 1, nsyst
            IF (mole (i) /= chain) THEN
               chain = mole (i)
               imol = ltm (i)
               nmol (imol) = nmol (imol) + 1.0_dp
            END IF
            IF (imol > 0) nbdmol (imol) = nbdmol (imol) + 1
         END DO

         DO i = 1, numbond
            imol = ltm (bndtbl (i,1))
            nbomol (imol) = nbomol (imol) + 1
         END DO
            
         DO i = 1, nmoldef
            rnmol = NINT (nmol (i))
            IF (rnmol>0) THEN
               nbdmol (i) = nbdmol (i) / rnmol
               nbomol (i) = nbomol (i) / rnmol
            END IF
         END DO

         ! Write to std output the arrays built
         WRITE (*,*) "# Check of beads: i, ltp(i), ltm(i), mole(i)"
         DO i = 1, nsyst
            WRITE(*,*) i, ltp (i), ltm (i), mole (i)
         END DO

         !Check of molecule beads and numbers
         IF (nmoldef>0) THEN
            WRITE (*,*) "# Check of molecules: nammol(i), nbdmol(i), nbomol(i), nmol(i)"
            DO i = 1, nmoldef
               WRITE (*,*) nammol (i), nbdmol (i), nbomol (i), NINT(nmol(i))
            END DO
            WRITE (*,*) "# Total number of molecules = ",nummol
         END IF

         ! Write to std output bndtbl
         IF (numbond > 0) THEN
            WRITE (*,*) "# Check of bonds: bndbtl(i,1), bndbtl(i,2)"
            DO i = 1, numbond
               WRITE (*,*) bndtbl (i,1), bndtbl (i,2)
            END DO
         END IF
      END IF

      !reading trajectories 
      DO j = 1, numnodes

         seof = 0
         k = 0
         
         IF (lcomm) WRITE (nformsion+j-1,*) "# --- TRAJECTORIES --- (key =", keytrj,")"
         SELECT CASE (keytrj)
         CASE (0)
            IF (lcomm) WRITE (nformsion+j-1,*) "# mglobal, x, y, z"
         CASE(1)
            IF (lcomm) WRITE (nformsion+j-1,*) "# mglobal, x, y, z, vx, vy, vz"
         CASE(2) 
            IF (lcomm) WRITE (nformsion+j-1,*) "# mglobal, x, y, z, vx, vy, vz, fx, fy, fz"
         END SELECT
         
         rank = j - 1
         chunknum = chun_d (j)
         posinchunk = pos_d (j)         
         CALL FSION_SEEK (sid, rank, chunknum, posinchunk, sierr)
!!!         
         DO WHILE (.true.)

            call fsion_feof (sid, seof)
            IF (seof /= 0) THEN
#ifdef DEBUG
               WRITE (6,*) "rank ", rank, ": End of file !"
#endif
               IF (k==0) THEN
                  PRINT *, 'ERROR: cannot find trajectory data in history.sion file'
                  STOP
               END IF
               EXIT
            END IF
            
            ! line k 
            nelem=8
            size=8
            buffer_r (1:8) = 0
            CALL FSION_READ(buffer_r,size,nelem,sid,sierr)
#ifdef DEBUG
            WRITE (6,*) "(k) in sion file, rank ", rank, ": buffer_r(1:8)=",buffer_r(1:8)
            CALL READ_CHECK (sierr, nelem)
#endif
            time = buffer_r (1)
            mbeads = buffer_r (2)
            dimx = buffer_r (3)
            dimy = buffer_r (4)
            dimz = buffer_r (5)
            shrdx = buffer_r (6)
            shrdy = buffer_r (7)
            shrdz = buffer_r (8)
            !
            IF (lcomm) WRITE (nformsion+j-1,*) "# time, mbeads, dimx, dimy, dimz, shrdx, shrdy, shrdz"
            WRITE (nformsion+j-1,98) time, mbeads, dimx, dimy, dimz, shrdx, shrdy, shrdz
                        
            k = k + 1
            
            IF (lcomm) WRITE (nformsion+j-1,*) "# snapshot number:", k
            
            nbeads = NINT (mbeads)
            
            SELECT CASE (keytrj)
            CASE (0)
               DO i = 1, nbeads
                  ! line l
                  nelem=4
                  size=8
                  buffer_r (1:4) = 0
                  CALL FSION_READ(buffer_r,size,nelem,sid,sierr)
#ifdef DEBUG
                  WRITE (6,*) "(l) in sion file, rank ", rank, ": buffer_r(1:4)=",buffer_r(1:4)
                  CALL READ_CHECK (sierr, nelem)
#endif
                  mglobal = buffer_r (1)
                  x = buffer_r (2)
                  y = buffer_r (3)
                  z = buffer_r (4)
                  !
                  WRITE (nformsion+j-1,99) mglobal, x, y, z
               END DO
            CASE (1)
               DO i = 1, nbeads
                  ! line m
                  nelem=7
                  size=8
                  buffer_r (1:7) = 0
                  CALL FSION_READ(buffer_r,size,nelem,sid,sierr)
#ifdef DEBUG
                  WRITE (6,*) "(m) in sion file, rank ", rank, ": buffer_r(1:7)=",buffer_r(1:7)
                  CALL READ_CHECK (sierr, nelem)
#endif
                  mglobal = buffer_r (1)
                  x = buffer_r (2)
                  y = buffer_r (3)
                  z = buffer_r (4)
                  vx = buffer_r (5)
                  vy = buffer_r (6)
                  vz = buffer_r (7)
                  !
                  WRITE (nformsion+j-1,99) mglobal, x, y, z, vx, vy, vz
               END DO
            CASE (2)
               DO i = 1, nbeads
                  ! line n and p
                  nelem=10
                  size=8
                  buffer_r (1:10) = 0
                  CALL FSION_READ(buffer_r,size,nelem,sid,sierr)
#ifdef DEBUG
                  WRITE (6,*) "(l) in sion file, rank ", rank, ": buffer_r(1:10)=",buffer_r(1:10)
                  CALL READ_CHECK (sierr, nelem)
#endif
                  mglobal = buffer_r (1)
                  x = buffer_r (2)
                  y = buffer_r (3)
                  z = buffer_r (4)
                  vx = buffer_r (5)
                  vy = buffer_r (6)
                  vz = buffer_r (7)
                  fx = buffer_r (8)
                  fy = buffer_r (9)
                  fz = buffer_r (10)
                  !
                  WRITE (nformsion+j-1,99) mglobal, x, y, z, vx, vy, vz, fx, fy, fz
               END DO
            END SELECT
            
         END DO
      END DO
      
      ! close the output files
      DO j = 1, numnodes
         CLOSE (nformsion+j-1)
      END DO

! SIONlib serial close
      call FSION_CLOSE (sid, sierr)
      CLOSE (nformsion)

#ifdef DEBUG
      WRITE (*,*) "The final chunknumbers and positions within chunks are:"
      WRITE (*,*) "chun_d=", chun_d
      WRITE (*,*) "pos_d=", pos_d
#endif
      
      DEALLOCATE (beads, bonds)
      DEALLOCATE (namspe, nammol)
      IF (lmcheck) DEALLOCATE (ltp, ltm, mole, nmol, nbdmol, bndtbl, nbomol)
! SIONlib related quantities
      DEALLOCATE (chunksizes, globalranks, pos_d, chun_d)

99    FORMAT(f10.1,2x,1p,9(e13.6,3x))
98    FORMAT(8(f10.3,3x))
97    FORMAT(4(f10.3,3x))
96    FORMAT(A9,3x,2(f10.3,3x),I2)

    CONTAINS

      SUBROUTINE DETERMINE_POS (rank, nbytes, chunk, pos)
!***********************************************************************************
! routine to determine the position in the .sion file at each write statement 
!
! author - s. chiacchiera, march 2018
!***********************************************************************************
! It is assumed that each rank has its chunks numbered as 0, 1, etc
        IMPLICIT none
        INTEGER, INTENT (OUT) :: rank
        INTEGER, INTENT (INOUT) :: chunk
        INTEGER*8, INTENT (INOUT) :: pos
        INTEGER*8, INTENT(IN) :: nbytes
        INTEGER*8 :: pos_try
        IF (nbytes > chunksizes (rank + 1)) THEN
           WRITE (*,*) "error: too large record (hint: increase chunksize)"
           STOP
        END IF
        pos_try = pos + nbytes
        IF (pos_try <= chunksizes (rank + 1)) THEN 
           pos = pos_try
        ELSE
           chunk = chunk + 1
           pos = nbytes
        END IF
        RETURN
      END SUBROUTINE DETERMINE_POS

      SUBROUTINE READ_CHECK (sierr, nelem)
!***********************************************************************************
! routine to signal eventual mismatch when reading the .sion file
!
! author - s. chiacchiera, march 2018
!***********************************************************************************

        IMPLICIT none
        INTEGER*8, INTENT (IN) :: sierr, nelem
        IF (sierr.ne.nelem) THEN
           WRITE (6,*) "error: number of elements read differs from expected! mismatch is ", sierr-nelem
           STOP
        END IF
      END SUBROUTINE READ_CHECK
     
END PROGRAM format_history_sion
