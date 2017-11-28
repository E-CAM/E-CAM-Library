PROGRAM format_history_sion
!***********************************************************************************
!
! module to format dl_meso HISTORY files written using SIONlib library
!
! authors - m. a. seaton & s. chiacchiera, february 2017
! adapted to use SIONlib: november 2017
!**********************************************************************************
  
IMPLICIT none
INCLUDE "mpif.h"
  
      INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND (15, 307)
      INTEGER, PARAMETER :: ntraj=10,nuser=5

      CHARACTER(80) :: text, a2
      CHARACTER(8), ALLOCATABLE :: namspe (:), nammol (:)
      CHARACTER(6) :: chan
      CHARACTER(8) :: a1
      
      INTEGER, ALLOCATABLE :: ltp (:), ltm (:), mole (:),  beads (:), bonds (:), bndtbl (:,:)
      INTEGER, ALLOCATABLE :: nbdmol (:), nbomol (:)
      INTEGER :: chain, imol, ioerror, i, k, j, nmoldef, ibond
      INTEGER :: nspe, nbeads, nusyst, nsyst, nbonds, global, species, molecule, numnodes, numbond
      INTEGER :: nummol, lfrzn, rnmol, keytrj, srfx, srfy, srfz
      INTEGER :: bead1, bead2
      INTEGER :: n1, n2, n3, n4
      INTEGER :: nform
      
      REAL(KIND=dp), ALLOCATABLE :: nmol (:)
      REAL(KIND=dp) :: volm, dimx, dimy, dimz, shrdx, shrdy, shrdz
      REAL(KIND=dp) :: amass, rcii
      REAL(KIND=dp) :: time, mbeads, mglobal, x, y, z, vx, vy, vz, fx, fy, fz
      REAL(KIND=dp) :: r1, r2, r3, r4
      
      LOGICAL :: eof, lcomm, lmcheck
!!!     for SIONlib
      CHARACTER*(*) :: fname, file_mode
      INTEGER :: numfiles, ntasks, fsblksize, sid
      INTEGER, ALLOCATABLE :: globalranks (:)
      INTEGER*8, ALLOCATABLE :: chunksizes (:)
      INTEGER*8 :: sierr
      INTEGER :: nformsion
      INTEGER*8 :: size, nelem
      INTEGER :: buffer_i(6)
      REAL(KIND=dp) :: buffer_r(10)
      CHARACTER(LEN=8) :: buffer_c
      INTEGER :: rank, chunknum
      INTEGER*8 :: posinchunk
      INTEGER*8, ALLOCATABLE :: pos (:)
      LOGICAL :: finish
      INTEGER :: seof
      PARAMETER (fname = 'test_sionfile.sion')
      PARAMETER (file_mode= 'br')
      
      ! Switches for commenting and checking molecules
      
      lcomm =   .TRUE.
      lmcheck = .TRUE.

      ! Get number of nodes 

      WRITE (*,*) "Number of nodes used in calculations ?"
      READ (*,*) numnodes
      
      ALLOCATE (beads (numnodes), bonds (numnodes))
      ALLOCATE (pos (numnodes))
      
      ! Open the output files
      nform = ntraj + numnodes
      
!  SIONlib: serial open
      ntasks = numnodes 
      numfiles = 1
      fsblksize = -1
      ALLOCATE (chunksizes (ntasks), globalranks (ntasks)) 
      chunksizes (:) = -1 
      DO j = 1, ntasks
         globalranks (j) = j-1 
      END DO
      call FSION_OPEN (fname, file_mode, ntasks, numfiles, &
           chunksizes, fsblksize, globalranks, sid)
      WRITE(6,*) "chunksizes=", chunksizes
      WRITE(6,*) "fsblksize=", fsblksize
      WRITE(6,*) "globalranks=",globalranks
      WRITE(6,*) "sid=", sid
      
      nformsion = nform + numnodes       
      DO j = 1, numnodes
         IF (numnodes>1)THEN
            WRITE (chan, '(i6.6)') j-1
            OPEN (nformsion+j-1, file = 'sion'//chan//'-F', status = 'replace')
         ELSE
            OPEN (nformsion+j-1, file = 'sion-F', status = 'replace')
         END IF
      END DO
         
! SIONlib read the header 
      numbond = 0

      DO j = 1, numnodes
         IF (j > 1) THEN 
            WRITE (6,*) "must be adapted to numnodes>1!"
         END IF
         finish = .false.
         seof = 0
         call fsion_feof (sid, seof)
         IF (seof /= 0) THEN
!            WRITE (6,*) "End of file !!!!!!!!!!!!!!!!!!!!!!!!!!"
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
!         WRITE (6,*) "in sion file, rank ", rank, ": buffer_i=",buffer_i
         nspe = buffer_i (1)
         nmoldef = buffer_i (2)
         nusyst = buffer_i (3)
         nsyst = buffer_i (4)
         nbeads = buffer_i (5)
         nbonds = buffer_i (6)
         !     line c
         nelem=4
         size=8
         buffer_r (1:4) = 0
         CALL FSION_READ(buffer_r,size,nelem,sid,sierr)
!         WRITE (6,*) "in sion file, rank ", rank, ": buffer_r(1:4)=",buffer_r(1:4)
         dimx = buffer_r (1)
         dimy = buffer_r (2)
         dimz = buffer_r (3)
         volm = buffer_r (4)
         !     line d
         nelem=4
         size=4
         buffer_i (1:4) = 0
         CALL FSION_READ(buffer_i,size,nelem,sid,sierr)
!         WRITE (6,*) "in sion file, rank ", rank, ": buffer_i(1:4)=",buffer_i(1:4)
         keytrj = buffer_i (1)
         srfx = buffer_i (2)
         srfy = buffer_i (3)
         srfz = buffer_i (4)
         
         beads (j) = nbeads
         bonds (j) = nbonds
         numbond = numbond + nbonds
         
         IF (lcomm) WRITE (nformsion+j-1,*) "# nspe, nmoldef, nusyst, nsyst, nbeads, nbonds"
         WRITE (nformsion+j-1,*) nspe, nmoldef, nusyst, nsyst, nbeads, nbonds
         IF (lcomm) WRITE (nformsion+j-1,*) "# dimx, dimy, dimz, volm"
         WRITE (nformsion+j-1,97) dimx, dimy, dimz, volm
         IF (lcomm) WRITE (nformsion+j-1,*) "# keytrj, srfx, srfy, srfz"
         WRITE (nformsion+j-1,*) keytrj, srfx, srfy, srfz
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

         chunknum = 0 
         posinchunk = 6*4 + 4*8 + 4*4 
         CALL FSION_SEEK (sid, rank, chunknum, posinchunk, sierr)
         pos (j) = posinchunk

         IF (lcomm) WRITE (nformsion+j-1,*) "# SPECIES:"
         IF (lcomm) WRITE (nformsion+j-1,*) "# namspe, amass, rcii, lfrzn"
         
         DO i = 1, nspe
            IF (j == 1) THEN 
               nelem = 1
               size = 8
               buffer_c = '        '
               CALL FSION_READ(buffer_c,size,nelem,sid,sierr)
!               WRITE (6,*) "in sion file, rank ", rank, ": buffer_c=",buffer_c
               namspe (i) = buffer_c

               nelem=2
               size=8
               buffer_r (1:2) = 0
               CALL FSION_READ(buffer_r,size,nelem,sid,sierr)
!               WRITE (6,*) "in sion file, rank ", rank, ": buffer_r (1:2)=",buffer_r (1:2)
               amass = buffer_r (1)
               rcii = buffer_r (2)
               
               nelem=1
               size=4
               buffer_i (1) = 0
               CALL FSION_READ(buffer_i,size,nelem,sid,sierr)
!               WRITE (6,*) "in sion file, rank ", rank, ": buffer_i (1)=",buffer_i (1)
               lfrzn = buffer_i (1)
                             
               !ADapt the other case, j \= 1
            ELSE
               nelem = 1
               size = 8
               CALL FSION_READ(buffer_c,size,nelem,sid,sierr)
               nelem=2
               size=8
               CALL FSION_READ(buffer_r,size,nelem,sid,sierr)                              
               nelem=1
               size=4
               CALL FSION_READ(buffer_i,size,nelem,sid,sierr)           
            ENDIF
            WRITE (nformsion+j-1,96) namspe (i), amass, rcii, lfrzn
            pos (j) = pos (j) + 1*8 + 2*8 + 1*4
         END DO
         
         IF (nmoldef>0) THEN
            IF (lcomm) WRITE (nformsion+j-1,*) "# MOLECULES:"
            IF (lcomm) WRITE (nformsion+j-1,*) "# nammol"

            DO i = 1, nmoldef
               IF (j==1) THEN
                  nelem=1
                  size=8
                  buffer_c = '       '
                  CALL FSION_READ(buffer_c,size,nelem,sid,sierr)
!                  WRITE (6,*) "in sion file, rank ", rank, ": buffer_c=",buffer_c
                  nammol (i) = buffer_c
                  !ADAPT for numnodes>1 !!!
               ELSE
                  nelem=1
                  size=8 
                  CALL FSION_READ(buffer_c,size,nelem,sid,sierr)
               END IF
               WRITE (nformsion+j-1,*) nammol (i)
               pos (j) = pos (j) + 1*8 
            END DO
         END IF

         IF (j == 1) THEN
            nelem=1
            size=80 
            text = '                                                                                '
            CALL FSION_READ(text,size,nelem,sid,sierr)
!            WRITE (6,*) "in sion file, rank ", rank, ": text=", text
            !ADAPT for numnodes>1!!!!            
         ELSE
            nelem=1
            size=80
            CALL FSION_READ(text,size,nelem,sid,sierr) 
         ENDIF
         pos (j) = pos (j) + 1*80
         IF (lcomm) WRITE (nformsion+j-1,*) "# Simulation name:"
         WRITE (nformsion+j-1,*) text

         IF (j==1) THEN
            nummol = 0 !counter for number of molecules      
            ibond = 0  !counter for bonds
         END IF
            
      !     fill in arrays for beads and bonds
         !      DO j = 1, numnodes
         rank = j - 1 

         IF (lcomm) WRITE (nformsion+j-1,*) "# BEADS:"
         IF (lcomm) WRITE (nformsion+j-1,*) "# global, species, molecule, chain"

         IF (lmcheck) THEN
            !Build ltp, ltm, mole
            DO i = 1, beads (j)
               nelem = 4
               size=4
               buffer_i (1:4) = 0
               CALL FSION_READ(buffer_i,size,nelem,sid,sierr)
!               WRITE (6,*) "in sion file, rank ", rank, ": buffer_i(1:4)=",buffer_i(1:4)
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
               nelem = 4
               size=4
               buffer_i (1:4) = 0
               CALL FSION_READ(buffer_i,size,nelem,sid,sierr)
!               WRITE (6,*) "in sion file, rank ", rank, ": buffer_i(1:4)=",buffer_i(1:4)
               global = buffer_i (1)
               species = buffer_i (2)
               molecule = buffer_i (3)
               chain = buffer_i (4)
               WRITE (nformsion+j-1,*) global, species, molecule, chain 
            END DO
         ENDIF
         pos (j) = pos (j) + 4*4*beads(j)
         
         IF (bonds (j)>0) THEN
            IF (lcomm) WRITE (nformsion+j-1,*) "# BONDS:"
            IF (lcomm) WRITE (nformsion+j-1,*) "# extremes of the bond"

            IF (lmcheck) THEN
               ! Build bndtbl
               DO i = 1, bonds (j)
                  ibond = ibond + 1
                  nelem=2
                  size=4
                  buffer_i (1:2) = 0 
                  CALL FSION_READ(buffer_i,size,nelem,sid,sierr)
!                  WRITE (6,*) "in sion file, rank ", rank, ": buffer_i(1:2)=",buffer_i(1:2)
                  bead1 = buffer_i (1)
                  bead2 = buffer_i (2)
                  bndtbl (ibond, 1) = bead1
                  bndtbl (ibond, 2) = bead2
                  WRITE (nformsion+j-1,*) bead1, bead2
               END DO
            ELSE
               DO i = 1, bonds (j)
                  nelem=2
                  size=4
                  buffer_i (1:2) = 0 
                  CALL FSION_READ(buffer_i,size,nelem,sid,sierr)
!                  WRITE (6,*) "in sion file, rank ", rank, ": buffer_i(1:2)=",buffer_i(1:2)
                  bead1 = buffer_i (1)
                  bead2 = buffer_i (2)
                  WRITE (nformsion+j-1,*) bead1, bead2 
               END DO
            END IF
         END IF
         pos (j) = pos (j) + 2*4*bonds(j)
         
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

      print*,pos
      
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
         chunknum = 0
         posinchunk = pos (j)

         CALL FSION_SEEK (sid, rank, chunknum, posinchunk, sierr)
!!!         
         DO WHILE (.true.)

            call fsion_feof (sid, seof)
            IF (seof /= 0) THEN
!               WRITE (6,*) "End of file !!!!!!!!!!!!!!!!!!!!!!!!!!"
               EXIT
            END IF
            
            ! line k 
            nelem=8
            size=8
            buffer_r (1:8) = 0
            CALL FSION_READ(buffer_r,size,nelem,sid,sierr)
!            WRITE (6,*) "in sion file, rank ", rank, ": buffer_r(1:4)=",buffer_r(1:8)
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
!                  WRITE (6,*) "in sion file, rank ", rank, ": buffer_r(1:4)=",buffer_r(1:4)
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
!                  WRITE (6,*) "in sion file, rank ", rank, ": buffer_r(1:7)=",buffer_r(1:7)
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
!                  WRITE (6,*) "in sion file, rank ", rank, ": buffer_r(1:7)=",buffer_r(1:10)
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
      
! SIONlib serial close
      call FSION_CLOSE (sid, sierr)
      CLOSE (nformsion)
      
      DEALLOCATE (beads, bonds)
      DEALLOCATE (pos)
      DEALLOCATE (namspe, nammol)
      IF (lmcheck) DEALLOCATE (ltp, ltm, mole, nmol, nbdmol, bndtbl, nbomol)

      DEALLOCATE (chunksizes, globalranks) 

99    FORMAT(f10.1,2x,1p,9(e13.6,3x))
98    FORMAT(8(f10.3,3x))
97    FORMAT(4(f10.3,3x))
96    FORMAT(A9,3x,2(f10.3,3x),I2)
      
END PROGRAM format_history_sion
