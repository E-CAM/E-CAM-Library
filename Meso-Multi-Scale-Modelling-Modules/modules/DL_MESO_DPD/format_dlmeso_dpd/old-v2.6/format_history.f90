PROGRAM format_history
!***********************************************************************************
!
! module to format dl_meso HISTORY files
!
! authors - m. a. seaton & s. chiacchiera, february 2017
!
!**********************************************************************************
IMPLICIT none
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

      ! Switches for commenting and checking molecules
      
      lcomm =   .TRUE.
      lmcheck = .TRUE.

      ! Get number of nodes 

      WRITE (*,*) "Number of nodes used in calculations ?"
      READ (*,*) numnodes
      
      ALLOCATE (beads (numnodes), bonds (numnodes))
      
      ! Determine if HISTORY files exist

      IF (numnodes>1) THEN
         INQUIRE (file = 'HISTORY000000', EXIST = eof)
      ELSE
         INQUIRE (file = 'HISTORY', EXIST = eof)
      END IF
      IF (.NOT. eof) THEN
         WRITE (*,*) "ERROR: cannot find HISTORY files"
         STOP
      END IF

      ! Open the output files
      nform = ntraj + numnodes 
      DO j = 1, numnodes
         WRITE (chan, '(i6.6)') j-1
         IF (numnodes>1)THEN
            OPEN (nform+j-1, file = 'HISTORY'//chan//"-F", status = 'replace')
         ELSE
            OPEN (nform+j-1, file = 'HISTORY'//"-F", status = 'replace')
         END IF
      END DO
         
      ! First reading, where the number of beads, molecules and bonds are determined
      ! Arrays are filled with names of particles and molecules
      ! If multiple HISTORY files are present, it is checked they are compatible
      
      numbond = 0

      DO j = 1, numnodes
         WRITE (chan, '(i6.6)') j-1
         IF (numnodes>1) THEN
            OPEN (ntraj+j-1, file = 'HISTORY'//chan, access = 'sequential', form = 'unformatted', status = 'unknown')
         ELSE
            OPEN (ntraj, file = 'HISTORY', access = 'sequential', form = 'unformatted', status = 'unknown')
         END IF
         
         IF (j == 1) THEN
            READ (ntraj+j-1) nspe, nmoldef, nusyst, nsyst, nbeads, nbonds
            READ (ntraj+j-1) dimx, dimy, dimz, volm
            READ (ntraj+j-1) keytrj, srfx, srfy, srfz
         ELSE
            READ (ntraj+j-1) n1, n2, n3, n4, nbeads, nbonds
            READ (ntraj+j-1) r1, r2, r3, r4
            IF (n1 /= nspe .OR. n2 /= nmoldef .OR. n3 /= nusyst .OR. n4 /= nsyst &
                .OR. r1 /= dimx .OR. r2 /= dimy .OR. r3 /= dimz .OR. r4 /= volm) THEN
               WRITE (*,*) "ERROR: HISTORY files do not refer to the same system!"
               STOP
            ENDIF
            READ (ntraj+j-1) n1, n2, n3, n4
            IF (n1 /= keytrj .OR. n2 /= srfx .OR. n3 /= srfy .OR. n4 /= srfz) THEN
               WRITE (*,*) "ERROR: HISTORY files do not refer to the same system!"
               STOP
            ENDIF
         ENDIF

         beads (j) = nbeads
         bonds (j) = nbonds
         numbond = numbond + nbonds
         
         IF (lcomm) WRITE (nform+j-1,*) "# nspe, nmoldef, nusyst, nsyst, nbeads, nbonds"
         WRITE (nform+j-1,*) nspe, nmoldef, nusyst, nsyst, nbeads, nbonds
         IF (lcomm) WRITE (nform+j-1,*) "# dimx, dimy, dimz, volm"
         WRITE (nform+j-1,97) dimx, dimy, dimz, volm
         IF (lcomm) WRITE (nform+j-1,*) "# keytrj, srfx, srfy, srfz"
         WRITE (nform+j-1,*) keytrj, srfx, srfy, srfz
      END DO ! loop over nodes
               
      ALLOCATE (namspe (nspe), nammol (nmoldef))
      IF (lmcheck) THEN 
         ALLOCATE (ltp (1:nsyst), ltm (1:nsyst), mole (1:nsyst))
         ALLOCATE (nmol (1:nmoldef), nbdmol (1:nmoldef), nbomol (1:nmoldef))
         ALLOCATE (bndtbl (numbond, 2))
      ENDIF
      
      DO j = 1, numnodes
         IF (lcomm) WRITE (nform+j-1,*) "# SPECIES:"
         IF (lcomm) WRITE (nform+j-1,*) "# namspe, amass, rcii, lfrzn"
         DO i = 1, nspe
            IF (j == 1) THEN
               READ (ntraj+j-1) namspe (i), amass, rcii, lfrzn
            ELSE
               READ (ntraj+j-1) a1, amass, rcii, lfrzn
               IF (a1 /= namspe (i))THEN
                  WRITE (*,*) "ERROR: HISTORY files do not refer to the same system!"
                  STOP
               ENDIF
            ENDIF
            WRITE (nform+j-1,96) namspe (i), amass, rcii, lfrzn
         END DO

         IF (nmoldef>0) THEN
            IF (lcomm) WRITE (nform+j-1,*) "# MOLECULES:"
            IF (lcomm) WRITE (nform+j-1,*) "# nammol"
            DO i = 1, nmoldef
               IF (j==1) THEN
                  READ (ntraj+j-1) nammol (i)
               ELSE
                  READ (ntraj+j-1) a1
                  IF (a1 /= nammol (i))THEN
                     WRITE (*,*) "ERROR: HISTORY files do not refer to the same system!"
                     STOP
                  ENDIF
               END IF
               WRITE (nform+j-1,*) nammol (i)
            END DO
         END IF

         IF (j == 1) THEN
            READ (ntraj+j-1) text
         ELSE
            READ (ntraj+j-1) a2
            IF (a2 /= text) THEN 
               WRITE (*,*) "ERROR: HISTORY files do not refer to the same system!"            
               STOP
            ENDIF
         ENDIF
            
         IF (lcomm) WRITE (nform+j-1,*) "# Simulation name:"
         WRITE (nform+j-1,*) text

      ENDDO ! end of loop over nodes
               
      DO j = 1, numnodes
         CLOSE (ntraj+j-1)
      END DO
      
      ! Second reading, where (if required) arrays are filled with properties
      ! of beads and molecules. Then, the snapshots of trajectories are read.

      DO j = 1, numnodes
         WRITE (chan, '(i6.6)') j-1
         IF (numnodes>1) THEN
            OPEN (ntraj+j-1, file = 'HISTORY'//chan, access = 'sequential', form = 'unformatted', status = 'unknown')
         ELSE
            OPEN (ntraj, file = 'HISTORY', access = 'sequential', form = 'unformatted', status = 'unknown')
         END IF     
      
         READ (ntraj+j-1) !nspe, nmoldef, nusyst, nsyst, nbeads, nbonds
         READ (ntraj+j-1) !dimx, dimy, dimz, volm
         READ (ntraj+j-1) !keytrj, srfx, srfy, srfz

         DO i = 1, nspe
            READ (ntraj+j-1) !namspe (i), amass, rcii, lfrzn
         END DO
         
         DO i = 1, nmoldef
            READ (ntraj+j-1) !nammol (i)
         END DO
         
         READ (ntraj+j-1) !text
      END DO

            

      nummol = 0 !counter for number of molecules      
      ibond = 0  !counter for bonds
      
      !     fill in arrays for beads and bonds
      DO j = 1, numnodes
         IF (lcomm) WRITE (nform+j-1,*) "# BEADS:"
         IF (lcomm) WRITE (nform+j-1,*) "# global, species, molecule, chain"
         IF (lmcheck) THEN
            !Build ltp, ltm, mole
            DO i = 1, beads (j)
               READ (ntraj+j-1) global, species, molecule, chain
               ltp (global) = species
               ltm (global) = molecule
               mole (global) = chain
               nummol = MAX (nummol, chain)
               WRITE (nform+j-1,*) global, species, molecule, chain 
            END DO
         ELSE
            DO i = 1, beads (j)
               READ (ntraj+j-1) global, species, molecule, chain         
               WRITE (nform+j-1,*) global, species, molecule, chain 
            END DO
         ENDIF
         
         IF (bonds (j)>0) THEN
            IF (lcomm) WRITE (nform+j-1,*) "# BONDS:"
            IF (lcomm) WRITE (nform+j-1,*) "# extremes of the bond"
            IF (lmcheck) THEN
               ! Build bndtbl
               DO i = 1, bonds (j)
                  ibond = ibond + 1
                  READ (ntraj+j-1) bead1, bead2
                  bndtbl (ibond, 1) = bead1
                  bndtbl (ibond, 2) = bead2
                  WRITE (nform+j-1,*) bead1, bead2
               END DO
            ELSE
               DO i = 1, bonds (j)
                  READ (ntraj+j-1) bead1, bead2
                  WRITE (nform+j-1,*) bead1, bead2
               END DO
            END IF
         END IF
            
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
    
         eof = .false.
         k = 0
      
         IF (lcomm) WRITE (nform+j-1,*) "# --- TRAJECTORIES --- (key =", keytrj,")"
         SELECT CASE (keytrj)
         CASE (0)
            IF (lcomm) WRITE (nform+j-1,*) "# mglobal, x, y, z"
         CASE(1)
            IF (lcomm) WRITE (nform+j-1,*) "# mglobal, x, y, z, vx, vy, vz"
         CASE(2) 
            IF (lcomm) WRITE (nform+j-1,*) "# mglobal, x, y, z, vx, vy, vz, fx, fy, fz"
         END SELECT
         
         DO WHILE (.true.)
            READ (ntraj+j-1, IOSTAT=ioerror) time, mbeads, dimx, dimy, dimz, shrdx, shrdy, shrdz
            IF (lcomm) WRITE (nform+j-1,*) "# time, mbeads, dimx, dimy, dimz, shrdx, shrdy, shrdz"
            WRITE (nform+j-1,98) time, mbeads, dimx, dimy, dimz, shrdx, shrdy, shrdz
            
            IF (ioerror/=0) THEN
               eof = .true.
               IF (k==0) THEN
                  PRINT *, 'ERROR: cannot find trajectory data in HISTORY files'
                  STOP
               END IF
               EXIT
            END IF
            
            k = k + 1
            
            IF (lcomm) WRITE (nform+j-1,*) "# snapshot number:", k
            
            nbeads = NINT (mbeads)
            
            SELECT CASE (keytrj)
            CASE (0)
               DO i = 1, nbeads
                  READ (ntraj+j-1) mglobal, x, y, z
                  WRITE (nform+j-1,99) mglobal, x, y, z
               END DO
            CASE (1)
               DO i = 1, nbeads
                  READ (ntraj+j-1) mglobal, x, y, z, vx, vy, vz
                  WRITE (nform+j-1,99) mglobal, x, y, z, vx, vy, vz
               END DO
            CASE (2)
               DO i = 1, nbeads
                  READ (ntraj+j-1) mglobal, x, y, z, vx, vy, vz, fx, fy, fz
                  WRITE (nform+j-1,99) mglobal, x, y, z, vx, vy, vz, fx, fy, fz
               END DO
            END SELECT
            
         END DO
      END DO
         
      ! Close the trajectory files
      DO j = 1, numnodes
         CLOSE (ntraj+j-1)
      END DO

      ! close the output files
      DO j = 1, numnodes
         CLOSE (nform+j-1)
      END DO
               
      DEALLOCATE (beads, bonds)
      DEALLOCATE (namspe, nammol)
      IF (lmcheck) DEALLOCATE (ltp, ltm, mole, nmol, nbdmol, bndtbl, nbomol)

99    FORMAT(f10.1,2x,1p,9(e13.6,3x))
98    FORMAT(8(f10.3,3x))
97    FORMAT(4(f10.3,3x))
96    FORMAT(A9,3x,2(f10.3,3x),I2)
      
END PROGRAM format_history
