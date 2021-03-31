PROGRAM format_history
!***********************************************************************************
!
! module to format dl_meso HISTORY files
!
! authors - m. a. seaton & s. chiacchiera, february 2017 (amended january 2021)
!
!**********************************************************************************
      IMPLICIT none
      INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND (15, 307)
      INTEGER, PARAMETER :: li = SELECTED_INT_KIND (12)
      INTEGER, PARAMETER :: ntraj=10,nuser=5
      INTEGER, PARAMETER :: endversion = 1

      CHARACTER(80) :: text
      CHARACTER(8), ALLOCATABLE :: namspe (:), nammol (:)

      INTEGER, ALLOCATABLE :: ltp (:), ltm (:), mole (:), bndtbl (:,:)
      INTEGER, ALLOCATABLE :: nbdmol (:), nbomol (:), readint (:), globindex (:)
      INTEGER :: chain, imol, ioerror, i, k, nmoldef, numframe
      INTEGER :: nspe, nbeads, nusyst, nsyst, global, species, molecule, numbond
      INTEGER :: nummol, lfrzn, rnmol, keytrj, srfx, srfy, srfz
      INTEGER :: bead1, bead2
      INTEGER :: nform
      INTEGER :: endver, Dlen, nstep, framesize, lend, leni
      INTEGER(KIND=li) :: filesize, mypos, headerpos, currentpos, lend_li, leni_li, framesizeli, numbeadsli

      REAL(KIND=dp), ALLOCATABLE :: nmol (:), readdata (:)
      REAL(KIND=dp) :: dimx, dimy, dimz, shrdx, shrdy, shrdz
      REAL(KIND=dp) :: amass, rcii, chge
      REAL(KIND=dp) :: time

      LOGICAL :: eof, lcomm, lmcheck, swapend, bigend, sorted

      ! Switches for commenting, checking molecules and sorting particles in output
      
      lcomm =   .TRUE.
      lmcheck = .TRUE.
      sorted  = .TRUE.

!     determine number of bytes for selected double precision kind
!     (the default SELECTED_REAL_KIND (15, 307) should return 8 bytes)

      lend = STORAGE_SIZE (1.0_dp) / 8
      leni = BIT_SIZE (1) / 8
      lend_li = INT (lend, KIND=li)
      leni_li = INT (leni, KIND=li)

!     check endianness of machine

      bigend = (IACHAR(TRANSFER(1,"a"))==0)

      ! Determine if HISTORY file exists, which endianness to use,
      ! if type of real is correct

      INQUIRE (file = 'HISTORY', EXIST = eof)
      IF (.NOT. eof) THEN
        PRINT *, "ERROR: cannot find HISTORY file"
        STOP
      END IF

      OPEN (ntraj, file = 'HISTORY', access = 'stream', form = 'unformatted', status = 'unknown')

      swapend = .false.
      READ (ntraj) endver, Dlen

      IF (endver/=endversion) THEN
        swapend = .true.
        CLOSE (ntraj)
        IF (bigend) THEN
          OPEN (ntraj, file = 'HISTORY', access = 'stream', form = 'unformatted', status = 'unknown', convert = 'little_endian')
        ELSE
          OPEN (ntraj, file = 'HISTORY', access = 'stream', form = 'unformatted', status = 'unknown', convert = 'big_endian')
        END IF
        READ (ntraj) endver, Dlen
        IF (endver/=endversion) THEN
          PRINT *, "ERROR: corrupted HISTORY file or created with incorrect version of DL_MESO"
          STOP
        END IF
      END IF

      IF (Dlen/=lend) THEN
        PRINT *, "ERROR: incorrect type of real number used in HISTORY file"
        PRINT *, "       recompile format_history.f90 with reals of ", Dlen, " bytes"
        STOP
      END IF

      ! Open the output file
      nform = ntraj + 1
      OPEN (nform, file = 'HISTORY'//"-F", status = 'replace')

!     read file size, number of frames and timestep numbers

      READ (ntraj) filesize, numframe, nstep

      ! read the number of beads, molecules and bonds
      ! Arrays are filled with names of particles and molecules: if checking molecules,
      ! arrays for species, molecule types etc. also filled

      READ (ntraj) text
      READ (ntraj) nspe, nmoldef, nusyst, nsyst, numbond, keytrj, srfx, srfy, srfz

      IF (lcomm) WRITE (nform,*) "# Simulation name:"
      WRITE (nform,*) text

      IF (lcomm) WRITE (nform,*) "# nspe, nmoldef, nusyst, nsyst, numbond"
      WRITE (nform,*) nspe, nmoldef, nusyst, nsyst, numbond
      IF (lcomm) WRITE (nform,*) "# keytrj, srfx, srfy, srfz"
      WRITE (nform,*) keytrj, srfx, srfy, srfz

      framesize = (keytrj+1) * 3
      ALLOCATE (namspe (nspe), nammol (nmoldef), globindex (nsyst), readint (nsyst), readdata (framesize))
      IF (lmcheck) THEN 
         ALLOCATE (ltp (1:nsyst), ltm (1:nsyst), mole (1:nsyst))
         ALLOCATE (nmol (1:nmoldef), nbdmol (1:nmoldef), nbomol (1:nmoldef))
         ALLOCATE (bndtbl (numbond, 2))
      END IF
      
      IF (lcomm) WRITE (nform,*) "# SPECIES:"
      IF (lcomm) WRITE (nform,*) "# namspe, amass, rcii, chge, lfrzn"
      DO i = 1, nspe
        READ (ntraj) namspe (i), amass, rcii, chge, lfrzn
        WRITE (nform,96) namspe (i), amass, rcii, chge, lfrzn
      END DO

      IF (nmoldef>0) THEN
        IF (lcomm) WRITE (nform,*) "# MOLECULES:"
        IF (lcomm) WRITE (nform,*) "# nammol"
        DO i = 1, nmoldef
          READ (ntraj) nammol (i)
          WRITE (nform,*) nammol (i)
        END DO
      END IF

      ! (if required) read and fill arrays with properties of beads and molecules

      nummol = 0 ! counter for number of molecules

      IF (lcomm) WRITE (nform,*) "# BEADS:"
      IF (lcomm) WRITE (nform,*) "# global, species, molecule, chain"
      IF (lmcheck) THEN
        ! Build ltp, ltm, mole
        DO i = 1, nsyst
          READ (ntraj) global, species, molecule, chain
          ltp (global) = species
          ltm (global) = molecule
          mole (global) = chain
          nummol = MAX (nummol, chain)
          WRITE (nform,*) global, species, molecule, chain
        END DO
      ELSE
        DO i = 1, nsyst
          READ (ntraj) global, species, molecule, chain
          WRITE (nform,*) global, species, molecule, chain
        END DO
      END IF

      IF (numbond>0) THEN
        IF (lcomm) WRITE (nform,*) "# BONDS:"
        IF (lcomm) WRITE (nform,*) "# extremes of the bond"
        IF (lmcheck) THEN
         ! Build bndtbl
          DO i = 1, numbond
            READ (ntraj) bead1, bead2
            bndtbl (i, 1) = bead1
            bndtbl (i, 2) = bead2
            WRITE (nform,*) bead1, bead2
          END DO
        ELSE
          DO i = 1, numbond
            READ (ntraj) bead1, bead2
            WRITE (nform,*) bead1, bead2
          END DO
        END IF
      END IF

!     reached end of header: find current position in file

      INQUIRE (unit=ntraj, POS=headerpos)
      framesizeli = INT (framesize, KIND=li)
      numbeadsli = INT (nsyst, KIND=li)

      IF (lmcheck) THEN
      ! determine numbers of molecules, beads and bonds per molecule type
         nmol = 0.0_dp
         nbdmol = 0
         nbomol = 0 
         chain = 0
         imol = 0 ! necessary to avoid out of bounds
         
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

      ! Now read in trajectories

      eof = .false.

      IF (lcomm) WRITE (nform,*) "# --- TRAJECTORIES --- (key =", keytrj,")"
      SELECT CASE (keytrj)
      CASE (0)
        IF (lcomm) WRITE (nform,*) "# mglobal, x, y, z"
      CASE(1)
        IF (lcomm) WRITE (nform,*) "# mglobal, x, y, z, vx, vy, vz"
      CASE(2)
        IF (lcomm) WRITE (nform,*) "# mglobal, x, y, z, vx, vy, vz, fx, fy, fz"
      END SELECT

      DO k = 1, numframe

        currentpos = headerpos + INT (k-1, KIND=li) * ((7_li + numbeadsli * framesizeli) * lend_li + (1_li + numbeadsli) * leni_li)
        READ (ntraj, POS=currentpos, IOSTAT=ioerror) time, nbeads, dimx, dimy, dimz, shrdx, shrdy, shrdz

        IF (ioerror/=0) THEN
          eof = .true.
          IF (k==1) THEN
            PRINT *, 'ERROR: cannot find trajectory data in HISTORY file'
            STOP
          END IF
          EXIT
        END IF

        IF (lcomm) WRITE (nform,*) "# time, nbeads, dimx, dimy, dimz, shrdx, shrdy, shrdz"
        WRITE (nform,98) time, nbeads, dimx, dimy, dimz, shrdx, shrdy, shrdz
            
        IF (lcomm) WRITE (nform,*) "# snapshot number:", k

        IF (sorted) THEN

          READ (ntraj) readint (1:nbeads)
          CALL quicksort_integer_indexed (readint, 1, nbeads, globindex)
          DO i = 1, nbeads
            global = globindex (i)
            mypos = currentpos + leni_li * (1_li + numbeadsli) + (7_li + INT (global-1, KIND=li) * framesizeli) * lend_li
            READ (ntraj, POS=mypos) readdata (1:framesize)
            WRITE (nform,99) global, readdata (1:framesize)
          END DO

        ELSE

          READ (ntraj) globindex (1:nbeads)
          DO i = 1, nbeads
            READ (ntraj, POS=mypos) readdata (1:framesize)
            WRITE (nform,99) global, readdata (1:framesize)
          END DO

        END IF

      END DO
         
      ! Close the trajectory file
      CLOSE (ntraj)

      ! close the output file
      CLOSE (nform)

      DEALLOCATE (readint, readdata, globindex)
      DEALLOCATE (namspe, nammol)
      IF (lmcheck) DEALLOCATE (ltp, ltm, mole, nmol, nbdmol, bndtbl, nbomol)

99    FORMAT(I10,2x,1p,9(e13.6,3x))
98    FORMAT(f10.3,3x,1x,I10,6(f10.3,3x))
96    FORMAT(A9,3x,3(f10.3,3x),I2)

CONTAINS

      SUBROUTINE quicksort_integer_indexed (list, stride, n, indices)

!**********************************************************************
!
!     sort integers in array into numerical order, recording original
!     positions of values (routine to prepare indices array)
!
!     copyright ukri stfc daresbury laboratory
!     authors - m. a. seaton august 2013
!
!**********************************************************************

      INTEGER, INTENT (INOUT) :: list (:)
      INTEGER, INTENT (IN) :: stride, n
      INTEGER, INTENT (OUT) :: indices (:)
      INTEGER :: i

      DO i = 1, n
        indices (i) = i
      END DO

      CALL qsort_integer (list, indices, stride, 1, n)

      END SUBROUTINE quicksort_integer_indexed

      RECURSIVE SUBROUTINE qsort_integer (list, index, stride, low, high)

!**********************************************************************
!
!     sort integers in array into numerical order, recording original
!     positions of values
!
!     copyright ukri stfc daresbury laboratory
!     authors - m. a. seaton august 2013
!
!**********************************************************************

      INTEGER, INTENT (INOUT) :: list (:), index (:)
      INTEGER, INTENT (IN) :: low, high
      INTEGER, INTENT (IN) :: stride
      INTEGER :: i, j, k, reference, temp

      IF (high < low + 6) THEN

!     resort to bubble sort for very small lists (5 items or fewer)

        DO i = low, high - 1
          DO j = i+1, high
            IF (list (stride * (i - 1) + 1) > list (stride * (j - 1) + 1)) THEN
              DO k = 1, stride
                temp = list (stride * (i - 1) + k)
                list (stride * (i - 1) + k) = list (stride * (j - 1) + k)
                list (stride * (j - 1) + k) = temp
              END DO
              temp = index (i)
              index (i) = index (j)
              index (j) = temp
            END IF
          END DO
        END DO

      ELSE

!     apply partition-based sort

        reference = list (stride * ((low+high)/2 - 1) + 1)
        i = low - 1
        j = high + 1
        DO
          DO
            i = i + 1
            IF (list (stride * (i-1) + 1) >= reference) EXIT
          END DO
          DO
            j = j - 1
            IF (list (stride * (j-1) + 1) <= reference) EXIT
          END DO
          IF (i < j) THEN
            DO k = 1, stride
              temp = list (stride * (i-1) + k)
              list (stride * (i-1) + k) = list (stride * (j-1) + k)
              list (stride * (j-1) + k) = temp
            END DO
            temp = index (i)
            index (i) = index (j)
            index (j) = temp
          ELSE IF (i == j) THEN
            i = i + 1
            EXIT
          ELSE
            EXIT
          END IF
        END DO

        IF (low<j) CALL qsort_integer (list, index, stride, low, j)
        IF (i<high) CALL qsort_integer (list, index, stride, i, high)

      END IF

      END SUBROUTINE qsort_integer

END PROGRAM format_history
