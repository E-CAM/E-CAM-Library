PROGRAM gen_moldipaf
!*************************************************************************************
! module to compute autocorrelation functions of individual charge dipole moments in
! DL_MESO_DPD
!
! authors: m. a. seaton and s. chiacchiera, March 2017 (amended August 2017, January
!          2021)
!*************************************************************************************         
      USE, INTRINSIC :: iso_c_binding
      IMPLICIT none
      INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND (15, 307)
      INTEGER, PARAMETER :: li = SELECTED_INT_KIND (12)
      INTEGER, PARAMETER :: ntraj=10
      INTEGER, PARAMETER :: endversion = 1
      REAL(KIND=dp), PARAMETER :: pi=3.141592653589793_dp
      
      CHARACTER(80) :: text
      CHARACTER(8), ALLOCATABLE :: namspe (:), nammol (:)

      INTEGER, ALLOCATABLE :: ltp (:), ltm (:), mole (:), bndtbl (:,:)
      INTEGER, ALLOCATABLE :: nbdmol (:), readint (:)
      INTEGER, ALLOCATABLE :: visit (:), from (:)
      INTEGER :: nrtout
      INTEGER :: chain, imol, ioerror, i, numtraj, j, k, l, nmoldef, nbdmolmx
      INTEGER :: nspe, nbeads, nusyst, nmbeads, nsyst, numbond, global, species, molecule
      INTEGER :: nummol, lfrzn, rnmol, keytrj, srfx, srfy, srfz
      INTEGER :: n1
      INTEGER :: naf, nsamp
      INTEGER :: endver, Dlen, nstep, framesize, lend, leni
      INTEGER(KIND=li) :: filesize, mypos, currentpos, lend_li, leni_li, framesizeli, numbeadsli

      REAL(KIND=dp), ALLOCATABLE :: xxx (:), yyy (:), zzz (:), readdata (:)
      REAL(KIND=dp), ALLOCATABLE :: nmol (:), chg (:), molchg (:)
      REAL(KIND=dp), ALLOCATABLE :: dipx_box (:), dipy_box (:), dipz_box (:)
      REAL(KIND=dp), ALLOCATABLE :: dipx (:), dipy (:), dipz (:)
      REAL(KIND=dp), ALLOCATABLE :: mdipdata (:,:,:), corrdata (:)
      REAL(KIND=dp) :: dimx, dimy, dimz, shrdx, shrdy, shrdz
      REAL(KIND=dp) :: amass, rcii
      REAL(KIND=dp) :: time
      REAL(KIND=dp) :: dt, time0, domega
      REAL(KIND=dp) :: dx0, dy0, dz0 

      INTEGER :: nftpts
      COMPLEX(C_DOUBLE_COMPLEX), ALLOCATABLE :: fftdata (:)
      
      LOGICAL :: eof, lfft, swapend, bigend

!     determine number of bytes for selected double precision and integer kinds
!     (the default SELECTED_REAL_KIND (15, 307) should return 8 bytes)

      lend = STORAGE_SIZE (1.0_dp) / 8
      leni = BIT_SIZE (1) / 8
      lend_li = INT (lend, KIND=li)
      leni_li = INT (leni, KIND=li)

!     check endianness of machine

      bigend = (IACHAR(TRANSFER(1,"a"))==0)

!     determine if HISTORY file exists, which endianness to use,
!     if type of real is correct

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
        PRINT *, "       recompile gen_dipole.f90 with reals of ", Dlen, " bytes"
        STOP
      END IF

!     read file size, number of frames and timestep numbers

      READ (ntraj) filesize, numtraj, nstep

      ! Read where the number of beads, molecules and bonds are determined
      ! Arrays are filled with names of particles and molecules

      READ (ntraj) text

      READ (ntraj) nspe, nmoldef, nusyst, nsyst, numbond, keytrj, srfx, srfy, srfz

      IF (numbond==0) THEN
        PRINT *, 'ERROR: no molecules in trajectory data!'
        STOP
      END IF

      IF (srfx > 1 .OR. srfy > 1 .OR. srfz > 1) THEN
         WRITE (*,*) "ERROR: Hard walls, electrostatics not implemented in DL_MESO_DPD yet!"
         STOP
      END IF
      
      IF (srfx == 1 .OR. srfy == 1 .OR. srfz == 1) THEN
         WRITE (*,*) "ERROR: System under shear, not implemented yet!"
         STOP
      END IF
            
      framesize = (keytrj+1) * 3
      ALLOCATE (readint (1:nsyst), readdata (1:framesize))

!     get number of beads to be tracked when reading trajectory file (molecular beads)
      nmbeads = nsyst - nusyst

      ALLOCATE (namspe (nspe), nammol (nmoldef))
      ALLOCATE (xxx (1:nmbeads), yyy (1:nmbeads), zzz (1:nmbeads))
      ALLOCATE (ltp (1:nmbeads), ltm (1:nmbeads), mole (1:nmbeads))
      ALLOCATE (nmol (1:nmoldef), nbdmol (1:nmoldef))
      ALLOCATE (chg (nspe))
      ALLOCATE (bndtbl (numbond, 2))
      ALLOCATE (visit (nmbeads), from (nmbeads)) 

      DO i = 1, nspe
        READ (ntraj) namspe (i), amass, rcii, chg (i), lfrzn
      END DO

      DO i = 1, nmoldef
        READ (ntraj) nammol (i)
      END DO

      ! reading of bead species and molecule types

      nummol = 0 ! counter for number of molecules
!      ibond = 0  !counter for bonds

      DO i = 1, nsyst
        READ (ntraj) global, species, molecule, chain
        IF (global>nusyst .AND. global<=nsyst) THEN
          ltp (global-nusyst) = species
          ltm (global-nusyst) = molecule
          mole (global-nusyst) = chain
          nummol = MAX (nummol, chain)
        END IF
      END DO

      ! reading of bond tables

      IF (numbond>0) THEN
        DO i = 1, numbond
          READ (ntraj) bndtbl (i, 1), bndtbl (i, 2)
        END DO
      END IF

      bndtbl = bndtbl - nusyst

!     reached end of header: find current position in file

      INQUIRE (unit=ntraj, POS=currentpos)

      ! find timestep size from times in first two frames

      framesizeli = INT (framesize, KIND=li)
      numbeadsli = INT (nsyst, KIND=li)

      READ (ntraj, IOSTAT=ioerror, POS=currentpos) time0
      mypos = currentpos + (numbeadsli + 1_li) * leni_li + (framesizeli * numbeadsli + 7_li) * lend_li
      READ (ntraj, IOSTAT=ioerror, POS=mypos) time

      dt = time - time0

      ! determine numbers of molecules and beads per molecule type

      nmol = 0.0_dp
      nbdmol = 0
      chain = 0
      imol = 0 ! necessary to avoid out of bounds
         
      DO i = 1, nmbeads
         IF (mole (i) /= chain) THEN
            chain = mole (i)
            imol = ltm (i)
            nmol (imol) = nmol (imol) + 1.0_dp
         END IF
         IF (imol > 0) nbdmol (imol) = nbdmol (imol) + 1
      END DO
                  
      DO i = 1, nmoldef
         rnmol = NINT (nmol (i))
         IF (rnmol>0) THEN
            nbdmol (i) = nbdmol (i) / rnmol
         END IF
      END DO

      nbdmolmx = MAXVAL (nbdmol (1:nmoldef))

      ! obtain connectivity information (needed only once)
      CALL connect (nmbeads, numbond, bndtbl, nbdmolmx, visit, from)
      
      ! Checking for charge neutrality of all molecules
      ALLOCATE (molchg (nummol))

      molchg (:) = 0.0_dp

      DO i = 1, nmbeads
         imol = mole (i)
         molchg (imol) = molchg (imol) + chg (ltp (i))
      END DO

      DO i = 1, nummol
         IF (ABS (molchg (i))>1.0e-16_dp) THEN
            WRITE (*,*) "molecule number ",i," is not neutral! (The dipole moment is frame-dependent)"
            WRITE (*,*) "its charge is=", molchg (i)
            WRITE (*,*) "its type is=", nammol (i)
            STOP
         ENDIF
      END DO

      CALL check_molecules ! checks that beads are labelled as expected
      
      ! Get the maximum number of time steps for autocorrelation                                                                                        
      ! and adjust it if necessary

      WRITE (*,*) "Number of time steps in autocorrelation profile? "
      READ (*,*) naf
      IF (naf<1 .OR. naf>numtraj) naf = numtraj

      ! Get the switch for FFT computation

      WRITE (*,*) "switch for FFT computation? (1=yes, 0 or any other integer=no)"
      READ (*,*) n1
      lfft = (n1 == 1)
      
      ALLOCATE (mdipdata (4, nummol, numtraj))
      ALLOCATE (dipx (nummol), dipy (nummol), dipz (nummol))
      
      !reading trajectories and computing charge dipole moments
      ALLOCATE (dipx_box (nmoldef), dipy_box (nmoldef), dipz_box (nmoldef))

      eof = .false.

      DO k = 1, numtraj

        mypos = currentpos + INT (k-1, KIND=li) * ((numbeadsli + 1_li) * leni_li + (framesizeli * numbeadsli + 7_li) * lend_li)
        READ (ntraj, POS = mypos, IOSTAT=ioerror) time, nbeads, dimx, dimy, dimz, shrdx, shrdy, shrdz

        IF (ioerror/=0) THEN
          eof = .true.
          IF (k==1) THEN
            PRINT *, 'ERROR: cannot find trajectory data in HISTORY files'
            STOP
          END IF
          EXIT
        END IF

        READ (ntraj) readint (1:nsyst)
        DO i = 1, nsyst
          global = readint (i)
          READ (ntraj) readdata (1:framesize)
          IF (global>nusyst .AND. global<=nsyst) THEN
            xxx (global-nusyst) = readdata (1)
            yyy (global-nusyst) = readdata (2)
            zzz (global-nusyst) = readdata (3)
          END IF
        END DO

        CALL compute_charge_dipoles (dipx_box, dipy_box, dipz_box, dipx, dipy, dipz)

         ! the dipole components for each individual molecule are stored for all the snapshots 
        DO j = 1, nummol
          mdipdata (1, j, k) = dipx (j)
          mdipdata (2, j, k) = dipy (j)
          mdipdata (3, j, k) = dipz (j)
          mdipdata (4, j, k) = time
        END DO
         
      END DO ! end of loop over trajectories

      IF (k <= numtraj) THEN
         WRITE (*,*) "ERROR: problem with the number of snapshots!"
         STOP
      END IF

      nsamp = numtraj - naf + 1
      
      ALLOCATE (corrdata (naf))

      ! define FFT size if needed

      IF (lfft) THEN
         nftpts = naf ! modify here to change the size of the DFT
         domega = 2 * pi / (dt * nftpts)
         ALLOCATE (fftdata (nftpts))
      END IF
      
      ! Open output file, compute the autocorrelation and write it there

      nrtout = ntraj + 1
      
      IF (numtraj>0) THEN
         
        OPEN (nrtout, file='MDIPAFDAT', status='replace')
        WRITE (nrtout, '(a80)') text
        WRITE (nrtout, '(2i10)') numtraj,naf
        WRITE (nrtout, '(/)')

        ! Open the FT output file if needed
        IF (lfft) THEN
          OPEN (nrtout+1, file='MDIPAFFFT', status='replace')
          WRITE (nrtout+1, '(a80)') text
          WRITE (nrtout+1, '(2i10)') numtraj,nftpts
          WRITE (nrtout+1, '(/)')
        END IF
         
        imol = 0 ! counter for molecules
        DO j = 1, nmoldef
          rnmol = NINT (nmol (j))
          corrdata = 0.0_dp
          WRITE (nrtout,'(a8)') nammol (j)
          IF (lfft) WRITE (nrtout+1,'(a8)') nammol (j)
          DO i = 1, nsamp
            DO k = imol + 1, imol + rnmol
              dx0 = mdipdata (1, k, i)
              dy0 = mdipdata (2, k, i)
              dz0 = mdipdata (3, k, i)
              DO l = 1, naf
                corrdata (l) = corrdata (l) + mdipdata (1, k, i+l-1) * dx0 + mdipdata (2, k, i+l-1) * dy0 &
                             + mdipdata (3, k, i+l-1) * dz0
              END DO
            END DO
          END DO
          corrdata = corrdata / (REAL (nsamp, KIND=dp) * nmol (j))
          imol = imol + rnmol

          DO i = 1, naf
            WRITE (nrtout, '(1p,3e14.6)') REAL (i-1, KIND=dp)*dt, corrdata (i), corrdata (i)/corrdata(1)
          END DO
          WRITE (nrtout, '(/)')
          IF (lfft) THEN
            fftdata (:) = corrdata (:) / corrdata (1) ! adapt here if nftpts differs from naf
            CALL fft (fftdata)
            DO i = 1, nftpts
              WRITE (nrtout+1, '(1p,3e14.6)') REAL (i-1, KIND=dp)*domega, fftdata (i)
            END DO
            WRITE (nrtout+1, '(/)')
          END IF
        END DO
        corrdata = 0.0_dp
        WRITE (nrtout, '("all species")')
        IF (lfft) WRITE (nrtout+1, '("all species")')
        DO i = 1, nsamp
          DO k = 1, nummol
            dx0 = mdipdata (1, k, i)
            dy0 = mdipdata (2, k, i)
            dz0 = mdipdata (3, k, i)
            DO l = 1, naf
              corrdata (l) = corrdata (l) + mdipdata (1, k, i+l-1) * dx0 + mdipdata (2, k, i+l-1) * dy0 &
                           + mdipdata (3, k, i+l-1) * dz0
            END DO
          END DO
        END DO
        corrdata = corrdata / (REAL (nsamp, KIND=dp) * nummol)
       
        DO i = 1, naf
          WRITE (nrtout, '(1p,3e14.6)') REAL (i-1, KIND=dp)*dt, corrdata (i), corrdata (i)/corrdata(1)
        END DO
        WRITE (nrtout, '(/)')
        IF (lfft) THEN
          fftdata (:) = corrdata (:)/ corrdata (1) ! adapt here if nftpts differs from naf
          CALL fft (fftdata)
          DO i = 1, nftpts
            WRITE (nrtout+1, '(1p,3e14.6)') REAL (i-1, KIND=dp)*domega, fftdata (i)
          END DO
          WRITE (nrtout+1, '(/)')
        END IF
      END IF
            
      ! Close the trajectory file
      CLOSE (ntraj)

      ! Close the output files
      CLOSE (nrtout)
      IF (lfft) CLOSE (nrtout+1)
      
      DEALLOCATE (readint, readdata)
      DEALLOCATE (namspe, nammol)
      DEALLOCATE (xxx, yyy, zzz)
      DEALLOCATE (ltp, ltm, mole)
      DEALLOCATE (nmol, nbdmol)
      DEALLOCATE (chg, molchg)
      DEALLOCATE (dipx_box, dipy_box, dipz_box)
      DEALLOCATE (bndtbl)
      DEALLOCATE (visit, from)
      DEALLOCATE (mdipdata, corrdata)
      DEALLOCATE (dipx, dipy, dipz)
      IF (lfft) DEALLOCATE (fftdata)
      
    CONTAINS

      SUBROUTINE check_molecules
!*************************************************************************************
! subroutine to check molecular content and labelling
!
! authors: s. chiacchiera, February 2017 
!*************************************************************************************         
        IMPLICIT NONE
        INTEGER i, j, k, tm, tp, imol, im, ibd
        INTEGER mxmolsize
        INTEGER, ALLOCATABLE :: molbeads (:,:)
        
        mxmolsize = 0
        DO i = 1, nmoldef
           mxmolsize = MAX (nbdmol(i), mxmolsize)
        END DO
        ALLOCATE (molbeads (nmoldef, mxmolsize))
        molbeads (:,:) = 0 
        
        imol = 0
        ibd = 0
        DO i = 1, nmoldef
           DO j = 1, NINT (nmol(i))
              imol = imol +1
              DO k = 1, nbdmol(i)
                 ibd = ibd +1
                 tm = ltm (ibd)
                 tp = ltp (ibd)
                 im = mole (ibd)
                 IF (j==1) THEN
                    molbeads (i, k) = tp
                 ELSE
                    IF (molbeads (i, k) /= tp) THEN
                       WRITE (*,*) "ERROR: Problem with molecular content!"
                       STOP   
                    ENDIF
                 ENDIF
                 IF (tm/=i.OR.im/=imol)THEN
                    WRITE (*,*) "ERROR: Problem with molecules labels!"
                    STOP
                 ENDIF
              END DO
           END DO
        END DO
        IF (imol/=nummol) THEN 
           WRITE (*,*) "ERROR: imol and nummol differ!"
           STOP
        ENDIF
        DEALLOCATE (molbeads)
        RETURN
      END SUBROUTINE check_molecules

      SUBROUTINE compute_charge_dipoles (dipx_box, dipy_box, dipz_box, px, py, pz)
!*************************************************************************************
! subroutine to compute charge dipole moments
!
! authors: m. a. seaton and s. chiacchiera, February 2017 
!
! input: xxx, yyy, zzz (at a given time step) and chg 
! input: visit and from (obtained using connect) 
! output: the x,y,z components of the total dipole, for each molecule type and all
!         individual dipoles (at a given time step) 
!
! (NB: this is a slightly modified version, with different output)
!*************************************************************************************   
        IMPLICIT NONE
        INTEGER i, j, k, tm, tp, imol, ibd, count, ipr 
        REAL(KIND=dp), DIMENSION(nmoldef) :: dipx_box, dipy_box, dipz_box
        REAL(KIND=dp) :: x, y, z, dx, dy, dz, xpre, ypre, zpre
        REAL(KIND=dp) :: dipx, dipy, dipz
        REAL(KIND=dp), DIMENSION(nmbeads) :: xabs, yabs, zabs
        REAL(KIND=dp), DIMENSION(nummol) :: px, py, pz 
        
        dipx_box (:) = 0._dp
        dipy_box (:) = 0._dp
        dipz_box (:) = 0._dp

        imol = 0
        count = 0 
        ! xabs = 0._dp ! just to keep it clean
        ! yabs = 0._dp
        ! zabs = 0._dp
        
        DO i = 1, nmoldef
           tm = i 
           DO j = 1, NINT (nmol(i))
              imol = imol + 1

              dipx = 0._dp ! dipole of a SINGLE molecule
              dipy = 0._dp
              dipz = 0._dp

              DO k = 1, nbdmol(i)
                 count = count + 1
                 ibd = visit (count)
                 ipr = from (count)
                 
                 IF (ipr /= 0) THEN
                    xpre = xabs (ipr)
                    ypre = yabs (ipr)
                    zpre = zabs (ipr)
                 ELSE
                    IF (k == 1) THEN
                       xpre = 0._dp
                       ypre = 0._dp
                       zpre = 0._dp
                    ELSE
                       WRITE (*,*) "Unconnected molecule!"
                       STOP
                    ENDIF
                 ENDIF

                 tp = ltp (ibd)
                 
                 dx = xxx (ibd) - xpre  
                 dy = yyy (ibd) - ypre  
                 dz = zzz (ibd) - zpre
                 
                 dx = dx - dimx * ANINT (dx/dimx)
                 dy = dy - dimy * ANINT (dy/dimy)
                 dz = dz - dimz * ANINT (dz/dimz)
                 
                 x = xpre + dx
                 y = ypre + dy
                 z = zpre + dz
                 
                 
                 dipx = dipx + x * chg (tp)
                 dipy = dipy + y * chg (tp)
                 dipz = dipz + z * chg (tp)
                 
                 xabs (ibd) = x
                 yabs (ibd) = y
                 zabs (ibd) = z
                 
               END DO

               ! storing dipole moments of individual molecules 
               px (imol) = dipx
               py (imol) = dipy               
               pz (imol) = dipz
               
              dipx_box (tm) = dipx_box (tm) + dipx
              dipy_box (tm) = dipy_box (tm) + dipy
              dipz_box (tm) = dipz_box (tm) + dipz
              
           END DO
        END DO

        IF (imol/=nummol) THEN 
           WRITE (*,*) "ERROR: imol and nummol differ!"
           STOP
        ENDIF
        
        RETURN
      END SUBROUTINE compute_charge_dipoles

      SUBROUTINE fft (x)
!*************************************************************************************         
! Subroutine to call FFTW (v3) one-dimensional complex DFT.
! Notice that the input array is overwritten with the its Discrete Fourier Transform.
!
! author: s. chiacchiera, August 2017
! amended: m. a. seaton, January 2021
!*************************************************************************************         
      USE, INTRINSIC :: iso_c_binding
      IMPLICIT none
      INCLUDE 'fftw3.f03'
      COMPLEX(C_DOUBLE_COMPLEX), INTENT(INOUT) :: x (:)
      INTEGER :: n
      TYPE(C_PTR) :: plan

      n = SIZE (x)

      plan = fftw_plan_dft_1d (n, x, x, FFTW_FORWARD, FFTW_ESTIMATE)
      CALL fftw_execute_dft (plan, x, x)
      CALL fftw_destroy_plan (plan)
      
      RETURN
      
      END SUBROUTINE fft
      
END PROGRAM gen_moldipaf

SUBROUTINE connect (nbeads, nbonds, bndtbl, mxmolsize, visit, from)
!**********************************************************************
!  Analyzes all the bonds (bndtbl) to obtain a schedule (visit, from) 
!  to visit the beads so that each cluster is visited along a connected
!  path. "visit" gives the order to include beads, "from" gives the bead 
!  to attach them to.
!  (Note: vocabulary from infection propagation used to move along
!  clusters)
!
!  author: s. chiacchiera, February 2017 
!  amended: m. a. seaton, January 2021
!**********************************************************************
  IMPLICIT none
      INTEGER, INTENT (IN) :: nbeads, nbonds
      INTEGER, INTENT (IN) :: bndtbl (nbonds,2)
      INTEGER, INTENT (IN) :: mxmolsize
      INTEGER :: ic, i, k, nn, nclu, nper, lab, ref, count !j
      INTEGER, ALLOCATABLE :: firstnn (:), lastnn (:), deg (:)
      INTEGER, ALLOCATABLE :: labnn (:)
      INTEGER, ALLOCATABLE :: state (:)
      INTEGER, ALLOCATABLE :: perlab (:), perref (:)
      INTEGER, ALLOCATABLE :: nchist (:)
      INTEGER, INTENT (OUT) :: visit (nbeads), from (nbeads)
      
      ALLOCATE (firstnn (nbeads), lastnn (nbeads), deg (nbeads), labnn (2*nbonds))
      ALLOCATE (state (nbeads))
      ALLOCATE (perlab (nbeads), perref (nbeads))
      ALLOCATE (nchist (mxmolsize)) 
      !-----------------------------------------------------------------------
      CALL organize (nbeads, nbonds, labnn, firstnn, lastnn, deg)
      !-----------------------------------------------------------------------
      state (:) = 0
      nchist (:) = 0
      visit (:) = 0
      from (:) = 0
      count = 0
      !-----------------------------------------------------------------------
      ic = 0 
      !-----------------------------------------------------------------------
      DO WHILE (ic < nbeads) ! ic = label of bead used to "grow" a cluster
         ic = ic + 1
         IF( state (ic) /= 0) THEN
            WRITE (*,*) "ERROR: labels are not as expected!"
            STOP
         END IF
         nclu = 1 
         count = count + 1
         visit (ic) = ic 
         IF (deg (ic) == 0) THEN
            state (ic) = -1
            IF (nclu <= mxmolsize) nchist (nclu) = nchist (nclu) +1
            CYCLE
         END IF
         state (ic) = 1          ! ic is "infected"

         ! nearest neighbours of ic are marked as "goint to be infected" -> a.k.a. perimeter  
         nper = 0 
         perlab (:) = 0
         perref (:) = 0         
         DO k = firstnn (ic), lastnn (ic)
            nn = labnn (k)
            IF( state (nn) /= 0) THEN
               WRITE (*,*) "ERROR: labels are not as expected!"
               STOP
            END IF
            nper = nper + 1
            perlab (nper) = nn !new bead in perimeter
            perref (nper) = ic  !its reference bead (origin of the link)
            state (nn) = 2
         END DO
         state (ic) = 3 ! ic is "dead"
         
         DO WHILE (nper > 0)
            i = 1 ! pick a bead of "perimeter" to be analyzed
            lab = perlab (i)
            ref = perref (i)            
            perlab (i) = perlab (nper)            
            perref (i) = perref (nper)                        
            nper = nper - 1 
            IF (state (lab) == 3) THEN
               CYCLE
            END IF
            state (lab) = 1 ! "lab" is added to the cluster
            nclu = nclu + 1
            count = count + 1
            visit (count) = lab
            from (count) = ref
            
            DO k = firstnn (lab), lastnn (lab)  ! check nn of newly added 
               nn = labnn (k)
               IF( (state (nn) == 2) .OR. (state (nn) == 3)) CYCLE
               nper = nper + 1
               perlab (nper) = nn !new bead in perimeter
               perref (nper) = lab  !its reference bead (origin of the link)
               state (nn) = 2
            END DO
            state (lab) = 3            
           
         END DO
         nchist (nclu) = nchist (nclu) +1
         ic = ic + nclu - 1 ! prepare ic for the next cluster
      END DO
      WRITE (*,*) "nchist: ", nchist
      !-----------------------------------------------------------------------      
      DEALLOCATE (firstnn, lastnn, deg, labnn)
      DEALLOCATE (state)
      DEALLOCATE (perlab, perref)
      DEALLOCATE (nchist) 
      RETURN
      !-----------------------------------------------------------------------
      CONTAINS
      !-----------------------------------------------------------------------
        SUBROUTINE organize (N, NL, labnn, firstnn, lastnn, deg)
        !**********************************************************************
        ! Analyzes the bonds (bndtbl) to obtain the degree (=number of bonds)
        ! of each bead, and the nearest neighbours list.
        ! N in the number of beads (vertices) and NL of bonds (links). 
        !
        ! author: s. chiacchiera, February 2017 
        !**********************************************************************
          IMPLICIT none
      INTEGER, INTENT(IN) :: N, NL
      INTEGER :: i,l,count_lab, i1,i2
      INTEGER, DIMENSION (N), INTENT(OUT) :: deg
      INTEGER, DIMENSION (N), INTENT(OUT) :: firstnn, lastnn
      INTEGER, DIMENSION (2*NL), intent(OUT) :: labnn
      
      deg(:)=0
      firstnn(:)=0
      lastnn(:)=0
      labnn(:)=0 

      count_lab=0

      DO i=1,N
         DO l=1,NL
            IF(bndtbl(l,1).EQ.i)THEN  
               deg(i)=deg(i)+1
               count_lab=count_lab+1
               labnn(count_lab)=bndtbl(l,2)
            ENDIF
            IF(bndtbl(l,2).EQ.i)THEN 
               deg(i)=deg(i)+1
               count_lab=count_lab+1
               labnn(count_lab)=bndtbl(l,1)
            ENDIF
         END DO
      END DO
      
      i1=1
      i2=0
      DO i=1,N
         IF (deg (i)==0) CYCLE
         firstnn(i)=i1
         i2=i1+deg(i)-1
         lastnn(i)=i2
         i1=i2+1
      END DO
      
      RETURN
      
    END SUBROUTINE organize
    !-----------------------------------------------------------------------
  END SUBROUTINE connect
  
