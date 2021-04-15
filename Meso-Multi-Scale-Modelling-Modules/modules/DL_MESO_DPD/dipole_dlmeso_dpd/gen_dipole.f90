PROGRAM gen_dipole
!***********************************************************************************
! module to analyze charge dipole moments in DL_MESO
!
! authors: m. a. seaton and s. chiacchiera, February 2017 (amended January 2021)
!**********************************************************************************            
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
      INTEGER :: chain, imol, ioerror, i, numtraj, j, k, nmoldef, ibond, nbdmolmx
      INTEGER :: nspe, nbeads, nusyst, nmbeads, nsyst, numbond, global, species, molecule
      INTEGER :: nummol, lfrzn, rnmol, keytrj, srfx, srfy, srfz
      INTEGER :: nav
      INTEGER :: endver, Dlen, nstep, framesize, lend, leni
      INTEGER(KIND=li) :: filesize, currentpos, lend_li, leni_li

      REAL(KIND=dp), ALLOCATABLE :: xxx (:), yyy (:), zzz (:), readdata (:)
      REAL(KIND=dp), ALLOCATABLE :: nmol (:), chg (:), molchg (:)
      REAL(KIND=dp), ALLOCATABLE :: dipx_box (:), dipy_box (:), dipz_box (:)
      REAL(KIND=dp), ALLOCATABLE :: dip2_box (:), dip2_ave (:)
      REAL(KIND=dp), ALLOCATABLE :: dip2_err (:)
      REAL(KIND=dp), ALLOCATABLE :: sum_dipx_box (:), sum_dipy_box (:), sum_dipz_box (:)
      REAL(KIND=dp), ALLOCATABLE :: sum_dipx_box2 (:), sum_dipy_box2 (:), sum_dipz_box2 (:)
      REAL(KIND=dp), ALLOCATABLE :: sum_dip2_box (:), sum_dip_box4 (:)
      REAL(KIND=dp), ALLOCATABLE :: dipx_box_ave (:), dipy_box_ave (:), dipz_box_ave (:)
      REAL(KIND=dp), ALLOCATABLE :: dipx_box2_ave (:), dipy_box2_ave (:), dipz_box2_ave (:)
      REAL(KIND=dp), ALLOCATABLE :: dipx_box_err (:), dipy_box_err (:), dipz_box_err (:)
      REAL(KIND=dp), ALLOCATABLE :: dip_box2_ave (:), dip_box2_err (:), dip_box4_ave (:) 
      REAL(KIND=dp), ALLOCATABLE :: gk (:), gk_err (:)
      REAL(KIND=dp), ALLOCATABLE :: sum_dip2_box2 (:)
      REAL(KIND=dp), ALLOCATABLE :: epsilon_r (:), epsilon_r_err(:)
      REAL(KIND=dp) :: bjerelec
      REAL(KIND=dp) :: volm, dimx, dimy, dimz, shrdx, shrdy, shrdz
      REAL(KIND=dp) :: amass, rcii
      REAL(KIND=dp) :: time

      LOGICAL :: eof, swapend, bigend
      
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

      IF (srfx==1 .OR. srfy==1 .OR. srfz==1) THEN
         WRITE (*,*) "ERROR: Systems under shear not yet implemented!"
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

      nummol = 0 !counter for number of molecules
      ibond = 0  !counter for bonds

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

      molchg (:) = 0._dp

      DO i = 1, nmbeads
         imol = mole (i)
         molchg (imol) = molchg (imol) + chg (ltp (i))
      END DO

      DO i = 1, nummol
         IF (ABS (molchg (i)) > 1.d-16) THEN
            WRITE (*,*) "molecule number",i," is not neutral! (The dipole moment is frame-dependent)"
            WRITE (*,*) "its charge is=", molchg (i)
            WRITE (*,*) "its type is=", nammol (i)
            STOP
         ENDIF
      END DO

      CALL check_molecules !checks that beads are labelled are as expected

      !reading trajectories and computing charge dipole moments
      ALLOCATE (dipx_box (nmoldef), dipy_box (nmoldef), dipz_box (nmoldef))
      ALLOCATE (dip2_box (nmoldef))
      ALLOCATE (sum_dipx_box (nmoldef), sum_dipy_box (nmoldef), sum_dipz_box (nmoldef))
      ALLOCATE (sum_dipx_box2 (nmoldef), sum_dipy_box2 (nmoldef), sum_dipz_box2 (nmoldef))
      ALLOCATE (sum_dip2_box (nmoldef), sum_dip_box4 (nmoldef))
      ALLOCATE (dipx_box_ave (nmoldef), dipy_box_ave (nmoldef), dipz_box_ave (nmoldef)) 
      ALLOCATE (dip2_ave (nmoldef), dip2_err (nmoldef))
      ALLOCATE (dipx_box2_ave (nmoldef), dipy_box2_ave (nmoldef), dipz_box2_ave (nmoldef)) 
      ALLOCATE (dipx_box_err (nmoldef), dipy_box_err (nmoldef), dipz_box_err (nmoldef)) 
      ALLOCATE (dip_box2_ave (nmoldef), dip_box2_err (nmoldef), dip_box4_ave (nmoldef))
      ALLOCATE (sum_dip2_box2 (nmoldef))
      ALLOCATE (gk (nmoldef), gk_err (nmoldef))
      ALLOCATE (epsilon_r (nmoldef), epsilon_r_err(nmoldef))

      ! Open and write output file(s)
      
      nrtout = ntraj + 1
      DO j = 1, nmoldef
         OPEN (nrtout+j-1, file = 'dipole_'//nammol(j), status ='replace')
      END DO

      eof = .false.
      nav = 0

      sum_dipx_box = 0.0_dp
      sum_dipy_box = 0.0_dp
      sum_dipz_box = 0.0_dp

      sum_dip2_box = 0.0_dp
      sum_dip_box4 = 0.0_dp

      sum_dip2_box2 = 0.0_dp
      
      sum_dipx_box2 = 0.0_dp
      sum_dipy_box2 = 0.0_dp
      sum_dipz_box2 = 0.0_dp

      DO k = 1, numtraj

         READ (ntraj, IOSTAT=ioerror) time, nbeads, dimx, dimy, dimz, shrdx, shrdy, shrdz
         
         IF (ioerror/=0) THEN
            eof = .true.
            IF (k==1) THEN
               WRITE (*,*) 'ERROR: cannot find trajectory data in HISTORY files'
               STOP
            END IF
            EXIT
         END IF
         
         nav = nav + 1
         volm = dimx * dimy * dimz

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

         CALL compute_charge_dipoles (dipx_box, dipy_box, dipz_box, dip2_box)
         
         DO j = 1, nmoldef
            WRITE (nrtout+j-1, '(1p,I8,5(2x,e14.6))')  k, dipx_box(j), dipy_box(j), dipz_box(j), dip2_box(j) / nmol(j) , &
                 (dipx_box(j)**2 + dipy_box(j)**2 + dipz_box(j)**2) / volm
         END DO
         
         sum_dipx_box =  sum_dipx_box + dipx_box
         sum_dipy_box =  sum_dipy_box + dipy_box
         sum_dipz_box =  sum_dipz_box + dipz_box

         sum_dipx_box2 = sum_dipx_box2 + dipx_box * dipx_box
         sum_dipy_box2 = sum_dipy_box2 + dipy_box * dipy_box
         sum_dipz_box2 = sum_dipz_box2 + dipz_box * dipz_box

         sum_dip_box4 =  sum_dip_box4 + (dipx_box**2 + dipy_box**2 + dipz_box**2)**2
                  
         sum_dip2_box =  sum_dip2_box + dip2_box

         sum_dip2_box2 = sum_dip2_box2 + dip2_box * dip2_box
         
      END DO ! end of loop over trajectories

      dipx_box_ave = sum_dipx_box/REAL(nav, KIND=dp)
      dipy_box_ave = sum_dipy_box/REAL(nav, KIND=dp)
      dipz_box_ave = sum_dipz_box/REAL(nav, KIND=dp)

      dipx_box2_ave = sum_dipx_box2/REAL(nav, KIND=dp)
      dipy_box2_ave = sum_dipy_box2/REAL(nav, KIND=dp)
      dipz_box2_ave = sum_dipz_box2/REAL(nav, KIND=dp)
      
      dip2_ave = sum_dip2_box(:)/REAL(nav, KIND=dp)/REAL(nmol(:), KIND=dp)
      dip_box2_ave = dipx_box2_ave + dipy_box2_ave + dipz_box2_ave
      dip_box4_ave =  sum_dip_box4/REAL(nav, KIND=dp)
      
      dipx_box_err = SQRT((dipx_box2_ave - dipx_box_ave**2)/REAL(nav, KIND=dp))
      dipy_box_err = SQRT((dipy_box2_ave - dipy_box_ave**2)/REAL(nav, KIND=dp))
      dipz_box_err = SQRT((dipz_box2_ave - dipz_box_ave**2)/REAL(nav, KIND=dp))

      dip_box2_err = sqrt((dip_box4_ave - dip_box2_ave**2)/REAL(nav, KIND=dp))

      ! Error on dip2_ave is computed considering each snapshot as a sample
      
      dip2_err = sum_dip2_box2 / dble(nav * nmol ** 2) - dip2_ave ** 2
      dip2_err =  sqrt (dip2_err / REAL(nav, KIND=dp))
      
      WRITE (*,*) "Number of snapshots: ",nav
      WRITE (*,*) "<P_x>, <P_y>, <P_z>:"
      WRITE (*,98) dipx_box_ave, dipy_box_ave, dipz_box_ave
      WRITE (*,*) "error:"
      WRITE (*,98) dipx_box_err, dipy_box_err, dipz_box_err
      WRITE (*,*) "<P^2>/V:"
      WRITE (*,98) dip_box2_ave/volm
      WRITE (*,*) "error:"
      WRITE (*,98) dip_box2_err/volm          
      WRITE (*,*) "<p^2>:"
      WRITE (*,98) dip2_ave
      WRITE (*,*) "error:"
      WRITE (*,98) dip2_err
      
      IF (nmoldef == 1) THEN
         gk = dip_box2_ave / dip2_ave / REAL(nmol, KIND=dp)
         gk_err = (dip_box2_err / dip_box2_ave + dip2_err / dip2_ave) * gk
         WRITE (*,*) "kirkwood factor:"
         WRITE (*,98) gk
         WRITE (*,*) "error:"
         WRITE (*,98) gk_err
         WRITE (*,*) "Bjerrum length?"
         READ (*,*) bjerelec
         epsilon_r = 1.0_dp + 4.0_dp * pi / 3.0_dp * bjerelec * dip_box2_ave / volm
         epsilon_r_err =  4.0_dp * pi / 3.0_dp * bjerelec * dip_box2_err / volm
         WRITE (*,*) "epsilon_r:"
         WRITE (*,98) epsilon_r
         WRITE (*,*) "error:"
         WRITE (*,98) epsilon_r_err
      ENDIF
      
      ! Close the trajectory file
      CLOSE (ntraj)

      !close output files
      DO j = 1, nmoldef
         CLOSE (nrtout+j-1)
      END DO

      DEALLOCATE (readint, readdata)
      DEALLOCATE (namspe, nammol)
      DEALLOCATE (xxx, yyy, zzz)
      DEALLOCATE (ltp, ltm, mole)
      DEALLOCATE (nmol, nbdmol)
      DEALLOCATE (chg, molchg)
      DEALLOCATE (dipx_box, dipy_box, dipz_box)
      DEALLOCATE (dip2_box)
      DEALLOCATE (sum_dipx_box, sum_dipy_box, sum_dipz_box)
      DEALLOCATE (sum_dipx_box2 , sum_dipy_box2 , sum_dipz_box2)
      DEALLOCATE (sum_dip2_box,  sum_dip_box4, sum_dip2_box2)
      DEALLOCATE (dipx_box_ave , dipy_box_ave , dipz_box_ave) 
      DEALLOCATE (dip2_ave, dip2_err)
      DEALLOCATE (dipx_box2_ave , dipy_box2_ave , dipz_box2_ave) 
      DEALLOCATE (dipx_box_err , dipy_box_err , dipz_box_err) 
      DEALLOCATE (dip_box2_ave, dip_box2_err, dip_box4_ave)
      DEALLOCATE (gk, gk_err)
      DEALLOCATE (epsilon_r, epsilon_r_err)
      DEALLOCATE (bndtbl)
      DEALLOCATE (visit, from)
      
98    FORMAT(1p,9(e13.6,3x))
       
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

SUBROUTINE compute_charge_dipoles (dipx_box, dipy_box, dipz_box, dip2_box)
!*************************************************************************************
! subroutine to compute charge dipole moments
!
! authors: m. a. seaton and s. chiacchiera, February 2017 
!
! input: xxx, yyy, zzz (at a given time step) and chg 
! input: visit and from (obtained using connect) 
! output: the x,y,z components of the total dipole, sum p_i^2/N_mol for each molecule
!         type (at a given time step)
!*************************************************************************************
        IMPLICIT NONE
        INTEGER i, j, k, tm, tp, imol, ibd, count, ipr
        REAL(KIND=dp), DIMENSION(nmoldef) :: dipx_box, dipy_box, dipz_box
        REAL(KIND=dp), DIMENSION(nmoldef) :: dip2_box
        REAL(KIND=dp) :: x, y, z, dx, dy, dz, xpre, ypre, zpre
        REAL(KIND=dp) :: dipx, dipy, dipz, dip2
        REAL(KIND=dp), DIMENSION(nmbeads) :: xabs, yabs, zabs
        
        dipx_box (:) = 0._dp
        dipy_box (:) = 0._dp
        dipz_box (:) = 0._dp

        dip2_box (:) = 0._dp

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

              dipx_box (tm) = dipx_box (tm) + dipx
              dipy_box (tm) = dipy_box (tm) + dipy
              dipz_box (tm) = dipz_box (tm) + dipz

              dip2 = dipx * dipx + dipy * dipy + dipz * dipz

              dip2_box (tm) = dip2_box (tm) + dip2
              
           END DO
        END DO

        IF (imol/=nummol) THEN 
           WRITE (*,*) "ERROR: imol and nummol differ!"
           STOP
        ENDIF
        
        RETURN
      END SUBROUTINE compute_charge_dipoles
      
End PROGRAM gen_dipole
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
      INTEGER, INTENT (INOUT) :: bndtbl (nbonds,2)
      INTEGER, INTENT (IN) :: nbeads, nbonds
      INTEGER, INTENT (IN) :: mxmolsize
      INTEGER :: ic, i, k, nn, nclu, nper, lab, ref, count
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
