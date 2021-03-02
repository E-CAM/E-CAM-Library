PROGRAM gen_dipole
!***********************************************************************************
! module to analyze charge dipole moments in DL_MESO
!
! authors: m. a. seaton and s. chiacchiera, February 2017
!**********************************************************************************            
      IMPLICIT none
      INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND (15, 307)
      INTEGER, PARAMETER :: ntraj=10
      REAL(KIND=dp), PARAMETER :: pi=3.141592653589793_dp

      CHARACTER(80) :: text, a2
      CHARACTER(8), ALLOCATABLE :: namspe (:), nammol (:)
      CHARACTER(6) :: chan
      CHARACTER(8) :: a1
      
      INTEGER, ALLOCATABLE :: ltp (:), ltm (:), mole (:), bndtbl (:,:), beads (:), bonds (:) 
      INTEGER, ALLOCATABLE :: nbdmol (:)
      INTEGER, ALLOCATABLE :: visit (:), from (:)
      INTEGER :: nrtout
      INTEGER :: chain, imol, ibead, ioerror, i, numtraj, j, k, l, m, nmoldef, ibond
      INTEGER :: nspe, numnodes, nbeads, nusyst, nmbeads, nsyst, nbonds, numbond, global, species, molecule
      INTEGER :: nummol, lfrzn, rnmol, keytrj, srfx, srfy, srfz
      INTEGER :: indx, nav
      INTEGER :: n1, n2, n3, n4
      INTEGER :: bead1, bead2
      
      REAL(KIND=dp), ALLOCATABLE :: xxx (:), yyy (:), zzz (:)
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
      REAL(KIND=dp) :: time, mbeads, mglobal, x, y, z, vx, vy, vz, fx, fy, fz, rsq
      REAL(KIND=dp) :: r1, r2, r3, r4
      
      LOGICAL :: eof
      
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
      END DO ! loop over nodes

      IF (numbond==0) THEN
	PRINT *, 'ERROR: no molecules in trajectory data!'
        STOP
      END IF
      
      IF (srfx == 3 .OR. srfy == 3 .OR. srfz == 3) THEN
         WRITE (*,*) "ERROR: System under shear, not implemented yet!"
         STOP
      END IF
            
!     get number of beads to be tracked when reading trajectory file (molecular beads)
      nmbeads = nsyst - nusyst

      ALLOCATE (namspe (nspe), nammol (nmoldef))
      ALLOCATE (xxx (1:nmbeads), yyy (1:nmbeads), zzz (1:nmbeads))
      ALLOCATE (ltp (1:nmbeads), ltm (1:nmbeads), mole (1:nmbeads))
      ALLOCATE (nmol (1:nmoldef), nbdmol (1:nmoldef))
      ALLOCATE (chg (nspe))
      ALLOCATE (bndtbl (numbond, 2))
      ALLOCATE (visit (nmbeads), from (nmbeads)) 

      DO j = 1, numnodes
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
         END DO

         IF (nmoldef>0) THEN
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
            
      ENDDO ! end of loop over nodes
               
      DO j = 1, numnodes
         CLOSE (ntraj+j-1)
      END DO
      
      ! Second reading, where arrays are filled with properties of beads and molecules.
      ! Then, the snapshots of trajectories are read.

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
         !Build ltp, ltm, mole
         DO i = 1, beads (j)
            READ (ntraj+j-1) global, species, molecule, chain
            IF (global>nusyst .AND. global<=nsyst) THEN
               ltp (global-nusyst) = species
               ltm (global-nusyst) = molecule
               mole (global-nusyst) = chain
               nummol = MAX (nummol, chain)
            ENDIF
         END DO
         
         IF (bonds (j)>0) THEN
            ! Build bndtbl
            DO i = 1, bonds (j)
               ibond = ibond + 1
               READ (ntraj+j-1) bead1, bead2
               bndtbl (ibond, 1) = bead1
               bndtbl (ibond, 2) = bead2
            END DO
         END IF   
      END DO ! over nodes
      
      IF (ibond /= numbond) THEN
         WRITE (*,*) "ERROR: bndtbl is not completely full!"
         STOP
      ENDIF
         
      bndtbl = bndtbl - nusyst

      ! obtain connectivity information (needed only once)
      CALL connect (nmbeads, numbond, bndtbl, visit, from) 
      
      ! determine numbers of molecules and beads per molecule type
      nmol = 0.0_dp
      nbdmol = 0
      chain = 0
      imol = 0 !necessary to avoid out of bounds
         
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

      !Asking the user to input the charges for each particle species
      DO i = 1, nspe
         WRITE (*,*) "Charges for SPECIES type ", namspe(i)," :"
         READ (*,*) chg (i)
      END DO

      WRITE (*,'("chg=",10(3x,f10.4))') chg
      
      !Checking for charge neutrality of all molecules
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

      call check_molecules !checks that beads are labelled are as expected

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
      
      nrtout = ntraj + numnodes 
      DO j = 1, nmoldef
         OPEN (nrtout+j-1, file = 'dipole_'//nammol(j), status ='replace')
      END DO

      eof = .false.
      k = 0
      nav = 0

      sum_dipx_box = 0._dp
      sum_dipy_box = 0._dp
      sum_dipz_box = 0._dp

      sum_dip2_box = 0._dp
      sum_dip_box4 = 0._dp

      sum_dip2_box2 = 0._dp
      
      sum_dipx_box2 = 0._dp
      sum_dipy_box2 = 0._dp
      sum_dipz_box2 = 0._dp

      DO WHILE (.true.)
         READ (ntraj, IOSTAT=ioerror) time, mbeads, dimx, dimy, dimz, shrdx, shrdy, shrdz 
         
         IF (ioerror/=0) THEN
            eof = .true.
            IF (k==0) THEN
               WRITE (*,*) 'ERROR: cannot find trajectory data in HISTORY files'
               STOP
            END IF
            EXIT
         END IF
         
         k = k + 1
         nav = nav + 1
         
         DO j = 1, numnodes

            IF (j>1) THEN
               READ (ntraj+j-1, IOSTAT=ioerror) time, mbeads, dimx, dimy, dimz, shrdx, shrdy, shrdz              
               IF (ioerror/=0) THEN
                  eof = .true.
                  WRITE (*,*) 'ERROR: End of file reached prematurely - ', k-1, ' timesteps written', &
                       ' to output files' 
                  EXIT
               END IF
            END IF
            
            nbeads = NINT (mbeads)
         
            SELECT CASE (keytrj)
            CASE (0)
               DO i = 1, nbeads
                  READ (ntraj+j-1) mglobal, x, y, z
                  global = NINT (mglobal)
                  IF (global>nusyst .AND. global<=nsyst) THEN
                     xxx (global-nusyst) = x
                     yyy (global-nusyst) = y
                     zzz (global-nusyst) = z
                  END IF
               END DO
            CASE (1)
               DO i = 1, nbeads
                  READ (ntraj+j-1) mglobal, x, y, z, vx, vy, vz
                  global = NINT (mglobal)
                  IF (global>nusyst .AND. global<=nsyst) THEN
                     xxx (global-nusyst) = x
                     yyy (global-nusyst) = y
                     zzz (global-nusyst) = z
                  END IF
               END DO
            CASE (2)
               DO i = 1, nbeads
                  READ (ntraj+j-1) mglobal, x, y, z, vx, vy, vz, fx, fy, fz
                  global = NINT (mglobal)
                  IF (global>nusyst .AND. global<=nsyst) THEN
                     xxx (global-nusyst) = x
                     yyy (global-nusyst) = y
                     zzz (global-nusyst) = z
                  END IF
               END DO
            END SELECT

         END DO ! over nodes
            
         call compute_charge_dipoles (dipx_box, dipy_box, dipz_box, dip2_box)
         
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

      dipx_box_ave = sum_dipx_box/dble(nav)
      dipy_box_ave = sum_dipy_box/dble(nav)
      dipz_box_ave = sum_dipz_box/dble(nav)

      dipx_box2_ave = sum_dipx_box2/dble(nav)
      dipy_box2_ave = sum_dipy_box2/dble(nav)
      dipz_box2_ave = sum_dipz_box2/dble(nav)
      
      dip2_ave = sum_dip2_box(:)/dble(nav)/dble(nmol(:)) 
      dip_box2_ave = dipx_box2_ave + dipy_box2_ave + dipz_box2_ave
      dip_box4_ave =  sum_dip_box4/dble(nav)
      
      dipx_box_err = sqrt((dipx_box2_ave - dipx_box_ave**2)/dble(nav))
      dipy_box_err = sqrt((dipy_box2_ave - dipy_box_ave**2)/dble(nav))
      dipz_box_err = sqrt((dipz_box2_ave - dipz_box_ave**2)/dble(nav))

      dip_box2_err = sqrt((dip_box4_ave - dip_box2_ave**2)/dble(nav))

      ! Error on dip2_ave is computed considering each snapshot as a sample
      
      dip2_err = sum_dip2_box2 / dble(nav * nmol ** 2) - dip2_ave ** 2
      dip2_err =  sqrt (dip2_err / dble(nav))
      
      WRITE (*,*) "Number of snapshots: ",k
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
         gk = dip_box2_ave / dip2_ave / dble(nmol)
         gk_err = (dip_box2_err / dip_box2_ave + dip2_err / dip2_ave) * gk
         WRITE (*,*) "kirkwood factor:"
         WRITE (*,98) gk
         WRITE (*,*) "error:"
         WRITE (*,98) gk_err
         WRITE (*,*) "Bjerrum length?"
         READ (*,*) bjerelec
         epsilon_r = 1._dp + 4 * pi / 3._dp * bjerelec * dip_box2_ave / volm
         epsilon_r_err =  4 * pi / 3._dp * bjerelec * dip_box2_err / volm
         WRITE (*,*) "epsilon_r:"
         WRITE (*,98) epsilon_r
         WRITE (*,*) "error:"
         WRITE (*,98) epsilon_r_err
      ENDIF
      
      ! Close the trajectory files
      DO j = 1, numnodes
         CLOSE (ntraj+j-1)
      END DO

      !close output files
      DO j = 1, nmoldef
         CLOSE (nrtout+j-1)
      END DO

      DEALLOCATE (beads, bonds)
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
      
99    FORMAT(f10.1,2x,9(e13.6,2x))
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
        INTEGER i, j, k, tm, tp, imol, im, ibd, count, ipr
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
SUBROUTINE connect (nbeads, nbonds, bndtbl, visit, from)
!**********************************************************************
!  Analyzes all the bonds (bndtbl) to obtain a schedule (visit, from) 
!  to visit the beads so that each cluster is visited along a connected
!  path. "visit" gives the order to include beads, "from" gives the bead 
!  to attach them to.
!  (Note: vocabulary from infection propagation used to move along
!  clusters)
!
!  author: s. chiacchiera, February 2017 
!**********************************************************************
      IMPLICIT none
      INTEGER, INTENT (IN) :: bndtbl (nbonds,2)
      INTEGER, INTENT (IN) :: nbeads, nbonds
      INTEGER :: ic, i, j, k, nn, nclu, nper, lab, ref, count
      INTEGER :: mxmolsize
      INTEGER, ALLOCATABLE :: firstnn (:), lastnn (:), deg (:)
      INTEGER, ALLOCATABLE :: labnn (:)
      INTEGER, ALLOCATABLE :: state (:)
      INTEGER, ALLOCATABLE :: perlab (:), perref (:)
      INTEGER, ALLOCATABLE :: nchist (:)
      INTEGER, INTENT (OUT) :: visit (nbeads), from (nbeads)
      
      mxmolsize = 10

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
