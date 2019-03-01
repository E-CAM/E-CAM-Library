PROGRAM tetrahedral
!***********************************************************************************
!
! module to analyze tetrahedral ordering in dl_meso HISTORY files
!
! authors - m. a. seaton & s. chiacchiera, january 2018
!
!**********************************************************************************
  IMPLICIT none
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND (15, 307)
  INTEGER, PARAMETER :: si = SELECTED_INT_KIND (8)
  INTEGER, PARAMETER :: li = SELECTED_INT_KIND (12)
  
  REAL(KIND=dp), PARAMETER :: pi=3.141592653589793_dp

  INTEGER, PARAMETER :: ntraj=10
  
  CHARACTER(80) :: text, a2
  CHARACTER(8), ALLOCATABLE :: namspe (:), nammol (:)
  CHARACTER(6) :: chan
  CHARACTER(8) :: a1
  
  INTEGER, ALLOCATABLE :: ltp (:), ltm (:), mole (:), bndtbl (:,:), beads (:), bonds (:), nspec (:)
  INTEGER :: nrtout
  INTEGER :: chain, imol, ibead, ioerror, i, numtraj, j, k, l, m, nmoldef, ibond
  INTEGER :: nspe, numnodes, nbeads, nusyst, nmbeads, nsyst, nbonds, numbond, global, species, molecule
  INTEGER :: nummol, lfrzn, rnmol, keytrj, srfx, srfy, srfz
  INTEGER :: indx, nav
  INTEGER :: n1, n2, n3, n4
  INTEGER :: bead1, bead2
      
  REAL(KIND=dp), ALLOCATABLE :: xxx (:), yyy (:), zzz (:)
  REAL(KIND=dp), ALLOCATABLE :: nmol (:)
  REAL(KIND=dp) :: volm, dimx, dimy, dimz, shrdx, shrdy, shrdz
  REAL(KIND=dp) :: amass, rcii
  REAL(KIND=dp) :: time, mbeads, mglobal, x, y, z, vx, vy, vz, fx, fy, fz, rsq
  REAL(KIND=dp) :: r1, r2, r3, r4
  
  LOGICAL :: eof

! Variables for tetrahedral ordering
  INTEGER :: nnlab(4), npart, sp, count
  REAL(KIND=dp), DIMENSION(3):: rt1, rt2, rt3, rt4, rt5  
  REAL(KIND=dp) :: qtetra, stetra
  REAL(KIND=dp) :: q, sk, q_sum, sk_sum, q_ave, sk_ave
  REAL(KIND=dp) :: q2_sum, sk2_sum, q2_ave, sk2_ave

  !-----------------------------------------------------------------------------------------
  
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

  ! IF (srfx == 3 .OR. srfy == 3 .OR. srfz == 3) THEN
  !    WRITE (*,*) "ERROR: System under shear, not implemented yet!"
  !    STOP
  ! END IF

  ALLOCATE (namspe (nspe), nammol (nmoldef), nspec (nspe)) !NB: I mean ALL beads of a type, not only unbonded
  ALLOCATE (xxx (1:nsyst), yyy (1:nsyst), zzz (1:nsyst))
  ALLOCATE (ltp (1:nsyst))
  
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
  
  ibond = 0  !counter for bonds
  nspec (:) = 0 ! populations
 !     fill in arrays for beads and bonds
  DO j = 1, numnodes
     !Build ltp, ltm, mole
     DO i = 1, beads (j)
        READ (ntraj+j-1) global, species, molecule, chain
        ltp (global) = species
        nspec (species) = nspec (species) + 1
     END DO

     IF (bonds (j)>0) THEN
        ! Build bndtbl
        DO i = 1, bonds (j)
           ibond = ibond + 1
           READ (ntraj+j-1) bead1, bead2
!           bndtbl (ibond, 1) = bead1
!           bndtbl (ibond, 2) = bead2
        END DO
     END IF
     
  END DO ! over nodes

  IF (ibond /= numbond) THEN
     WRITE (*,*) "ERROR: mismatch in number of bonds!"
     STOP
  ENDIF
  
! Find number of beads for which trajectories are needed  
  WRITE(*,*) "Which species number has to be analyzed?"
  READ(*,*) sp  
  IF (sp<0 .OR. sp>nspe) THEN
     WRITE(*,*) "error: undefined species!"
     STOP
  END IF
  
  npart = nspec (sp)

  WRITE(*,*) "nsyst=",nsyst
  WRITE(*,*) "nspec=",nspec
  WRITE(*,*) "npart=",npart
  
!  ALLOCATE (xxx (1:npart), yyy (1:npart), zzz (1:npart))! I think better to keep nsyst
  
  ! Open and write output file
      
  nrtout = ntraj + numnodes 
  OPEN(nrtout, file = 'TETRADAT', status ='replace')
  WRITE (nrtout,*) "# Local ordering for beads of species: ", namspe (sp)
  WRITE (nrtout,*) "# dimx, dimy, dimz=", dimx, dimy, dimz
  WRITE (nrtout,*) "# snapshot number, q, sk"  
  
  eof = .false.
  k = 0
  nav = 0

  q_sum = 0
  sk_sum = 0
  q2_sum = 0
  sk2_sum = 0
  
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

        xxx (:) = 0._dp !OK?
        yyy (:) = 0._dp
        zzz (:) = 0._dp

        count = 0 
        
        SELECT CASE (keytrj)
        CASE (0)
           DO i = 1, nbeads
              READ (ntraj+j-1) mglobal, x, y, z
              global = NINT (mglobal)
              IF (ltp (global) == sp) THEN
                 xxx (global) = x
                 yyy (global) = y
                 zzz (global) = z
                 count = count + 1
              END IF
           END DO
        CASE (1)
           DO i = 1, nbeads
              READ (ntraj+j-1) mglobal, x, y, z, vx, vy, vz
              global = NINT (mglobal)
              IF (ltp (global) == sp) THEN
                 xxx (global) = x
                 yyy (global) = y
                 zzz (global) = z
                 count = count + 1
              END IF
           END DO
        CASE (2)
           DO i = 1, nbeads
              READ (ntraj+j-1) mglobal, x, y, z, vx, vy, vz, fx, fy, fz
              global = NINT (mglobal)
              IF (ltp (global) == sp) THEN
                 xxx (global) = x
                 yyy (global) = y
                 zzz (global) = z
                 count = count + 1
              END IF
           END DO
        END SELECT

        IF (count /= npart) THEN
           WRITE (*,*) "Number of particles of species ",sp," differs from expected!" 
           STOP
        END IF
           
     END DO ! over nodes

! ... Analyze the trajectories (snapshot by snapshot) ...
     q = 0._dp
     sk = 0._dp

     DO i = 1, nsyst
     IF (ltp(i) /= sp) CYCLE
     CALL closest4 (i,nnlab)
     WRITE (*,*) i, nnlab
     call compute_tetra_lab (i,nnlab, qtetra, stetra)
     print*,"q=",qtetra
     print*,"s=",stetra  
     q = q + qtetra
     sk = sk + stetra     
  END DO
  q = q / npart
  sk = sk / npart

  WRITE (nrtout,*) nav, q, sk  
  
  q_sum = q_sum + q
  sk_sum = sk_sum + sk
  q2_sum = q2_sum + q ** 2
  sk2_sum = sk2_sum + sk ** 2

  ! ...        
  END DO ! end of loop over trajectories

  ! q_ave = q_sum / (npart * nav) ! average over particles and snapshots
  ! sk_ave = sk_sum / (npart * nav)

  q_ave = q_sum / nav ! average over snapshots
  sk_ave = sk_sum / nav

  q2_ave = q2_sum / nav ! average over snapshots
  sk2_ave = sk2_sum / nav

  WRITE (nrtout,*)
  WRITE (nrtout,*)
  
  WRITE (*,*) "<q> = ", q_ave
  WRITE (*,*) "error = ", sqrt( q2_ave - q_ave **2)/sqrt(1._dp*nav)
  WRITE (*,*) "<s_k> = ", sk_ave
  WRITE (*,*) "error = ", sqrt( sk2_ave - sk_ave **2)/sqrt(1._dp*nav)
  
  WRITE (nrtout,*) "<q> = ", q_ave
  WRITE (nrtout,*) "error = ", sqrt( q2_ave - q_ave **2)/sqrt(1._dp*nav)
  WRITE (nrtout,*) "<s_k> = ", sk_ave
  WRITE (nrtout,*) "error = ", sqrt( sk2_ave - sk_ave **2)/sqrt(1._dp*nav)
  
  ! Close the trajectory files
  DO j = 1, numnodes
     CLOSE (ntraj+j-1)
  END DO
  
  !close output file
  CLOSE (nrtout)

  DEALLOCATE (beads, bonds)
  DEALLOCATE (namspe, nammol, nspec)
  DEALLOCATE (xxx, yyy, zzz)
  DEALLOCATE (ltp)
  
99 FORMAT(f10.1,2x,9(e13.6,2x))
98 FORMAT(1p,9(e13.6,3x))

 !-----------------------------------------------------------------------------------------
  CONTAINS 

SUBROUTINE compute_tetra_lab (gb0, nnlab, qtetra, stetra)
!*************************************************************************************
! subroutine to compute q and sk for five particles given their global labels
! (a central one and its four nearest neighbours)
  
! authors: s. chiacchiera, january 2018
!************************************************************************************* 

  IMPLICIT none
  ! NB: I should finally recover the use of subroutine "images"
  INTEGER, INTENT(IN):: gb0, nnlab (4)
  
  REAL(KIND=dp), INTENT(OUT) :: qtetra, stetra
  REAL(KIND=dp) :: theta, ctheta, angle_ave, cangle_ave

  REAL(KIND=dp) :: xab, yab, zab, rab, rrab, xcb, ycb, zcb, rcb, rrcb
  REAL(KIND=dp) :: r_ave, r2_ave

!  REAL(KIND=dp) :: xxx (5), yyy (5), zzz (5)
    
  INTEGER :: nn1, nn2, i,j,k !change if needed

!-----------------------------------------------------------------------------------------
  qtetra = 0._dp
  angle_ave = 0._dp 
  cangle_ave = 0._dp 

  j = gb0 ! central particle for angle computations
  DO nn1 = 1, 3
     i = nnlab (nn1)
     DO nn2 =nn1+1, 4
        k = nnlab (nn2)                
!-----------------------------------------------------------------------------------------
! part to compute the ijk angle (from bond_module.f90)
        xab = xxx (i) - xxx (j)
        yab = yyy (i) - yyy (j)
        zab = zzz (i) - zzz (j)
!!!
!  CALL images (xab, yab, zab, dimx, dimy, dimz, srfx, srfy, srfz, shrdx, shrdy, shrdz)
        xab = xab - dimx * ANINT (xab/dimx)
        yab = yab - dimy * ANINT (yab/dimy)
        zab = zab - dimz * ANINT (zab/dimz)
!!!        
        rab = SQRT(xab * xab + yab * yab + zab * zab)
        rrab = MAX (rab, 1e-10_dp)
        rrab = 1.0_dp / rrab
        xab = xab * rrab
        yab = yab * rrab
        zab = zab * rrab
  
        xcb = xxx (k) - xxx (j)
        ycb = yyy (k) - yyy (j)
        zcb = zzz (k) - zzz (j)
!!!
        !  CALL images (xcb, ycb, zcb, dimx, dimy, dimz, srfx, srfy, srfz, shrdx, shrdy, shrdz)
        xcb = xcb - dimx * ANINT (xcb/dimx)
        ycb = ycb - dimy * ANINT (ycb/dimy)
        zcb = zcb - dimz * ANINT (zcb/dimz)
!!!
        rcb = SQRT(xcb * xcb + ycb * ycb + zcb * zcb)
        rrcb = MAX (rcb, 1e-10_dp)
        rrcb = 1.0_dp / rrcb
        xcb = xcb * rrcb
        ycb = ycb * rrcb
        zcb = zcb * rrcb
  
        ctheta = xab * xcb + yab * ycb + zab * zcb
        IF (ABS(ctheta)>1.0_dp) ctheta = SIGN(1.0_dp, ctheta) ! shouldn't I check how much >1 it is?
        theta = ACOS (ctheta)
!-----------------------------------------------------------------------------------------  
        qtetra = qtetra + (ctheta + 1._dp/3) ** 2
        angle_ave = angle_ave + theta 
        cangle_ave = cangle_ave + ctheta 
!-----------------------------------------------------------------------------------------
        WRITE(*,'(i2,1x,i2,1x,f13.6,1x,f13.6)') nn1, nn2, ctheta, ACOS(ctheta)/pi*180
     END DO
  END DO
!  qtetra = qtetra/ 6.
  qtetra = 1 - 3._dp/8 * qtetra
  
  angle_ave = angle_ave/ 6.
  cangle_ave = cangle_ave/ 6.

  print*,"average angle=", angle_ave
  print*,"average cosine angle=", cangle_ave,"-> angle", ACOS(cangle_ave)," and in degrees ",ACOS(cangle_ave)/pi*180
!-----------------------------------------------------------------------------------------
!  stetra = 0._dp
  r_ave = 0._dp
  r2_ave = 0._dp
  
  j = gb0 ! central particle for distance computations
  DO nn1 = 1, 4
     i = nnlab (nn1)
     
     xab = xxx (i) - xxx (j)
     yab = yyy (i) - yyy (j)
     zab = zzz (i) - zzz (j)
!!!
     !  CALL images (xab, yab, zab, dimx, dimy, dimz, srfx, srfy, srfz, shrdx, shrdy, shrdz)
        xab = xab - dimx * ANINT (xab/dimx)
        yab = yab - dimy * ANINT (yab/dimy)
        zab = zab - dimz * ANINT (zab/dimz)
!!!
     rab = SQRT(xab * xab + yab * yab + zab * zab)
     
     r_ave = r_ave + rab
     r2_ave = r2_ave + rab ** 2
     !     stetra
     WRITE(*,'(i2,1x,f13.6)') nn1,rab 
  END DO
  
  r_ave = r_ave / 4
  r2_ave = r2_ave / 4
  
  stetra = 1 - 1./(3*r_ave**2) * (r2_ave - r_ave ** 2)
  
  RETURN
END SUBROUTINE compute_tetra_lab

SUBROUTINE closest4 (gb0, sorted)
!*************************************************************************************
! subroutine to find the four closest particles of a given species to a given particle
!
! authors: s. chiacchiera, january 2018
!************************************************************************************* 
  !
  ! input: - all the coordinates of beads of species "sp" (the others are set to "0")
  !        - one selected particle of species "sp"
  ! output: the (ordered by distance) labels of the four closest "sp" particles to it
  IMPLICIT none

  INTEGER, INTENT(IN) :: gb0
  INTEGER, INTENT(OUT) :: sorted (4)
  INTEGER :: i, count, ncut, indx, size

  REAL(KIND=dp) :: x, y, z, r
  REAL(KIND=dp) :: rcut, rmin, sorted_r (4)
  REAL(KIND=dp), ALLOCATABLE :: list (:,:)

  ncut = 15 !10 ! a bit more than 4, to be safe.
  
10 size = MIN (npart - 1, 2 * ncut)
   rcut = (3/(4*pi) * ncut / npart * volm) ** (1./3.)
   count =0 
 
  ALLOCATE (list (size, 2))
!  WRITE(*,*) "shape (list)=",shape(list), "ncut=",ncut
  list = 0._dp
  
  DO i = 1, nsyst
     IF (i == gb0) CYCLE  
     IF (ltp (i) /= sp) CYCLE  
     x = xxx (i) - xxx (gb0)
     y = yyy (i) - yyy (gb0)
     z = zzz (i) - zzz (gb0)
!!!
     !  CALL images (xab, yab, zab, dimx, dimy, dimz, srfx, srfy, srfz, shrdx, shrdy, shrdz)
     x = x - dimx * ANINT (x/dimx)
     y = y - dimy * ANINT (y/dimy)
     z = z - dimz * ANINT (z/dimz)
!!!        
     r = SQRT(x * x + y * y + z * z)
  
     IF (r > rcut) CYCLE
     count = count + 1
!     WRITE(*,*) "count=", count
     IF (count>size) THEN
        WRITE(*,*) "error: too many particles!"
        STOP
     END IF
     list (count,1) = i ! store the global index
     list (count,2) = r ! store the distance to gb0
  END DO

  WRITE (*,*) "rcut=", rcut
  WRITE (*,*) gb0, count
  
  IF (count < 4) THEN
     WRITE (*,*) "error: fewer than 4 neighbours - ",count," - found! Increase the searched volume (-> ncut)"
     WRITE (*,*) "time=",time
     ! IF (ncut >= 20) THEN
     !    WRITE(*,*)"error: even with ncut>=20 could not find 4 neighbours"
     !    STOP
     ! ELSE     
     !    DEALLOCATE (list)
     !    ncut = ncut - 2
     !    GO TO 10
     ! END IF
     STOP
  END IF
!  WRITE (*,*) "rcut=", rcut
!  WRITE (*,*) gb0, count
!  DO i =1, size !ncut*2
!     WRITE (*,*) list (i,:)
!  END DO
  
  ! sorting by distance 
  sorted (:) = 0
  sorted_r (:) = 0._dp
  DO j = 1, 4
     rmin = rcut
     indx = 0
     DO i = 1, count
        IF ((list (i,1) == sorted (1)) .OR. (list (i,1) == sorted (2)) .OR. &
             (list (i,1) == sorted (3)) .OR. (list (i,1) == sorted (4))) CYCLE
        IF (list (i,2) < rmin) THEN 
           rmin = list(i,2)
           indx = list(i,1)
        END IF
     END DO
     sorted (j) = indx
     sorted_r (j) = rmin
     !     WRITE (*,*) rmin, indx
  END DO
  WRITE (*,*) sorted
  WRITE (*,*) sorted_r
!  WRITE (*,*) (list (sorted(i),2), i=1,4)
!  DO i =1,4
!     WRITE (*,*) list (sorted(i),2)     
!  END DO
  
  DEALLOCATE (list) ! allocate/deallocate here, or in the main?
  RETURN
END SUBROUTINE closest4




END PROGRAM tetrahedral
