PROGRAM tetrahedral
!***********************************************************************************
!
! module to analyze tetrahedral ordering in dl_meso HISTORY files
!
! authors - m. a. seaton & s. chiacchiera, january 2018 (tidied up and amended
!           january 2021)
!
!**********************************************************************************
  IMPLICIT none
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND (15, 307)
  INTEGER, PARAMETER :: si = SELECTED_INT_KIND (8)
  INTEGER, PARAMETER :: li = SELECTED_INT_KIND (12)
  INTEGER, PARAMETER :: endversion = 1

  REAL(KIND=dp), PARAMETER :: pi=3.141592653589793_dp

  INTEGER, PARAMETER :: ntraj=10
  
  CHARACTER(80) :: text
  CHARACTER(8), ALLOCATABLE :: namspe (:), nammol (:)

  INTEGER, ALLOCATABLE :: ltp (:), nspec (:), readint (:)
  INTEGER :: nrtout
  INTEGER :: chain, ioerror, i, numtraj, j, k, nmoldef, ibond
  INTEGER :: nspe, nbeads, nusyst, nsyst, numbond, global, species, molecule
  INTEGER :: lfrzn, keytrj, srfx, srfy, srfz
  INTEGER :: nav
  INTEGER :: bead1, bead2
  INTEGER :: endver, Dlen, nstep, framesize, lend
  INTEGER(KIND=li) :: filesize

  REAL(KIND=dp), ALLOCATABLE :: xxx (:), yyy (:), zzz (:), readdata (:)
  REAL(KIND=dp) :: dimx, dimy, dimz, shrdx, shrdy, shrdz
  REAL(KIND=dp) :: amass, rcii, chg
  REAL(KIND=dp) :: time

  LOGICAL :: eof, swapend, bigend

! Variables for tetrahedral ordering
  INTEGER :: nnlab(4), npart, sp, count
  REAL(KIND=dp) :: qtetra, stetra
  REAL(KIND=dp) :: q, sk, q_sum, sk_sum, q_ave, sk_ave
  REAL(KIND=dp) :: q2_sum, sk2_sum, q2_ave, sk2_ave

  !-----------------------------------------------------------------------------------------

  ! determine number of bytes for selected double precision kind
  ! (the default SELECTED_REAL_KIND (15, 307) should return 8 bytes)

  lend = STORAGE_SIZE (1.0_dp) / 8

  ! check endianness of machine

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
    PRINT *, "       recompile tetrahedral.f90 with reals of ", Dlen, " bytes"
    STOP
  END IF

  ! read file size, number of trajectory frames and timestep numbers

  READ (ntraj) filesize, numtraj, nstep

  ! Read the number of beads, molecules and bonds
  ! Arrays are filled with names of particles and molecules: if checking molecules,
  ! arrays for species, molecule types etc. also filled

  READ (ntraj) text
  READ (ntraj) nspe, nmoldef, nusyst, nsyst, numbond, keytrj, srfx, srfy, srfz

  ALLOCATE (namspe (nspe), nammol (nmoldef), nspec (nspe)) ! NB: nspec here counts ALL beads of a type, not only unbonded ones
  ALLOCATE (xxx (1:nsyst), yyy (1:nsyst), zzz (1:nsyst))
  ALLOCATE (ltp (1:nsyst))

  framesize = (keytrj+1) * 3
  ALLOCATE (readint (1:nsyst), readdata (1:framesize))

  DO i = 1, nspe
    READ (ntraj) namspe (i), amass, rcii, chg, lfrzn
  END DO

  DO i = 1, nmoldef
    READ (ntraj) nammol (i)
  END DO

  ! Read properties of beads and molecules

  nspec (:) = 0 ! populations
  ibond = 0  !counter for bonds

  DO i = 1, nsyst
    READ (ntraj) global, species, molecule, chain
    ltp (global) = species
    nspec (species) = nspec (species) + 1
  END DO

  IF (numbond>0) THEN
    DO i = 1, numbond
      READ (ntraj) bead1, bead2
    END DO
  END IF

! Find number of beads for which trajectories are needed

  DO i = 1, nspe
    WRITE(*,*) "Species ",i,": ",namspe (i)
  END DO
  WRITE(*,*) "Which species number has to be analyzed?"
  READ(*,*) sp  
  IF (sp<0 .OR. sp>nspe) THEN
     WRITE(*,*) "error: undefined species!"
     STOP
  END IF
  
  npart = nspec (sp)

  WRITE(*,*) "total number of beads:      ", nsyst
  WRITE(*,*) "number of beads by species: ", nspec
  WRITE(*,*) "number of analyzed beads:   ", npart
  
  ! Open and write output file
      
  nrtout = ntraj + 1
  OPEN (nrtout, file = 'TETRADAT', status ='replace')
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

  ! Read snapshots of trajectories

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
     
! The full coordinate arrays are used to avoid re-labelling, but they are filled *only* for particles of species "sp"
     xxx (:) = 0.0_dp
     yyy (:) = 0.0_dp
     zzz (:) = 0.0_dp

     count = 0

     READ (ntraj) readint (1:nbeads)
     DO i = 1, nbeads
       global = readint (i)
       READ (ntraj) readdata (1:framesize)
       IF (ltp (global) == sp) THEN
         xxx (global) = readdata (1)
         yyy (global) = readdata (2)
         zzz (global) = readdata (3)
         count = count + 1
       END IF
     END DO

     IF (count /= npart) THEN
       WRITE (*,*) " Number of particles of species ",sp," differs from expected!"
       STOP
     END IF
           
! ... Analyze the trajectories (snapshot by snapshot) ...
     q = 0.0_dp
     sk = 0.0_dp

     DO i = 1, nsyst
       IF (ltp (i) /= sp) CYCLE
       CALL closest4 (i, nnlab, npart)
       ! WRITE (*,*) i, nnlab  ! uncomment to see nn labels
       CALL compute_tetra_label (i, nnlab, qtetra, stetra)
!       print*,"q=",qtetra ! uncomment to print q for each single set of 5 particles
!       print*,"s=",stetra ! uncomment to print sk for each single set of 5 particles
       q = q + qtetra
       sk = sk + stetra
     END DO
     q = q / REAL(npart, KIND=dp)
     sk = sk / REAL(npart, KIND=dp)

     WRITE (nrtout,'(1p,I8,2(2x,e14.6))') nav, q, sk
  
     q_sum = q_sum + q
     sk_sum = sk_sum + sk
     q2_sum = q2_sum + q * q
     sk2_sum = sk2_sum + sk * sk

  ! ...        
  END DO ! end of loop over trajectories

  q_ave = q_sum / REAL(nav, KIND=dp) ! average over snapshots
  sk_ave = sk_sum / REAL(nav, KIND=dp)

  q2_ave = q2_sum / REAL(nav, KIND=dp) ! average over snapshots
  sk2_ave = sk2_sum / REAL(nav, KIND=dp)

  WRITE (nrtout,*)
  WRITE (nrtout,*)
  
  WRITE (*,'(A9,2x,e14.6)') " <q>   = ", q_ave
  WRITE (*,'(A9,2x,e14.6)') " error = ", SQRT ((q2_ave - q_ave * q_ave)/REAL(nav, KIND=dp))
  WRITE (*,'(A9,2x,e14.6)') " <s_k> = ", sk_ave
  WRITE (*,'(A9,2x,e14.6)') " error = ", SQRT ((sk2_ave - sk_ave * sk_ave)/REAL(nav, KIND=dp))
  
  WRITE (nrtout,'(A11,2x,e14.6)') " # <q>   = ", q_ave
  WRITE (nrtout,'(A11,2x,e14.6)') " # error = ", SQRT ((q2_ave - q_ave * q_ave)/REAL(nav, KIND=dp))
  WRITE (nrtout,'(A11,2x,e14.6)') " # <s_k> = ", sk_ave
  WRITE (nrtout,'(A11,2x,e14.6)') " # error = ", SQRT ((sk2_ave - sk_ave * sk_ave)/REAL(nav, KIND=dp))
  
  ! Close the trajectory file
  CLOSE (ntraj)

  !close output file
  CLOSE (nrtout)

  DEALLOCATE (namspe, nammol, nspec)
  DEALLOCATE (xxx, yyy, zzz)
  DEALLOCATE (ltp)
  
 !-----------------------------------------------------------------------------------------
  CONTAINS 

SUBROUTINE compute_tetra_label (gb0, nnlab, qtetra, stetra)
!*************************************************************************************
! subroutine to compute q and sk for five particles given their global labels
! (a central one and its four nearest neighbours)
  
! authors: s. chiacchiera, january 2018
!************************************************************************************* 

  IMPLICIT none
  ! NB: I should finally recover the use of subroutine "images"
  INTEGER, INTENT(IN):: gb0, nnlab (4)
  
  REAL(KIND=dp), INTENT(OUT) :: qtetra, stetra
  REAL(KIND=dp) :: theta, ctheta!, angle_ave, cangle_ave ! can be uncommented for checks

  REAL(KIND=dp) :: xab, yab, zab, rab, rrab, xcb, ycb, zcb, rcb, rrcb
  REAL(KIND=dp) :: r_ave, r2_ave
    
  INTEGER :: nn1, nn2, i,j,k !change if needed

!-----------------------------------------------------------------------------------------
  qtetra = 0.0_dp
  ! angle_ave = 0._dp 
  ! cangle_ave = 0._dp 

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
        IF (ABS(ctheta)>1.0_dp) ctheta = SIGN(1.0_dp, ctheta) ! could add a check of how much >1 it is
        theta = ACOS (ctheta)
!-----------------------------------------------------------------------------------------  
        qtetra = qtetra + (ctheta + 1.0_dp/3.0_dp) * (ctheta + 1.0_dp/3.0_dp)
        ! angle_ave = angle_ave + theta 
        ! cangle_ave = cangle_ave + ctheta 
!-----------------------------------------------------------------------------------------
!        WRITE(*,'(i2,1x,i2,1x,f13.6,1x,f13.6)') nn1, nn2, ctheta, ACOS(ctheta)/pi*180
     END DO
  END DO

  qtetra = 1.0_dp - 0.375_dp * qtetra
  
  ! angle_ave = angle_ave/ 6.
  ! cangle_ave = cangle_ave/ 6.

  ! print*,"average angle=", angle_ave
  ! print*,"average cosine angle=", cangle_ave,"-> angle", ACOS(cangle_ave)," and in degrees ",ACOS(cangle_ave)/pi*180
!-----------------------------------------------------------------------------------------

  r_ave = 0.0_dp
  r2_ave = 0.0_dp
  
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
     r2_ave = r2_ave + rab * rab
     
  END DO
  
  r_ave = 0.25_dp * r_ave
  r2_ave = 0.25_dp * r2_ave
  
  stetra = 1.0_dp - 1.0_dp/(3.0_dp*r_ave*r_ave) * (r2_ave - r_ave * r_ave)
  
  RETURN

END SUBROUTINE compute_tetra_label

SUBROUTINE closest4 (gb0, sorted, npart)
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

  INTEGER, INTENT(IN) :: gb0, npart
  INTEGER, INTENT(OUT) :: sorted (4)
  INTEGER :: i, count, ncut, indx, size

  REAL(KIND=dp) :: x, y, z, r, volm
  REAL(KIND=dp) :: rcut, rmin, sorted_r (4)
  REAL(KIND=dp), ALLOCATABLE :: list (:,:)

  ncut = 15 !10 ! a bit more than 4, to be safe.

  volm = dimx*dimy*dimz
  size = MIN (npart - 1, 2 * ncut)
  rcut = (0.75_dp/pi * ncut / npart * volm) ** (1.0_dp/3.0_dp)
  count = 0
 
  ALLOCATE (list (size, 2))

  list = 0.0_dp
  
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
     IF (count>size) THEN
        WRITE(*,*) "error: too many particles!"
        STOP
     END IF
     list (count, 1) = REAL(i, KIND=dp) ! store the global index
     list (count, 2) = r ! store the distance to gb0
  END DO

!  WRITE (*,*) "rcut=", rcut  ! uncomment to see radius of search region
!  WRITE (*,*) gb0, count ! uncomment to see the number of particles within it
  
  IF (count < 4) THEN
     WRITE (*,*) "error: fewer than 4 neighbours - ",count," - found! Increase the searched volume (-> ncut)"
     WRITE (*,*) "time=",time
     STOP
  END IF
  
  ! sorting by distance 
  sorted (:) = 0
  sorted_r (:) = 0.0_dp
  DO j = 1, 4
     rmin = rcut
     indx = 0
     DO i = 1, count
        IF ((NINT(list (i,1)) == sorted (1)) .OR. (NINT(list (i,1)) == sorted (2)) .OR. &
             (NINT(list (i,1)) == sorted (3)) .OR. (NINT(list (i,1)) == sorted (4))) CYCLE
        IF (list (i,2) < rmin) THEN 
           rmin = list(i,2)
           indx = NINT(list(i,1))
        END IF
     END DO
     sorted (j) = indx
     sorted_r (j) = rmin
  END DO
!  WRITE (*,'(4(1x,I6))') sorted ! uncomment to see the labels of nn of gb0 (sorted by distance)
!  WRITE (*,'(4(1x,f13.6))') sorted_r ! uncomment to see the corresponding distances
  
  DEALLOCATE (list) ! for fixed size, could allocate/deallocate in the main
  RETURN
END SUBROUTINE closest4

END PROGRAM tetrahedral
