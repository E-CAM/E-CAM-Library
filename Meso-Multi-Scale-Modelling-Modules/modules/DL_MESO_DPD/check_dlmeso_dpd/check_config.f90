PROGRAM check_config
!***********************************************************************
! Program to check the input files for DL_MESO.
! It is checked that the provided CONFIG file (optional input) 
! is consistent with the CONTROL and FIELD files (necessary input).
!
! authors: m. a. seaton and s. chiacchiera, February 2018 
!***********************************************************************
!NB: frozen wall or shearing surface not included yet

  IMPLICIT none

  LOGICAL l_exist, safe 

  INTEGER, PARAMETER :: nread = 15 

!  CHARACTER(LEN=80) :: text 
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND (15, 307)
  INTEGER, PARAMETER :: si = SELECTED_INT_KIND (8)
  INTEGER, PARAMETER :: li = SELECTED_INT_KIND (12)
  INTEGER, PARAMETER :: mxword=16
  INTEGER :: i, voltype
  LOGICAL :: lnfold, lvol
  INTEGER :: nfold, nfoldx, nfoldy, nfoldz
  INTEGER :: levcfg, imcon
  INTEGER :: mxmolsize, mxbonds
! total system volume and dimensions
  REAL(KIND=dp) :: volm, dimx, dimy, dimz
! unit cell dimensions
  REAL(KIND=dp) :: dimxcell, dimycell, dimzcell
! number of species
  INTEGER :: nspe
! number of molecule types
  INTEGER :: nmoldef
! total number of beads (overall, unbonded, frozen) in system
  INTEGER, TARGET :: nsyst
  INTEGER :: nusyst, nfsyst
! number of beads (overall, unbonded, frozen) in unit cell
  INTEGER :: nsystcell, nusystcell, nfsystcell
! total number of molecules in system
  INTEGER :: nummol
! total number of molecules in unit cell
  INTEGER :: nummolcell
! total number of bonds in system
  INTEGER :: numbond
! total number of bonds in unit cell
  INTEGER :: numbondcell
! species names
  CHARACTER(LEN=8), ALLOCATABLE, SAVE :: namspe (:)
! frozen bead indicator
  INTEGER, ALLOCATABLE :: lfrzn (:)
! populations of species (single particles and within molecules)
  INTEGER, ALLOCATABLE :: nspec (:), nspecmol (:)
! molecule populations and number of beads per molecule
  INTEGER, ALLOCATABLE :: nmol (:), nbdmol (:)
! molecule names (0:mxmoldef)
  CHARACTER(LEN=8), ALLOCATABLE, SAVE :: nammol (:)
! molecule insertion (only species and bonds) 
  INTEGER, ALLOCATABLE, SAVE :: mlstrtspe (:,:), nbond (:)
  INTEGER, ALLOCATABLE, SAVE :: bdinp1 (:,:), bdinp2 (:,:)
  INTEGER, ALLOCATABLE, SAVE :: molstart (:)
! bond look-up tables
  INTEGER, ALLOCATABLE, SAVE :: bndtbl (:,:)
  INTEGER :: nbonds
! wall/surface parameters
! surface type
  INTEGER :: srftype
! surface dimensions
  INTEGER :: srfx, srfy, srfz
! maximum value for numbers of beads
  INTEGER, POINTER :: maxdim 
! switch to determine whether to ignore global bead numbers in CONFIG file
  LOGICAL :: ligindex
! switch for bonds
  LOGICAL :: lbond 
! switch for coordinate system in CONFIG file
  LOGICAL :: lconfzero
! switch to determine whether to calculate bonds globally
  LOGICAL :: lgbnd
! positions
  REAL(KIND=dp), ALLOCATABLE, TARGET :: xxyyzz (:,:)
  REAL(KIND=dp), POINTER :: xxx (:), yyy (:), zzz (:)
! global number, particle type, molecule type
  INTEGER, ALLOCATABLE, SAVE :: lab (:), ltp (:), ltm (:)
! varibales needed for the checks
  INTEGER, ALLOCATABLE :: nspec0 (:), nspecmol0 (:)
  INTEGER :: tp, tm, tp0, ibd, j, k, l
  INTEGER :: fail (13)
  INTEGER :: bead1, bead2
  
  lbond = .false. 
  lgbnd = .true.  ! kept to minimize changes to the code
  
! check that the three files are all present  

  safe = .true.

  INQUIRE (file = 'CONTROL', EXIST=l_exist)
  IF (.NOT. l_exist) THEN
     safe = .false.
     WRITE (*,*) "error: CONTROL file is missing"
  ENDIF    
  INQUIRE (file = 'FIELD', EXIST=l_exist)  
  IF (.NOT. l_exist) THEN
     safe = .false.
     WRITE (*,*) "error: FIELD file is missing"
  ENDIF    
  INQUIRE (file = 'CONFIG', EXIST=l_exist)
  IF (.NOT. l_exist) THEN
     safe = .false.
     WRITE (*,*) "error: CONFIG file is missing"
  ENDIF    
  
  IF (.NOT. safe) STOP

! assign default values 
  lnfold = .false. 
  nfoldx = 1
  nfoldy = 1
  nfoldz = 1
  nfold = 1
  
  lvol = .false. 
  imcon = 0

  dimx = 0._dp
  dimy = 0._dp
  dimz = 0._dp
  dimxcell = 0._dp
  dimycell = 0._dp
  dimzcell = 0._dp

  voltype = 0 

  srftype = 0
  srfx = 0; srfy = 0; srfz = 0
  
  ligindex = .false.
  lconfzero = .false.
    
! scan and read all the input files
  
  CALL read_control 

  IF (lvol .AND. (nfold>1)) THEN
     WRITE (*,*) "error: incompatible choices in CONTROL file (vol and nfold /=1)"
     STOP
  END IF

  IF (srftype==1 .AND. (srfx==0 .AND. srfy==0 .AND. srfz==0)) THEN
     WRITE (*,*) "error: missing choice of plane for hard walls in CONTROL file"     
     STOP
  END IF
  
  IF (lvol) voltype = voltype + 1  

  CALL scan_config

  IF (imcon>0) voltype = voltype + 2      
  
  SELECT CASE (voltype)
  CASE (0)
     WRITE (*,*) "error: system volume not defined in input files"
     STOP
  CASE (1)
     dimx = dimxcell 
     dimy = dimycell 
     dimz = dimzcell 
     volm = dimx * dimy * dimz
  CASE (2) 
     dimx = dimxcell * nfoldx
     dimy = dimycell * nfoldy
     dimz = dimzcell * nfoldz    
     volm = dimx * dimy * dimz
  CASE (3)
     WRITE (*,*) "error: volume defined in both CONFIG and CONTROL files"
     WRITE (*,*) "solution: please remove one of the two definitions"
     STOP
  END SELECT

  IF (volm < 1.e-10_dp) THEN
     WRITE (*,*) "error: system volume is zero"
     STOP
  ELSE
     WRITE (*,*) "unit cell sizes: ", dimxcell, dimycell, dimzcell      
     WRITE (*,*) "nfoldx, nfoldy, nfoldz =", nfoldx, nfoldy, nfoldz
     WRITE (*,*) "system sizes: ", dimx, dimy, dimz 
  ENDIF
  
  WRITE (*,*) "imcon =", imcon !may be removed...
  WRITE (*,*) "levcfg =", levcfg !may be removed...
  WRITE (*,*) "lconfzero =", lconfzero ! may be removed...
  WRITE (*,*) "srftype =", srftype
  IF (srftype/=0)   WRITE (*,*) "srfx, srfy, srfz =", srfx, srfy, srfz
  
  CALL scan_field

  WRITE (*,*) "nspe = ", nspe
  WRITE (*,*) "nmoldef = ", nmoldef 
  WRITE (*,*) "mxmolsize = ", mxmolsize
  WRITE (*,*) "mxbonds = ", mxbonds

  IF (nspe == 0) THEN
     WRITE (*,*) "error: no particle species defined in FIELD file"
     STOP
  END IF

  CALL read_field

  WRITE (*,*) "nspec = ", nspec
  WRITE (*,*) "nspecmol = ", nspecmol
  WRITE (*,*) "numbond = ", numbond
  DO i = 1, nmoldef
     WRITE (*,*) "mlstrtspe (i,:)=", mlstrtspe (i,:)
  ENDDO

  IF (numbond > 0) lbond = .true. !slightly adapted (uses numbond instead of nbonddef)
  
! keep maxdim (to minimize the changes to the code)
  maxdim => nsyst

  CALL define_molstart

  fail = 0

  ALLOCATE (xxyyzz (4, maxdim), STAT=fail(1))  ! I have used 4 instead of csize
  ALLOCATE (lab (maxdim), ltp (maxdim), ltm (maxdim), STAT=fail(2))
  ALLOCATE (bndtbl (numbond,2), STAT=fail(3)) ! NB: I disregard the bond type (smaller dimension for the array)
  
! assign pointers                                                                                                                                                          
  xxx => xxyyzz (1,:)
  yyy => xxyyzz (2,:)
  zzz => xxyyzz (3,:)

  CALL read_config

! checks (when needed to avoid confusion, the expected values are named *0)
! use ltp and ltm to derive nspec, nspecmol and molecules obtained using CONFIG file 
  
  ALLOCATE (nspec0 (nspe), nspecmol0 (nspe), STAT=fail(4))

  IF (ANY (fail/=0)) THEN
     WRITE (*,*) "error: allocation failure"
     STOP
  END IF
  
  nspec0 = nspec
  nspecmol0 = nspecmol
  nmol = nfold * nmol ! to rescale from unit cell to system
  
  nspec = 0
  nspecmol = 0  

  DO i = 1, nsyst
     tp = ltp (i)
     tm = ltm (i)
     IF (tm == 0) THEN
        nspec (tp) = nspec (tp) + 1
     ELSE
        nspecmol (tp) = nspecmol (tp) + 1        
     END IF
  END DO
  
  safe = .true.
  DO i = 1, nspe
     IF (nspec (i) /= nspec0 (i)) THEN
        WRITE (*,*) "ERROR: problem with unbonded beads of species ", namspe (i), ":", nspec (i), &
             " instead of ", nspec0 (i)
        safe = .false.
     END IF
     IF (nspecmol (i) /= nspecmol0 (i) ) THEN
        WRITE (*,*) "ERROR: problem with molecular beads of species ", namspe (i), ":", nspecmol (i), & 
             " instead of ", nspecmol0 (i)
        safe = .false.
     END IF
  END DO
  
  ibd = nusyst
  DO i = 1, nmoldef
     DO j = 1, nmol (i)
        DO k = 1, nbdmol (i)
           ibd = ibd + 1
           l = 1 
           DO WHILE (lab (l) /= ibd)
              l = l + 1
           END DO
!           print*, "l, lab(l), ibd= ", l, lab(l), ibd
           tp = ltp (l) ! not ltp (ibd)!!!
           tp0 = mlstrtspe (i, k)
           IF (tp /= tp0) THEN
              WRITE (*,*) "ERROR: problem with the molecular content of  ", nammol (i), ": ",k, "-th bead is ", & 
                   namspe (tp), " instead of ", namspe (tp0), "(bead label =", ibd, ")"              
              safe = .false.
           END IF
        END DO
     END DO
  END DO

  IF (.not.safe) THEN
     WRITE (*,"(/,1x,'error: CONFIG file is not consistent with FIELD file')")
  ELSE
     WRITE (*,"(/,1x,'OK: CONFIG file is consistent with FIELD file')")     
     WRITE (*,"(1x,'(composition and bead content of molecules)')")
  END IF

! Check that no stretching bond crosses a hard wall
  IF (srftype==1) THEN
     safe = .true.
     DO i = 1, numbond
        bead1 = bndtbl (i,1)
        bead2 = bndtbl (i,2)        
        IF (srfx==1) THEN
           IF (ABS(xxx (bead1) - xxx (bead2)) > dimx/2) THEN
              WRITE (*,"('error: bond between beads',2x,i7,2x,'and',2x,i7,2x,'crosses hard wall perp. to x')")bead1, bead2 
              safe = .false.
           END IF
        END IF
        IF (srfy==1) THEN
           IF (ABS(yyy (bead1) - yyy (bead2)) > dimy/2) THEN
              WRITE (*,"('error: bond between beads',2x,i7,2x,'and',2x,i7,2x,'crosses hard wall perp. to y')")bead1, bead2 
              safe = .false.
           END IF
        END IF
        IF (srfz==1) THEN
           IF (ABS(zzz (bead1) - zzz (bead2)) > dimz/2) THEN
              WRITE (*,"('error: bond between beads',2x,i7,2x,'and',2x,i7,2x,'crosses hard wall perp. to z')")bead1, bead2 
              safe = .false.
           END IF
        END IF
     END DO
     
     IF (.NOT. safe) THEN
        WRITE (*,"(/'error: at least one streching bond is crossing a hard wall, please correct the CONFIG file')")
        IF (nfold>1) WRITE (*,"('(hint: the bonds needing corrections are easier to indetify if nfold=1)')")        
     ELSE
        WRITE (*,"(/,1x,'OK: none of the stretching bonds is crossing a hard wall')")             
     END IF

  END IF


! de-allocate arrays, (from second line on) as in free_memory
  
  fail = 0

  DEALLOCATE (nspec0, nspecmol0, STAT=fail(1))
  DEALLOCATE (xxyyzz, STAT=fail(2))
  DEALLOCATE (namspe, nspec, nspecmol, lfrzn, nammol, STAT=fail(4))
  DEALLOCATE (mlstrtspe, nmol, nbdmol, nbond, STAT=fail(5))
  DEALLOCATE (bdinp1, bdinp2, STAT=fail(6))
  DEALLOCATE (lab, ltp, ltm, STAT=fail(10))
  DEALLOCATE (bndtbl, molstart, STAT=fail(13))
  
  IF (ANY (fail/=0)) THEN
     WRITE (*,*) "error: deallocation failure"
     STOP
  END IF
  
CONTAINS

  SUBROUTINE read_control 

!***********************************************************************
!
!     read CONTROL file for system and simulation data
!
!     copyright stfc daresbury laboratory
!     authors - w. smith & m. a. seaton july 2015
!
!***********************************************************************
!     This is a stripped down version, adapted by s. chiacchiera 2017
!     Details on its behaviour:
!     input: CONTROL
!     output: may alter from default the values of these global variables
!     lnfold, nfold, nfoldx, nfoldy, nfoldz
!     lvol, dimxcell, dimycell, dimzcell
!     ligindex, lconfzero
!     srftype, srfx, srfy, srfz    
!***********************************************************************    
    IMPLICIT none
  
    CHARACTER(LEN=mxword) :: key1, key2, word1, word2, word3!, word4
    CHARACTER(LEN=200) :: record
    INTEGER :: ioerror
    LOGICAL :: safe  
    REAL (dp) :: cubeside, volume
    
!     open input channel

    OPEN ( nread, file = 'CONTROL', form= 'formatted', IOSTAT=ioerror)
    
    safe = (ioerror==0)

    IF (.NOT. safe) THEN
       WRITE (*,*) "error: unable to open CONTROL"
       STOP
    END IF
    
    READ (nread, '(a80)') record !text ! unless I need to check that titles are equal
    
    DO WHILE (.true.)
       
       READ (nread, '(a200)', IOSTAT=ioerror) record
       
       IF (ioerror/=0) EXIT
       
       key1 = getword (record, 1)
       key2 = getword (record, 2)
       
       CALL lowercase (key1)
       CALL lowercase (key2)
       
       IF (key1 (1:6) =="finish") EXIT

       IF (key1 (1:4) =="conf") THEN

          word1 = getword (record, 3)
          CALL lowercase (word1)
          IF (key2 (1:4)=="zero" .OR. word1 (1:4)=="zero") lconfzero = .true.
          
       ELSE IF (key1 (1:5) =="nfold") THEN
          
          lnfold = .true.
          nfoldx = INT (getint (record, 2), KIND=si)
          nfoldy = INT (getint (record, 3), KIND=si)
          nfoldz = INT (getint (record, 4), KIND=si)
          nfold = nfoldx * nfoldy * nfoldz
          
       ELSE IF (key1 (1:2) =="no") THEN
          
          IF (key2 (1:3) =="ind") THEN
             ligindex = .true.
             WRITE (*,*) "warning: ignoring the bead index in CONFIG file"
          END IF
          
          !   ELSE IF (key1 (1:7) =="restart") THEN
       ELSE IF (key1 (1:4) =="surf") THEN
          
          !     IF (key2 (1:3) =="cut") srfzcut = getdble (record, 3)
          
          IF (key2 (1:4) =="hard") THEN
             srftype = 1
             word1 = getword (record, 3)
             word2 = getword (record, 4)
             word3 = getword (record, 5)
             CALL lowercase (word1)
             CALL lowercase (word2)
             CALL lowercase (word3)
             IF (word1(1:1)=="x" .OR. word2(1:1)=="x".OR. word3(1:1)=="x") srfx = 1
             IF (word1(1:1)=="y" .OR. word2(1:1)=="y".OR. word3(1:1)=="y") srfy = 1
             IF (word1(1:1)=="z" .OR. word2(1:1)=="z".OR. word3(1:1)=="z") srfz = 1
          !     ELSE IF (key2 (1:4) =="froz") THEN
          !       srftype = 2
          !       word1 = getword (record, 3)
          !       word2 = getword (record, 4)
          !       word3 = getword (record, 5)
          !       CALL lowercase (word1)
          !       CALL lowercase (word2)
          !       CALL lowercase (word3)
          !       IF (word1(1:1)=="x" .OR. word2(1:1)=="x".OR. word3(1:1)=="x") srfx = 1
          !       IF (word1(1:1)=="y" .OR. word2(1:1)=="y".OR. word3(1:1)=="y") srfy = 1
          !       IF (word1(1:1)=="z" .OR. word2(1:1)=="z".OR. word3(1:1)=="z") srfz = 1
          !     ELSE IF (key2 (1:4) =="shea") THEN
          !       srftype = 3
          !       word1 = getword (record, 3)
          !       CALL lowercase (word1)
          !       IF (word1(1:1)=="x") srfx = 1
          !       IF (word1(1:1)=="y") srfy = 1
          !       IF (word1(1:1)=="z") srfz = 1
          !       IF (srfx==0 .AND. srfy==0 .AND. srfz==0) srftype = 0
          END IF
          
       ELSE IF (key1 (1:3) =="vol") THEN
          lvol = .true.
          dimxcell = getdble (record, 2)
          dimycell = getdble (record, 3)
          dimzcell = getdble (record, 4)
          volume = dimxcell * dimycell * dimzcell  
          IF (volume <1.0e-10_dp) THEN
             cubeside = dimxcell ** (1.0_dp/3.0_dp) 
             dimxcell = cubeside 
             dimycell = cubeside 
             dimzcell = cubeside 
          END IF
          !     lnfold = .false. NB: I remove this for clarity
          !     nfoldx = 1
          !     nfoldy = 1
          !     nfoldz = 1
       END IF
       
    END DO
    
    CLOSE (nread)
    
    RETURN
  END SUBROUTINE read_control

SUBROUTINE scan_config

!***********************************************************************
!
!     scan CONFIG file for system dimensions
!
!     copyright stfc daresbury laboratory
!     authors - m. a. seaton august 2011
!
!***********************************************************************
!     This version was adapted by s. chiacchiera 2017
!     Details on its behaviour:
!     input: CONFIG
!     output:
!     defines/alters from default the values of these global variables:
!     levcfg, imcon
!     MAY alter from default:  
!     dimxcell, dimycell, dimzcell
!***********************************************************************    

  IMPLICIT none 
        
      LOGICAL :: safe
      INTEGER :: ioerror
      CHARACTER(LEN=200) :: record
 
!     open config channel
  
      OPEN (nread, file = 'CONFIG', IOSTAT=ioerror)

      safe = (ioerror==0)
      IF (.NOT. safe) THEN
         WRITE (*,*) "error: unable to open CONFIG"
         STOP
      END IF
      
!     read title line (not used)

      READ (nread, '(a80)') record

!     read keys for config data and boundaries

      READ (nread, '(a200)') record
      levcfg = INT (getint (record, 1), KIND=si)
      imcon = INT (getint (record, 2), KIND=si)

!     if imcon>0, read cell vectors and obtain dimensions (assuming orthorhombic)

      IF (imcon>0) THEN
        READ (nread, '(a200)') record
        dimxcell = getdble (record, 1)
        READ (nread, '(a200)') record
        dimycell = getdble (record, 2)
        READ (nread, '(a200)') record
        dimzcell = getdble (record, 3)
      END IF

      CLOSE (nread)

      RETURN
      END SUBROUTINE scan_config


      SUBROUTINE scan_field

!***********************************************************************
!
!     scan FIELD file for array bounds
!
!     copyright stfc daresbury laboratory
!     author - m. a. seaton july 2014
!
!***********************************************************************
!     This is a stripped down version, adapted by s. chiacchiera 2017
!     Details on its behaviour:
!     input: FIELD
!     output:
!     defines the values of these global variables:
!     nspe, nmoldef, mxmolsize, mxbonds
!----------------------------------------------------------------------
      IMPLICIT none 
      LOGICAL :: finish, safe!lexist, 
      CHARACTER(LEN=mxword) :: key, word
      CHARACTER(LEN=200) :: record, record1
      INTEGER :: i, ioerror, imol, ibond!, iangle, idihed, ipot, j

!     open FIELD channel for first pass: determine numbers of species and 
!     molecule types, and maximum numbers of beads.

      OPEN (nread, file = 'FIELD', IOSTAT=ioerror)

      safe = (ioerror==0)
      IF (.NOT. safe) THEN
         WRITE (*,*) "error: unable to open FIELD"
         STOP
      END IF

      nspe = 0 ! added by SC
      nmoldef = 0 ! added by SC
      mxmolsize = 0
      mxbonds = 0
      
      READ (nread, '(a80)') record

      finish = .false.

      DO WHILE (.NOT. finish)

        READ (nread, '(a200)', IOSTAT=ioerror) record

        IF (ioerror/=0) finish = .true.

        key = getword (record, 1)
        CALL lowercase (key)

        IF (key (1:6) =="close") finish = .true.

        IF (key (1:5) =="speci") THEN

          nspe = INT (getint (record, 2), KIND=si)

        ELSE IF (key (1:7) =="molecul") THEN

          nmoldef = INT (getint (record, 2), KIND=si)

          DO i = 1, nmoldef

            DO WHILE (.true.)
              READ (nread, '(a200)', IOSTAT=ioerror) record1
              word = getword (record1, 1)
              CALL lowercase (word)

              IF (word (1:4) =="bead") THEN

                imol = INT (getint (record1, 2), KIND=si)
                mxmolsize = MAX (mxmolsize, imol)

             ELSE IF (word (1:4) =="bond") THEN

                ibond = INT (getint (record1, 2), KIND=si)
                mxbonds = MAX (mxbonds, ibond)
                
             ELSE IF (word (1:6) =="finish") THEN
                
                EXIT

              END IF

            END DO

          END DO

       END IF

      END DO

      CLOSE (nread)

      RETURN

      END SUBROUTINE scan_field


      SUBROUTINE read_field

!***********************************************************************
!
!     read FIELD file for interaction and molecule data
!
!     copyright stfc daresbury laboratory
!     author - m. a. seaton august 2015
!
!***********************************************************************
!     This is a stripped down version, adapted by s. chiacchiera 2017
!     Details on its behaviour:
!     input: FIELD
!     output: ALLOCATES and defines the values of these global variables
!     namspe, nspec, nspecmol, lfrzn, nammol, mlstrtspe, nmol, nbdmol,
!     nbond, bdinp1, bdinp2             
!     defines these global variables:
!     nsystcell, nusystcell, nfsystcell, nummolcell, numbondcell
!     nsyst, nusyst, nfsyst, nummol, numbond
!------------------------------------------------------------------------
      IMPLICIT none 
      
      CHARACTER(LEN=8) :: spenam
      CHARACTER(LEN=mxword) :: key, word, word1
      CHARACTER(LEN=200) :: record, record1
      INTEGER :: i, ioerror, ispe, j, k, finmol !ibond, iang, idhd, jspe, typ
!      REAL(KIND=dp) :: aa, bb, cc, dd, ee, ff, gg, x0, y0, z0, maxside
!      REAL(KIND=dp) :: el2, fac, pl2, frzwid, eunit
      LOGICAL :: safe
!      LOGICAL, ALLOCATABLE :: interact (:,:)

      INTEGER :: fail (13)

!     open FIELD channel

      OPEN (nread, file = 'FIELD', IOSTAT=ioerror)

      safe = (ioerror==0)

      IF (.NOT. safe) THEN
         WRITE (*,*) "error: unable to open FIELD"
         STOP
      END IF      
      
!     allocate arrays for reading in species, molecule and interaction data

      fail = 0
      ALLOCATE (namspe (nspe), STAT=fail(1))
      ALLOCATE (nspec (nspe), nspecmol (nspe), lfrzn (nspe), STAT=fail(3))
      ALLOCATE (nammol (0:nmoldef), STAT=fail(4))
      ALLOCATE (mlstrtspe (nmoldef, mxmolsize), nmol (nmoldef), nbdmol (nmoldef), STAT=fail(6))
      ALLOCATE (nbond (nmoldef), STAT=fail(7))
      ALLOCATE (bdinp1 (nmoldef, mxbonds), bdinp2 (nmoldef, mxbonds), STAT=fail(8))
      
      IF (ANY (fail/=0)) THEN
         WRITE (*,*) "allocation failure in subroutine read_field" 
         STOP
      END IF
         
      nammol (0) = '        '
      nsystcell = 0
      nusystcell = 0
      nfsystcell = 0
      nspec = 0
      nspecmol = 0
      nbond = 0
      numbond = 0
      numbondcell = 0
      nmol = 0 
      nbdmol = 0 
      nummol = 0 !added here by SC
      nummolcell = 0 !added here by SC

!       frzwdens = 0.0_dp
!       frzwxwid = 0.0_dp
!       frzwywid = 0.0_dp
!       frzwzwid = 0.0_dp

!     skip over file header

      READ (nread, '(a80)') record

!     read in species, molecular structures (and interaction data)

      DO WHILE (.true.)

         READ (nread, '(a200)', IOSTAT=ioerror) record

         IF (ioerror/=0) EXIT
         
         key = getword (record, 1)
         CALL lowercase (key)
         
         IF (key (1:5) =="close") EXIT
         
         IF (key (1:7) =="species") THEN
            
            DO i = 1, nspe

               READ (nread, '(a200)', IOSTAT=ioerror) record1
               word = getword (record1, 1)
               IF (LEN (TRIM (word))>8) &
                    WRITE (*,"('warning: name for species number ',i3,' in FIELD file truncated to 8 characters')") i               
               namspe (i) = word (1:8)
               nspec (i) = INT (getint (record1, 4), KIND=si)
               lfrzn (i) = INT (getint (record1, 5), KIND=si)
               nusystcell = nusystcell + nspec (i)
               IF (lfrzn (i)>0) nfsystcell = nfsystcell + nspec (i)

            END DO
            nsystcell = nsystcell + nusystcell

         ELSE IF (key (1:7) =="molecul") THEN

            DO i = 1, nmoldef

               READ (nread, '(a200)', IOSTAT=ioerror) record1
             word = getword (record1, 1)
             IF (LEN (TRIM (word))>8) &
                  WRITE (*,"('warning: name for molecule number ',i3,' in FIELD file truncated to 8 characters')") i               
             nammol (i) = word (1:8)
             
             DO WHILE (.true.)

               READ (nread, '(a200)', IOSTAT=ioerror) record1
               word = getword (record1, 1)
               CALL lowercase (word)

               IF (word (1:6) =="nummol") THEN

                 nmol (i) = INT (getint (record1, 2), KIND=si)

               ELSE IF (word (1:4) =="bead") THEN

                 finmol = INT (getint (record1, 2), KIND=si)
                 nbdmol (i) = finmol

                 DO j = 1, finmol

                   READ (nread, '(a200)', IOSTAT = ioerror) record1
                   ispe = 0
                   word1 = getword (record1, 1)
                   spenam = word1 (1:8)
                   DO k = 1, nspe
                     IF (spenam==namspe (k)) ispe = k
                   END DO
                   IF (ispe==0) ispe = INT (getint (record1, 1), KIND=si)
                   safe = (ispe>0 .AND. ispe<=nspe)
                   IF (.NOT. safe) THEN
                      WRITE (*,"('error: non-existent species given in FIELD file for molecule ',i3)") i
                      STOP
                   ENDIF

                   mlstrtspe (i, j) = ispe

                   nspecmol (ispe) = nspecmol (ispe) + nmol (i)
                   IF (lfrzn (ispe)>0) nfsystcell = nfsystcell + nmol (i)

                 END DO

              ELSE IF (word (1:4) =="bond") THEN

                 finmol = INT (getint (record1, 2), KIND=si)
                 nbond (i) = finmol
                 
                 DO j = 1, finmol

                    READ (nread, '(a200)', IOSTAT = ioerror) record1
                    word1 = getword (record1, 1)
                    CALL lowercase (word1)
                    safe = ((word1 (1:4) =="harm") .OR. (word1 (1:4) =="fene") .OR. (word1 (1:3) =="wlc") &
                         .OR. (word1 (1:4) =="mors"))
                    IF (.NOT. safe) THEN 
                       WRITE (*, "(/,1x,'error: unrecognised bond type defined in FIELD file')")
                       WRITE (*,"(1x,'hint: check molecule',2x,A8,2x,'bond number',2x,I3)") nammol (i), j
                       STOP
                    END IF
                       
                    bdinp1 (i, j) = INT (getint (record1, 2), KIND=si)
                    bdinp2 (i, j) = INT (getint (record1, 3), KIND=si)
                    IF (bdinp1 (i, j) == bdinp2 (i, j)) THEN !added by SC
                       WRITE (*, "(/,1x,'error: bond of a bead with itself in FIELD file')")                       
                       WRITE (*,"(1x,'hint: check molecule',2x,A8,2x,'bond number',2x,I3)") nammol (i), j
                       STOP
                    END IF
                 END DO

                 numbondcell = numbondcell + nbond (i) * nmol (i)
                 
              ELSE IF (word (1:6) =="finish") THEN

                 EXIT

              END IF

           END DO
          
           nsystcell = nsystcell + nbdmol (i) * nmol (i)
           nummolcell = nummolcell + nmol (i)

        END DO

!         ELSE IF (key (1:4) =="surf") THEN

!           SELECT CASE (srftype)
!           CASE (1)
!             DO i = 1, nspe
!               READ (nread, '(a200)', IOSTAT = ioerror) record1
!               word1 = getword (record1, 1)
!               spenam = word1 (1:8)
!               ispe = 0
!               DO j = 1, nspe
!                 IF (spenam==namspe (j)) ispe = j
!               END DO
!               IF (ispe==0) ispe = INT (getint (record1, 1), KIND=si)
!               safe = (ispe>0 .AND. ispe<=nspe)
!               IF (.NOT. safe) CALL error (idnode, 42, i)
!               aasrf (ispe) = getdble (record1, 2)
!             END DO
!           CASE (2)
!             READ (nread, '(a200)', IOSTAT = ioerror) record1
!             word1 = getword (record1, 1)
!             spenam = word1 (1:8)
!             frzwspe = 0
!             DO j = 1, nspe
!               IF (spenam==namspe (j)) frzwspe = j
!             END DO
!             IF (frzwspe==0) frzwspe = INT (getint (record1, 1), KIND=si)
!             safe = (frzwspe>0 .AND. frzwspe<=nspe)
!             IF (.NOT. safe) CALL error (idnode, 43, 0)
!             frzwdens = getdble (record1, 2)
!             frzwid = getdble (record1, 3)
!             IF (srfx>0) frzwxwid = frzwid
!             IF (srfy>0) frzwywid = frzwid
!             IF (srfz>0) frzwzwid = frzwid
!           END SELECT

!         ELSE IF (key (1:6) =="extern") THEN

!           READ (nread, '(a200)', IOSTAT = ioerror) record1

!           word1 = getword (record1, 1)
!           CALL lowercase (word1)
!           IF (word1 (1:4) =="grav") THEN
!             bdfrcx = getdble (record1, 2)
!             bdfrcy = getdble (record1, 3)
!             bdfrcz = getdble (record1, 4)
!             IF (ABS(bdfrcx)>1.0e-16_dp .OR. ABS(bdfrcy)>1.0e-16_dp .OR. ABS(bdfrcz)>1.0e-16_dp) ldyn = .true.
!           ELSE IF (word1 (1:4) =="shea") THEN
!             shrvx = getdble (record1, 2)
!             shrvy = getdble (record1, 3)
!             shrvz = getdble (record1, 4)
!             IF (srftype==3) THEN
!               IF (srfx>0) shrvx = 0.0_dp
!               IF (srfy>0) shrvy = 0.0_dp
!               IF (srfz>0) shrvz = 0.0_dp
!             END IF
!             IF (ABS(shrvx)>1.0e-16_dp .OR. ABS(shrvy)>1.0e-16_dp .OR. ABS(shrvz)>1.0e-16_dp) ldyn = .true.
!           ELSE IF (word1 (1:4) =="elec") THEN
!             elecx = getdble (record1, 2)
!             elecy = getdble (record1, 3)
!             elecz = getdble (record1, 4)
!           END IF

     END IF

  END DO

!     determine numbers of particles, bonds etc. from unit cell values

       nsyst = nsystcell * nfold
       nusyst = nusystcell * nfold
       nfsyst = nfsystcell * nfold
       numbond = numbondcell * nfold
       nspec = nspec * nfold
       nspecmol = nspecmol * nfold
       nummol = nummolcell * nfold
       
       CLOSE (nread) !added by SC
       
      RETURN
      END SUBROUTINE read_field

      SUBROUTINE read_config

!***********************************************************************
!
!     read initial configuration from DL_POLY-formatted CONFIG file
!
!     copyright stfc daresbury laboratory
!     authors - w. smith & m. a. seaton july 2015
!
!***********************************************************************
!     This is a stripped down version, adapted by s. chiacchiera 2017
!     Details on its behaviour:
!     input: CONFIG
!     output: 
!     defines these global variables (for all the system beads):
!     lab, ltp, ltm 
!     xxx, yyy, zzz, bndtbl 
!***********************************************************************    
        
      IMPLICIT none

      REAL(KIND=dp) :: x, y, z, xf, yf, zf, xs, ys, zs, halfx, halfy, halfz
      ! REAL(KIND=dp) :: disx, disy, disz, xdispl, ydispl, zdispl, shfx, shfy, shfz
      INTEGER :: ioerror, species, gb, global, numpart, i, imol, imoltyp, j, k
!      INTEGER :: bondsize, msyst, ntop, ifx, ify, ifz, finmol, numwlbd, isx, isy, isz, mtbead
      INTEGER :: bondsize, ntop, ifx, ify, ifz, finmol, numwlbd
      ! number of beads, frozen beads in domain cell (keep them to minimize changes)
      INTEGER :: nbeads, nfbeads 
      CHARACTER(LEN=200) :: record
      CHARACTER(LEN=mxword) :: word
      CHARACTER(LEN=8) :: specname
      LOGICAL :: safe, numsafe, linside, molbead (nsyst-nusyst)
      
      REAL(KIND=dp), ALLOCATABLE :: mlxxx (:), mlyyy (:), mlzzz (:)
      INTEGER, ALLOCATABLE :: mlspe (:)

      INTEGER :: fail (4)
      INTEGER :: aux1
      
! !     determine number of beads in frozen bead walls to be added to system

!       numwlbd = npxfwx*npxfwy*npxfwz + npyfwx*npyfwy*npyfwz + npzfwx*npzfwy*npzfwz

      numwlbd = 0 ! to be changed if frozen bead wall is present
      
!     allocate arrays for bonded particle positions, species, velocity and force

      fail = 0
      ALLOCATE (mlxxx (nsystcell-nusystcell), mlyyy (nsystcell-nusystcell), mlzzz (nsystcell-nusystcell), STAT=fail(1))
      ALLOCATE (mlspe (nsystcell-nusystcell), STAT=fail(2))
      IF (ANY (fail/=0)) THEN
         WRITE (*,*) "error: allocation failure in subroutine read_config"
         STOP
      END IF

!     determine adjustment for positions

      IF (lconfzero) THEN
        halfx = 0.0_dp
        halfy = 0.0_dp
        halfz = 0.0_dp
      ELSE
        halfx = 0.5_dp * dimxcell
        halfy = 0.5_dp * dimycell
        halfz = 0.5_dp * dimzcell
     END IF

!     open config channel
  
      OPEN (nread, file = 'CONFIG', IOSTAT=ioerror)

      safe = (ioerror==0)
      IF (.NOT. safe) THEN
         WRITE (*,*) "error: unable to open CONFIG"
         STOP
      END IF

!     read title line (not used)

      READ (nread, '(a80)') record

!     skip keys for config data and boundaries (already read)

      READ (nread, '(a80)') record

!     skip over cell vectors if imcon>0 (already read)

      IF (imcon>0) THEN
        READ (nread, '(a80)') record
        READ (nread, '(a80)') record
        READ (nread, '(a80)') record
      END IF

!     read in data for each particle

      nbeads = 0   ! in a serial run (as it is here) must coincide with nsyst
      nfbeads = 0  ! in a serial run (as it is here) must coincide with nfsyst
      numpart = 0  ! must coincide with nsystcell
      molbead = .false.
      numsafe = .true. 

      DO WHILE (.true.)

        READ (nread, '(a80)', IOSTAT=ioerror) record
        IF (ioerror/=0) EXIT

!     find species type, first by name then by number

        species = 0
        word = getword (record, 1)
        specname = word (1:8)
        DO i = 1, nspe
          IF (specname==namspe (i)) species = i
        END DO

        IF (species==0) species = INT (getint (record, 1), KIND=si)

        safe = (species>0 .AND. species<=nspe)
        IF (.NOT. safe) THEN
           WRITE (*,"('error: non-existent species given in CONFIG file for bead after ',i10)") global !i
           STOP
        ENDIF
        
!     find global particle number; if none given or ignoring given value, provide one

        global = INT (getint (record, 2), KIND=si)
        numpart = numpart + 1

        IF (ligindex .OR. global==0) THEN
          global = numpart
        END IF
        
!    read position of particle

        READ (nread, '(a80)') record
        x = getdble (record, 1) + halfx
        y = getdble (record, 2) + halfy
        z = getdble (record, 3) + halfz
        
!     if included, skip velocity and force of particle (not needed for the check)

        IF (levcfg>0) READ (nread, '(a80)') record

        IF (levcfg>1) READ (nread, '(a80)') record

!     give a warning if any coordinate is out of the unit cell (added by SC)
        IF (x>dimxcell .OR. x<0) &
             WRITE (*,"('warning: x coord. for bead ',i10,2x, 'in CONFIG file out of unit cell (will be wrapped)')") global
        IF (y>dimycell .OR. y<0) &
             WRITE (*,"('warning: y coord. for bead ',i10,2x, 'in CONFIG file out of unit cell (will be wrapped)')") global
        IF (z>dimzcell .OR. z<0) &
             WRITE (*,"('warning: z coord. for bead ',i10,2x, 'in CONFIG file out of unit cell (will be wrapped)')") global
        
!     ensure particle is inside unit cell

        x = x - dimxcell * FLOOR (x/dimxcell)
        y = y - dimycell * FLOOR (y/dimycell)
        z = z - dimzcell * FLOOR (z/dimzcell)        
        
!     if particle is unbonded, add to each copy of unit cell (adjustment for frozen bead
!     walls NOT active -> recover if needed)

        IF (global<=nusystcell) THEN

          DO ifz = 0, nfoldz-1
            DO ify = 0, nfoldy-1
              DO ifx = 0, nfoldx-1

                gb = global + (ifx + nfoldx * (ify + nfoldy * ifz)) * nusystcell + numwlbd

                xf = x + REAL(ifx, KIND=dp) * dimxcell !+ frzwxwid ! to be changed if frozen bead wall is present
                yf = y + REAL(ify, KIND=dp) * dimycell !+ frzwywid
                zf = z + REAL(ifz, KIND=dp) * dimzcell !+ frzwzwid
                
                linside = .true. ! kept to minimize code changes (serial run here)
                
                IF (linside) THEN
                  nbeads = nbeads + 1
                  IF (nbeads>maxdim) THEN
                    numsafe = .false.                    
                  ELSE
                    lab (nbeads) = gb
                    ltp (nbeads) = species
                    ltm (nbeads) = 0

                    xxx (nbeads) = xf !- delx ! serial run is assumed, then domain=system (delx=dely=delz=0)
                    yyy (nbeads) = yf !- dely
                    zzz (nbeads) = zf !- delz

                    IF (lfrzn (species)>0) nfbeads = nfbeads + 1
                  END IF
                END IF

              END DO
            END DO
          END DO
          
       ELSE IF (global<=nsystcell) THEN !        ELSE IF (lbond .AND. global<=nsystcell) THEN ! corrected
          
!     if particle is bonded, add species to arrays so these can be added later 
          mlxxx (global-nusystcell) = x
          mlyyy (global-nusystcell) = y
          mlzzz (global-nusystcell) = z
          mlspe (global-nusystcell) = species

       END IF

    END DO ! end of the loop over the CONFIG file

!     check that the total number of beads is not larger than maxdim (= nsyst here)
    IF (.NOT. numsafe) THEN
       WRITE (*,"(/,1x,'error: too many beads defined in the (possibly folded) CONFIG file')")
       STOP
    END IF
    
!     check total number of beads corresponds with FIELD file ! this check partially overlaps with the previous one, kept to minimize changes

      IF (numpart/=nsystcell) THEN
         aux1 = numpart - nsystcell
         IF (aux1>0) THEN
            WRITE (*,"(/,1x,'error: discrepancy in total number of beads in CONFIG  - ',i10,' too many')") aux1
         ELSE
            WRITE (*,"(/,1x,'error: discrepancy in total number of beads in CONFIG - ',i10,' too few')") ABS(aux1)
         END IF
      END IF

!     rearrange bonded bead positions to give numbering across unit cell boundaries
            
      IF (nmoldef>0) THEN      !IF (lbond) THEN ! changed by SC

        imol = 0 ! molecule type
        ntop = 0 ! molecule upper limit counter   

        DO i = 1, nummolcell

          DO WHILE (i>ntop)
            imol = imol + 1
            ntop = ntop + nmol (imol)
          END DO

          k = molstart (i) - nusystcell

          xs = mlxxx (k+1)
          ys = mlyyy (k+1)
          zs = mlzzz (k+1)

          finmol = nbdmol (imol)

          DO j = 2, finmol

            x = mlxxx (k+j) - xs
            y = mlyyy (k+j) - ys
            z = mlzzz (k+j) - zs

            x = x - dimxcell * ANINT (x/dimxcell)
            y = y - dimycell * ANINT (y/dimycell)
            z = z - dimzcell * ANINT (z/dimzcell)

            xs = xs + x
            ys = ys + y
            zs = zs + z

            mlxxx (k+j) = xs
            mlyyy (k+j) = ys
            mlzzz (k+j) = zs

          END DO
           
        END DO
        
!     add bonded particles to each copy of unit cell

        imol = 0 ! molecule counter 
        imoltyp = 0 ! molecule type
        ntop = 0 ! molecule upper limit counter 
        DO i = nusystcell+1, nsystcell

          DO WHILE (i>molstart (imol+1))
            imol = imol + 1
          END DO

          DO WHILE (imol>ntop)
            imoltyp = imoltyp + 1
            ntop = ntop + nmol (imoltyp)
          END DO

          x = mlxxx (i-nusystcell)
          y = mlyyy (i-nusystcell)
          z = mlzzz (i-nusystcell)          
          species = mlspe (i-nusystcell)

          DO ifz = 0, nfoldz-1
            DO ify = 0, nfoldy-1
              DO ifx = 0, nfoldx-1

                gb = nusyst + (i - molstart (imol)) + nfold * (molstart (imol) - nusystcell) + &
                    &(ifx + nfoldx * (ify + nfoldy * ifz)) * (molstart(imol+1) - molstart(imol))

                xf = x + REAL(ifx, KIND=dp) * dimxcell !+ frzwxwid ! to be changed if frozen bead wall is present
                yf = y + REAL(ify, KIND=dp) * dimycell !+ frzwywid
                zf = z + REAL(ifz, KIND=dp) * dimzcell !+ frzwzwid

                xf = xf - dimx * FLOOR (xf/dimx)
                yf = yf - dimy * FLOOR (yf/dimy)
                zf = zf - dimz * FLOOR (zf/dimz)
                
                linside = .true. ! kept to minimize code changes (serial run here)
                
                IF (linside) THEN
                  nbeads = nbeads + 1
                  IF (nbeads>maxdim) THEN
                    numsafe = .false.
                  ELSE
                    lab (nbeads) = gb
                    ltp (nbeads) = species
                    ltm (nbeads) = imoltyp
                    xxx (nbeads) = xf !- delx ! serial run is assumed, then domain=system (delx=dely=delz=0)
                    yyy (nbeads) = yf !- dely
                    zzz (nbeads) = zf !- delz                   
                    molbead (gb-nusyst) = .true.
                    IF (lfrzn (species)>0) nfbeads = nfbeads + 1
                  END IF
                END IF

              END DO
            END DO
          END DO

        END DO

        END IF ! over "nmoldef>0"
        
!     check that the total number of beads is not larger than maxdim (= nsyst here)
        IF (.NOT. numsafe) THEN
           WRITE (*,"(/,1x,'error: too many beads defined in the (possibly folded) CONFIG file')")
           STOP
        END IF

! !     insert frozen walls as simple cubic lattices (not active: recover missing part if needed)
!       IF (srftype==2) THEN
!        ... cut ...
!       END IF ! end of the case srftype = 2

!     check total number of beads ! this check partially overlaps with the previous one, kept to minimize changes

      IF (nbeads/=nsyst) THEN
         aux1 = nbeads - nsyst
         IF (aux1>0) THEN
            WRITE (*,"(/,1x,'error: discrepancy in total number of beads in CONFIG  - ',i10,' too many')") aux1
         ELSE
            WRITE (*,"(/,1x,'error: discrepancy in total number of beads in CONFIG - ',i10,' too few')") ABS(aux1)
         END IF
      END IF      
      
! !     re-order local beads to give frozen beads first ! -> commented since not needed here

!       CALL sort_beads  
      
!     add bonds to system

      nbonds = 0
      
      IF (lbond) THEN
         
         imol = 0 ! molecule type
         ntop = 0 ! molecule upper limit counter   
         
         DO j = 1, nummolcell
            
            DO WHILE (j>ntop)
               imol = imol + 1
               ntop = ntop + nmol (imol)
            END DO
            
!     add bonds to table
            
            bondsize = nbond (imol)
            
            DO i = 0, nfold-1
               
               gb = nusyst + nfold * (molstart (j) - nusystcell) + i * (molstart (j+1) - molstart (j))
               
               DO k = 1, bondsize
                  
                  IF (lgbnd .OR. molbead (gb + bdinp1 (imol, k) - nusyst)) THEN
                     nbonds = nbonds + 1
                     bndtbl (nbonds, 1) = bdinp1 (imol, k) + gb
                     bndtbl (nbonds, 2) = bdinp2 (imol, k) + gb
                  END IF
                  
               END DO
               
            END DO

         END DO
            
      END IF
            
      CLOSE (nread)

      fail = 0
      DEALLOCATE (mlxxx, mlyyy, mlzzz, mlspe, STAT=fail(1))
      IF (ANY (fail/=0)) THEN
        WRITE (*,*) "error: deallocation failure in subroutine read_config"
        STOP
     END IF
     ! ! added by SC - for testing
     ! WRITE (*,*) "within READ_CONFIG"
     ! DO i = 1, nsyst
     !    WRITE (*,*) lab (i), ltp (i), ltm (i)
     ! END DO
     ! WRITE (*,*) "within READ_CONFIG, Re-ordered"
     ! DO j = 1, nsyst
     !    DO i = 1, nsyst 
     !       IF (lab (i) == j) THEN
     !          WRITE (*,*) lab (i), ltp (i), ltm (i)
     !          WRITE (1,*) namspe (ltp (i)), lab (i) 
     !          WRITE (1,'(3F16.10)') xxx (i), yyy (i), zzz (i)
     !          EXIT
     !       END IF
     !    END DO
     ! END DO

     ! ! end of added
     
      END SUBROUTINE read_config

      SUBROUTINE define_molstart
!***********************************************************************    
!       Extracted from the "subroutine start", 
!       authored by w. smith & m. a. seaton, july 2015
!       Adapted by s. chiacchiera, 2017
!        
!       molstart (i) = global id of the last particle in the (i-1)th 
!       molecule
!***********************************************************************    
        IMPLICIT none
        INTEGER :: fail, j, imol, ntop
        fail = 0
        ! set up array for global bead numbers of molecules (for unit cell)

        ALLOCATE (molstart (nummolcell+1), STAT=fail)
        IF (fail/=0) THEN
           WRITE (*,*) "error: allocation failure in define_molstart"
           STOP         
        END IF
        
        molstart (1) = nusystcell
        
        IF (nmoldef>0) THEN
           
           imol = 0 ! molecule type
           ntop = 0 ! molecule counter
           
           DO j=1, nummolcell
              
              DO WHILE (j>ntop)
                 imol = imol + 1
                 ntop = ntop + nmol (imol)
              END DO
              
              molstart (j+1) = molstart (j) + nbdmol (imol)
              
           END DO
           
           molstart (nummolcell+1) = nsystcell

        END IF
        RETURN
      END SUBROUTINE define_molstart
      
! MODULE parse_utils

! !*********************************************************************
! !
! ! dl_meso module for parsing text input
! !
! ! copyright - stfc daresbury laboratory
! ! authors   - w. smith & m. a. seaton november 2015
! ! 
! !*********************************************************************

! !      USE constants
!       IMPLICIT none

!       PUBLIC :: getword, parseint, parsedble, getint, getdble

! CONTAINS

      CHARACTER(LEN=mxword) FUNCTION getword (txt, n)

!*********************************************************************
!  
!     copyright - stfc daresbury laboratory
!     author    - w. smith 2001
!
!*********************************************************************

      IMPLICIT none

      CHARACTER(LEN=1) :: u
      CHARACTER(LEN=*), INTENT(IN) :: txt
      INTEGER :: a, b, c, k, n, m
      CHARACTER(LEN=mxword) :: word

      k = 0
      a = 1
      b = 0
      c = 1
      m = LEN (txt)

      word = " "

      DO WHILE (k<n .AND. c<m)

        u = txt (c:c)

        IF (u=="," .OR. u==" " .OR. u==ACHAR(9)) THEN

          IF (b>a) k = k + 1
          IF (k<n) a = c + 1

        ELSE

          b = c + 1

        END IF

        c = c + 1

      END DO

      IF (k==n) THEN

        word = txt (a:b)

      ELSE IF (k==(n-1) .AND. c==m) THEN

        word = txt (a:m)

      END IF

      getword = word

      RETURN
      END FUNCTION getword

      INTEGER(KIND=li) FUNCTION parseint (word)

!*********************************************************************
!
!     copyright - daresbury laboratory
!     author    - w. smith 2001
!
!*********************************************************************

      IMPLICIT none

      CHARACTER(LEN=1) :: u
      CHARACTER(LEN=*), INTENT(IN) :: word
      INTEGER :: i, m
      INTEGER(KIND=li) :: k, s

      k = 0_li
      s = 1_li
      m = LEN (word)

      DO i = 1, m

        u = word (i:i)

        SELECT CASE (u)
        CASE ("-")
          s = -1
        CASE ("0")
          k = 10_li * k
        CASE ("1")
          k = 10_li * k + 1_li
        CASE ("2")
          k = 10_li * k + 2_li
        CASE ("3")
          k = 10_li * k + 3_li
        CASE ("4")
          k = 10_li * k + 4_li
        CASE ("5")
          k = 10_li * k + 5_li
        CASE ("6")
          k = 10_li * k + 6_li
        CASE ("7")
          k = 10_li * k + 7_li
        CASE ("8")
          k = 10_li * k + 8_li
        CASE ("9")
          k = 10_li * k + 9_li
        CASE (".")
          EXIT
        END SELECT

      END DO

      parseint = s * k

      RETURN
      END FUNCTION parseint

      REAL(KIND=dp) FUNCTION parsedble (word)

!*********************************************************************
!
!     copyright - daresbury laboratory
!     author    - w. smith 2001
!     revision  - m. a. seaton may 2012
!
!*********************************************************************

      IMPLICIT none

      CHARACTER(1) :: u
      CHARACTER(LEN=*), INTENT(IN) :: word
      INTEGER :: i, m, z, p, e, y
      REAL(KIND=dp) :: x, s

      z = 1
      p = 0
      e = 0
      y = 0
      s = 1.0_dp
      x = 0.0_dp
      m = LEN (word)

      DO i = 1, m

        u = word (i:i)

        SELECT CASE (u)

        CASE ("-")

          IF (e==0) THEN
            s = -1.0_dp
          ELSE
            z = -1
          END IF

        CASE (".")
          p = 1
        CASE ("e", "E")
          e = 1
        CASE ("0")

          IF (e==0) THEN
            x = 10.0_dp * x
            IF (p>0) p = p + 1
          ELSE
            y = 10 * y
          END IF

        CASE ("1")

          IF (e==0) THEN
            x = 10.0_dp * x + 1.0_dp
            IF (p>0) p = p + 1
          ELSE
            y = 10 * y + 1
          END IF

        CASE ("2")

          IF (e==0) THEN
            x = 10.0_dp * x + 2.0_dp
            IF (p>0) p = p + 1
          ELSE
            y = 10 * y + 2
          END IF

        CASE ("3")

          IF (e==0) THEN
            x = 10.0_dp * x + 3.0_dp
            IF (p>0) p = p + 1
          ELSE
            y = 10 * y + 3
          END IF

        CASE ("4")

          IF (e==0) THEN
            x = 10.0_dp * x + 4.0_dp
            IF (p>0) p = p + 1
          ELSE
            y = 10 * y + 4
          END IF

        CASE ("5")

          IF (e==0) THEN
            x = 10.0_dp * x + 5.0_dp
            IF (p>0) p = p + 1
          ELSE
            y = 10 * y + 5
          END IF

        CASE ("6")

          IF (e==0) THEN
            x = 10.0_dp * x + 6.0_dp
            IF (p>0) p = p + 1
          ELSE
            y = 10 * y + 6
          END IF

        CASE ("7")

          IF (e==0) THEN
            x = 10.0_dp * x + 7.0_dp
            IF (p>0) p = p + 1
          ELSE
            y = 10 * y + 7
          END IF

        CASE ("8")

          IF (e==0) THEN
            x = 10.0_dp * x + 8.0_dp
            IF (p>0) p = p + 1
          ELSE
            y = 10 * y + 8
          END IF

        CASE ("9")

          IF (e==0) THEN
            x = 10.0_dp * x + 9.0_dp
            IF (p>0) p = p + 1
          ELSE
            y = 10 * y + 9
          END IF

        END SELECT

      END DO

      IF (p==0) p = 1
      parsedble = s * x * 10.0_dp ** (z * y - p + 1)

      RETURN
      END FUNCTION parsedble

      INTEGER(KIND=li) FUNCTION getint (txt, n)

!*********************************************************************
!
!     copyright - daresbury laboratory
!     author    - w. smith 2001
!
!*********************************************************************

      IMPLICIT none

      CHARACTER(1) :: u
      CHARACTER(LEN=mxword) :: word
      CHARACTER(LEN=*), INTENT(IN) :: txt
      INTEGER :: a, b, c, n, k, m
      INTEGER(KIND=li) :: d

      a = 1
      b = 0
      c = 1
      d = 0_li
      k = 0
      m = LEN (txt)

      DO WHILE (k<n .AND. c<m)

        u = txt (c:c)

        IF (u=="," .OR. u==" ") THEN

          IF (b>a) k = k + 1
          IF (k<n) a = c + 1

        ELSE

          b = c + 1

        END IF

        c = c + 1

      END DO

      IF (k==n) THEN

        word = txt (a:b)
        d = parseint (word)

      ELSE IF (k==(n-1) .AND. c==m) THEN

        word = txt (a:m)
        d = parseint (word)

      END IF

      getint = d

      RETURN
      END FUNCTION getint

      REAL(KIND=dp) FUNCTION getdble (txt, n)

!*********************************************************************
!
!     copyright - daresbury laboratory
!     author    - w. smith 2001
!
!*********************************************************************

      IMPLICIT none

      CHARACTER(LEN=1) :: u
      CHARACTER(LEN=mxword) :: word
      CHARACTER(LEN=*), INTENT(IN) :: txt
      INTEGER :: a, b, c, k, m, n
      REAL(KIND=dp) :: d

      a = 1
      b = 0
      c = 1
      k = 0
      d = 0.0_dp
      m = LEN (txt)

      DO WHILE (k<n .AND. c<m)

        u = txt (c:c)

        IF (u==" " .OR. u==",") THEN

          IF (b>a) k = k + 1
          IF (k<n) a = c + 1

        ELSE

          b = c + 1

        END IF

        c = c + 1

      END DO

      IF (k==n) THEN

        word = txt (a:b)
        d = parsedble (word)

      ELSE IF (k==(n-1) .AND. c==m) THEN

        word = txt (a:m)
        d = parsedble (word)

      END IF

      getdble = d

      RETURN
      END FUNCTION getdble

      SUBROUTINE lowercase (word)

!*********************************************************************
!  
!     copyright - stfc daresbury laboratory
!     author    - i. t. todorov & m. a. seaton 2011
!
!*********************************************************************

      IMPLICIT none

      CHARACTER(LEN=*), INTENT(INOUT) :: word
      INTEGER :: i

      DO i = 1, LEN(word)

        SELECT CASE (word(i:i))
        CASE ('A')
          word(i:i) = 'a'
        CASE ('B')
          word(i:i) = 'b'
        CASE ('C')
          word(i:i) = 'c'
        CASE ('D')
          word(i:i) = 'd'
        CASE ('E')
          word(i:i) = 'e'
        CASE ('F')
          word(i:i) = 'f'
        CASE ('G')
          word(i:i) = 'g'
        CASE ('H')
          word(i:i) = 'h'
        CASE ('I')
          word(i:i) = 'i'
        CASE ('J')
          word(i:i) = 'j'
        CASE ('K')
          word(i:i) = 'k'
        CASE ('L')
          word(i:i) = 'l'
        CASE ('M')
          word(i:i) = 'm'
        CASE ('N')
          word(i:i) = 'n'
        CASE ('O')
          word(i:i) = 'o'
        CASE ('P')
          word(i:i) = 'p'
        CASE ('Q')
          word(i:i) = 'q'
        CASE ('R')
          word(i:i) = 'r'
        CASE ('S')
          word(i:i) = 's'
        CASE ('T')
          word(i:i) = 't'
        CASE ('U')
          word(i:i) = 'u'
        CASE ('V')
          word(i:i) = 'v'
        CASE ('W')
          word(i:i) = 'w'
        CASE ('X')
          word(i:i) = 'x'
        CASE ('Y')
          word(i:i) = 'y'
        CASE ('Z')
          word(i:i) = 'z'
        END SELECT

      END DO

      END SUBROUTINE lowercase

! END MODULE
      
    END PROGRAM check_config

