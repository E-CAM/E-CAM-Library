PROGRAM check_config
!***********************************************************************
! Program to check the input files for DL_MESO.
! It is checked that the provided CONFIG file (optional input) 
! is consistent with the CONTROL and FIELD files (necessary input).
!
! authors: m. a. seaton and s. chiacchiera, October 2017 (WIP)
!***********************************************************************
  IMPLICIT none

  LOGICAL l_exist, safe 

  INTEGER, PARAMETER :: nread = 15 ! change?

!  CHARACTER(LEN=80) :: text ! needed?
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND (15, 307)
  INTEGER, PARAMETER :: si = SELECTED_INT_KIND (8)
  INTEGER, PARAMETER :: li = SELECTED_INT_KIND (12)
  INTEGER, PARAMETER :: mxword=16
  INTEGER :: i, voltype
  LOGICAL :: lnfold, lvol
  INTEGER :: nfold, nfoldx, nfoldy, nfoldz
  INTEGER :: levcfg, imcon
!  REAL(dp) :: aux1!, aux2
  INTEGER :: mxmolsize
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
! molecule insertion (only species) 
  INTEGER, ALLOCATABLE, SAVE :: mlstrtspe (:,:)
  INTEGER, ALLOCATABLE, SAVE :: molstart (:)
! maximum value for numbers of beads
  INTEGER, POINTER :: maxdim  !remove later?
! switch to determine whether to ignore global bead numbers in CONFIG file
  LOGICAL :: ligindex
! switch for bonds
  LOGICAL :: lbond
! global number, particle type, molecule type
  INTEGER, ALLOCATABLE, SAVE :: lab (:), ltp (:), ltm (:)
! varibales needed for the checks
  INTEGER, ALLOCATABLE :: nspec0 (:), nspecmol0 (:)
  INTEGER :: tp, tm, tp0, ibd, j, k, l
  INTEGER :: fail (13)

  lbond = .true. ! I keep it to minimize changes. remove later??? Ask user? Use FIELD?
  
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
  lnfold = .false. ! needed?
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

  ligindex = .false.
  
! scan and read all the input files
  
  CALL read_control 

  IF (lvol .AND. (ABS(nfold-1)>1.e-10_dp)) THEN
     WRITE (*,*) "error (warning?): incompatible choices in CONTROL files (vol and nfold /=1)"
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
     STOP
  END SELECT

  IF (volm < 1.e-10_dp) THEN
     WRITE (*,*) "error: system volume is zero"
     STOP
  ELSE
!     IF (ABS(nfold-1)>1.e-10_dp) THEN
        WRITE (*,*) "unit cell sizes: ", dimxcell, dimycell, dimzcell      
        WRITE (*,*) "nfoldx, nfoldy, nfoldz =", nfoldx, nfoldy, nfoldz
!     END IF
     WRITE (*,*) "system sizes: ", dimx, dimy, dimz 
  ENDIF
  
  WRITE (*,*) "imcon =", imcon !may be removed...
  WRITE (*,*) "levcfg =", levcfg !may be removed...
  
  CALL scan_field

  WRITE (*,*) "nspe = ", nspe
  WRITE (*,*) "nmoldef = ", nmoldef 
  WRITE (*,*) "mxmolsize = ", mxmolsize

  IF (nspe == 0) THEN
     WRITE (*,*) "error: no particle species defined in FIELD file"
     STOP
  END IF

  CALL read_field

  WRITE (*,*) "nspec = ", nspec
  WRITE (*,*) "nspecmol = ", nspecmol
  DO i = 1, nmoldef
     WRITE (*,*) "mlstrtspe (i,:)=", mlstrtspe (i,:)
  ENDDO

! temporarily keep maxdim (to minimize the changes to the code)
  maxdim => nsyst

  CALL define_molstart
  
  ALLOCATE (lab (maxdim), ltp (maxdim), ltm (maxdim))

  CALL read_config

! checks (when needed to avoid confusion, the expected values are named *0)
! use ltp and ltm to derive nspec, nspecmol and molecules obtained using CONFIG file 
  
  ALLOCATE (nspec0 (nmoldef), nspecmol0 (nmoldef))

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
  DO i = 1, nmoldef
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
           l = 1 ! should it start from nusyst? 
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
     
  !  DEALLOCATE (lab, ltp, ltm)
  DEALLOCATE (nspec0, nspecmol0)
! de-allocate arrays, as in free_memory
  
  fail = 0

  DEALLOCATE (namspe, nspec, nspecmol, lfrzn, nammol, STAT=fail(4))
  DEALLOCATE (mlstrtspe, nmol, nbdmol, STAT=fail(5))
  DEALLOCATE (lab, ltp, ltm, STAT=fail(10))
  DEALLOCATE (molstart, STAT=fail(13))
  
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
!     This is a simplified version, adapted by s. chiacchiera 2017
!     Details on its behaviour:
!     input: CONTROL
!     output: may alter from default the values of these global variables
!     lnfold, nfold, nfoldx, nfoldy, nfoldz
!     lvol, dimxcell, dimycell, dimzcell
!     ligindex    
!***********************************************************************    
!  USE parse_utils

    IMPLICIT none
  
    CHARACTER(LEN=mxword) :: key1, key2!, word1, word2, word3, word4
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
       
       IF (key1 (1:5) =="nfold") THEN
          
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
          !   ELSE IF (key1 (1:4) =="surf") THEN
          
          !     IF (key2 (1:3) =="cut") srfzcut = getdble (record, 3)
          
          !     IF (key2 (1:4) =="hard") THEN
          !       srftype = 1
          !       word1 = getword (record, 3)
          !       word2 = getword (record, 4)
          !       word3 = getword (record, 5)
          !       CALL lowercase (word1)
          !       CALL lowercase (word2)
          !       CALL lowercase (word3)
          !       IF (word1(1:1)=="x" .OR. word2(1:1)=="x".OR. word3(1:1)=="x") srfx = 1
          !       IF (word1(1:1)=="y" .OR. word2(1:1)=="y".OR. word3(1:1)=="y") srfy = 1
          !       IF (word1(1:1)=="z" .OR. word2(1:1)=="z".OR. word3(1:1)=="z") srfz = 1
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
          !     END IF
          
       ELSE IF (key1 (1:3) =="vol") THEN
          lvol = .true.
          dimxcell = getdble (record, 2)  ! dimx = getdble (record, 2)
          dimycell = getdble (record, 3)  ! dimy = getdble (record, 3)           
          dimzcell = getdble (record, 4)  ! dimz = getdble (record, 4)      
          volume = dimxcell * dimycell * dimzcell  
          IF (volume <1.0e-10_dp) THEN
             cubeside = dimxcell ** (1.0_dp/3.0_dp) !dimx ** (1.0_dp/3.0_dp) !
             dimxcell = cubeside ! dimx = cubeside  !       
             dimycell = cubeside ! dimy = cubeside  !       
             dimzcell = cubeside !dimz = cubeside  !       
          END IF
          !     lnfold = .false. !Why??? I remove this
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

!      USE parse_utils
        IMPLICIT none ! added by SC
        
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
!     This is a simplified version, adapted by s. chiacchiera 2017
!     Details on its behaviour:
!     input: FIELD
!     output:
!     defines the values of these global variables:
!     nspe, nmoldef, mxmolsize
!----------------------------------------------------------------------
        
!      USE parse_utils
        IMPLICIT none ! added by SC        
      LOGICAL :: finish, safe!lexist, 
      CHARACTER(LEN=mxword) :: key, word
      CHARACTER(LEN=200) :: record, record1
      INTEGER :: i, ioerror, imol!, ibond, iangle, idihed, ipot, j

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
!     This is a simplified version, adapted by s. chiacchiera 2017
!     Details on its behaviour:
!     input: FIELD
!     output: ALLOCATES and defines the values of these global variables
!     namspe, nspec, nspecmol, lfrzn, nammol, mlstrtspe, nmol, nbdmol
!     defines these global variables:
!     nsystcell, nusystcell, nfsystcell, nummolcell
!     nsyst, nusyst, nfsyst, nummol
!------------------------------------------------------------------------
        
!      USE parse_utils

      IMPLICIT none ! added by SC        
      
      CHARACTER(LEN=8) :: spenam
      CHARACTER(LEN=mxword) :: key, word, word1
      CHARACTER(LEN=200) :: record, record1
      INTEGER :: i, ibond, iang, idhd, ioerror, ispe, j, jspe, k, finmol, typ
      REAL(KIND=dp) :: aa, bb, cc, dd, ee, ff, gg, x0, y0, z0, maxside
      REAL(KIND=dp) :: el2, fac, pl2, frzwid, eunit
      LOGICAL :: safe
      LOGICAL, ALLOCATABLE :: interact (:,:)

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
       nspec = nspec * nfold
       nspecmol = nspecmol * nfold
       nummol = nummolcell * nfold
! !
! !      WRITE (*,*) "nspecmol=",nspecmol
! !      WRITE (*,*) "nspec=",nspec
! !      STOP
! !     

!       ! PRINT *, "from READ_FIELD"
!       ! PRINT *, "mlstrtspe is"
!       ! DO i = 1, nmoldef
!       !    PRINT *, nammol (i), mlstrtspe (i,:)
!       ! END DO

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
!     This is a simplified version, adapted by s. chiacchiera 2017
!     Details on its behaviour:
!     input: CONFIG
!     output: 
!     defines these global variables: lab, ltp, ltm (for all the system beads)        
!***********************************************************************    

        
!      USE comms_module !ok like this?
!      USE parse_utils 
!      USE domain_module !ok like this?
      IMPLICIT none

      ! REAL(KIND=dp) :: x, y, z, xf, yf, zf, xs, ys, zs, vx, vy, vz, fx, fy, fz, halfx, halfy, halfz
      ! REAL(KIND=dp) :: disx, disy, disz, xdispl, ydispl, zdispl, shfx, shfy, shfz
      !INTEGER :: ioerror, species, gb, global, numpart, i, imol, imoltyp, inod, j, k
      INTEGER :: ioerror, species, gb, global, numpart, i, imol, imoltyp!, inod, j, k
!      INTEGER :: bondsize, msyst, ntop, ifx, ify, ifz, finmol, numwlbd, isx, isy, isz, mtbead
      INTEGER :: ntop, ifx, ify, ifz, finmol, numwlbd
      ! number of beads, frozen beads in domain cell (keep them temporarily)
      INTEGER :: nbeads, nfbeads 
      CHARACTER(LEN=200) :: record
      CHARACTER(LEN=mxword) :: word
      CHARACTER(LEN=8) :: specname
      LOGICAL :: safe, numsafe, linside, molbead (nsyst-nusyst)
      
      ! REAL(KIND=dp), ALLOCATABLE :: mlxxx (:), mlyyy (:), mlzzz (:), mlvelx (:), mlvely (:), mlvelz (:)
      ! REAL(KIND=dp), ALLOCATABLE :: mlfrcx (:), mlfrcy (:), mlfrcz (:)
      INTEGER, ALLOCATABLE :: mlspe (:)

      ! INTEGER :: fail (4), tbead (6)
      INTEGER :: fail (4)
      INTEGER :: aux1
      
! !     determine number of beads in frozen bead walls to be added to system

!       numwlbd = npxfwx*npxfwy*npxfwz + npyfwx*npyfwy*npyfwz + npzfwx*npzfwy*npzfwz

      numwlbd = 0 ! for simplicity ONLY, to be changed later
      
!     allocate arrays for bonded particle positions, species, velocity and force

      fail = 0
!      ALLOCATE (mlxxx (nsystcell-nusystcell), mlyyy (nsystcell-nusystcell), mlzzz (nsystcell-nusystcell), STAT=fail(1))
      ALLOCATE (mlspe (nsystcell-nusystcell), STAT=fail(2))
!      ALLOCATE (mlvelx (nsystcell-nusystcell), mlvely (nsystcell-nusystcell), mlvelz (nsystcell-nusystcell), STAT=fail(3))
!      ALLOCATE (mlfrcx (nsystcell-nusystcell), mlfrcy (nsystcell-nusystcell), mlfrcz (nsystcell-nusystcell), STAT=fail(4))
      IF (ANY (fail/=0)) THEN
         WRITE (*,*) "error: allocation failure in subroutine read_config"
         STOP
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

      nbeads = 0   !keep for the moment, rename later? 
      nfbeads = 0  !keep for the moment, rename later? 
      numpart = 0
      molbead = .false.
      numsafe = .true. !needed? Not really, remove at the end

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
        
        WRITE (*,*) global, species ! added by SC - for testing
                
!    skip position of particle (not needed for the check)

        READ (nread, '(a80)') record ! add a STAT to check?

!     if included, skip velocity and force of particle (not needed for the check)

        IF (levcfg>0) READ (nread, '(a80)') record

        IF (levcfg>1) READ (nread, '(a80)') record
        
!     if particle is unbonded, add to each copy of unit cell (adjusting for frozen bead
!     walls if used)

        IF (global<=nusystcell) THEN

          DO ifz = 0, nfoldz-1
            DO ify = 0, nfoldy-1
              DO ifx = 0, nfoldx-1

                gb = global + (ifx + nfoldx * (ify + nfoldy * ifz)) * nusystcell + numwlbd

                linside = .true. ! temporary solution, to be removed later
                
                IF (linside) THEN
                  nbeads = nbeads + 1
                  IF (nbeads>maxdim) THEN
                    numsafe = .false.                    
                  ELSE
                    lab (nbeads) = gb
                    ltp (nbeads) = species
                    ltm (nbeads) = 0

                    IF (lfrzn (species)>0) nfbeads = nfbeads + 1
                  END IF
                END IF

              END DO
            END DO
          END DO

        ELSE IF (lbond .AND. global<=nsystcell) THEN

!     if particle is bonded, add species to arrays so these can be added later !needed to keep like this?

          mlspe (global-nusystcell) = species

       END IF

    END DO ! end of the loop over the CONFIG file

!     check that the total number of beads is not larger than nsyst?      
!      IF (.NOT. numsafe) then ...
      
!     check total number of beads corresponds with FIELD file

      IF (numpart/=nsystcell) THEN
         aux1 = numpart - nsystcell
         IF (aux1>0) THEN
            WRITE (*,"(/,1x,'error: discrepency in total number of beads in CONFIG  - ',i10,' too many')") aux1
         ELSE
            WRITE (*,"(/,1x,'error: discrepency in total number of beads in CONFIG - ',i10,' too few')") ABS(aux1)
         END IF
      END IF

!     add bonded particles to each copy of unit cell

        imol = 0
        imoltyp = 0
        ntop = 0
        DO i = nusystcell+1, nsystcell

          DO WHILE (i>molstart (imol+1))
            imol = imol + 1
          END DO

          DO WHILE (imol>ntop)
            imoltyp = imoltyp + 1
            ntop = ntop + nmol (imoltyp)
          END DO

          species = mlspe (i-nusystcell)

          DO ifz = 0, nfoldz-1
            DO ify = 0, nfoldy-1
              DO ifx = 0, nfoldx-1

                gb = nusyst + (i - molstart (imol)) + nfold * (molstart (imol) - nusystcell) + &
                    &(ifx + nfoldx * (ify + nfoldy * ifz)) * (molstart(imol+1) - molstart(imol))

                linside = .true. ! added to minimize the changes
                
                IF (linside) THEN
                  nbeads = nbeads + 1
                  IF (nbeads>maxdim) THEN
                    numsafe = .false.
                  ELSE
                    lab (nbeads) = gb
                    ltp (nbeads) = species
                    ltm (nbeads) = imoltyp

                    molbead (gb-nusyst) = .true.
                    IF (lfrzn (species)>0) nfbeads = nfbeads + 1
                  END IF
                END IF

              END DO
            END DO
          END DO

        END DO

!     check that the total number of beads is not larger than nsyst?      
!      IF (.NOT. numsafe) then ...
        
! FINO A QUI. 

! !     insert frozen walls as simple cubic lattices

!       IF (srftype==2) THEN

!         IF (srfx>0) THEN
!           disx = 2.0_dp * frzwxwid / REAL (npxfwx-1, KIND=dp)
!           disy = dimy / REAL (npxfwy, KIND=dp)
!           disz = dimz / REAL (npxfwz, KIND=dp)
!           xdispl = dimx - frzwxwid
!           ydispl = 0.5_dp * disy
!           zdispl = 0.5_dp * disz

!           shfz = zdispl
!           DO k = 1, npxfwz
!             shfy = ydispl
!             DO j = 1, npxfwy
!               shfx = xdispl
!               DO i = 1, npxfwx
!                 isx = i - 1
!                 isy = j - 1
!                 isz = k - 1
!                 gb = 1 + isx + npxfwx * (isy + npxfwy * isz)
!                 inod = INT (shfx/sidex) + npx * (INT (shfy/sidey) + npy * INT (shfz/sidez))
!                 IF (idnode==inod) THEN
!                   nbeads = nbeads + 1
!                   nfbeads = nfbeads + 1
!                   IF (nbeads>maxdim) THEN
!                     numsafe = .false.
!                   ELSE
!                     xxx (nbeads) = shfx - delx
!                     yyy (nbeads) = shfy - dely
!                     zzz (nbeads) = shfz - delz
!                     lab (nbeads) = gb
!                     ltp (nbeads) = frzwspe
!                     ltm (nbeads) = 0
!                     vxx (nbeads) = 0.0_dp
!                     vyy (nbeads) = 0.0_dp
!                     vzz (nbeads) = 0.0_dp
!                     fxx (nbeads) = 0.0_dp
!                     fyy (nbeads) = 0.0_dp
!                     fzz (nbeads) = 0.0_dp
!                   END IF
!                 END IF
!                 shfx = shfx + disx
!                 IF (shfx>=dimx) shfx = shfx - dimx
!               END DO
!               shfy = shfy + disy
!             END DO
!             shfz = shfz + disz
!           END DO
!         END IF

!         IF (srfy>0) THEN
!           disx = (dimx - 2.0_dp * frzwxwid) / REAL (npyfwx, KIND=dp)
!           disy = 2.0_dp * frzwywid / REAL (npyfwy-1, KIND=dp)
!           disz = dimz / REAL (npyfwz, KIND=dp)
!           xdispl = frzwxwid + 0.5_dp * disx
!           ydispl = dimy - frzwywid 
!           zdispl = 0.5_dp * disz

!           shfz = zdispl
!           DO k = 1, npyfwz
!             shfy = ydispl
!             DO j = 1, npyfwy
!               shfx = xdispl
!               DO i = 1, npyfwx
!                 isx = i - 1
!                 isy = j - 1
!                 isz = k - 1
!                 gb = npxfwx*npxfwy*npxfwz + 1 + isx + npyfwx * (isy + npyfwy * isz)
!                 inod = INT (shfx/sidex) + npx * (INT (shfy/sidey) + npy * INT (shfz/sidez))
!                 IF (idnode==inod) THEN
!                   nbeads = nbeads + 1
!                   nfbeads = nfbeads + 1
!                   IF (nbeads>maxdim) THEN
!                     numsafe = .false.
!                   ELSE
!                     xxx (nbeads) = shfx - delx
!                     yyy (nbeads) = shfy - dely
!                     zzz (nbeads) = shfz - delz
!                     lab (nbeads) = gb
!                     ltp (nbeads) = frzwspe
!                     ltm (nbeads) = 0
!                     vxx (nbeads) = 0.0_dp
!                     vyy (nbeads) = 0.0_dp
!                     vzz (nbeads) = 0.0_dp
!                     fxx (nbeads) = 0.0_dp
!                     fyy (nbeads) = 0.0_dp
!                     fzz (nbeads) = 0.0_dp
!                   END IF
!                 END IF
!                 shfx = shfx + disx
!               END DO
!               shfy = shfy + disy
!               IF (shfy>=dimy) shfy = shfy - dimy
!             END DO
!             shfz = shfz + disz
!           END DO
!         END IF

!         IF (srfz>0) THEN
!           disx = (dimx - 2.0_dp * frzwxwid) / REAL (npzfwx, KIND=dp)
!           disy = (dimy - 2.0_dp * frzwywid) / REAL (npzfwy, KIND=dp)
!           disz = 2.0_dp * frzwzwid / REAL (npzfwz-1, KIND=dp)
!           xdispl = frzwxwid + 0.5_dp * disx
!           ydispl = frzwywid + 0.5_dp * disy
!           zdispl = dimz - frzwzwid

!           shfz = zdispl
!           DO k = 1, npzfwz
!             shfy = ydispl
!             DO j = 1, npzfwy
!               shfx = xdispl
!               DO i = 1, npzfwx
!                 isx = i - 1
!                 isy = j - 1
!                 isz = k - 1
!                 gb = npxfwx*npxfwy*npxfwz + npyfwx*npyfwy*npyfwz + 1 + isx + npzfwx * (isy + npzfwy * isz)
!                 inod = INT (shfx/sidex) + npx * (INT (shfy/sidey) + npy * INT (shfz/sidez))
!                 IF (idnode==inod) THEN
!                   nbeads = nbeads + 1
!                   nfbeads = nfbeads + 1
!                   IF (nbeads>maxdim) THEN
!                     numsafe = .false.
!                   ELSE
!                     xxx (nbeads) = shfx - delx
!                     yyy (nbeads) = shfy - dely
!                     zzz (nbeads) = shfz - delz
!                     lab (nbeads) = gb
!                     ltp (nbeads) = frzwspe
!                     ltm (nbeads) = 0
!                     vxx (nbeads) = 0.0_dp
!                     vyy (nbeads) = 0.0_dp
!                     vzz (nbeads) = 0.0_dp
!                     fxx (nbeads) = 0.0_dp
!                     fyy (nbeads) = 0.0_dp
!                     fzz (nbeads) = 0.0_dp
!                   END IF
!                 END IF
!                 shfx = shfx + disx
!               END DO
!               shfy = shfy + disy
!             END DO
!             shfz = shfz + disz
!             IF (shfz>=dimz) shfz = shfz - dimz
!           END DO
!         END IF

! !        CALL global_sca_and (numsafe, 0, idnode)
!         IF (.NOT. numsafe) CALL error (idnode, 53, 0)

!      END IF ! end of the case srftype = 2

!     check total number of beads ! is this check necessary? Or not?

      IF (nbeads/=nsyst) THEN
         aux1 = nbeads - nsyst
         IF (aux1>0) THEN
            WRITE (*,"(/,1x,'error: discrepency in total number of beads in CONFIG  - ',i10,' too many')") aux1
         ELSE
            WRITE (*,"(/,1x,'error: discrepency in total number of beads in CONFIG - ',i10,' too few')") ABS(aux1)
         END IF
      END IF      
      
! !     re-order local beads to give frozen beads first !Needed??? 

!       CALL sort_beads  
 
! !     assign particle/molecule names and masses ! Needed?

!       DO i = 1, nbeads

!         weight (i) = amass (ltp (i))
!         atmnam (i) = namspe (ltp (i))
!         molnam (i) = nammol (ltm (i))

!       END DO

! !     assign particle velocities to give required system temperature

!       IF (levcfg==0) CALL initialvelocity

      CLOSE (nread)

      fail = 0
!      DEALLOCATE (mlxxx, mlyyy, mlzzz, mlspe, STAT=fail(1))
!      DEALLOCATE (mlvelx, mlvely, mlvelz, STAT=fail(2))
!      DEALLOCATE (mlfrcx, mlfrcy, mlfrcz, STAT=fail(3))
!      DEALLOCATE (molstart) !move from here to end of code?
      DEALLOCATE (mlspe, STAT=fail(1))
      IF (ANY (fail/=0)) THEN
        WRITE (*,*) "error: deallocation failure in subruotine read_config"
        STOP
     END IF
     ! added by SC - for testing
     WRITE (*,*) "within READ_CONFIG"
     DO i = 1, nsyst
        WRITE (*,*) lab (i), ltp (i), ltm (i)
     END DO
     WRITE (*,*) "within READ_CONFIG, Re-ordered"
     DO j = 1, nsyst
        DO i = 1, nsyst 
           IF (lab (i) == j) THEN
              WRITE (*,*) lab (i), ltp (i), ltm (i)
              EXIT
           END IF
        END DO
     END DO
     ! end of added
     
      END SUBROUTINE read_config

      SUBROUTINE define_molstart
        IMPLICIT none
        INTEGER :: fail (4), j, imol, ntop
        fail = 0
        ! set up array for global bead numbers of molecules (for unit cell)

        ALLOCATE (molstart (nummolcell+1), STAT=fail(4))
        IF (ANY (fail/=0)) THEN
           WRITE (*,*) "error: allocation failure"
           STOP         
        END IF
        
        molstart (1) = nusystcell
        
        IF (nmoldef>0) THEN
           
           imol = 0
           ntop = 0
           
           DO j=1, nummolcell
              
              DO WHILE (j>ntop)
                 imol = imol + 1
                 ntop = ntop + nmol (imol)
              END DO
              
              molstart (j+1) = molstart (j) + nbdmol (imol)
              
           END DO
           
           molstart (nummolcell+1) = nsystcell

        END IF
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

! MODULE variables

!       USE constants
!       IMPLICIT none

! !     node properties
!       INTEGER :: idnode, nodes
! !     filename for restart files
!       CHARACTER(LEN=12) :: exportname

! !     system parameters
! !     switches for temperature rescaling, CONFIG file, origin of CONFIG file, CORREL file, HISTORY file, bonds, angles, dihedrals
!       LOGICAL :: ltemp, lconfig, lconfzero, lcorr, ltraj, lbond, langle, ldihed
! !     switch to determine whether to calculate bonds globally
!       LOGICAL :: lgbnd
! !     switch to determine whether variable forces are required
!       LOGICAL :: lvarfc
! !     isotropy of system pressure
!       LOGICAL :: lisoprs
! !     switch to determine whether to ignore global bead numbers in CONFIG file
!       LOGICAL :: ligindex
! !     switch to determine if many-body DPD interactions are included
!       LOGICAL :: lmb
! !     number of duplicates to be included from CONFIG file
!       INTEGER :: nfoldx, nfoldy, nfoldz, nfold
! !     CONFIG file parameters
!       INTEGER :: levcfg, imcon
! !     printout selection for OUTPUT file
!       INTEGER :: outsel
! !     HISTORY file parameter for trajectory output
!       INTEGER :: keytrj
! !     energy unit
!       INTEGER :: engunit
! !     random number generator seed
!       INTEGER :: rndseed

! !     maximum values for numbers of beads, bonds, angles, potentials, etc.
!       INTEGER :: maxdim, maxpair, maxbfbd, maxbuf, mxmolsize, mxbonds, mxangles, mxdiheds, mxprm
! !     density variation for non-even system densities
!       REAL(KIND=dp) :: dvar

! !     name of dl_meso_dpd calculation
!       CHARACTER(LEN=80) :: text

! !     total number of time steps
!       INTEGER :: nrun
! !     frequency of outputs to OUTPUT, CORREL, HISTORY, export
!       INTEGER :: nsbpo, iscorr, ntraj, straj, ndump
! !     temperature scaling interval
!       INTEGER :: nsbts
! !     number of equilibration time steps
!       INTEGER :: nseql
! !     calculation restart parameter
!       INTEGER :: kres
! !     number of species and potential types
!       INTEGER :: nspe, npot
! !     number of molecule types
!       INTEGER :: nmoldef
! !     numbers of defined bond, angle, dihedral types
!       INTEGER :: nbonddef, nangdef, ndhddef
! !     size of statistical rolling average stack
!       INTEGER :: nstk
! !     total number of beads (overall, unbonded, frozen) in system
!       INTEGER :: nsyst, nusyst, nfsyst
! !     number of beads (overall, unbonded, frozen) in unit cell
!       INTEGER :: nsystcell, nusystcell, nfsystcell
! !     total number of molecules in system
!       INTEGER :: nummol
! !     total number of molecules in unit cell
!       INTEGER :: nummolcell
! !     total number of bonds, angles, dihedrals in system
!       INTEGER :: numbond, numang, numdhd
! !     total number of bonds, angles, dihedrals in unit cell
!       INTEGER :: numbondcell, numangcell, numdhdcell
! !     current time step number
!       INTEGER :: nstep

! !     force calculation time accumulator
!       REAL(KIND=dp) :: timfrc
! !     step time accumulator
!       REAL(KIND=dp) :: timstp
! !     specified system temperature
!       REAL(KIND=dp) :: temp
! !     size of time step
!       REAL(KIND=dp) :: tstep, rtstep
! !     boundary halo sizes (user-defined and derived for each dimension)
!       REAL(KIND=dp) :: rhalo, rhalox, rhaloy, rhaloz
! !     maximum cutoff radius, square and square reciprocal
!       REAL(KIND=dp) :: rcut, rct2, rrct2
! !     thermostat cutoff radius, reciprocal and square
!       REAL(KIND=dp) :: rtcut, rrtcut, rtct2
! !     many-body interaction cutoff radius, square and reciprocal
!       REAL(KIND=dp) :: rmbcut, rmbct2, rrmbcut
! !     short-range electrostatic interaction length, square of interaction length
!       REAL(KIND=dp) :: relec, rel2
! !     surface interaction length and square
!       REAL(KIND=dp) :: srfzcut, srfzct2
! !     maximum time available for calculation
!       REAL(KIND=dp) :: timjob
! !     time required to close calculation
!       REAL(KIND=dp) :: tclose

! !     total system volume and dimensions
!       REAL(KIND=dp) :: volm, dimx, dimy, dimz
! !     unit cell dimensions
!       REAL(KIND=dp) :: dimxcell, dimycell, dimzcell

! !     domain decomposition
! !     numbers of domain cells
!       INTEGER :: npx, npy, npz
! !     list of neighbouring processes
!       INTEGER :: map(6)
! !     position of domain cell
!       INTEGER :: idx, idy, idz
! !     position of domain cell within system volume
!       REAL(KIND=dp) :: delx, dely, delz
! !     size of domain cell
!       REAL(KIND=dp) :: sidex, sidey, sidez
! !     number of beads, frozen beads in domain cell
!       INTEGER :: nbeads, nfbeads

! !     link cells
! !     number of link cells within domain cell
!       INTEGER :: nlx, nly, nlz
! !     number of link cells in both domain cell and boundary halo
!       INTEGER :: nlx2, nly2, nlz2
! !     maximum number of particles per link cell
!       INTEGER :: mxpcell
! !     size of link cell
!       REAL(KIND=dp) :: wdthx, wdthy, wdthz
! !     link cell population, link cell number, list of cell ids, list of cell neighbours (local for each processor)
!       INTEGER, ALLOCATABLE :: lct (:), link (:), lcell (:), lcell_neighbour (:)
! !     thermostat switch for link cell neighbours
!       INTEGER, ALLOCATABLE :: lcell_therm (:)

! !     number of link cells for electrostatic interactions within domain cell
!       INTEGER :: nlewx, nlewy, nlewz
! !     number of link cells for electrostatic interactions in both domain cell and boundary halo
!       INTEGER :: nlewx2, nlewy2, nlewz2
! !     size of electrostatic interaction link cell
!       REAL(KIND=dp) :: wdthewx, wdthewy, wdthewz
! !     link cell population, link cell number, list of cell ids, list of cell neighbours for electostatic interactions
!       INTEGER, ALLOCATABLE :: lctew (:), linkew (:), lcellew (:), lcellew_neighbour (:)

! !     number of link cells for many-body interactions within domain cell
!       INTEGER :: nlmbx, nlmby, nlmbz
! !     number of link cells for many-body interactions in both domain cell and boundary halo
!       INTEGER :: nlmbx2, nlmby2, nlmbz2
! !     size of many-body interaction link cell
!       REAL(KIND=dp) :: wdthmbx, wdthmby, wdthmbz
! !     link cell population, link cell number, list of cell ids, list of cell neighbours for many-body calculations
!       INTEGER, ALLOCATABLE :: lctmb (:), linkmb (:), lcellmb (:), lcellmb_neighbour (:)

! !     species names
!       CHARACTER(LEN=8), ALLOCATABLE, SAVE :: namspe (:)

! !     interaction type
!       INTEGER, ALLOCATABLE :: ktype (:)
! !     species masses, charges and interaction parameters
!       REAL(KIND=dp), ALLOCATABLE :: amass (:), chge (:), vvv (:,:)
! !     frozen bead indicator
!       INTEGER, ALLOCATABLE :: lfrzn (:)
! !     long-range potential energy and virial corrections (only for lennard-jones potentials)
!       REAL(KIND=dp) :: clr (2)
! !     corrections for electrostatic forces, potential energy, stresses and virial between frozen beads
!       REAL(KIND=dp), ALLOCATABLE, TARGET :: cfxfyfz (:,:)
!       REAL(KIND=dp), POINTER :: fcfx (:), fcfy (:), fcfz (:)
!       REAL(KIND=dp) :: strcfz (9), vrlcfz (3)
!       REAL(KIND=dp) :: potcfz

! !     integrator type
!       INTEGER :: itype
! !     dissipative and random parameters (dpd, lowe-andersen, peters, stoyanov/groot)
!       REAL(KIND=dp), ALLOCATABLE :: gamma (:), sigma (:)
! !     stoyanov/groot thermostat coupling parameter
!       REAL(KIND=dp) :: alphasg

! !     pair lists for velocity correction-based thermostats
!       REAL(KIND=dp), ALLOCATABLE, SAVE :: pldxyz (:)
!       INTEGER, ALLOCATABLE, SAVE :: plparti (:), plpartj (:), plproci (:), plprocj (:), plbound (:), plintij (:)
!       INTEGER :: npair

! !     barostat type
!       INTEGER :: btype
! !     barostat target pressure and parameters
!       REAL(KIND=dp) :: prszero, abaro, bbaro, cbaro, dbaro

! !     barostat variables
! !     piston velocities
!       REAL(KIND=dp) :: upx, upy, upz, up1x, up1y, up1z
! !     piston mass and force
!       REAL(KIND=dp) :: psmass, rpsmass, fpx, fpy, fpz
! !     instantaneous virial
!       REAL(KIND=dp) :: ivrl (3)
! !     langevin random force parameter
!       REAL(KIND=dp) :: sigmalang

! !     electrostatic model
!       INTEGER :: etype
! !     electrostatic coupling constant and bjerrum length
!       REAL(KIND=dp) :: gammaelec, bjerelec
! !     total charge of system
!       REAL(KIND=dp) :: qchg

! !     electrostatic variables
! !     ewald sum parameters (including reciprocal)
!       REAL(KIND=dp) :: alphaew, ralphaew
! !     reciprocal vector dimensions
!       INTEGER :: kmax1, kmax2, kmax3
! !     ewald self-interaction correction
!       REAL(KIND=dp) :: engsic
! !     charged system correction
!       REAL(KIND=dp) :: qfixv
! !     slater-type smearing parameter
!       REAL(KIND=dp) :: betaew
! !     lists of reciprocal space vectors
!       REAL(KIND=dp), ALLOCATABLE :: rkxyz (:,:)
!       INTEGER, ALLOCATABLE :: kxyz (:,:)
!       INTEGER :: nlistew

! !     bond parameters
!       REAL(KIND=dp), ALLOCATABLE :: aabond (:), bbbond (:), ccbond (:), ddbond (:)
! !     angle parameters
!       REAL(KIND=dp), ALLOCATABLE :: aaang (:), bbang (:), ccang (:), ddang (:)
! !     dihedral parameters
!       REAL(KIND=dp), ALLOCATABLE :: aadhd (:), bbdhd (:), ccdhd (:), dddhd (:)
! !     bond, angle and dihedral types
!       INTEGER, ALLOCATABLE :: bdtype (:), angtype (:), dhdtype (:)
! !     molecule isomers
!       LOGICAL, ALLOCATABLE :: moliso (:)

! !     populations of species (single particles and within molecules)
!       INTEGER, ALLOCATABLE :: nspec (:), nspecmol (:)
! !     molecule populations and number of beads per molecule
!       INTEGER, ALLOCATABLE :: nmol (:), nbdmol (:)

! !     molecule names (0:mxmoldef)
!       CHARACTER(LEN=8), ALLOCATABLE, SAVE :: nammol (:)

! !     molecule insertion (including bonds, angles, dihedrals)
!       INTEGER, ALLOCATABLE, SAVE :: mlstrtspe (:,:), nbond (:), nangle (:), ndihed (:)
!       INTEGER, ALLOCATABLE, SAVE :: bdinp1 (:,:), bdinp2 (:,:), bdinp3 (:,:)
!       INTEGER, ALLOCATABLE, SAVE :: anginp1 (:,:), anginp2 (:,:), anginp3 (:,:), anginp4 (:,:)
!       INTEGER, ALLOCATABLE, SAVE :: dhdinp1 (:,:), dhdinp2 (:,:), dhdinp3 (:,:), dhdinp4 (:,:), dhdinp5 (:,:)
!       INTEGER, ALLOCATABLE, SAVE :: molstart (:)
!       REAL(KIND=dp), ALLOCATABLE, SAVE :: cbsize (:), mlstrtxxx (:,:), mlstrtyyy (:,:), mlstrtzzz (:,:)

! !     bond, angle, dihedral look-up tables
!       INTEGER, ALLOCATABLE, SAVE :: bndtbl (:,:), angtbl (:,:), dhdtbl (:,:)
!       INTEGER :: nbonds, nangles, ndiheds

! !     global/local bead number list
!       INTEGER, ALLOCATABLE, SAVE :: lblclst (:,:)
!       INTEGER :: nlist

! !     localized densities for many-body dpd
!       REAL(KIND=dp), ALLOCATABLE :: rhomb (:, :)

! !     wall/surface parameters
! !     surface type
!       INTEGER :: srftype
! !     surface dimensions
!       INTEGER :: srfx, srfy, srfz
! !     switches to determine existence of surface in current node
!       LOGICAL :: srflgc(6)
! !     wall repulsion parameters
!       REAL(KIND=dp), ALLOCATABLE :: aasrf (:)
! !     frozen particle wall species, numbers of particles in each direction per wall
!       INTEGER :: frzwspe, npxfwx, npxfwy, npxfwz, npyfwx, npyfwy, npyfwz, npzfwx, npzfwy, npzfwz
! !     frozen particle wall density, wall widths
!       REAL(KIND=dp) :: frzwdens, frzwxwid, frzwywid, frzwzwid
! !     shearing velocity and distance
!       REAL(KIND=dp) :: shrvx, shrvy, shrvz, shrdx, shrdy, shrdz

! !     external body accelerations
!       REAL(KIND=dp) :: bdfrcx, bdfrcy, bdfrcz
! !     electric field
!       REAL(KIND=dp) :: elecx, elecy, elecz

! !     particle properties
! !     forces
!       REAL(KIND=dp), ALLOCATABLE, TARGET :: fxfyfz (:,:)
!       REAL(KIND=dp), POINTER :: fxx (:), fyy (:), fzz (:)
! !     variable forces
!       REAL(KIND=dp), ALLOCATABLE, TARGET :: vfxfyfz (:,:)
!       REAL(KIND=dp), POINTER :: fvx (:), fvy (:), fvz (:)
! !     velocities
!       REAL(KIND=dp), ALLOCATABLE, TARGET :: vxvyvz (:,:)
!       REAL(KIND=dp), POINTER :: vxx (:), vyy (:), vzz (:)
! !     positions
!       REAL(KIND=dp), ALLOCATABLE, TARGET :: xxyyzz (:,:)
!       REAL(KIND=dp), POINTER :: xxx (:), yyy (:), zzz (:)
! !     global number, particle type, molecule type, processor identifier, local particle number for domain
!       INTEGER, ALLOCATABLE, SAVE :: lab (:), ltp (:), ltm (:), lmp (:), loc (:)
! !     particle names, molecule names, masses
!       CHARACTER(LEN=8), ALLOCATABLE, SAVE :: atmnam (:), molnam (:)
!       REAL(KIND=dp), ALLOCATABLE, SAVE :: weight (:)


! !     statistical properties (pe = potential energy, vir = virial, tke = kinetic energy, te = total energy, 
! !                             be = bond energy, ae = angle energy, de = dihedral energy, ee = electrostatic energy,
! !                             bdl/bdlng = bond length, ang/bdang = bond angle, dhd/bddhd = bond dihedral,
! !                             prs = pressure, vlm = volume, ttp = temperature, tpx/tpy/tpz = temperature in x/y/z dimension)
! !     accumulators
!       REAL(KIND=dp) :: pe, vir, be, ae, de, ee, bdlng, bdlmin, bdlmax, bdang, bddhd
!       REAL(KIND=dp) :: tke (3), stress (9)
! !     mean properties at current timestep
!       REAL(KIND=dp) :: stppe, stpvir, stptke, stpte, stpprs, stpvlm, stpttp, stptpx, stptpy, stptpz
!       REAL(KIND=dp) :: stpbe, stpae, stpde, stpee
! !     mean, maximum and minimum bond lengths, angles and dihedrals
!       REAL(KIND=dp) :: stpbdl, stpbdmx, stpbdmn, stpang, stpdhd
! !     rolling averages over stack length
!       REAL(KIND=dp) :: ravpe, ravvir, ravtke, ravte, ravprs, ravvlm, ravttp, ravtpx, ravtpy, ravtpz, ravbe, ravae, ravde, ravee
! !     averages over all time steps
!       REAL(KIND=dp) :: avepe, avevir, avetke, avete, aveprs, avevlm, avettp, avetpx, avetpy, avetpz, avebe, aveae, avede, aveee
! !     property fluctuations
!       REAL(KIND=dp) :: flcpe, flcvir, flctke, flcte, flcprs, flcvlm, flcttp, flctpx, flctpy, flctpz, flcbe, flcae, flcde, flcee
! !     data stacks
!       REAL(KIND=dp), ALLOCATABLE :: stkpe (:), stktke (:), stktkex (:), stktkey (:), stktkez (:)
!       REAL(KIND=dp), ALLOCATABLE :: stkbe (:), stkae (:), stkde (:), stkee (:), stkvir (:), stkvlm (:)
!       INTEGER :: nav
! !     accumulated values
!       REAL(KIND=dp) :: zumpe, zumtke, zumtkex, zumtkey, zumtkez, zumbe, zumae, zumde, zumee, zumvir, zumvlm

! !     MJ: Preallocated communication buffers
! !         Allocated in config_module.f90          
!       REAL(KIND=dp), ALLOCATABLE, SAVE, TARGET :: commsinbuf(:,:), commsoutbuf(:,:)

! END MODULE
