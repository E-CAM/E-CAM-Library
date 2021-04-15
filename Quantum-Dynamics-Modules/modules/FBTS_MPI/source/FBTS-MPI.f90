module variables
implicit none

integer :: num_states
integer :: num_bath
integer :: num_traj
integer :: num_timestep
integer :: timestep_block 
integer :: init_state
integer :: idum

double precision :: timestep
double precision :: omega_c
double precision :: temp
double precision :: reorg_E

character(len=30) :: filename_H_subsys 

integer :: IB  
integer :: time 
integer :: seed

 
double precision, allocatable :: sb_coupling(:) 
double precision, allocatable :: ome(:)

double precision, allocatable :: bath_R(:,:)
double precision, allocatable :: bath_P(:,:)
double precision, allocatable :: subsys_q_F(:)
double precision, allocatable :: subsys_q_B(:)
double precision, allocatable :: subsys_p_F(:)
double precision, allocatable :: subsys_p_B(:)

double precision, allocatable :: site_pop(:,:)        ! populations
double precision, allocatable :: real_site_coh(:,:,:) ! coherences real component 
double precision, allocatable :: im_site_coh(:,:,:)   ! coherences imaginary component 

double precision, allocatable :: collect_site_pop(:,:)
double precision, allocatable :: collect_im_site_coh(:,:,:)
double precision, allocatable :: collect_real_site_coh(:,:,:)

double precision, allocatable :: force(:,:)
double precision, allocatable :: H_subsys_0(:,:)
double precision, allocatable :: H_subsys(:,:)

double complex, allocatable :: subsys_weight_0(:)

double precision, allocatable :: coh_len(:)
double precision, allocatable :: MSD(:)

! variables for MPI parallelization 
integer :: ierr
integer :: process_total
integer :: process_rank
integer, parameter :: master = 0 
double precision :: starttime, endtime

end module variables

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

PROGRAM FBTS_MPI
!=======================================================================================!
! FBTS dynamics treatment for N quantum DOF using MPI parallelization                   ! 
!                                                                                       !
! CANONICAL EQUILIBRIUM INITIAL STATE for all Classical-like variables                  !                    
!                                                                                       !
! SINGLE EXCITATION INITIAL STATE for Quantum Subsystem                                 ! 
!                                                                                       ! 
!=======================================================================================!

use luxury
use variables
use mpi
implicit none 

integer :: remainder 
integer :: start_traj
integer :: end_traj
integer :: I,J

call MPI_INIT(ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD,process_total,ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD,process_rank,ierr)


call read_input_parameters

call MPI_BCAST(timestep, 1, MPI_DOUBLE_PRECISION, master, MPI_COMM_WORLD, ierr) 
call MPI_BCAST(temp, 1, MPI_DOUBLE_PRECISION, master, MPI_COMM_WORLD, ierr) 
call MPI_BCAST(omega_c, 1, MPI_DOUBLE_PRECISION, master, MPI_COMM_WORLD, ierr) 
call MPI_BCAST(reorg_E, 1, MPI_DOUBLE_PRECISION, master, MPI_COMM_WORLD, ierr) 
call MPI_BCAST(num_states, 1, MPI_INTEGER, master, MPI_COMM_WORLD, ierr)
call MPI_BCAST(num_bath, 1, MPI_INTEGER, master, MPI_COMM_WORLD, ierr)
call MPI_BCAST(num_traj, 1, MPI_INTEGER, master, MPI_COMM_WORLD, ierr)
call MPI_BCAST(num_timestep, 1, MPI_INTEGER, master, MPI_COMM_WORLD, ierr)
call MPI_BCAST(timestep_block, 1, MPI_INTEGER, master, MPI_COMM_WORLD, ierr)
call MPI_BCAST(init_state, 1, MPI_INTEGER, master, MPI_COMM_WORLD, ierr)
call MPI_BCAST(idum, 1, MPI_INTEGER, master, MPI_COMM_WORLD, ierr)


time = num_timestep/timestep_block + 1


call allocate_arrays
call initialize_subsys 

call MPI_BCAST(H_subsys_0,num_states*num_states,MPI_DOUBLE_PRECISION,master,             &
               MPI_COMM_WORLD,ierr) 

call initialize_bath

! initialize random number generator 
seed = idum + process_rank 
call RLUXGO(3,seed,0,0)  

starttime = MPI_Wtime()

! determine number of trajectories for each process
remainder = MOD(num_traj,process_total)
end_traj = num_traj / process_total
if (process_rank.EQ.process_total - 1) then 
    end_traj = end_traj + remainder
end if 


do start_traj=1,end_traj ! begin Monte Carlo sampling loop  

    ! prepare bath initial conditions for each Monte Carlo realization 
    call initial_cond_bath

    ! prepare quantum subsystem initial conditions for each Monte Carlo realization 
    call initial_cond_subsys
    
    ! propagate system through time  
    call FBTS_dynamics

end do ! Monte Carlo loop ends here 

! bring information together 
call MPI_Barrier(MPI_COMM_WORLD,ierr)

call MPI_ALLREDUCE(site_pop,collect_site_pop,time*num_states,MPI_DOUBLE_PRECISION,       &
                   MPI_SUM,MPI_COMM_WORLD,ierr)

call MPI_ALLREDUCE(im_site_coh,collect_im_site_coh,time*num_states*num_states,           &
                   MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)

call MPI_ALLREDUCE(real_site_coh,collect_real_site_coh,time*num_states*num_states,       &
                   MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)

! take the average 
collect_site_pop(:,:) = collect_site_pop(:,:)/REAL(num_traj)
collect_im_site_coh(:,:,:) = collect_im_site_coh(:,:,:)/REAL(num_traj)
collect_real_site_coh(:,:,:) = collect_real_site_coh(:,:,:)/REAL(num_traj)

! output 
if(process_rank.EQ.master) then  
    call coherence_len
    call write_output
end if  

endtime = MPI_Wtime()

open(30, file="MPI_time.out")
write(30,*) endtime - starttime

call MPI_FINALIZE(ierr)

end program FBTS_MPI

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine read_input_parameters
use variables 
implicit none 

if(process_rank.EQ.master)then 
OPEN(5,file='Input_File.dat',form='formatted',status='old',action='read')

READ(5,*) num_states
READ(5,*) num_bath
READ(5,*) num_traj
READ(5,*) num_timestep
READ(5,*) timestep
READ(5,*) timestep_block
READ(5,*) init_state
READ(5,*) filename_H_subsys
READ(5,*) omega_c
READ(5,*) temp
READ(5,*) reorg_E
READ(5,*) idum

CLOSE(5)
end if

end subroutine read_input_parameters

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine allocate_arrays
use variables
implicit none

  ALLOCATE(sb_coupling(num_bath));sb_coupling(:) = 0.0d0
  ALLOCATE(ome(num_bath));ome(:) = 0.0d0
  
  ALLOCATE(force(num_bath,num_states));force(:,:)=0.d0 
  
  ALLOCATE(site_pop(time,num_states)); site_pop(:,:)=0.D0                   
  ALLOCATE(real_site_coh(time,num_states,num_states));real_site_coh(:,:,:)=0.d0         
  ALLOCATE(im_site_coh(time,num_states,num_states));im_site_coh(:,:,:)=0.d0         
  
  ALLOCATE(H_subsys_0(num_states,num_states));H_subsys_0(:,:)=0.0d0
  ALLOCATE(H_subsys(num_states,num_states));H_subsys(:,:)=0.0d0
  
  ALLOCATE(subsys_q_F(num_states));subsys_q_F(:)=0.0d0
  ALLOCATE(subsys_q_B(num_states));subsys_q_B(:)=0.0d0
  ALLOCATE(subsys_p_F(num_states));subsys_p_F(:)=0.0d0
  ALLOCATE(subsys_p_B(num_states));subsys_p_B(:)=0.0d0
  ALLOCATE(bath_R(num_bath,num_states));bath_R(:,:)=0.0d0
  ALLOCATE(bath_P(num_bath,num_states));bath_P(:,:)=0.d0
  
  ALLOCATE(subsys_weight_0(num_states*num_states));subsys_weight_0(:)=0.0d0 
 
  ALLOCATE(collect_site_pop(time,num_states));collect_site_pop(:,:)=0.d0 
  ALLOCATE(collect_real_site_coh(time,num_states,num_states));collect_real_site_coh(:,:,:)=0.d0         
  ALLOCATE(collect_im_site_coh(time,num_states,num_states));collect_im_site_coh(:,:,:)=0.d0         

  ALLOCATE(coh_len(time));coh_len(:)=0.d0 
  ALLOCATE(MSD(time));MSD(:)=0.d0
   
end subroutine allocate_arrays

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine initialize_subsys
use variables
implicit none 
integer :: I,J

! read in the initial subsystem hamiltonian 
if(process_rank.EQ.master)then  
    open(16,file=trim(filename_H_subsys),form='formatted',status='old',action='read')
         do I=1,num_states
             read(16,*) (H_subsys_0(I,J), J=1, num_states)
         end do
    close(16)
    
    ! put into atomic units from wavenumbers 
    H_subsys_0(:,:) = H_subsys_0(:,:) / (219474.63d0) 
end if

! put into atomic units from wavenumbers 
reorg_E = reorg_E / (219474.63d0)  
omega_c = omega_c / (219474.63d0)  
temp = (315775.024804d0)/temp          

end subroutine initialize_subsys

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine initialize_bath
!=======================================================================================!
! Debye-type - over damped brownian oscillator bath model                               ! 
!                                                                                       ! 
!=======================================================================================!
use variables
implicit none
double precision, PARAMETER :: PI=4.0d0*atan(1.0d0)
double precision :: kondo,omega_max
integer :: J

! kondo parameter corresponding to some reorganizational energy reorg_E
kondo     = 2.0d0*reorg_E 

! fastest bath frequency 
omega_max = 5.0d0*omega_c 

do J=1,num_bath
    ome(J) = omega_max*(DBLE(J)/DBLE(num_bath))**2
    sb_coupling(J) = SQRT((4.0d0/(REAL(num_bath)*PI))* &
        ((kondo*ome(J)**2*omega_c)/(omega_c**2+ome(J)**2))*SQRT(omega_max*ome(J)))
end do

end subroutine initialize_bath

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine  gauss_noise(RAND1,RAND2)    
!=======================================================================================! 
! Returns a normal random number used for the integration of the equations of motion    !
! BOX - MULLER METHOD                                                                   !
!=======================================================================================! 
use LUXURY
implicit none

REAL(8),PARAMETER :: PI2=8.0d0*atan(1.0d0)
REAL(8) :: RAND1,RAND2
REAL(4) :: Z1,Z2,RVEC(1000)

call RANLUX(RVEC,2)
Z1=RVEC(1)
Z2=RVEC(2)
RAND1=SQRT(-2.0d0*LOG(Z1))*COS(PI2*Z2)
RAND2=SQRT(-2.0d0*LOG(Z1))*SIN(PI2*Z2)

end subroutine gauss_noise

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine initial_cond_bath 
use variables 
implicit none 
double precision :: sigma_R 
double precision :: sigma_P 

double precision :: gauss_1 
double precision :: gauss_2 

integer :: I,J 

do I=1,num_states
    do J=1,num_bath

        sigma_R = SQRT(1/(2.d0*ome(J)*DTANH(0.5d0*temp*ome(J))))
        sigma_P = SQRT(ome(J)/(2.d0*DTANH(0.5d0*temp*ome(J))))       

        call gauss_noise(gauss_1,gauss_2)

        bath_R(J,I)=sigma_R*gauss_1
        bath_P(J,I)=sigma_P*gauss_2

   end do
end do

end subroutine initial_cond_bath

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine initial_cond_subsys
use variables 
implicit none 

integer :: I,J
double precision :: gauss_1 
double precision :: gauss_2 

double complex :: subsys_weight_F0(num_states)
double complex :: subsys_weight_B0(num_states)

subsys_weight_F0(:)=0.d0
subsys_weight_B0(:)=0.d0

do I=1,num_states
    call gauss_noise(gauss_1,gauss_2)
        
    subsys_q_F(I) = gauss_1/SQRT(2.0d0)
    subsys_p_F(I) = gauss_2/SQRT(2.0d0)
        
    call gauss_noise(gauss_1,gauss_2)
        
    subsys_q_B(I) = gauss_1/SQRT(2.0d0)
    subsys_p_B(I) = gauss_2/SQRT(2.0d0)
          
    subsys_weight_F0(I) = DCMPLX(subsys_q_F(I),subsys_p_F(I))
    subsys_weight_B0(I) = DCMPLX(subsys_q_B(I),-subsys_p_B(I))
        
end do
	  
do I=1,num_states
    do J=1,num_states
        subsys_weight_0(I + num_states*(J-1)) = subsys_weight_F0(I)*subsys_weight_B0(J)
    end do
end do  

end subroutine initial_cond_subsys

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine FBTS_dynamics
!=======================================================================================!
! Propagate the system through time recording only the points for each timestep block   ! 
!                                                                                       ! 
!=======================================================================================!
use variables
implicit none

double precision :: half_timestep
integer :: I,J,T

half_timestep = 0.5d0 * timestep

    IB = 1 ! set initial data-block index 
    T  = 0 ! subsys uses T and reset IB: set T to zero when time = 0
    call collect 

do T=1,num_timestep ! time loop begins here 

    do I=1,num_states
        do J=1,num_bath
            bath_P(J,I) = bath_P(J,I) + half_timestep*force(J,I)
        end do
    end do

    do I=1,num_states
        do J=1,num_states 
            subsys_p_F(I) = subsys_p_F(I) - half_timestep*H_subsys(I,J)*subsys_q_F(J)
            subsys_p_B(I) = subsys_p_B(I) - half_timestep*H_subsys(I,J)*subsys_q_B(J)
        end do
    end do
 
    do I=1,num_states
        do J=1,num_bath
            bath_R(J,I) = bath_R(J,I) + timestep*bath_P(J,I)
        end do
    end do
  
! update forces and Hamiltonian 
    call subsys 

    do I=1,num_states
        do J=1,num_states 
            subsys_q_F(I) = subsys_q_F(I) + timestep*H_subsys(I,J)*subsys_p_F(J)
            subsys_q_B(I) = subsys_q_B(I) + timestep*H_subsys(I,J)*subsys_p_B(J)
        end do
    end do

    do I=1,num_states
        do J=1,num_states 
            subsys_p_F(I) = subsys_p_F(I) - half_timestep*H_subsys(I,J)*subsys_q_F(J)
            subsys_p_B(I) = subsys_p_B(I) - half_timestep*H_subsys(I,J)*subsys_q_B(J)
        end do
    end do

! update forces and Hamiltonian 
    call subsys 

    do I=1,num_states
        do J=1,num_bath
            bath_P(J,I) = bath_P(J,I) + half_timestep*force(J,I)
        end do
    end do

! calculations
    if(MOD(T,timestep_block).EQ.0)then
         IB = T/timestep_block + 1
         call collect
    end if

end do ! end of time loop

end subroutine FBTS_dynamics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine subsys
!=======================================================================================!
! Calculate the forces for the quantum subsystem along with the quantum subsystem and   ! 
! subsystem-bath coupling hamiltonian                                                   ! 
!=======================================================================================!
use variables
implicit none
integer :: J,I

H_subsys(:,:) = 0.0d0 !free quantum system part 
force(:,:)    = 0.0d0

! build Hamiltonian matrix and force matrices for this timestep 
do I=1,num_states
    do J=1,num_bath
        force(J,I) = -ome(J)**2*bath_R(J,I)
    end do
end do

do I=1,num_states
    do J=1,num_bath 
        force(J,I) = force(J,I) +0.5d0*sb_coupling(J)*(subsys_q_F(I)*subsys_q_F(I)       & 
                                +subsys_p_F(I)*subsys_p_F(I)+subsys_q_B(I)*subsys_q_B(I) &
                                +subsys_p_B(I)*subsys_p_B(I))
    end do 
end do
  
do J=1,num_states       
    do I=1,num_states
        H_subsys(I,J) = H_subsys_0(I,J)
    end do
end do

do I=1,num_states
    do J=1,num_bath
        H_subsys(I,I) = H_subsys(I,I) - sb_coupling(J)*bath_R(J,I)
    end do
end do

end subroutine subsys

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
subroutine collect
!=======================================================================================!
! Calculate the total energy of the system and the time-dependent observables           ! 
!                                                                                       ! 
!=======================================================================================!
use variables
implicit none
integer :: I,J 
double precision :: bath_potE 
double precision :: bath_kinE 
double precision :: H_tot 
double complex :: subsys_weight_Ft(num_states)  
double complex :: subsys_weight_Bt(num_states) 
double complex :: subsys_weight_T(num_states*num_states)

call subsys


subsys_weight_Ft(:) = 0.d0
subsys_weight_Bt(:) = 0.d0
subsys_weight_T(:)  = 0.d0

bath_potE = 0.d0
bath_kinE = 0.d0
H_tot     = 0.d0 
   
! calculate the total energy 
do I=1,num_states
    do J=1,num_bath
        bath_potE = bath_potE + 0.5d0 * ome(J)**2 * bath_R(J,I)**2
        bath_kinE = bath_kinE + 0.5d0 * bath_P(J,I)**2
    end do
end do
 
do I=1,num_states
    do J=1,num_states
        H_tot = H_tot + 0.5d0*H_subsys(I,J)*(subsys_q_F(I)*subsys_q_F(J)+subsys_p_F(I)   &
                   *subsys_p_F(J)+subsys_q_B(I)*subsys_q_B(J)+subsys_p_B(I)*subsys_p_B(J)) 
    end do
end do
 
H_tot = H_tot + bath_potE + bath_kinE
 

subsys_weight_Ft(:) = DCMPLX(subsys_q_F(:),-subsys_p_F(:))
subsys_weight_Bt(:) = DCMPLX(subsys_q_B(:),subsys_p_B(:))
  
do I=1,num_states
    do J=1,num_states
        subsys_weight_T(I + num_states*(J-1)) = subsys_weight_Ft(I)*subsys_weight_Bt(J)
    end do
end do  

! construct time-dependent observables
site_pop(IB,:) = site_pop(IB,:) + subsys_weight_0(init_state+num_states*(init_state-1))  &
                 *DCMPLX(subsys_q_F(:),-subsys_p_F(:))*DCMPLX(subsys_q_B(:),subsys_p_B(:))

do I=1,num_states
    do J=1,num_states
        if(I.NE.J)then
            real_site_coh(IB,I,J) = real_site_coh(IB,I,J) + 0.5d0                        &
               *REAL(subsys_weight_0(init_state+num_states*(init_state-1))               &
               *(subsys_weight_T(I+num_states*(J-1))+subsys_weight_T(J+num_states*(I-1))))
                                                
            im_site_coh(IB,I,J) = im_site_coh(IB,I,J) + 0.5d0                            &
               *AIMAG(subsys_weight_0(init_state+num_states*(init_state-1))              &
               *(subsys_weight_T(I+num_states*(J-1))-subsys_weight_T(J+num_states*(I-1))))
        end if 
    end do
end do
 
  
end subroutine collect

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine coherence_len 
!=======================================================================================!
! Calculate the coherence length for a single exciton in the site basis                 ! 
!                                                                                       ! 
!=======================================================================================!

use variables
implicit none

integer :: I,J,K
double precision :: numerator_sum_R,numerator_sum_I,denominator_sum

do K=1,time 
    numerator_sum_R = 0.d0
    numerator_sum_I = 0.d0
    denominator_sum = 0.d0
 
 
    do J=1,num_states 
        do I=1,num_states 
        if(I.NE.J)then 
            denominator_sum = denominator_sum + collect_real_site_coh(K,I,J)**2          &
                                              + collect_im_site_coh(K,I,J)**2 
        end IF 
        
        if(I.EQ.J)then
        denominator_sum = denominator_sum + collect_site_pop(K,J)**2
        end if 
        
    end do 
end do 

do J=1,num_states 
    do I=1,num_states 
        if(I.NE.J)then 
            numerator_sum_R = numerator_sum_R + ABS(collect_real_site_coh(K,I,J))
            numerator_sum_I = numerator_sum_I + ABS(collect_im_site_coh(K,I,J))
        end if 
    
        if(I.EQ.J)then 
            numerator_sum_R = numerator_sum_R + ABS(collect_site_pop(K,J))
        end if 
    end do  
end do 

coh_len(K) = (1.d0/REAL(num_states)) * (numerator_sum_R**2 + numerator_sum_I**2)         &
                                     / denominator_sum

end do 

end subroutine coherence_len

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine write_output
use variables 
implicit none 

integer :: I,J,K
character(len=20) :: FMT
write(FMT, *) num_states + 1 

open(40, file="site_pop.out")
do K=1,time
    write(40,"("//ADJUSTL(FMT)//"G20.10)") (K-1)*timestep*timestep_block,                &
                                           (collect_site_pop(K,J), J=1,num_states)
end do
close(40)

open(41,file="im_site_coh.out")
do K=1,time
    do I=1,num_states 
        do J=1,num_states 
            IF(I>J) THEN 
                write(41,"("//ADJUSTL(FMT)//"G20.10)") (K-1)*timestep*timestep_block,    &
                                                       collect_im_site_coh(K,I,J)
            end IF 
        end do
    end do 
end do 
close(41)

open(42,file="real_site_coh.out")
do K=1,time
    do I=1,num_states 
        do J=1,num_states 
            IF(I>J) THEN 
                write(42,"("//ADJUSTL(FMT)//"G20.10)") (K-1)*timestep*timestep_block,    &
                                                       collect_real_site_coh(K,I,J)
            end IF 
        end do
    end do 
end do
close(42) 

open(40,file='coherence_length.out')
do K=1,time
    write(40,*) (K-1)*timestep*timestep_block, coh_len(K)
end do 
close(40)

end subroutine write_output

