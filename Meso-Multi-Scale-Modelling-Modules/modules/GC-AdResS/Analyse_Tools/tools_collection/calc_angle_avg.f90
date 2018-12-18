program calc_angle_avg
implicit none

real(8),dimension(1100000) :: a,b,c,avga,avgb,avgc,db
real(8),dimension(1100000) :: x1,y1,z1
real(8),dimension(1100000) :: x2,y2,z2
real(8),dimension(1100000) :: dx,dy,dz
real(8),dimension(3) :: ex,ey,ez
real(8) :: dr,mb,tmpa,tmpb,tmpc
real(8) :: boxx,boxy,boxz,boxx2,boxy2,boxz2
real(8) :: avea,aveb,avec,sdeva,sdevb,sdevc
real(8) :: xmin,xmax,dnorm,curt,skew,var,adev
real(8) :: pi=acos(-1.0d0)
character(4) :: t
integer :: i,j,k,l,m,n
integer :: n1,n2
integer :: nd1,nd2,nd

open(unit=27,file="t_C",status="old")
open(unit=28,file="t_H",status="old")
open(unit=18,file="angs_tot.dat")
open(unit=11,file="angs_avg.dat")
open(unit=15,file="angles_totavg.dat")
open(unit=16,file="angles_binavg.dat")

write(6,*) "amount of points"
read (5,*) nd1
read (5,*) nd2

if(nd1.ne.nd2) then
   write(6,*) "FUCK, that was wrong!"
   stop
endif

nd=nd1

read (5,*) xmin,xmax
write(6,*) "bin size"
read (5,*) dr
write(6,*) "box size (x, y, z)"
read (5,*) boxx,boxy,boxz

boxx2=boxx/2.0d0
boxy2=boxy/2.0d0
boxz2=boxz/2.0d0

do i=1,nd1
   read(27,*) n1,t,x1(i),y1(i),z1(i)
enddo
do i=1,nd2
   read(28,*) n2,t,x2(i),y2(i),z2(i)
enddo

!norm eigenvectors ex,ey,ez are 1 already
ex(1)=1.0d0
ey(1)=0.0d0
ez(1)=0.0d0

ex(2)=0.0d0
ey(2)=1.0d0
ez(2)=0.0d0

ex(3)=0.0d0
ey(3)=0.0d0
ez(3)=1.0d0

write(6,*) pi

do i=1,nd
   dx(i)=x2(i)-x1(i)
   dy(i)=y2(i)-y1(i)
   dz(i)=z2(i)-z1(i)

   if(dx(i).lt.-boxx2) dx(i)=dx(i)+boxx
   if(dx(i).gt.boxx2)  dx(i)=dx(i)-boxx
   if(dy(i).lt.-boxy2) dy(i)=dy(i)+boxy
   if(dy(i).gt.boxy2)  dy(i)=dy(i)-boxy
   if(dz(i).lt.-boxz2) dz(i)=dz(i)+boxz
   if(dz(i).gt.boxz2)  dz(i)=dz(i)-boxz
   
   dnorm=sqrt(dx(i)**2+dy(i)**2+dz(i)**2)                        !norm vector (molecule)

!write(6,*)  dnorm,dx(i),dy(i),dz(i)
 
   tmpa=(dx(i)*ex(1)+dy(i)*ey(1)+dz(i)*ez(1))/dnorm
   tmpb=(dx(i)*ex(2)+dy(i)*ey(2)+dz(i)*ez(2))/dnorm
   tmpc=(dx(i)*ex(3)+dy(i)*ey(3)+dz(i)*ez(3))/dnorm

!   do j=1,nd
!      ad(i)=ad(i)+(dx(i)*dx(j)+dy(i)*dy(j)+dz(i)*dz(j))/dnorm
!   enddo
!   ad(i)=ad(i)/nd

! IMPORTANT 
      a(i)=abs(tmpa)
      b(i)=abs(tmpb)
      c(i)=abs(tmpc)
enddo
 
do i=1,nd,100
   write(18,*) x1(i),a(i),b(i),c(i)
enddo

!call moment(ad,nd,avea,adev,sdeva,var,skew,curt)

call moment(a,nd,avea,adev,sdeva,var,skew,curt)
call moment(b,nd,aveb,adev,sdevb,var,skew,curt)
call moment(b,nd,avec,adev,sdevc,var,skew,curt)

write(15,*) avea,aveb,avec,sdeva,sdevb,sdevc

mb=(xmax-xmin)/dr
m=int(mb)
db(1)=xmin
!write(6,*) xmin,xmax,mb,db(1)

do l=2,m+1
   db(l)=db(l-1)+dr
   k=0.0d0
   do i=1,nd
      if (x1(i).le.db(l).and.x1(i).gt.db(l-1)) then
         k=k+1
         avga(l)=avga(l)+a(i)
         avgb(l)=avgb(l)+b(i)
         avgc(l)=avgc(l)+c(i)
      endif
   enddo
   avga(l)=avga(l)/(k*1.0d0)
   avgb(l)=avgb(l)/(k*1.0d0)
   avgc(l)=avgc(l)/(k*1.0d0)
!   write(6,*) l,m
   write(11,*)  db(l-1)+dr/2.0d0, avga(l),avgb(l),avgc(l)
enddo

call moment(avga,m,avea,adev,sdeva,var,skew,curt)
call moment(avgb,m,aveb,adev,sdevb,var,skew,curt)
call moment(avgc,m,avec,adev,sdevc,var,skew,curt)

write(16,*) avea,aveb,avec,sdeva,sdevb,sdevc



end program 
!=============================================================

      subroutine moment(data,n,ave,adev,sdev,var,skew,curt)
!  data: vector with the available data from program
!  n: number of data points
      implicit real*8(a-h,o-z)
      parameter(zero=0.0d0,one=1.0d0)
      dimension data(n)

      if(n.le.1) then
!        write(6,*) 'n must be at least 2 in moment'
         return
      endif

      s=zero
      do 112 j=1,n
         s=s+data(j)
 112  enddo
      ave=s/n
      adev=zero
      sdev=zero
      var=zero
      skew=zero
      curt=zero
      ep=zero
      do 122 j=1,n
         s=data(j)-ave
         ep=ep+s
         adev=adev+abs(s)
         p=s*s
         var=var+p
         p=p*s
         skew=skew+p
         curt=curt+p
 122  enddo
      adev=adev/n
      var=(var-ep**2/n)/(n-1)
      sdev=sqrt(var)
      if(var.ne.zero) then
        skew=skew/(n*sdev**3)
        curt=curt/(n*var**2)-3
      else
!       write(6,*) 'no skew or kurtosis when zero variance in momnet'
      endif
      return
      end
