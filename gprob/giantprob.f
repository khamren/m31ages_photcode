      program giantprob

      implicit none
      integer LMAX,id,j,nsamp,maxsamp,numout,chif,rouf
      double precision Mo,T,D,tgral,region(6),var,avg,maglim
      double precision Merr,Terr,Derr,MTerr,MDerr,func,errc
      double precision MTo,MDo,area,expval,chi,rou,ra,dec
      double precision l,b,ebv,emt,emd,am,x,y,error,toterror
      double precision interror,staterror,M1,perror,erad,crad,drad
      double precision vhel,vlsr,peak,q,prob,xsi,eta,radii,flag
      character*40 inpfile,outfile
      character*4 field
      EXTERNAL func

      write(6,*)'Enter the input file name:'
      read(5,*) inpfile

      write(6,*) 'Enter the output file, with probabilities:'
      read(5,*) outfile
      open(10,file=inpfile)
      open(11,file=outfile)
      maxsamp=10000000

      write(6,*) 'Enter the error cut, limiting magnitude,' 
      write(6,*) ' number of samples: '
      read(5,*) errc,maglim,maxsamp

      j=1 
      expval=0.0
      toterror=0.0
      do 100 while(j.ne.0)
        read(10,*,END=200) id,ra,dec,D,Derr,Mo,Merr,T,Terr,chi,rou,
     &  flag,prob,l,b,ebv,am,emt,emd,xsi,eta
     &  ,radii,field
C,prob,perror,interror,staterror,
C     &  erad,crad,drad,vhel,vlsr,peak,q
        if(Merr.ge.errc.or
     &  .Terr.ge.errc.or.Derr.ge.errc) goto 100
        M1=Mo
        MTerr=sqrt(Merr**2+Terr**2)
        MDerr=sqrt(Merr**2+Derr**2)
        MTo=Mo-T-EMT
        MDo=Mo-D-EMD
        Mo=Mo-AM
      region(1)=MTo-10*MTerr
      region(2)=MDo-10*MDerr
      region(3)=Mo-10*Merr
      region(4)=MTo+10*MTerr
      region(5)=MDo+10*MDerr
      region(6)=Mo+10*Merr 
C        print*, MTo,MDo,Mo,MTerr,MDerr,Merr
C        print*, '  '
C        print*, region(1),region(2),region(3),region(4),region(5),
c     &  region(6) 
C        print*, '  '
500     call sobseq(-2,3)
        call mcintegrate(func,region,3,maxsamp,avg,var,nsamp,
     &  MTerr,MDerr,Merr,MTo,MDo,Mo,numout,maglim)
        area=abs(region(1)-region(4))*abs(region(2)-region(5))*
     &  abs(region(3)-region(6))
        tgral=avg*area*(dble(numout)/dble(nsamp)) 
        interror=sqrt(var)*area*(dble(numout)/dble(nsamp))
C      print*, '  '
        staterror=sqrt((tgral*(1.-tgral))**2)
        error=sqrt(interror**2+staterror**2)
        if(numout.eq.0) then
           tgral=0.0
           interror=0.0
           staterror=0.0
           error=0.0
        endif
      print*, tgral,error,interror,staterror,nsamp,numout,area
        toterror=toterror+error**2
        expval=expval+tgral
c        print*, expval,error,var
         write(11,45)id,ra,dec,D,Derr,M1,Merr,T,Terr,chi,rou,
     &  flag,prob,l,b,ebv,am,emt,emd,xsi,eta
     &  ,radii,field
     &  ,tgral
     &  ,error,interror,staterror
         call flush(11)

        j=j+1
C      pause
100   continue 
200   print*, expval,sqrt(toterror),LMAX
      print*, '   '
      LMAX=j-1
45    format(I12,2f15.10,10f9.4,2f9.3,4f9.5,3f15.4,2x,a4,4f9.4)
      stop
      end


      FUNCTION func(pt,MTerr,MDerr,Merr,MTo,MDo,Mo,FLAG,maglim)

      implicit none
      double precision pt(3),MTerr,MDerr,Merr,Pi,func
      double precision M,MT,MD,hicase,locase,maglim
      double precision Mo,MTo,MDo,getprob,slope,yint
      double precision MTcut,MDcut,Mmaglim,Dmaglim
      double precision Tmaglim,T,D
      integer FLAG
      External getprob


C      open(14,file='test.in')
C      open(15,file='test.out')
      M=pt(3)
      MT=pt(1)
      MD=pt(2) 
      T=-1*MT+M
      D=-1*MD+M
      Pi=3.141592654
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C define Carina polynomials here
CC      if(MT.le.(1.1)) then
C         hicase=39.98699-37.07959*(MT)+16.95270*(MT**2)
C      else
C          hicase=31.90549-16.79336*(MT)+4.985536*(MT**2)
C      endif
C      locase=134.9220-192.0889*MT+105.4961*(MT**2)-19.38386*(MT**3)
C       slope=-.313
C       yint=0.3691
C       MTcut=1.8
C       MDcut=-0.05
C       Mmaglim=maglim
C       Dmaglim=99
C       Tmaglim=99
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C define And I polynomials here
C        locase=3.383618E1-9.572048E0*(mt)+2.052820E0*(mt)**2 
C        hicase=3.206604E1-1.048427E1*(mt)+2.722914E0*(mt)**2 
C        slope=-0.313
C        yint=0.4191
C        MTcut=2.20
C        MDcut=-0.1
C        Mmaglim=23.91
C        Tmaglim=22.55
C        Dmaglim=23.33
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C define And II polynomials her
C        hicase=3.172871E1-1.134623E1*(mt)+3.232846E0*(mt)**2 
C        locase=4.486385E1-2.261609E1*(mt)+5.743177E0*(mt)**2 
C        slope=-0.313
C       yint=0.4291
C        MTcut=2.10
C        MDcut=-0.05
C        Mmaglim=24.3
C        Tmaglim=23.32
C        Dmaglim=23.84
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C define AndIII polynomials here
C        hicase=3.585178E1-1.513477E1*(mt)+4.150210E0*(mt)**2 
C        locase=5.161186E1-2.895123E1*(mt)+7.141535E0*(mt)**2 
C        slope=-0.313
C        yint=0.4791
C        MTcut=2.
C        MDcut=-0.05
C        Mmaglim=23.87
C        Tmaglim=22.34
C        Dmaglim=23.33

C   Make giant cuts
       FLAG=0

       if(MD.gt.(-.313*(MT)+.4291).and.MD.gt.(0.33*(MT)-.79999).and
     &  .MD.gt.(-0.1)) then
            FLAG=1
C            write(15,45) M,MT,MD,func
       endif
      func=getprob(pt,MTerr,MDerr,Merr,MTo,MDo,Mo)
C      write(14,45) M,MT,MD,func
C45    format(4f9.4)
      return
      end

      FUNCTION getprob(pt,MTerr,MDerr,Merr,MTo,MDo,Mo)
 
      implicit none
      double precision pt(3),MTerr,MDerr,Merr,Pi,getprob
      double precision detcov,varMT,Mo,MTo,MDo
      double precision varMD,varM,t1,t2,t14,delta(3)
      double precision t13,t17,t16,Q,t3
      
      Pi=3.141592654
      varM=Merr**2
      varMD=MDerr**2
      varMT=MTerr**2

      t3 = varM**2
      detcov = varM*varMT*varMD+t3*varM-t3*varMD-t3*varMT

      delta(2)=pt(1)-MTo
      delta(3)=pt(2)-MDo
      delta(1)=pt(3)-Mo

      t1 = varMT*varMD
      t2 = varM**2
      t13 = 1/(varM-varMT)
      t14 = delta(2)*t13
      t16 = 1/(-varMD+varM)
      t17 = delta(3)*t16
      Q =(-1.*delta(1)*(-t1+t2)/varM/(t1+t2-varM*varMD-varM*varMT)+
     &t14+t17)*delta(1)+(delta(1)*t13-t14)*
     &delta(2)+(delta(1)*t16-t17)*delta(3)
 
      getprob=(1.0/((2.*Pi)**1.5*detcov**.5) ) * (exp(-1*Q/2.))

      return
      end



      
