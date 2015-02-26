      program giantprob

      implicit none
      integer LMAX,id,j,nsamp,maxsamp,numout,chif,rouf
      double precision Mo,T,D,tgral,region(6),var,avg,maglim
      double precision Merr,Terr,Derr,MTerr,MDerr,func,errc
      double precision MTo,MDo,area,expval,chi,rou,ra,dec
      double precision l,b,ebv,emt,emd,am,x,y,error,toterror
      double precision interror,staterror
      character*40 inpfile,outfile
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

      region(1)=1.0
      region(2)=-.07
      region(3)=15.0
      region(4)=2.3
      region(5)=1.0
      region(6)=19.3

      j=1 
      expval=0.0
      toterror=0.0
      do 100 while(j.ne.0)
        read(10,*,END=200) id,x,y,Mo,Merr,T,Terr,D,Derr,chi,rou,ra,dec
     &  ,l,b,ebv,emt,emd,am,chif,rouf

        if(CHIF.eq.0.or.ROUF.eq.0.or.Merr.ge.errc.or
     &  .Terr.ge.errc.or.Derr.ge.errc) goto 100
        MTerr=sqrt(Merr**2+Terr**2)
        MDerr=sqrt(Merr**2+Derr**2)
        MTo=Mo-T-EMT
        MDo=Mo-D-EMD
        Mo=Mo-AM
C      region(1)=MTo-3*MTerr
C      region(2)=MDo-3*MDerr
C      region(3)=Mo-3*Merr
C      region(4)=MTo+3*MTerr
C      region(5)=MDo+3*MDerr
C      region(6)=Mo+3*Merr 
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
      print*, tgral,error,interror,staterror,nsamp,numout,area
        toterror=toterror+error**2
        expval=expval+tgral
c        print*, expval,error,var
         write(11,45)id,x,y,Mo,Merr,T,Terr,D,Derr,chi,rou,ra,dec
     &   ,l,b,ebv,emt,emd,am,chif,rouf,tgral,error,interror,staterror
        j=j+1
C      pause
100   continue 
200   print*, expval,sqrt(toterror),LMAX
      print*, '   '
      LMAX=j-1
45    format(I8,2f9.3,9f9.4,f9.3,2f11.5,4f11.6,2I2,4f9.4)
      stop
      end


      FUNCTION func(pt,MTerr,MDerr,Merr,MTo,MDo,Mo,FLAG,maglim)

      implicit none
      double precision pt(3),MTerr,MDerr,Merr,Pi,func
      double precision M,MT,MD,hicase,locase,maglim
      double precision Mo,MTo,MDo,getprob
      integer FLAG
      External getprob


C      open(14,file='test.in')
C      open(15,file='test.out')
      M=pt(3)
      MT=pt(1)
      MD=pt(2) 
      Pi=3.141592654
C define polynomials here
      if(MT.le.(1.1)) then
         hicase=39.98699-37.07959*(MT)+16.95270*(MT**2)
      else
          hicase=31.90549-16.79336*(MT)+4.985536*(MT**2)
      endif
      locase=134.9220-192.0889*MT+105.4961*(MT**2)-19.38386*(MT**3)

C   Make giant cuts
       FLAG=0
C       if(MD.lt.(-.313*(MT)+0.3691).or.MT.gt.(2.0).or
C     &  .MD.lt.(-0.05)) then

C          if(M.le.hicase.or.M.ge.locase.or.MT.ge.1.8.or.M.ge.
C     &   maglim) then

       if(MD.gt.(-.313*(MT)+0.3691).and.MT.lt.(2.0).and
     &  .MD.gt.(-0.05).and.M.ge.hicase.and.M.le.locase
     &  .and.MT.le.1.8.and.M.le.maglim) then
            FLAG=1
C            write(15,45) M,MT,MD,func
          func=getprob(pt,MTerr,MDerr,Merr,MTo,MDo,Mo)
            return
       endif
C          expfac=( ((MT-MTo)/MTerr)**2 + ((MD-MDo)/MDerr)**2  
C     &    + ((M-Mo)/Merr)**2 )
C          normfac=(MTerr*Merr*MDerr*2.0*Pi*(sqrt(2.0*PI)))**(-1.0)
C          func=(normfac)*exp(-1.0*expfac)
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



      
