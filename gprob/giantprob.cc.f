      program giantprob

      implicit none
      integer LMAX,id,j,nsamp,maxsamp,numout,chif,rouf
      double precision Mo,T,D,tgral,region(4),var,avg
      double precision Merr,Terr,Derr,MTerr,MDerr,func
      double precision MTo,MDo,area,expval,chi,rou,ra,dec
      double precision l,b,ebv,emt,emd,am,x,y,error,toterror
      double precision interror,staterror,D1,D1err,M1,M1err
      double precision T1,T1err,equinox,maglim
      integer mf,tf,df
      character*40 inpfile,outfile
      EXTERNAL func

      write(6,*)'Enter the input file name:'
      read(5,*) inpfile

      write(6,*) 'Enter the output file, with probabilities:'
      read(5,*) outfile
      open(10,file=inpfile)
      open(11,file=outfile)
      maxsamp=10000000

      write(6,*) 'Number of samples: '
      read(5,*) maxsamp

      region(1)=-1.0
      region(2)=-1.0
      region(3)=5.0
      region(4)=1.0
      maglim=99.9
      j=1 
      expval=0.0
      toterror=0.0
      do 100 while(j.ne.0)
        read(10,*,END=200) id,RA,DEC,D,Derr,D1,D1err,Mo,Merr,
     &  M1,M1err,T,Terr,T1,T1err
     &  ,x,y,equinox,l,b,ebv,am,emt,emd,mf,tf,df 

        MTerr=sqrt(Merr**2+Terr**2)
        MDerr=sqrt(Merr**2+Derr**2)
        MTo=Mo-T-EMT
        MDo=Mo-D-EMD
        Mo=Mo-AM
500     call sobseq(-2,2)
        call mcintegrate(func,region,2,maxsamp,avg,var,nsamp,
     &  MTerr,MDerr,Merr,MTo,MDo,Mo,numout,maglim)
        area=abs(region(1)-region(3))*abs(region(2)-region(4))
        tgral=avg*area*(dble(numout)/dble(nsamp)) 
        interror=sqrt(var)*area*(dble(numout)/dble(nsamp))
C      print*, '  '
        staterror=sqrt((tgral*(1.-tgral))**2)
        error=sqrt(interror**2+staterror**2)
      print*, tgral,error,interror,staterror,nsamp,numout,area
        toterror=toterror+error**2
        expval=expval+tgral
c        print*, expval,error,var
         write(11,45)id,RA,DEC,D,Derr,D1,D1err,Mo,Merr,
     &  M1,M1err,T,Terr,T1,T1err
     &  ,x,y,equinox,l,b,ebv,am,emt,emd,mf,tf,df  
     &   ,tgral,error,interror,staterror
        j=j+1
C      pause
100   continue 
200   print*, expval,sqrt(toterror),LMAX
      print*, '   '
      LMAX=j-1
45    format(I8,2f15.10,12f9.4,2f9.4,f7.1,2f10.6,f9.4,3f9.5,3I2,4f9.4)
      stop
      end


      FUNCTION func(pt,MTerr,MDerr,Merr,MTo,MDo,Mo,FLAG,maglim)

      implicit none
      double precision pt(2),MTerr,MDerr,Merr,Pi,func
      double precision M,MT,MD,hicase,locase,maglim
      double precision Mo,MTo,MDo,getprob
      integer FLAG
      External getprob


C      open(14,file='test.in')
C      open(15,file='test.out')
      MT=pt(1)
      MD=pt(2) 
      Pi=3.141592654

C   Make giant cuts
       FLAG=1

       if(MD.gt.(-.313*(MT)+0.4191).and
     &  .MD.gt.(-0.15).and.MD.gt.(.18*(MT)-.51) ) then
            FLAG=0
C            write(15,45) M,MT,MD,func
            return
       endif
      func=getprob(pt,MTerr,MDerr,Merr,MTo,MDo,Mo)
C      write(14,45) M,MT,MD,func
C45    format(4f9.4)
      return
      end

      FUNCTION getprob(pt,MTerr,MDerr,Merr,MTo,MDo,Mo)
 
      implicit none
      double precision pt(2),MTerr,MDerr,Merr,Pi,getprob
      double precision detcov,varMT,Mo,MTo,MDo
      double precision varMD,varM,t3,t5,t14,delta(2)
      double precision t13,t17,t16,Q,t3
      
      Pi=3.141592654
      varM=Merr**2
      varMD=MDerr**2
      varMT=MTerr**2

      detcov = varMT*varMD-varM**2

      delta(1)=pt(1)-MTo
      delta(2)=pt(2)-MDo

      t3 = varM**2
      t5 = 1./(varMT*varMD-t3)
      Q = (delta(1)*varMD*t5-delta(2)*varM*t5)*delta(1)+
     #(-delta(1)*varM*t5+delta(2)*varMT*t5)*delta(2)
      getprob=(1.0/((2.*Pi)**1*detcov**.5) ) * (exp(-1.*Q/2.))

      return
      end



      
