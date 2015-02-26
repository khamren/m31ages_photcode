      subroutine mcintegrate(func,region,ndim,maxsamp,avg,var,nsamp,
     &MTerr,MDerr,Merr,MTo,MDo,Mo,numout,maglim)

      implicit none
      
      integer minsamp
      parameter (minsamp=10000000)
      double precision tol,Merr,MTErr,MDerr
      parameter (tol=1.e-2)
      
      integer ndim,maxsamp,nsamp,nextsamp,numout
      double precision func,region(2*ndim),avg,var
      EXTERNAL func
      double precision sum,sum2,fval,err2,pt(ndim)
      double precision MTo,MDo,Mo,avgold,maglim
      integer FLAG
      
      sum = 0.
      sum2 = 0.
      
      nextsamp = minsamp
      numout=0
      avgold=0.0
      do nsamp=1,maxsamp
         call ranpt(pt,region,ndim)
C        print*, pt(1),pt(2),pt(3)
         fval = func(pt,MTerr,MDerr,Merr,MTo,MDo,Mo,FLAG,maglim)
C if flag equal 0 then integrate over inside of box
C if flag equal 1 then integrate over outside of box
         if(FLAG.eq.1) then
           numout=numout+1
         else
           fval=0.0
         endif
         sum = sum+fval
         sum2 = sum2 + fval*fval
      end do
      avg = sum/dble(numout)
      var = (sum2/dble(numout)-sum*sum/dble(numout)**2)/(dble(numout))
C      print*, numout
      return
      end
      
