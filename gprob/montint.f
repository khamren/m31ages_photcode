      SUBROUTINE montint(fxn,region,ndim,tgral,tol,var,MTerr,MDerr,Merr)

      implicit none
      integer ndim,j,numtot
      real region(2*ndim),tgral,tol,var,n,err2
      real x(3),f,MTerr,MDerr,Merr
      real sumf,area,sumf2,pt(3),fxn
      EXTERNAL fxn

      n=0.0
      sumf=0.0
      sumf2=0.0
      var=9999999999.9
      err2=999999999.9
      area=abs(region(1)-region(4))*abs(region(2)-region(5))*
     &abs(region(3)-region(6))
C      print*, area
      do 100 while(err2.gt.tol)
C Get our random point
           call sobseq(3,x)
C Normalize it to be in our volume
           do 11 j=1,ndim
             pt(j)=region(j)+(region(j+n)-region(j))*x(j)
C             print*, pt(j)
11         continue
           f=fxn(pt,MTerr,MDerr,Merr)
           if(f.eq.(0.0)) then
            n=n+1
           endif
           sumf=sumf+f
           sumf2=sumf2+f*f
           numtot=numtot+1.0
           if(n.gt.1000) then
           var=(sumf2 - sumf*sumf/n)
           err2=var/(sumf*sumf)
           print*, err2,tol,n,numtot
           endif
100    continue

       area=area*real(n/numtot)
       tgral=area*(sumf/n)
       var = (sumf2-sumf*sumf/n)/(n*n)
       return 
       end
         
