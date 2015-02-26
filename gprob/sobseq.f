      SUBROUTINE sobseq(n,x)
      INTEGER n,MAXBIT,MAXDIM
      double precision x(*)
      PARAMETER (MAXBIT=30,MAXDIM=6)
      INTEGER i,im,in,ipp,j,k,l,ip(MAXDIM),iu(MAXDIM,MAXBIT),iv(MAXBIT*
     *MAXDIM),ix(MAXDIM),mdeg(MAXDIM),ippp(MAXDIM),mdegp(MAXDIM),
     *ixp(MAXDIM),ivp(MAXBIT*MAXDIM)
      double precision fac
      SAVE ip,mdeg,ix,iv,in,fac,ippp,mdegp,ixp,ivp
      EQUIVALENCE (iv,iu)
      DATA ippp /0,1,1,2,1,4/, mdegp /1,2,3,3,4,4/, ixp /6*0/
      DATA ivp /6*1,3,1,3,3,1,1,5,7,7,3,3,5,15,11,5,15,13,9,156*0/
      if (n.lt.0) then
C Add kluge
        do 50 i=1,MAXDIM
           ip(i)=ippp(i)
           mdeg(i)=mdegp(i)
           ix(i)=ixp(i)
50      continue

        do 60 j=1,MAXDIM*MAXBIT
            iv(j)=ivp(j)
60      continue
           
        do 14 k=1,MAXDIM
          do 11 j=1,mdeg(k)
            iu(k,j)=iu(k,j)*2**(MAXBIT-j)
11        continue
          do 13 j=mdeg(k)+1,MAXBIT
            ipp=ip(k)
            i=iu(k,j-mdeg(k))
            i=ieor(i,i/2**mdeg(k))
            do 12 l=mdeg(k)-1,1,-1
              if(iand(ipp,1).ne.0)i=ieor(i,iu(k,j-l))
              ipp=ipp/2
12          continue
            iu(k,j)=i
13        continue
14      continue
        fac=1./2.**MAXBIT
        in=0
      else
        im=in
        do 15 j=1,MAXBIT
          if(iand(im,1).eq.0)goto 1
          im=im/2
15      continue
        pause 'MAXBIT too small in sobseq'
1       im=(j-1)*MAXDIM
        do 16 k=1,min(n,MAXDIM)
          ix(k)=ieor(ix(k),iv(im+k))
          x(k)=ix(k)*fac
16      continue
        in=in+1
      endif
      return
      END
