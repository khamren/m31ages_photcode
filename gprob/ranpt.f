      SUBROUTINE ranpt(pt,region,n)
      INTEGER n,idum
      double precision pt(n),region(2*n),x(3)
      COMMON /ranno/ idum
      SAVE /ranno/
CU    USES ran2
      real ran2
      INTEGER j
      call sobseq(3,x)
C      print*, x(1),x(2),x(3)
      do 11 j=1,n
C        pt(j)=region(j)+(region(j+n)-region(j))*ran2(idum)
        pt(j)=region(j)+(region(j+n)-region(j))*x(j)
C       print*, pt(j)
11    continue
      return
      END
