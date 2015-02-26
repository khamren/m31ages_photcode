      PROGRAM GOODPSF
C
C=====================================================================
C
C This program generates a file with only good PSF candidates.
C
C=====================================================================
C
      INTEGER nmax
      REAL minchi
      PARAMETER(nmax=1000,minchi=0.5)
C
C 'minchi' is the minimum CHI value allowed for a PSF star.
C You may change this value to fit your needs.
C
      INTEGER i,j,k,n,nr,id(nmax),iid(nmax),nstar,npsf,istar,ibadstar
      REAL x(nmax),y(nmax),mag(nmax),err(nmax),sky(nmax)
      REAL chi(nmax)
      CHARACTER flag(nmax)*1,line*85
      CHARACTER*60 lstfile,chifile,outfile
      CHARACTER*69 line1,line2
      
      PRINT *,'File with PSF candiates (.lst)?'
      READ(*,'(A60)') lstfile
      PRINT *,'File with chi values of PSF candidates (.lst.chi)?'
      READ(*,'(A60)') chifile
      PRINT *,'Output file ?'
      READ(*,'(A60)') outfile
C
C=====================================================================
C Count total number of PSF candidates.
C=====================================================================
C
      OPEN(11,FILE=lstfile)
      READ(11,'(A69)') line1
      READ(11,'(A69)') line2
      READ(11,*)
      nstar = 0
      DO i = 1, nmax
         READ(11,*,END=9) id(i),x(i),y(i),mag(i),err(i),sky(i)
         nstar = nstar + 1
      ENDDO
9     CLOSE(11)
C           
C=====================================================================
C Read in chi values and flags from .chi file.
C Stole this part from Stetson's code (~line 580 of psf.f).
C=====================================================================
C
      k = (nstar-1)/5 + 1
      OPEN(12,FILE=chifile)
      DO i = 1, k
         READ(12,'(A85)') line
         DO istar = i, nstar, k
            j = 17*(istar-i)/k + 1
            READ (line(j:j+16),69) iid(istar),chi(istar),flag(istar)
         ENDDO
      ENDDO
      CLOSE(12)
69    FORMAT(I7, F7.3, 1X, A1, 1X)
C           
C=====================================================================
C  Process PSF candidates flags and prune out.
C=====================================================================
C
      npsf = nstar
      OPEN(21,FILE=outfile)
      WRITE(21,'(A69)') line1
      WRITE(21,'(A69)') line2
      WRITE(21,*)
      DO i = 1, nstar
         IF (flag(i).NE.'?' .AND. flag(i).NE.'*' 
     .                      .AND. chi(i).LT.minchi) THEN
            DO j = 1, nstar
               IF (id(j).EQ.iid(i)) THEN
                  WRITE(21,70) id(j),x(j),y(j),mag(j),err(j),sky(j)
                  GOTO 1
               ENDIF
            ENDDO
         ELSE
            npsf = npsf -1
         ENDIF
1     ENDDO
      CLOSE(21)
70    FORMAT(I7,5F9.3)
C
      WRITE(6,71) npsf,nstar
71    FORMAT(/'You have ',I3,' (out of ',I3,') PSF candidates left.'/)
C
      STOP
      END




