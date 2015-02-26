      PROGRAM calcfwhm
       
C  Automatically calculates the FWHM by sigma rejection based on the output
C  of the IRAF script <fwhm.cl>.
      
      PARAMETER(nmax=10000,th=2.)
      INTEGER nmax,i,j,n,nr
      REAL fwhm(nmax),ff(nmax)
      REAL ave,adev,sdev,var,skew,curt
      CHARACTER infile*40
      
      PRINT *,'Input file (.fwhm) ?'
      READ(*,'(A40)') infile
      
      OPEN(1,FILE=infile,STATUS='OLD')
      DO i = 1, nmax
         READ(1,*,END=99) fwhm(i)
         n = n + 1
      ENDDO
99    CLOSE(1)

C  Sigma rejection.

      nr = n
      DO WHILE (nr .GT. 0)
         DO i = 1, n
            ff(i) = fwhm(i)
         ENDDO
         nr = 0
         CALL moment(ff,n,ave,adev,sdev,var,skew,curt)
         j = 0         
         DO i = 1, n
            IF (ABS(ff(i)-ave) .GT. th*sdev) THEN
               nr = nr + 1
            ELSE
               j = j + 1
               fwhm(j) = ff(i)
            ENDIF
         ENDDO
         n = n - nr
      ENDDO

C  Print out the average and standard deviation.

      WRITE(6,*)
      WRITE(6,100) ave,sdev
100   FORMAT('Mean FWHM          =  ',F5.3,/,
     +       'Standard Deviation =  ',F5.3)
      WRITE(6,*)
      
      STOP
      END

C  Subroutine for calculating the moments.
C  Ref) Press et al., Numerical Recipes in Fortran 77, 2nd Ed., p 607.

      SUBROUTINE moment(data,n,ave,adev,sdev,var,skew,curt) 
      INTEGER n 
      REAL adev,ave,curt,sdev,skew,var,data(n) 
      INTEGER j 
      REAL p,s,ep 
      if(n.le.1) pause 'n must be at least 2 in moment' 
      s=0. 
      do j=1,n 
           s=s+data(j) 
      enddo 
      ave=s/n 
      adev=0. 
      var=0. 
      skew=0. 
      curt=0. 
      ep=0. 
      do j=1,n 
           s=data(j)-ave 
           ep=ep+s 
           adev=adev+abs(s) 
           p=s*s 
           var=var+p 
           p=p*s 
           skew=skew+p 
           p=p*s 
           curt=curt+p 
      enddo 
      adev=adev/n 
      var=(var-ep**2/n)/(n-1) 
      sdev=sqrt(var)
      if(var.ne.0.) then 
          skew=skew/(n*sdev**3) 
          curt=curt/(n*var**2)-3. 
      else 
          pause 'no skew or kurtosis when zero variance in moment'
      endif
      return 
      END
