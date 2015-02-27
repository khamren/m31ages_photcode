      PROGRAM GOODPSF
C
C=====================================================================
C
C This program generates a file with only good PSF candidates.
C
C=====================================================================
C
C -- This is declaring a bunch of variables
C
      INTEGER nmax
      REAL minchi
C
C -- This is the internal daophot CHI value that is measuring "fluffiness"
C -- A perfect star would have CHI=1
C -- (CHI - 1.0) < 0.5 is a good set of cuts.
C
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
C -- READING IN THE FILE
C=====================================================================
C
      OPEN(11,FILE=lstfile)
C
C -- Reading the header of the lst file that contains the daophot gobbly gook
C
      READ(11,'(A69)') line1
      READ(11,'(A69)') line2
      READ(11,*)
      nstar = 0
C
C -- reading in each line of the file putting the parameters into 
C --  arrays taht were initialized in the previous block
C -- The columns are ID#, X pos, Y pos, Mag, Mag_err, Sky Value
C 
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
C
C -- this should be reading the section of the log file that contains
C --  the chi-squred fits to the PSF
C -- An example of this input is given in 
C --                    /example_splash_files/allframe/obj175_1.psf.log
C --  in lines 53-69.
C -- its a stupid output: starid chi^2 optional flag
C -- if a star is saturated it will say saturated instead of chi^2 
C --  (this might be where the code breaks)
C -- another option is chi^2 = defective, I don't know 100% what that means, but 
C --  assumed it was really bad and I remove those stars.
C -- **Looks like the shell script tries to remove the saturate & defective**
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
C -- format of the file
69    FORMAT(I7, F7.3, 1X, A1, 1X)
C           
C=====================================================================
C  Process PSF candidates flags and prune out.
C=====================================================================
C
C -- number of stars from the readin in the previous line
      npsf = nstar
C -- stupid complicated reading of the file
      OPEN(21,FILE=outfile)
      WRITE(21,'(A69)') line1
      WRITE(21,'(A69)') line2
      WRITE(21,*)
      DO i = 1, nstar
C
C -- check to see what the flags are, if its ? or *, then reject the star
C --  practically, this is 2x and 3x sigma of the chi^2 values for the set of stars
C -- Then its testing the values to fit in some parameter range
C --
C 
         IF (flag(i).NE.'?' .AND. flag(i).NE.'*' 
     .                      .AND. chi(i).LT.minchi) THEN
            DO j = 1, nstar
C
C -- matching stars from the .lst file and the .chi file by their IDs
C --  if a star passes the previous test, then write out the information from
C --  the .lst file to a new .lst file that will be used for the next PSF run
C -- 
C
               IF (id(j).EQ.iid(i)) THEN
                  WRITE(21,70) id(j),x(j),y(j),mag(j),err(j),sky(j)
                  GOTO 1
               ENDIF
            ENDDO
         ELSE
C -- if we reject the star, then we decrement the number of input stars
            npsf = npsf -1
         ENDIF
1     ENDDO
      CLOSE(21)
70    FORMAT(I7,5F9.3):
C
C -- Writing some results to the screen here based on the count of stars
C --  where npsf is decremented for each star removed because there is
C --  no way in fortran to say n_elements(array)
C
      WRITE(6,71) npsf,nstar
71    FORMAT(/'You have ',I3,' (out of ',I3,') PSF candidates left.'/)
C
      STOP
      END




