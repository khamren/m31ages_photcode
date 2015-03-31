C######################################################################
C# compile the 
C# FORTRAN source using the following command.
C#
C# $ g77 lstfilter.f -o lstfilter
C#
C# Tested under gcc version 3.3.5.
C#
C######################################################################
C#
C#      PROGRAM LSTFILTER
C
C=====================================================================
C
C This program filters out bad sources from the .lst file
C
C=====================================================================
C
      INTEGER nmax
      PARAMETER(nmax=50000)
C
C -- Declaring all the things
C
      INTEGER i,j,nstar,nnstar,nleft
      INTEGER id(nmax),cid(nmax)
      REAL x(nmax),y(nmax),mag(nmax),err(nmax),sky(nmax)
      REAL sharp(nmax),round(nmax),dum
      CHARACTER flag(nmax)*1,line*85
      CHARACTER*60 lstfile,coofile,outfile
      CHARACTER*69 line1,line2
C      
C -- interaction with the screen to get the input files      
C      
C -- this is the .lst file made by PIck and used to fit the PSF
      PRINT *,'File with PSF candiates (.lst)?'
      READ(*,'(A60)') lstfile
C -- this is the .coo files made by FInd       
      PRINT *,'File with FIND output (.coo)?'
      READ(*,'(A60)') coofile
C -- this is the edited lst file output by this program      
      PRINT *,'Output file (.lst1) ?'
      READ(*,'(A60)') outfile
C
C=====================================================================
C Read in .lst file
C=====================================================================
C
      OPEN(11,FILE=lstfile)
C      
C -- reading in the header to all of the DAOphot files
C
      READ(11,'(A69)') line1
      READ(11,'(A69)') line2
      READ(11,*)
      nstar = 0
      DO i = 1, nmax
C
C -- from the list file readin, ID#, Xcenter, Ycenter, Mag, Magerr, sky
C --  for each star
C
         READ(11,*,END=9) id(i),x(i),y(i),mag(i),err(i),sky(i)
C
C -- count the number of lines read in         
C
         nstar = nstar + 1
      ENDDO
9     CLOSE(11)
C           
C=====================================================================
C Read in .coo file
C=====================================================================
C
      OPEN(12,FILE=coofile)
      READ(12,*)
      READ(12,*)
      READ(12,*)
      nnstar = 0
      DO i = 1, nmax
C 
C -- this is only pulling in the star id (cid), sharp (sharp), and round (round)
C --  from the coo file. everything else is written to the same dummy variable
C -- sharp and round are the "internal" and pre-PSF fit versions of the output sharp and chi
C --  these "internal" versions are based on idealized profiles instead of the PSF itself
C -- we can use these to a priori reject galaxies, cosmic rays/bad pixels, and weird things
C --  from ever being put through the PSF fit algorithm 
C -- aside: why daophot doesn't do this automatically is a ... question for stetson.
C 
         READ(12,*,END=99) cid(i),dum,dum,dum,sharp(i),round(i),dum
         nnstar = nnstar + 1
      ENDDO
99    CLOSE(12)
C           
C=====================================================================
C Filter out non-stellar sources
C=====================================================================
C
C --  
C 
      nleft = nstar
C 
C  -- opening the output file
C 
      OPEN(21,FILE=outfile)
C 
C -- writing the stupid 2 daophot header lines to the file verbatim + 1 empty line
C
      WRITE(21,'(A69)') line1
      WRITE(21,'(A69)') line2
      WRITE(21,*)
C
C -- now, we are going to loop through the files
C
      DO i = 1, nstar
         DO j = 1, nnstar
C
C -- matching on the star IDs in the two files
C
            IF (id(i).EQ.cid(j)) THEN
C 
C -- applying a cut on the sharp value 
C --  why just the sharp value, when we also have the chi value??
C --   like the # of licks to the center of the tootsie pop, 
C --                                            the world may never know.
C
               IF (sharp(j).GT.0.3 .AND. sharp(j).LT.1.0) THEN
C
C -- writing the things from lst back out to the new lst file
C
                  WRITE(21,70) id(i),x(i),y(i),mag(i),err(i),sky(i)
               ELSE
C
C -- if the star doesn't cut it, decrement our counter variable
C
                  nleft = nleft-1
               ENDIF
C
C -- one of those fucking weird things that fortran does to end the loop
C
               GOTO 111
            ENDIF
         ENDDO
111   ENDDO
70    FORMAT(I7,5F9.3)
C      
C
C -- write out the results of the test
C
      WRITE(6,71) nleft,nstar
71    FORMAT(/'You have ',I3,' (out of ',I3,') stars left.'/)
C
      STOP
      END
