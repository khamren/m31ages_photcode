      PROGRAM apcor

C  =====================================================================
C
C             APERTURE CORRECTION and EXPOSURE TIME CORRECTION
C
C                                      March 13, 2001
C                                Programmed by Sangmo Sohn
C               (University of Virginia, Dept. of Astronomy)
C
C  =====================================================================

C  This simple program derives the instrumental magnitude for a given frame 
C  by performing the aperture correction and the exposure time correction.
C  The program requires the following files ;
C
C  aalsfile : output file from the ALLSTAR task run only on PSF stars
C  atotfile : output file from the DAOGROW task
C  alsfile  : output file from the ALLSTAR task run on all stars
C
C  The exposure time of the frame in seconds (exptime) is also required.
C  
C  First, the program compares the magnitude in aalsfile with the 
C  magnitude in atotfile for each star. This magnitude difference is 
C  written in the adelfile. If a star has an unusual difference value,
C  the star should be deleted from both aalsfile and atotfile and this
C  program should be run again.
C  Now the program takes the mean value of the difference which is the 
C  actual apperture correction value. Along with the exposure time,
C  this aperture correction value is used to derive the instrumental
C  magnitude of a star by evaluating the following equation ;
C
C           cmag = amag + 2.5*log10(exptime) - ac
C
C  where cmag = instrumental magnitude, amag = magnitude in alsfile,
C  exptime = exposure time, and ac = aperture correction value.


      IMPLICIT REAL(a-z)
      INTEGER i,j,id1,id2,n
      CHARACTER*30 aalsfile,atotfile,adelfile
      CHARACTER*30 alsfile,nalsfile
      CHARACTER*69 line1,line2

      n = 0
      totac = 0.
      PRINT *,'Output file of ALLSTAR task done on PSF stars ? (*a.als)'
      READ(*,'(A30)') aalsfile
      PRINT *,'Output file of DAOGROW task ? (*a.tot) '
      READ(*,'(A30)') atotfile
      PRINT *,'File for the magnitude difference of each star ?(*a.del)'
      READ(*,'(A30)') adelfile
           
      OPEN(10,FILE=adelfile)
      OPEN(11,FILE=aalsfile)
      READ(11,'(2(/))')
      DO i = 1, 500
         READ(11,50,ERR=99) id1,x1,y1,amag,amagerr,sky,it,chi,sharp
         OPEN(12,FILE=atotfile)
         READ(12,'(2(/))')
         DO J = 1, 500
            READ(12,60,ERR=9) id2,x2,y2,tmag,tmagerr
            IF (id1.EQ.id2 .AND. tmag.LT.40.) THEN
               n = n + 1
               del = amag - tmag
               WRITE(10,'(1X,I5,F9.4)') id2,del
               totac = totac + del
            ENDIF
         ENDDO
9        CLOSE(12)
      ENDDO
50    FORMAT(1X,I5,3F9.3,F9.4,F9.3,5X,F4.0,2F9.3)
60    FORMAT(1X,I5,2F9.3,2F9.4)
99    CLOSE(11)
      CLOSE(10)

      ac = totac / REAL(n)
      WRITE(*,'(A22,F7.4)') ' Mean aperture correction value =',ac
      PRINT *,' '
      PRINT *,'File with results of ALLSTAR for all stars ? (*.als)'
      READ(*,'(A30)') alsfile
      PRINT *,'Exposure time in seconds ? '
      READ(*,'(F8.3)') exptime
      PRINT *,'New file name for corrected magnitudes ? (*.nals)'
      READ(*,'(A30)') nalsfile

      OPEN(11,FILE=alsfile)
      OPEN(13,FILE=nalsfile)
      READ(11,'(A69)') line1
      READ(11,'(A69)') line2
      READ(11,*)
      WRITE(13,'(A69,(/),A69)') line1,line2
      WRITE(13,*)

      DO i = 1, 100000
         READ(11,50,ERR=999) id,x,y,amag,amagerr,sky,it,chi,sharp
         cmag = amag + 2.5*log10(exptime) - ac
         WRITE(13,50) id,x,y,cmag,amagerr,sky,it,chi,sharp
      ENDDO

999   CLOSE(11)
      CLOSE(13)
      STOP
      END
