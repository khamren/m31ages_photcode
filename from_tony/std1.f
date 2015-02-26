      PROGRAM std1

C  =====================================================================
C
C               STANDARD STAR PHOTOMETRY - STAR SELECTION 1
C
C                                      March 13, 2001
C                                Programmed by Sangmo Sohn
C               (University of Virginia, Dept. of Astronomy)
C
C  =====================================================================

C  This simple program is used for selecting standard stars out of the
C  .ap files.

      PARAMETER(n=10000)
      INTEGER i,j,id(n),cid(n),m,mm
      CHARACTER*30 apfile,coordfile,lstfile,outfile
      CHARACTER*69 line1,line2
      REAL x(n),y(n),sky(n),err(n),stdev,skew
      REAL cx(100),cy(100),r,rold
      
      PRINT *,'Aperture photometry file ? (.ap)'
      READ(*,'(A30)') apfile
      PRINT *,'File with the coordinates of standard stars ?'
      READ(*,'(A30)') coordfile
      PRINT *,'Output file from PICKPSF task ? (.lst)'
      READ(*,'(A30)') lstfile
      PRINT *,''
      PRINT *,'Output file ?'
      READ(*,'(A30)') outfile
      
      OPEN(11,FILE=apfile)
      OPEN(12,FILE=coordfile)
      OPEN(13,FILE=lstfile)
      OPEN(14,FILE=outfile)

      READ(11,'(A69)') line1
      READ(11,'(A69)') line2
      READ(11,*)
      WRITE(14,'(A69)') line1
      WRITE(14,'(A69)') line1
      WRITE(14,*)
      
      DO i = 1, n
         READ(11,*,END=8)
         READ(11,*) id(i),x(i),y(i),mag(i)
         READ(11,*) sky(i),stdev,skew,err(i)
         m = m + 1
      ENDDO
8     CLOSE(11)
      
      READ(12,*)
      DO i = 1, 100
         READ(12,'(F7.2,F8.2)',END=9) cx(i),cy(i)
         mm = mm + 1
      ENDDO
9     CLOSE(12)
      
      DO i = 1, mm
         rold = 10.
         DO j = 1, m
            r = SQRT((cx(i)-x(j))**2 + (cy(i)-y(j))**2)
            IF (r.LT.2. .AND. r.LT.rold(i)) THEN
               rold = r
               cid(i) = id(j)
            ENDIF
         ENDDO
         IF (rold.GT.2.) cid(i) = 0
      ENDDO
      
      DO i = 1, mm
         IF (cid(i).EQ.0) THEN
            WRITE(14,50) 99999,99.999,99.999,99.999,99.999
         ELSE
            DO j = 1, m
               IF (cid(i).EQ.id(j)) THEN
                  WRITE(14,50) id(j),x(j),y(j),mag(j),sky(j)
               ENDIF
            ENDDO
         ENDIF
      ENDDO
50    FORMAT(1X,I5,4F9.3)            
      
      READ(13,'(2(/))')
      DO i = 1, n
         READ(13,*,END=99) id(i),x(i),y(i),mag(i),sky(i)
         WRITE(14,50) id(i),x(i),y(i),mag(i),sky(i)
      ENDDO
99    CLOSE(13)
      CLOSE(14)

      STOP
      END
