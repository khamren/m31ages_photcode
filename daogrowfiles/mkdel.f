      PROGRAM mkdel

C  Run this program AFTER running DAOGROW.

      IMPLICIT REAL(a-z)
      INTEGER i,j,k,id1,id2,n,nf
      CHARACTER inffile*30,infile*8
      CHARACTER*12 aalsfile(1000),atotfile(1000),adelfile(1000)

C  Read in file names from .inf file.

      PRINT *,'.inf file used in DAOGROW ?'
      READ(*,'(A30)') inffile
      OPEN(1,FILE=inffile)
      DO i = 1, 1000
         READ(1,'(1X,A8)',END=999) infile
         aalsfile(i) = infile//'.als'
         atotfile(i) = infile//'.tot'
         adelfile(i) = infile//'.del'
         nf = nf + 1
      ENDDO
999   CLOSE(1)
           

      DO k = 1, nf
         n = 0
         totac = 0.
         OPEN(10,FILE=adelfile(k))
         OPEN(11,FILE=aalsfile(k))
         READ(11,'(2(/))')
         DO i = 1, 500
            READ(11,50,ERR=99) id1,x1,y1,amag,amagerr,sky,it,chi,sharp
            OPEN(12,FILE=atotfile(k))
            READ(12,'(2(/))')
            DO j = 1, 500
               READ(12,60,ERR=9) id2,x2,y2,tmag,tmagerr
               IF (id1.EQ.id2 .AND. tmag.LT.40.) THEN
                  n = n + 1
                  del = amag - tmag
                  WRITE(10,'(1X,I5,F9.4)') id2,del
                  totac = totac + del
               ENDIF
            ENDDO
9           CLOSE(12)
         ENDDO
99       CLOSE(11)
         CLOSE(10)
      ENDDO

50    FORMAT(I6,3F9.3,F9.4,F9.3,F9.0,2F9.3)
60    FORMAT(1X,I5,2F9.3,2F9.4)

      STOP
      END
