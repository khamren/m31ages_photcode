      PROGRAM moscen
      
      INTEGER i
      REAL*8 cenra,cendec,ra(8),dec(8)
      
      WRITE(6,*)
      WRITE(6,10)
10    FORMAT('What is the central RA and Dec in decimal ?')
      READ(5,*) cenra,cendec
      
      ra(1)  = cenra  + 0.00000478D0*2048D0
      dec(1) = cendec + 0.0000717D0*3072D0
      ra(2)  = cenra  + 0.00000478D0*2048D0
      dec(2) = cendec + 0.0000717D0*1024D0
      ra(3)  = cenra  + 0.00000478D0*2048D0
      dec(3) = cendec - 0.0000717D0*1024D0
      ra(4)  = cenra  + 0.00000478D0*2048D0
      dec(4) = cendec - 0.0000717D0*3072D0
      ra(5)  = cenra  - 0.00000478D0*2048D0
      dec(5) = cendec + 0.0000717D0*3072D0
      ra(6)  = cenra  - 0.00000478D0*2048D0
      dec(6) = cendec + 0.0000717D0*1024D0
      ra(7)  = cenra  - 0.00000478D0*2048D0
      dec(7) = cendec - 0.0000717D0*1024D0
      ra(8)  = cenra  - 0.00000478D0*2048D0
      dec(8) = cendec - 0.0000717D0*3072D0
      
      WRITE(6,*)
      DO i = 1, 8
         WRITE(6,20) i,ra(i),dec(i)
      ENDDO
20    FORMAT('Chip',I1,'  -  RA :',F11.7,' , Dec :',F11.7)
      WRITE(6,*)
      END
      
      