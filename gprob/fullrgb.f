        program rgb

        implicit none
        REAL X,Y,M,Merr,T,Terr,D,Derr,chi,sharp,hicase
        REAL RA,DEC,l,b,EBV,EMT,EMD,AM,radii,MT,locase
        REAL increment,MD,M1,maglim
        integer id,ROUF,CHIF,LMAX,totnum
        character*40 inpfile
        character*40 outfile


        write(6,*)'Enter input file: '
        read(5,*) inpfile

        write(6,*)'Enter output file: '
        read(5,*) outfile

        write(6,*)'Enter magnitude limit:'
        read(5,*) maglim

        open(10,file=inpfile)
        open(12,file=outfile)
C        open(13,file='locase.db')
C        open(14,file='hicase.db')
        open(15,file='data.db')
        open(16,file='data2.db')


200	read(10,*,END=100)id,X,Y,M,Merr,T,Terr,D,Derr,chi,sharp,RA,DEC,
     &	l,b,EBV,EMT,EMD,AM,CHIF,ROUF,radii
        MT=M-T-EMT
        M1=M-AM
        MD=M-D-EMD	
         if(MT.lt.(1.1)) then		
          hicase=39.98699-37.07959*(MT)+16.95270*(MT**2)
         else
          hicase=31.90549-16.79336*(MT)+4.985536*(MT**2)
         endif
        locase=134.9220-192.0889*MT+105.4961*(MT**2)-19.38386*(MT**3)
C        write(13,*) MT,locase
C        write(14,*) MT,hicase
        if(M1.ge.hicase.and.M1.le.locase.and.MT.lt.1.8.and.
     &		M1.le.maglim.and.MD.gt.(-.313*(MT)+0.3691).and.
     &          MT.lt.(2.0).and.MD.gt.(-0.05)) then
        write(15,45) id,MT,M1,MD
        totnum=totnum+1
        write(12,*) id 
        goto 200
       else
        write(16,45) id,MT,M1,MD 
       endif
       goto 200
45     format(I8,3f9.4) 


100     stop
        end
