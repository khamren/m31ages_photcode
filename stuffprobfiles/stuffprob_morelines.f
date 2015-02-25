      program stuffprob

      implicit none
      character*40 sexfile,magfile,outfile
      real xsex,ysex,msex,merrsex,prob(120000),x,y,D,Derr
      real M,Merr,T,Terr,chi,sharp,crap
      integer i,j,id,flag(120000),offset,isave
      integer SEXMAX,LMAX,idsex(120000)

      write(6,*)'Enter the input magma file:'
      read(5,*) magfile
      open(10,file=magfile)
  
      write(6,*)'Enter the SExtractor file:'
      read(5,*) sexfile
      open(11,file=sexfile)

      write(6,*)'Enter the id offset that has been applied:'
      read(5,*)offset

      write(6,*)'Enter the output file:'
      read(5,*) outfile
      open(12,file=outfile)

      do j=1,3
         read(11,*)
      end do

      j=1
      do while(j.ne.0)
       read(11,*,END=200) idsex(j),xsex,ysex,msex,crap,merrsex,
     & flag(j),prob(j)
C       print*, idsex(j)
       j=j+1
      end do
200   SEXMAX=j-1
      
      j=1
      do while(j.ne.0)
        read(10,*,END=100) id,X,Y,D,Derr,M,Merr,T,Terr,chi,sharp
        isave=-999999
        do i=1,SEXMAX
          if((id-offset).eq.idsex(i)) then
              isave=i
          endif
        end do
        if(isave.eq.-999999) print*, 'uh oh!'
        write(12,48) id,X,Y,D,Derr,M,Merr,T,Terr,chi,sharp,
     &  flag(isave),prob(isave)
        j=j+1
      end do
100   LMAX=j-1

48    format(I8,2f9.3,8f9.4,I3,f9.4) 
      stop
      end
        
