      PROGRAM mkopt
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  MKOPT.F -- makes .opt and .als.opt files for running DAOPHOT & ALLSTAR
C
C  DESCRIPTION - This program first reads in the <observation>.log file
C                to get exposure time, readout noise, gain, hi-limit,
C                and average FWHM for each image. The .opt files are
C                then made from parameters based on these and the 
C                common values. Each parameter is explained in program.
C                The .log file has to be in the form of following :
C                IMAGE  EXPTIME  GAIN  RDNOISE  FWHM  HI
C                An example is shown below
C
C      Image     exptime    gain  rdnoise   FWHM   Hi-limit Filters
C       Name       (s)    (e/ADU)   (e)   (pixels)  (ADU)
C    --------------------------------------------------------------
C    obj2059_1   300.000    2.30    8.20    4.47    30000.    M
C
C
C  Program written by Sangmo Tony Sohn
C
C  HISTORY -  9/21/2000 : made from scratch
C            12/18/2001 : modified with comments
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT REAL(a-z)
      PARAMETER(m=100)
      DIMENSION ga(m),re(m),hi(m),fw(m),fi(m),ps(m),is(m),os(m)
      CHARACTER logfile*30,image(m)*9,optfile(m)*13,alsoptfile(m)*17
      INTEGER i,n

C      
C  Common values that should be applied to every image.
C
C  (1) DAOPHOT parameters
C
C  LO    : Low good datum (7. works fine on most imags)
C  TH    : Threshold (3.0 works fine)
C  LS,HS : Low and high sharpness (default : 0.2 - 1.0)
C  LR,HR : Low roundness and high roundness (default : -1.0 - 1.0)
C  WA    : Watch progress
C  VA    : Variable PSF
C  AN    : Analytic model PSF
C  EX    : Extra PSF cleaning passes
C  PE    : Percent error
C  PR    : Profile error
C
C  (2) ALLSTAR parameters
C
C  CR    : Clipping range (leave it)
C  CE    : Clipping exponent (leave it)
C  MA    : Maximum group size
C  RED   : Redetermine centroid (0 = no, 1 = yes)
C

      LO =  7.0
      TH =  3.5
      LS =  0.2
      HS =  1.0
      LR = -1.0
      HR =  1.0
      WA = -2
      VA =  2
      AN = -6
      EX =  5
      PE =  0.75
      PR =  5.00
      CR =  2.5
      CE =  6.0
      MA = 50.
      RED = 1.0
      WA2 = 0.0
      
C
C  Frame-specific parameters.
C
C  GA    : gain (e/ADU)
C  RD    : readout noise (e)
C  RE    : readout noise (ADU)
C  FW    : FWHM
C  HI    : hi good datum in ADU - saturation level
C  FI    : fitting radius
C  PS    : PSF radius
C  IS,OS : inner and outer sky annalus
C
      
C
C  Read in the .log file.
C

      PRINT *,'Log file ?'
      READ(*,'(A30)') logfile
      OPEN(1,FILE=logfile)
      READ(1,*)
      READ(1,*)
      READ(1,*)            
      DO i = 1, 100
         READ(1,10,ERR=99) image(i),exptime,GA(i),RD,FW(i),HI(i)
         optfile(i) = image(i)//'.opt'
         alsoptfile(i) = image(i)//'.als.opt'
         RE(i) = RD/GA(i)
         FI(i) = FW(i)
         PS(i) = 4.0*fw(i)
         IS(i) = FI(i) - 1
         OS(i) = PS(i) + 1
         n = n + 1
      ENDDO
10    FORMAT(A9,F10.3,3F8.2,F10.0)
99    CLOSE(1)

C
C  Write into <image>.opt
C

      DO i = 1, n
        OPEN(2,FILE=optfile(i))
        WRITE(2,50) RE(i),GA(i),LO,HI(i),FW(i),TH,LS,HS,LR,HR,
     &              WA,FI(i),PS(i),VA,AN,EX,PE,PR
50      FORMAT('RE = ',F8.2       /
     &         'GA = ',F8.2       /
     &         'LO = ',F8.2       /
     &         'HI = ',F8.2       /
     &         'FW = ',F8.2       /
     &         'TH = ',F8.2       /
     &         'LS = ',F8.2       /
     &         'HS = ',F8.2       /
     &         'LR = ',F8.2       /
     &         'HR = ',F8.2       /
     &         'WA = ',F8.2       /
     &         'FI = ',F8.2       /
     &         'PS = ',F8.2       /
     &         'VA = ',F8.2       /
     &         'AN = ',F8.2       /
     &         'EX = ',F8.2       /
     &         'PE = ',F8.2       /
     &         'PR = ',F8.2       /)
        CLOSE(2)
      ENDDO     

C
C  Write into <image>.als.opt
C

      DO i = 1, n
        OPEN(3,FILE=alsoptfile(i))
        WRITE(3,60) FI(i),IS(i),OS(i),RED,WA2,PE,PR,CR,CE,MA
60      FORMAT('FI = ',F8.2       /
     &         'IS = ',F8.2       /
     &         'OS = ',F8.2       /
     &         'RE = ',F8.2       /
     &         'WA = ',F8.2       /
     &         'PE = ',F8.2       /
     &         'PR = ',F8.2       /
     &         'CR = ',F8.2       /
     &         'CE = ',F8.2       /
     &         'MA = ',F8.2       /)
        CLOSE(3)
      ENDDO     

      STOP
      END
