      SUBROUTINE RFOPEN (MEMBER,LU)        
C        
C     THIS .MIS ROUTINE OPENS THE RIGID FORMAT FILE, AS AN ORDINARY     
C     FORTRAN FILE. USE REGULAR FORTRAN READ TO READ THE FILE        
C        
C     ENTRY POINT RFCLSE TO CLOSE IT        
C        
C     IF RIGID FORMAT FILE OPENS OK, LU IS THE FORTRAN UNIT NUMBER      
C     OTHERWISE, LU = 0        
C        
C     THIS ROUTINE REPLACES ALL THE MACHINE DEPENDENT DSXOPN, DSXCLS,   
C     DSXREA, AND DSXFRE ROUTINES. PLUS DSXRDS, DSXIO, AND DSXSIO IN    
C     IBM VERSION, AND DSXRET AND DSXZER IN CDC        
C        
C     NOTE - FORTRAN UNIT 'IN' IS USED TO READ THE RIGID FORMAT FILE.   
C            UNIT 'IN' IS SYNCHRONOUS WITH ANY READFILE OR NESTED       
C            READFILE OPERATION.        
C        
C     WRITTEN BY G.CHAN/UNISYS.   10/1990        
C        
      INTEGER         MEMBER(2),FACSF        
      CHARACTER*1     BK,MB1(8)        
      CHARACTER       MB5*5,MB6*6        
      CHARACTER*8     MB8,FREE8,ADD(3)        
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25        
      COMMON /XMSSG / UFM,UWM,UIM,SFM        
      COMMON /MACHIN/ MACH        
      COMMON /XXREAD/ IN        
      COMMON /SYSTEM/ IBUF,NOUT,NOGO        
      EQUIVALENCE     (MB1(1),MB5,MB6,MB8)        
      DATA    BK,     ADD(1),   ADD(3), FREE8     /        
     1        ' '   , '@ADD,E ',' .  ', '@FREE   '/        
C        
      CALL A42K8 (MEMBER(1),MEMBER(2),MB8)        
c      IF (MACH .EQ. 3) GO TO 30        
      IN = IN + 1        
      IF (IN .LT. 60) IN = 60        
      J  = 5        
      IF (MB1(6) .NE. BK) J = 6        
C        
C     ALL OTHERS MACHINES -        
C        
C     'SHEARED AND READONLY' ARE NEEDED FOR VAX AND ULTRIX        
C        
 50   IF (J .EQ. 5) OPEN (UNIT=IN,FILE=MB5,ACCESS='SEQUENTIAL',ERR=100, 
     1                    FORM='FORMATTED',STATUS='OLD',
     2                    action='read')
C        
C     VERIFY FILE EXISTANCE        
C        
 80   READ (IN,90,ERR=100,END=100) J        
 90   FORMAT (A1)        
      REWIND IN        
      LU = IN        
      GO TO 130

 100  WRITE  (NOUT,110) SFM,MB8        
 110  FORMAT (A25,', RFOPEN CAN NOT OPEN ',A8)        
      IF (MACH.EQ.5 .OR. MACH.EQ.21) WRITE (NOUT,115)        
 115  FORMAT (/5X,'MAKE SURE RIGID FORMAT FILE IS OPENED WITH "READONLY"
     1 IN SUBROUTINE RFOPEN.MIS', /5X,'IF IT IS NOT, MAKE SURE FILE ',  
     2 'PROTECTION IS SET FOR READ+WRITE')        
C        
      IF (MACH.GT.7 .AND. MACH.NE.21) WRITE (NOUT,120) MACH        
 120  FORMAT (5X,'MACHINE',I4,' IS NOT AVAILABLE/RFOPEN')        
      LU   = 0        
      NOGO = 1        
C        
 130  continue
      RETURN
C        
C        
      ENTRY RFCLSE (LU)
C     =================
      IF (MACH .EQ. 3) GO TO 150        
      IF (LU  .LT. 60) WRITE (NOUT,140) SFM,LU        
 140  FORMAT (A25,'. RFCLSE/RFOPEN ERROR.  LU =',I4)        
      CLOSE (UNIT=LU)        
      IN = IN - 1        
      IF (IN .LT. 60) IN = 0        
      GO TO 160        
C        
 150  ADD(1) = FREE8        
      J = FACSF(ADD)        
 160  LU = 0        
      RETURN        
      END        
