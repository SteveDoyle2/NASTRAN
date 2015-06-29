      SUBROUTINE SOFUT        
C        
C     THE PURPOSE OF THE MODULE IS TO PERFORM THE TASKS OF ALTERING THE 
C     SOF FILE IN ORDER TO EDIT, PURGE, AND EQUIVALENCE THE DATA ITEMS  
C     OF SELECTED SUBSTRUCTURES.  THE CALLING SEQUENCE TO THE MODULE IS 
C        
C     SOFUT     //V,N,DRY/C,N,NAME1/C,N,OPER/C,N,OPT/C,N,NAME2/        
C                 C,N,PREFX/C,N,IA/C,N,IB/C,N,IC/C,N,ID/C,N,IE $        
C        
      LOGICAL         DITUP        
      INTEGER         DRY,OPER,OPT,PREFX,SYSBUF,DELE,RENAM,NAME(2)      
      CHARACTER       UFM*23,UWM*25        
      COMMON /XMSSG / UFM,UWM        
      COMMON /BLANK / DRY,NAME1(2),OPER(2),OPT,NAME2(2),PREFX(2),       
     1                ITEMS(10)        
      COMMON /SOF   / SSS(33),DITUP        
CZZ   COMMON /ZZSOFU/ IZ(1)        
      COMMON /ZZZZZZ/ IZ(1)        
      COMMON /SYSTEM/ SYSBUF,NOUT        
      DATA    IEDIT , IDEST,IEQUIV / 4HEDIT    ,4HDEST ,4HEQUI   /      
      DATA    IPRNT / 4HSOFP/        
      DATA    DELE  / 4HDELE/        
      DATA    RENAM / 4HRENA/        
      DATA    NAME  / 4HSOFU,4HT   /        
      DATA    ISCR1 / 301   /        
C        
      ITASK = 0        
      IF (OPER(1) .EQ.  IEDIT) ITASK = 1        
      IF (OPER(1) .EQ.  IDEST) ITASK = 2        
      IF (OPER(1) .EQ. IEQUIV) ITASK = 3        
      IF (OPER(1) .EQ.  IPRNT) ITASK = 4        
      IF (OPER(1) .EQ.   DELE) ITASK = 5        
      IF (OPER(1) .EQ.  RENAM) ITASK = 6        
      IF (ITASK .EQ. 0) GO TO 1000        
C        
C     ALLOCATE BUFFERS FOR THE SOF UTILITY SUBROUTINES        
C        
      NZ  = KORSZ(IZ)        
      IF (3*SYSBUF .GT. NZ) CALL MESAGE (-8,0,NAME(1))        
      IB1 = NZ  - SYSBUF + 1        
      IB2 = IB1 - SYSBUF - 1        
      IB3 = IB2 - SYSBUF        
      CALL SOFOPN (IZ(IB1),IZ(IB2),IZ(IB3))        
      NZ  = IB3 - 1        
      GO TO (20,30,40,130,180,200), ITASK        
C        
C     EDIT OPERATION        
C        
   20 CALL EDIT (NAME1(1),OPT,ITEST)        
      GO TO 50        
C        
C     DESTROY OPERATION        
C        
   30 I = NZ/2 + 1        
      CALL DSTROY (NAME1(1),ITEST,IZ,IZ(I),I-1)        
      GO TO 50        
C        
C     EQUIVALENCE OPERATION        
C        
   40 I = NZ/2 + 1        
      CALL SETEQ (NAME1,NAME2,PREFX,DRY,ITEST,IZ,I-1)        
C        
C     TEST RETURN CODE        
C        
   50 GO TO (110,110,110,60,110,70,110,80,90,100), ITEST        
   60 WRITE (NOUT,1010) UWM,NAME1        
      GO TO 100        
   70 WRITE (NOUT,1020) UWM,NAME1        
      GO TO 100        
   80 WRITE (NOUT,1030) UWM,NAME2        
      GO TO 100        
   90 WRITE (NOUT,1040) UWM,NAME2        
  100 DRY = -2        
  110 CALL SOFCLS        
      GO TO 1100        
C        
C     PRINT OPERATIONS        
C        
  130 IF (OPT) 140,140,150        
C        
C     PRINT SOF TABLE OF CONTENTS (DIT MDI)        
C        
  140 CALL SOFTOC        
      IF (OPT .EQ. 0) GO TO 170        
C        
C     PRINT SOF DATA ITEMS        
C        
  150 DO 160 I = 1,5        
      II = ITTYPE(ITEMS(2*I-1))        
      IF (II) 160,152,154        
C        
C     TABLE ITEM        
C        
  152 CALL ITMPRT (NAME1,ITEMS(2*I-1),NZ,OPT)        
      GO TO 160        
C        
C     MATRIX ITEM        
C        
  154 CALL MATWRT (ISCR1,NAME1,ITEMS(2*I-1),NZ)        
C        
  160 CONTINUE        
  170 CALL SOFCLS        
      GO TO 1100        
C        
C     DELETE OPERATION        
C        
  180 DO 190 I = 1,10        
  190 CALL DELETE (NAME1,ITEMS(I),ITEST)        
      GO TO 50        
C        
C     RENAME OPERATION        
C        
  200 CALL SRENAM (NAME1,NAME2,IZ(1),NZ,ITEST)
      GO TO 50        
C        
C     ERROR MESSAGES        
C        
 1000 WRITE  (NOUT,1001) UWM,OPER(1),OPER(2)        
 1001 FORMAT (A25,' 6217, MODULE SOFUT - ',2A4,' IS AN ILLEGAL ',       
     1       'PARAMETER NAME.')        
      GO TO 1100        
C        
 1010 FORMAT (A25,' 6212, MODULE SOFUT - THE SUBSTRUCTURE ',2A4,        
     1       ' DOES NOT EXIST.')        
C        
 1020 FORMAT (A25,' 6218, MODULE SOFUT - THE SUBSTRUCTURE ',2A4,1X,     
     1       'CANNOT BE DESTROYED BECAUSE IT IS AN IMAGE SUBSTRUCTURE.')
C        
 1030 FORMAT (A25,' 6219, MODULE SOFUT - RUN EQUALS DRY OR STEP AND ',  
     1       'SUBSTRUCTURE ',2A4, /33X,        
     2       'OR ONE OF THE NEW NAMES ALREADY EXISTS.')        
C        
 1040 FORMAT (A25,' 6220, MODULE SOFUT - RUN = GO AND SUBSTRUCTURE ',   
     1        2A4,' OR ONE OF THE NEW NAMES DOES NOT EXIST')        
C        
 1100 RETURN        
      END        
