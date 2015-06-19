      SUBROUTINE MESAGE (NO,PARM,NAME)        
C        
C     MESAGE IS USED TO QUEUE NON-FATAL MESSAGES DURING THE EXECUTION   
C     OF A MODULE, AND EXITS IF MESSAGE IS FATAL        
C        
C     REVISED 1/92 BY G.CHAN/UNISYS.        
C     IF MESSAGE IS FATAL AND DIAG 1 IS ON -        
C        
C     IBM, CDC AND UNIVAC - PRINT THE MESSAGE(S), GIVE A CORE DUMP AND  
C     CALL PEXIT        
C        
C     VAX OR UNIX (MACHINE TYPE .GE. 5) - IF LAST MESSAGE IS NOT INSUFF.
C     CORE OR INSUFFICIENT TIME, AND FATAL ERROR IS NOT IN LINK 1, PRINT
C     ONLY THE MESSAGE NO(S). AND GIVE AN ERROR TRACEBACK. NO CORE DUMP.
C     TO MAKE SURE THAT THE CURRENT MODULE (WHICH CALLS FATAL MESSAGE)  
C     IS UTILL IN CORE, THE MESSAGE PRINTOUT MODULE CAN NOT BE CALLED,  
C     AND THEREFORE THE TEXT(S) OF THE MESSAGE(S) CAN NOT BE PRINTED.   
C        
      INTEGER         PARM,NAME(2)        
      COMMON /SYSTEM/ IBUF,NOUT,DUM(19),LINKNO        
      COMMON /MACHIN/ MACH        
      COMMON /MSGX  / N,M,MSG(4,1)        
      DATA    LINK1 / 4HNS01 /        
C        
C     N        =  CURRENT NUMBER OF MESSAGES STORED        
C     M        =  MAXIMUM NUMBER POSSIBLE        
C     MSG(4,I) =  STORAGE SPACE FOR THE MESSAGE PARAMETERS        
C        
      N = N + 1        
      IF (N .LE. M) GO TO 10        
      N = M        
      IF (NO .GT. 0) GO TO 120        
C        
   10 MSG(1,N) = NO        
      MSG(2,N) = PARM        
      MSG(3,N) = NAME(1)        
      MSG(4,N) = NAME(2)        
      IF (NO .GT. 0) GO TO 120        
C        
C     MESSAGE IS FATAL, TERMINATE RUN        
C        
      CALL SSWTCH (1,J)        
      IF (J    .EQ. 0) GO TO 110        
      IF (MACH .GE. 5) GO TO 20        
C        
C     IBM, CDC AND UNIVAC        
C        
      CALL PDUMP        
      GO TO 110        
C        
C     VAX, UNIX (MACHINE TYPE 5 AND HIGHER)        
C        
   20 IF (LINKNO .EQ. LINK1) GO TO 110        
      I  = IABS(MSG(1,N))        
      IF (I.EQ.8  .OR. I.EQ.119 .OR. I.EQ.45 .OR. I.EQ.50) GO TO 110    
C             INSUFF. CORE             INSUFFICIENT TIME        
C        
      IF (I .NE. 30) GO TO 30        
      J  = MSG(2,N)        
C        
C     INSUFFECIENT CORE        
      IF (J.EQ.142 .OR. J.EQ.289 .OR. J.EQ.296 .OR. J.EQ.253 .OR.       
     1    J.EQ.365) GO TO 110        
C        
C     INSUFFECIENT TIME        
      IF (J.EQ.234 .OR. J.EQ.228) GO TO 110        
C        
   30 WRITE (NOUT,40) N        
   40 FORMAT ('0*** DUE TO SYSTEM ERROR-TRACEBACK, THE TEXT(S) OF THE ',
     1        'FOLLOWING',I3,' MSG NO(S). CAN NOT BE PRINTED')        
      DO 90 K = 1,N        
      I = MSG(1,K)        
      IF (IABS(I) .EQ. 30) GO TO 50        
      J = 3000 + IABS(I)        
      GO TO 60        
   50 I = MSG(2,K)        
      J = 2000 + IABS(I)        
   60 WRITE  (NOUT,70) I,J        
   70 FORMAT (5X,'ERROR',I4,' (or ',I5,1H))        
      IF (I.NE.30 .AND. MSG(2,K).GT.100 .AND. MSG(2,K).LT.400)        
     1    WRITE (NOUT,80) MSG(2,K)        
   80 FORMAT (1H+,30X,'GINO UNIT=',I4)        
   90 CONTINUE        
      WRITE  (NOUT,100)        
  100 FORMAT (/5X,'(SEE MESSAGES IN USER MANUAL SECTIONS 6.4 AND 6.5,', 
     1        ' AND IGNORE ANY COMPUTER FATAL MESSAGE HEREAFTER ',      
     2        'OR IN THE LOG FILE)')        
C        
C     FORCE A SYSTEM FATAL ERROR FOR TRACEBACK        
C        
      CALL ERRTRC ('MESAGE  ',105)        
C        
  110 CALL MSGWRT        
      CALL PEXIT        
  120 RETURN        
      END        