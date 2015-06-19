      SUBROUTINE CHKOPN (NAME)        
C        
C     CHECKS IF A CALL TO SOFOPN HAS BEEN MADE.        
C        
      LOGICAL         OPNSOF        
      DIMENSION       NAME(2)        
      CHARACTER       UFM*23        
      COMMON /XMSSG / UFM        
      COMMON /SOFCOM/ SOFDUM(25),OPNSOF        
      COMMON /SYSTEM/ NBUFF,NOUT        
C        
      IF (OPNSOF) GO TO 20        
      WRITE  (NOUT,10) UFM,NAME        
   10 FORMAT (A23,' 6204, SUBROUTINE ',2A4,' - THE SUBROUTINE SOFOPN ', 
     1       'SHOULD BE CALLED PRIOR TO ANY OF THE SOF UTILITY ',       
     2       'SUBROUTINES.')        
      CALL MESAGE (-61,0,0)        
   20 RETURN        
      END        