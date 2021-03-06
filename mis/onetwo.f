      SUBROUTINE ONETWO(*,IX,X,DX,ITERMM)
C*******
C     PROGRAM TO SOLVE A MATRIX OF ORDER ONE OR TWO FOR DECOMP
C*******
      DOUBLE PRECISION DX(6),DET,MINDIA,DZ  ,DA
      INTEGER SYSBUF,RDP,DUM
      INTEGER TYPEL
      INTEGER SCRFLG,JPOSL,BBAR,CBCNT,R,BBBAR1
     1 ,BBBAR,SR2FL ,SR2FIL
      DIMENSION SUB(2),X(1),IX(1)
C
      COMMON /SYSTEM/SYSBUF
      COMMON /DCOMPX/IFILA(7),IFILL(7),IFILU(7),DUM(3),DET,POWER,
     1 NX,MINDIA
      COMMON /NAMES/ RD,RDREW,WRT,WRTREW,REW,NOREW,EOFNRW ,RSP,RDP
      COMMON /ZBLPKX/DZ(2),JJ
      COMMON /PACKX/ITYPE1,ITYPE2,IY,JY,INCRY
      COMMON /UNPAKX/ITYPEX,IXY,JXY,INCRX
C
      EQUIVALENCE (IFILA(2),NCOL),(IFILL(5),TYPEL),(SR2FIL,DUM(2))
C
      DATA SUB/4HONET,4HWO  /
C
C ----------------------------------------------------------------------
C
      IBUF1 = NX-SYSBUF
      IBUF2 = IBUF1-SYSBUF
      IBUF3 = IBUF2-SYSBUF
      IFILE = IFILU(1)
      CALL CLOSE(DUM(2),REW)
      IF(ITERMM.EQ.1)IFILE = DUM(2)
      CALL GOPEN(IFILE,IX(IBUF3),1)
      CALL GOPEN(IFILA,IX(IBUF1),0)
      ITYPEX = RDP
      ITYPE1 = RDP
      ITYPE2 = TYPEL
      INCRX = 1
      INCRY = 1
      IF(NCOL .EQ. 2)GO TO 100
      IF( NCOL .NE. 1)GO TO 5000
C*******
C     SOLVE A (1X1)
C*******
      IXY = 1
      JXY = 1
      CALL UNPACK(*5060,IFILA(1),DX)
      DET = DX(1)
      MINDIA = DABS(DX(1))
      IY = 1
      JY = 1
      CALL PACK(DX,IFILE,IFILU)
      DX(1) = 0.D0
      CALL PACK(DX,IFILL(1),IFILL)
      IF(ITERMM.EQ.0)GO TO 90
      CALL CLOSE(IFILE,EOFNRW)
      GO TO 95
   90 CALL CLOSE(IFILE,REW)
   95 CALL CLOSE(IFILA(1),REW)
      CALL CLOSE(IFILL(1),REW)
      RETURN
  100 IXY = 1
C*******
C     SOLVE A (2X2)
C*******
      JXY = 2
      CALL UNPACK(*5060,IFILA(1),DX)
      CALL UNPACK(*5060,IFILA(1),DX(3))
      A = 1.
      IF(DABS(DX(1)) .GE. DABS(DX(2)))GO TO 150
C*******
C     PERFORM INTERCHANGE
C*******
      DET = DX(1)
      DX(1) = DX(2)
      DX(2) = DET
      DET = DX(3)
      DX(3) = DX(4)
      DX(4) = DET
      A = -1.
  150 CONTINUE
      DX(2) = DX(2)/DX(1)
      DX(4) = DX(4)-DX(2)*DX(3)
      DET = DX(4)*DX(1)*A
      IF(DX(1) .EQ. 0.D0 .OR. DX(4) .EQ. 0.D0)GO TO 5060
      MINDIA = DMIN1 (DABS(DX(1)),DABS(DX(4)))
      IY = 1
      JY = 2
      DX(5) = 0.0D0
      IF(A.LT.0.0)  DX(5) = 1.0D0
      DX(6) = DX(2)
      CALL PACK(DX(5),IFILL(1),IFILL)
      DX(6) = 0.
      JY = 1
      CALL PACK(DX(6),IFILL(1),IFILL)
      IF(ITERMM .EQ. 1)GO TO 160
      DX(2) = DX(3)
      DX(3) = DX(4)
      DX(4) = DX(2)
      JY = 2
      CALL PACK(DX(3),IFILE,IFILU)
      IY = 2
      CALL PACK(DX,IFILE,IFILU)
      GO TO 90
  160 JY = 1
      CALL PACK(DX,IFILE,IFILU)
      JY=2
      CALL PACK(DX(3),IFILE,IFILU)
      CALL CLOSE(IFILE,EOFNRW)
      GO TO 95
      ENTRY FINWRT(ITERM,SCRFLG,SR2FL,JPOSL,I1SP,BBAR,I1,CBCNT,
     1IPAK,R,BBBAR1,BBBAR,I6SP,I4,I4SP,IX,DX,X,LCOL)
      IBUF1 = NX-SYSBUF
      IBUF2 = IBUF1-SYSBUF
      IBUF3 = IBUF2-SYSBUF
      CALL CLOSE(IFILA(1),REW)
      CALL GOPEN(SR2FIL,IX(IBUF1),WRT)
      CALL CLOSE(SR2FIL,EOFNRW)
      K=0
      CALL GOPEN(IFILL,IX(IBUF2),WRT)
      IF(SCRFLG.EQ.0)GO TO 2005
      CALL GOPEN(SR2FL,IX(IBUF3),RD)
 2005 LL = 0
 2010 JPOSL = JPOSL+1
      CALL BLDPK(RDP,TYPEL,IFILL(1),0,0)
      IN1 = I1SP+K
      JJ = JPOSL
      DZ(1) = IX(IN1)
      CALL ZBLPKI
      KK = 0
      IEND = MIN0(BBAR,NCOL-JJ)
      IF(IEND .EQ. 0)GO TO 2030
      IN1 = I1+LL*BBAR
 2020 JJ = JJ+1
      IN2 = IN1+KK
      DZ(1) =DX(IN2)
      CALL ZBLPKI
      KK = KK+1
      IF(KK-IEND)2020,2030,5050
 2030 IF(CBCNT.EQ.0)GO TO 2050
C*******
C     PACK ACTIVE ROW ELEMENTS ALSO
C*******
      KK = 0
 2035 IN1 = I6SP + KK
      IN2 = I4 + IX(IN1)*BBBAR + K
      DZ(1) = DX(IN2)
      IF(DZ(1) .EQ. 0.D0)GO TO 2040
      IN1 = I4SP + IX(IN1)
      JJ = IX(IN1)
      CALL ZBLPKI
 2040 KK = KK + 1
      IF(KK .LT. CBCNT)GO TO 2035
 2050 CALL BLDPKN(IFILL(1),0,IFILL)
      LL = LL + 1
      K = K + 1
      IF(K.EQ.LCOL)GO TO 2080
      IF(K-R+1)2010,2060,2070
 2060 IF(R-BBBAR1)2070,2010,5050
 2070 LL =LL-1
      IN1 = I1+LL*BBAR
      CALL FREAD(SR2FL,DX(IN1),2*BBAR,0)
      GO TO 2010
 2080 CALL CLOSE(IFILL(1),REW)
      IF(SCRFLG.GT.0)CALL CLOSE(SR2FL,REW)
      IF(ITERM .NE. 0)RETURN
C*******
C     RE-WRITE THE UPPER TRIANGLE WITH THE RECORDS IN THE REVERSE ORDER
C*******
      INCRX = 1
      INCRY = 1
      ITYPE1 = TYPEL
      ITYPE2 = TYPEL
      ITYPEX = TYPEL
      IFILU(2) = 0
      IFILU(6) = 0
      IFILU(7) = 0
      CALL GOPEN(SR2FIL,IX(IBUF1),RD)
      CALL GOPEN(IFILU,IX(IBUF2),1)
      DO 2300 I = 1,NCOL
      IXY = 0
      CALL BCKREC(SR2FIL)
      CALL UNPACK(*5060,SR2FIL,IX)
      CALL BCKREC(SR2FIL)
      KK = JXY-IXY+1
      K = KK/2
      KK = KK + 1
      IF(TYPEL .EQ. 1)GO TO 2095
      DO 2090 J = 1,K
      L = KK-J
      DA = DX(J)
      DX(J) = DX(L)
 2090 DX(L) = DA
      GO TO 2100
 2095 DO 2097 J = 1,K
      L = KK-J
      A    = X(J)
      X(J) = X(L)
 2097 X(L) = A
 2100 IY = NCOL-JXY+1
      JY = NCOL-IXY+1
      CALL PACK(IX,IFILU(1),IFILU)
 2300 CONTINUE
      CALL CLOSE(IFILU(1),REW)
      CALL CLOSE(SR2FIL,REW)
      RETURN
 5000 NO = -8
      GO TO 5500
 5050 NO = -25
      GO TO 5500
 5060 RETURN 1
 5500 CALL MESAGE(NO,0,SUB)
      RETURN
      END
