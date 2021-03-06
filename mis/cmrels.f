      SUBROUTINE CMRELS
C
C     THIS SUBROUTINE ENFORCES THE RELES DATA SPECIFIED FOR THE
C     COMB1 MODULE.
C
      EXTERNAL        ANDF
      LOGICAL         FIRST
      INTEGER         IX(7,3),SCBDAT,Z,SCORE,BUF1,BUF2,SCCONN,PS1,PS2
      INTEGER         LIST(32),ANDF,STCE,AAA(2)
      COMMON /CMB001/ SCR1,SCR2,SCBDAT,SCSFIL,SCCONN
      COMMON /CMB002/ BUF1,BUF2,BUF3,BUF4,BUF5,SCORE,LCORE,INPT,OUTT
      COMMON /CMB003/ JUNK(38),NPSUB
CZZ   COMMON /ZZCOMB/ Z(1)
      COMMON /ZZZZZZ/ Z(1)
      DATA    AAA   / 4HCMRE,4HLS   /
C
      IFILE = SCBDAT
      KJ = 0
      DO 20 I = 1,7
      DO 10 J = 1,3
      IX(I,J) = 0
   10 CONTINUE
   20 CONTINUE
      DO 70 I = 1,NPSUB
      FIRST = .TRUE.
      CALL OPEN (*150,SCBDAT,Z(BUF1),0)
      CALL SKPFIL (SCBDAT,3)
   30 CALL READ (*60,*170,SCBDAT,ID,1,0,N)
      IF (ID .EQ. I) GO TO 40
      CALL FWDREC (*60,SCBDAT)
      GO TO 30
   40 CALL READ (*160,*50,SCBDAT,Z(SCORE+KJ),LCORE,1,NW)
      GO TO 180
   50 IF (FIRST) IX(I,2) = SCORE + KJ
      FIRST = .FALSE.
      IX(I,3) = IX(I,3) + NW/2
      KJ = KJ + NW
      LCORE = LCORE - NW
      IX(I,1) = 1
      GO TO 30
   60 CALL CLOSE (SCBDAT,1)
   70 CONTINUE
      DO 80 I = 1,NPSUB
      IF (IX(I,1) .EQ. 0) GO TO 80
      IST = IX(I,2)
      NW  = IX(I,3)*2
      CALL SORT (0,0,2,1,Z(IST),NW)
   80 CONTINUE
      IFILE = SCCONN
      CALL OPEN (*150,SCCONN,Z(BUF2),0)
      NWRD = 2 + NPSUB
      NCE  = 0
      STCE = SCORE + KJ
   90 CALL READ (*110,*100,SCCONN,Z(SCORE+KJ),LCORE,1,NNN)
      GO TO 180
  100 KJ  = KJ + NWRD
      NCE = NCE + 1
      GO TO 90
  110 CALL CLOSE (SCCONN,1)
      NCE = NWRD*NCE
      DO 130 I = 1,NCE,NWRD
      II = I - 1
      ICODE = Z(STCE+II+1)
      CALL DECODE (ICODE,LIST,NC)
      IF (NC .NE. 2) GO TO 130
      PS1  = LIST(1) + 1
      PS2  = LIST(2) + 1
      IST1 = IX(PS1,2)
      IST2 = IX(PS2,2)
      NW1  = IX(PS1,3)
      NW2  = IX(PS2,3)
      IF (IX(PS1,1) .EQ. 0) GO TO 120
      KID  = Z(STCE+II+1+PS1)
      CALL BISLOC (*120,KID,Z(IST1),2,NW1,IW)
      Z(STCE+II) = Z(STCE+II) - ANDF(Z(STCE+II),Z(IST1+IW))
  120 IF (IX(PS2,1) .EQ. 0) GO TO 130
      KID = Z(STCE+II+1+PS2)
      CALL BISLOC (*130,KID,Z(IST2),2,NW2,IW)
      Z(STCE+II) = Z(STCE+II) - ANDF(Z(STCE+II),Z(IST2+IW))
  130 CONTINUE
      CALL OPEN (*150,SCCONN,Z(BUF1),1)
      DO 140 I = 1,NCE,NWRD
      II = I - 1
      IF (Z(STCE+II) .NE. 0) CALL WRITE (SCCONN,Z(STCE+II),NWRD,1)
  140 CONTINUE
      CALL EOF (SCCONN)
      CALL CLOSE (SCCONN,1)
      RETURN
C
  150 IMSG = -1
      GO TO 190
  160 IMSG = -2
      GO TO 190
  170 IMSG = -3
      GO TO 190
  180 IMSG = -8
  190 CALL MESAGE (IMSG,IFILE,AAA)
      RETURN
      END
