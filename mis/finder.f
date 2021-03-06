      SUBROUTINE FINDER( NAM , SUBNO , COMNO )
C
C
C     THIS SUBROUTINE READS THE TABLE OF CONTENTS OF SUBSTRUCTURES
C     BEING COMBINED ( SCRATCH FILE SCTOC ) AND FOR ANY GIVEN
C     BASIC SUBSTRUCTURE NAME ( NAM ) RETURNS THE ID NUMBER OF THE
C     PSEUDO-STRUCTURE CONTAINING IT ( SUBNO ) AND ITS POSITION IN
C     THE COMPONENT LIST FOR THAT STRUCTURE ( COMNO ).  IF A NAME
C     DOES NOT APPEAR IN THE SCTOC AN ERROR MESSAGE IS ISSUED.
C
      INTEGER SCTOC,BUF4,ID(3),SUBNO,COMNO,NAM(2),CNAM(2),OUTT
      LOGICAL TOCOPN
      COMMON/CMB001/ SCR1,SCR2,SCBDAT,SCSFIL,SCCONN,SCMCON,
     1               SCTOC,GEOM4,CASECC
CZZ   COMMON/ZZCOMB/ Z(1)
      COMMON/ZZZZZZ/ Z(1)
      COMMON/CMB002/ BUF1,BUF2,BUF3,BUF4,BUF5,SCORE,LCORE,INPT,OUTT
      COMMON/CMB003/ COMBO(7,5),CONSET,IAUTO,TOLER,NPSUB,CONECT,TRAN,
     1               MCON,RESTCT(7,7),ISORT,ORIGIN(7,3),IPRINT,TOCOPN
      COMMON/CMBFND/ INAM(2),IERR
C
C     OPEN SCTOC FILE
C
      IERR = 0
      IF(.NOT.TOCOPN)CALL OPEN(*2001,SCTOC,Z(BUF4),0)
      CALL REWIND( SCTOC )
C
      DO 1 I=1,NPSUB
      CALL READ(*2001,*2002,SCTOC,ID,3,0,NNN)
      NCOM = ID(3)
      DO 2 J=1,NCOM
      IEOR = 0
      IF( J .EQ. NCOM ) IEOR = 1
      CALL READ(*2001,*2002,SCTOC,CNAM,2,IEOR,NNN)
      IF( NAM(1).EQ.CNAM(1) .AND. NAM(2).EQ.CNAM(2) ) GO TO 11
2     CONTINUE
1     CONTINUE
C
C     IERR = 1 MEANS THAT THE SUBSTRUCTURE NAME IS NOT IN THE TOC
C
      IERR = 1
      RETURN
11    SUBNO = I
      INAM(1) = ID(1)
      INAM(2) = ID(2)
      COMNO = J
      IF( .NOT. TOCOPN ) CALL CLOSE( SCTOC , 1 )
2001  CONTINUE
2002  CONTINUE
      RETURN
      END
