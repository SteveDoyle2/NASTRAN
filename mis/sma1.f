      SUBROUTINE SMA1
C*****
C THIS ROUTINE IS A DRIVER AND INITIALIZATION PROGRAM FOR MODULE
C 2.4.1 OF THE NASTRAN SYSTEM.  IT GENERATES THE STIFFNESS MATRIX, KGG,
C THE STRUCTURAL DAMPING MATRIX, K4GG, AND THE GRID POINT SINGULARITY
C TABLE, GPST.
C*****
      DOUBLE PRECISION
     1                   DZ                 ,DPDUM
C
      INTEGER
     1                   IZ(1)              ,EOR
     2,                  CLSRW              ,CLSNRW
     3,                  FROWIC
     4,                  TNROWS             ,OUTRW
     5,                  OPTION
C
      LOGICAL           ANYTAB        ,LINEAR
      LOGICAL            DODET              ,HEAT
C
      DIMENSION
     1                   NMSMA1(2)
      DIMENSION IBUF(7)
C
      COMMON /BLANK/  NOGENL             ,NOK4GG   ,OPTION(2)
      COMMON   /SYSTEM/  ISYS,SKIP(53),IPREC,ITHERM
C
C SMA1 I/O PARAMETERS
C
      COMMON   /SMA1IO/
     1                   IFCSTM             ,IFMPT
     2,                  IFDIT              ,IDUM1
     3,                  IFECPT             ,IGECPT
     4,                  IFGPCT             ,IGGPCT
     5,                  IFGEI              ,IGGEI
     6,                  IFKGG              ,IGKGG
     7,                  IF4GG              ,IG4GG
     8,                  IFGPST             ,IGGPST
     9,                  INRW               ,OUTRW
     T,                  CLSNRW             ,CLSRW
     1,                  NEOR               ,EOR
     2,                  MCBKGG(7)          ,MCB4GG(7)
C
C SMA1 VARIABLE CORE
C
CZZ   COMMON   /ZZSMA1 /  Z(1)
      COMMON   /ZZZZZZ /  Z(1)
C
C SMA1 VARIABLE CORE BOOKKEEPING PARAMETERS
C
      COMMON   /SMA1BK/
     1                   ICSTM              ,NCSTM
     2,                  IGPCT              ,NGPCT
     3,                  IPOINT             ,NPOINT
     4,                  I6X6K              ,N6X6K
     5,                  I6X64              ,N6X64
C
C SMA1 PROGRAM CONTROL PARAMETERS
C
      COMMON   /SMA1CL/
     1                   IOPT4              ,K4GGSW
     2,                  NPVT               ,LEFT
     3,                  FROWIC             ,LROWIC
     4,                  NROWSC             ,TNROWS
     5,                  JMAX               ,NLINKS
     6,                  LINK(10)           ,IDETCK
     7,                  DODET              ,NOGO
C
C ELEMENT DATA
C
      COMMON /GPTA1/ NELEMS, LAST, INCR, NE(1)
C
C ECPT COMMON BLOCK
C
      COMMON   /SMA1ET/
     1                   ECPT(100)
C
C SCRATCH COMMON BLOCK USED BY ELEMENT ROUTINES.
C
      COMMON   /SMA1DP/
     1                   DPDUM(300)
C
C COMMON INTERFACE FOR HMAT -HEAT- MATERIAL ROUTINE.
C
      COMMON /HMATDD/ IHMAT,NHMAT,MPTMPT,IDIT,LINEAR,ANYTAB
C
      COMMON   /SMA1HT/  HEAT
C
      EQUIVALENCE
     1                   (Z(1),IZ(1),DZ)
C
      DATA
     1                   NMSMA1(1) /4HSMA1/ ,NMSMA1(2) /4H    /
C*****
C  SET THE LOGICAL HEAT FLAG IF THIS IS A -HEAT- FORMULATION
C*****
      CALL DELSET
      LINEAR =.TRUE.
      OPTION(1) = -1
      HEAT = .FALSE.
      IF( ITHERM .NE. 0 )   HEAT = .TRUE.
C
      IZMAX = KORSZ(Z)
C
C IF NOGENL .GT. 0, GENERAL ELEMENTS EXIST AND HENCE THE GPST IS NOT
C CREATED AND SO DETCK WILL NOT BE CALLED.
C
      DODET = .TRUE.
      IF (NOGENL .GT. 0) DODET = .FALSE.
      IBUF(1) = IFECPT
      CALL RDTRL(IBUF(1))
      IF (IBUF(3).EQ.1) DODET = .FALSE.
C
C SET K4GG PURGE FLAGS
C
      NOK4GG = -1
      K4GGSW = -1
C
C ATTEMPT TO OPEN THE OUTPUT FILE FOR THE KGG  MATRIX.  IF IT IS NOT
C IN THE OSCAR, EXECUTION WILL BE TERMINATED SINCE WE DO NOT ALLOW
C THE USER TO GENERATE ONLY A K4GG.
C
      IGKGG = IZMAX - ISYS
      CALL OPEN(*100,IFKGG,Z(IGKGG),OUTRW)
C
C WRITE A TWO WORD BCD HEADER AND CLOSE THE KGG FILE WITHOUT REWIND.
C
      CALL FNAME (IFKGG,Z(1))
      CALL WRITE (IFKGG,Z(1),2,EOR)
      CALL CLOSE (IFKGG,CLSNRW)
C
C ATTEMPT TO OPEN THE K4GG FILE.
C
      IG4GG = IGKGG
      IOPT4 = 0
      CALL OPEN(*10,IF4GG,Z(IG4GG),OUTRW)
      IOPT4 = 1
      IG4GG = IG4GG - ISYS
      CALL FNAME (IF4GG,Z(1))
      CALL WRITE (IF4GG,Z(1),2,EOR)
      CALL CLOSE(IF4GG,CLSNRW)
C
C SET UP POINTERS TO GINO BUFFERS AND SET UP MATRIX CONTROL BLOCKS.
C
   10 IGECPT = IG4GG - ISYS
      IGGPCT = IGECPT - ISYS
      IGGPST = IGGPCT - ISYS
      IF (.NOT. DODET) IGGPST = IGGPST + ISYS
      MCBKGG(1) = IFKGG
      MCBKGG(2) = 0
      MCBKGG(3) = 0
      MCBKGG(4) = 6
      MCBKGG(5) = IPREC
      MCBKGG(6) = 0
      MCBKGG(7) = 0
      IF (IOPT4 .EQ. 0) GO TO 30
      MCB4GG(1) = IF4GG
      DO 20 I = 2,7
   20 MCB4GG(I) = MCBKGG(I)
C
C ATTEMPT TO READ THE CSTM INTO CORE.
C
   30 NCSTM = 0
      ICSTM = 0
      LEFT = IGGPST - 1
      CALL OPEN(*50,IFCSTM,Z(IGKGG),INRW)
      CALL FWDREC(*9020,IFCSTM)
      CALL READ(*9030,*40,IFCSTM,Z(1),LEFT,EOR,NCSTM)
C
C IF CORE WAS FILLED WITHOUT HITTING AN EOR CALL MESAGE
C
      CALL MESAGE (-8,IFCSTM,IFCSTM)
   40 LEFT = LEFT - NCSTM
C
C PRETRD SETS UP FUTURE CALLS TO TRANSD.
C
      CALL PRETRD (Z(ICSTM+1),NCSTM)
      CALL PRETRS(Z(ICSTM+1),NCSTM)
      CALL CLOSE (IFCSTM,CLSRW)
   50 IMAT1 = NCSTM
      NMAT1 = 0
      NMAT2 = 0
      NMAT3 = 0
      NMAT4 = 0
C
C CALL PREMAT TO READ MPT AND THE DIT INTO CORE
C
      IMAT11 = IMAT1 + 1
C*****
C  IF THIS IS A -HEAT- PROBLEM THE HMAT ROUTINE IS USED TO READ MAT4 AND
C  MAT5 CARDS INTO CORE.
C*****
      IF( .NOT. HEAT ) GO TO 56
      IHMAT = IMAT11 + 1
      NHMAT = IMAT11 + LEFT - 2
      MPTMPT = IFMPT
      IDIT = IFDIT
      CALL HMAT( 0 )
      LEFT = LEFT - NHMAT + IHMAT
      IGPCT = NHMAT + 1
      GO TO 58
C*****
C  NORMAL PREMAT PROCESSING.
C*****
   56 CALL PREMAT (IZ(IMAT11),Z(IMAT11),Z(IGKGG),LEFT,MATCR,IFMPT,IFDIT)
      LEFT = LEFT - MATCR
      IGPCT = NCSTM + MATCR
C
C OPEN THE ECPT AND GPCT INPUT FILES AND THE GPST OUTPUT FILE.
C
   58 CALL OPEN(*9070,IFECPT,Z(IGECPT),INRW)
      CALL FWDREC(*9080,IFECPT)
      CALL OPEN(*9090,IFGPCT,Z(IGGPCT),INRW)
      CALL FWDREC(*9100,IFGPCT)
      IF (.NOT. DODET) GO TO 60
      CALL OPEN(*9110,IFGPST,Z(IGGPST),OUTRW)
      CALL FNAME(IFGPST,ECPT(1))
      CALL WRITE(IFGPST,ECPT(1),2,EOR)
C
C REOPEN THE KGG OUTPUT FILE WITHOUT REWIND, AND THE K4GG, IF CALLED FOR
C
   60 CALL OPEN(*9120,IFKGG,Z(IGKGG),3)
      IF(IOPT4.NE.0)CALL OPEN(*9130,IF4GG,Z(IG4GG),3)
C
C CALL SUBROUTINE SMA1A WHICH WILL PERFORM ALL THE COMPUTATIONS.
C
      CALL SMA1A
      IF(.NOT. LINEAR) OPTION(1)= 1
C
C CLOSE FILES AND WRITE TRAILERS.
C
      CALL CLOSE(IFECPT,CLSRW)
      CALL CLOSE(IFGPCT,CLSRW)
      IF (.NOT. DODET) GO TO 70
      CALL CLOSE (IFGPST,CLSRW)
      CALL WRTTRL (IFGPST)
   70 CALL CLOSE (IFKGG,CLSRW)
      MCBKGG(3) = MCBKGG(2)
      CALL WRTTRL (MCBKGG(1))
      IF (IOPT4 .EQ. 0) GO TO 100
      CALL CLOSE(IF4GG,CLSRW)
      IF (MCB4GG(6) .EQ. 0) GO TO 80
      MCB4GG(3) = MCB4GG(2)
      CALL WRTTRL (MCB4GG(1))
      NOK4GG = 1
      GO TO 100
   80 DO 90 I = 2,7
   90 MCB4GG(I) = 0
      NOK4GG = -1
  100 RETURN
C
C SUBROUTINE SMA1 ERROR EXITS.
C
 9020 IFILE = IFCSTM
      GO TO 10002
 9030 IFILE = - IFCSTM
      GO TO 10002
 9070 IFILE = IFECPT
      GO TO 10001
 9080 IFILE = IFECPT
      GO TO 10002
 9090 IFILE = IFGPCT
      GO TO 10001
 9100 IFILE = IFGPCT
      GO TO 10002
 9110 IFILE = IFGPST
      GO TO 10001
 9120 IFILE = IFKGG
      GO TO 10001
 9130 IFILE = IF4GG
10001 IPARM = -1
      GO TO 10010
10002 IPARM = -2
10010 CALL MESAGE (IPARM,IFILE,NMSMA1(1))
      RETURN
      END
