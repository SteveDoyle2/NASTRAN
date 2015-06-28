      SUBROUTINE XSORT        
C        
C     SORT READS BULK DATA CARDS FROM THE INPUT TAPE, ADJUSTS THE       
C     FIELDS, PERFORMS AN ALPHA-NUMERIC SORT ON THE CARD IMAGES FROM    
C     LEFT TO RIGHT, INSERTS CONTINUATION CARDS IN THEIR PROPER        
C     POSITION, AND PLACES THE RESULTING SORTED IMAGES ON THE NEW       
C     PROBLEM TAPE.        
C        
      IMPLICIT INTEGER (A-Z)        
      EXTERNAL        LSHIFT,RSHIFT,ANDF,ORF        
      LOGICAL         DEC        
      DIMENSION       HEADU(32),HEADS(32),HEADN(32),IBLKDA(2),CDCNT(3), 
     1                BK(4),MK(4),IBUF1(20),IBUF2(20),IBUF3(2),        
     2                KPARNT(2),IBUF1A(2),IBUF2A(2),NSORT(2),IIEND(2)   
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25        
      COMMON /XMSSG / UFM,UWM,UIM,SFM        
      COMMON /MACHIN/ MACH        
      COMMON /SYSTEM/ IBUFSZ,OUTTAP,NOGO,INTAPE,D1(14),IECHO,D,IAPPRC,  
     1                DUM1(2),IUEDIT,DUM44(44),ISUBS,DUM12(12),ICPFLG,  
     2                DUM8(8),LPCH        
      COMMON /OUTPUT/ DUM2(96),HEAD1(32),HEAD2(32),HEAD3(32)        
CZZ   COMMON /ZZXSRT/ BUF(1)        
      COMMON /ZZZZZZ/ SKIP1,BUF(1)        
      COMMON /XSRTCM/ BIMSK1(6),BIMSK2(5),BIMSK3(4),BIMSK4(4),BIMSK5(2),
     1                BIMSK6,BKMSK1(8),BKMSK2,SHIFTS(4),        
     2                ICON1,ICON2,STAR,PLUS,DOLLAR,STARL,SLASH,SFTM,    
     3                MASK,BLANK,MKA,IS,MBIT4        
      COMMON /STAPID/ KRAP(12),KUMF        
      COMMON /XECHOX/ FFFLAG,ECHOU,ECHOS,ECHOP        
      EQUIVALENCE     (BK(1),BKMSK1(5)),(MK(1),BIMSK2(2)),        
     1                (MKB  ,BIMSK5(1)),(INF  ,BIMSK2(1)),        
     2                (SFTA ,SHIFTS(2)),(MKD  ,BIMSK2(2)),        
     3                (MKE  ,BIMSK5(2)),(MKC  ,BIMSK4(1))        
      EQUIVALENCE     (BLANX,BKMSK1(8))        
      DATA HEADU/10*4H    ,4H I N,4H P U,4H T  ,4H B U,4H L K,4H   D,   
     1       4H A T,4H A  ,4H D E,4H C K,4H   E,4H C H,4H O  ,9*4H    / 
      DATA HEADS/11*4H    ,4H S O,4H R T,4H E D,4H   B,4H U L,4H K  ,   
     1       4H D A,4H T A,4H   E,4H C H,4H O  ,10*4H    /        
      DATA HEADN/ 3*4H    ,4H    ,4H    ,4H    ,4H .  ,4H 1  ,4H..  ,   
     1       4H 2  ,4H..  ,4H 3  ,4H..  ,4H 4  ,4H..  ,4H 5  ,4H..  ,   
     2       4H 6  ,4H..  ,4H 7  ,4H..  ,4H 8  ,4H..  ,4H 9  ,4H..  ,   
     3       4H10  ,4H.   ,5*4H    /        
      DATA CDCNT/4HCARD,4HCOUN,4HT   /,NSORT/4HXSOR,4HT   /        
C     DATA BK/4H000 ,4H00  ,4H0   ,4H    /        
C     DATA (MK(I),I=1,4)/O777777007777,O777700007777,O770000007777,O0/  
C     DATA MKA,MKB,INF,SFTA/O000000777777,O377777777777,O777777777777,6/
C     DATA MKC/O007777777777/,MKD/O777777007777/,MKE/O377777007777/     
      DATA IEND1,IEND2/4HENDD,4HATA /        
      DATA IEND3,IEND4/4HENDA,4HTA  /        
      DATA IEND5,IEND6/4HEND ,4HDATA/        
C     DATA STAR,PLUS,DOLLAR,STARL/4H000*,4H+000,4H$000,4H*000/        
      DATA IBLKDA/4HBULK,4HDATA/, IPTP/4HOPTP/, NPTP/4HNPTP/        
      DATA ITAPE1,ITAPE2,ITAPE3,ITAPE4,ITAPE5/301,302,303,304,305/      
      DATA UMF/4HUMF /        
      DATA IDUP,IOK/4HDUPL, 4HOK  /        
C        
C        
C     XSORT MAY NOT WORK PROPERLY IN ALL UNIX MACHINES, WHICH FOLLOW    
C     THE VAX LINE.        
C        
      DEC    = MACH.EQ.5 .OR. MACH.EQ.6 .OR. MACH.EQ.21        
      IF (DEC .AND. LPCH.NE.77) WRITE (OUTTAP,5) UWM        
    5 FORMAT (A25,', SWITCHING TO OLD XSORT VIA DIAG 42 HAS NOT BEEN ', 
     1       'THOROUGHLY TESTED', /5X,'FOR THE UNIX MACHINES.')        
C        
C     INITIALIZE XSORT AND TURN ON FREE-FIELD FLAG FOR XREAD        
C        
      FFFLAG = 1234        
      ECHOU  = 0        
      ECHOS  = 0        
      ECHOP  = 0        
      IEND   = 0        
      ISEQ   = 0        
      ICCBRK = 0        
      NOTSOR = 0        
      OPTP   = IPTP        
      KIN    = 0        
      IRESTR = -IAPPRC        
      IF (KUMF .LE. 0) GO TO 90        
      KIN    = 1        
      CALL OPEN (*50,UMF,BUF(1),2)        
C        
C     FIND PARTICULAR BULK DATA FILE ON UMF AS REQUESTED BY USER        
C        
   10 CALL READ (*30,*60,UMF,PID,1,1,IFLG)        
      IF (KUMF-PID) 30,80,20        
   20 CALL SKPFIL (UMF,1)        
      GO TO 10        
   30 WRITE  (OUTTAP,35) UFM,KUMF        
   35 FORMAT (A23,' 201, REQUESTED BULK DATA DECK',I8,' NOT ON USER ',  
     1       'MASTER FILE.')        
      CALL PAGE2 (2)        
      NOGO =-1        
      CALL CLOSE (UMF,1)        
      RETURN        
C        
   50 WRITE  (OUTTAP,55) SFM        
   55 FORMAT (A25,' 202, UMF COULD NOT BE OPENED')        
      GO TO  1800        
   60 WRITE  (OUTTAP,65) SFM        
   65 FORMAT (A25,' 203, ILLEGAL EOR ON UMF')        
      GO TO  1800        
   80 CALL CLOSE (UMF,2)        
C        
   90 CALL INITCO        
      IF (IECHO .LT. 0) GO TO 110        
      ECHOU = ANDF(IECHO,1)        
      ECHOS = ANDF(IECHO,2)        
      ECHOP = ANDF(IECHO,4)        
      IF (ICPFLG .NE. 0) ECHOS = 1        
  110 ASSIGN 1260 TO IBRANA        
      ASSIGN 810  TO IBRANB        
      ASSIGN 1220 TO IBRANF        
C        
C     SET ASSIGN GO TO SWITCHES FOR MACHINE CONFIGURATIONS        
C     THE 8 BIT CHARACTER BYTE OF THE 360 WILL HOLD THE INTERNAL        
C     CHARACTER CODE (MAX=37) WITHOUT USE OF THE 1ST BIT POSITION -     
C     THE OTHER 3 MACHINES HAVE 6 BIT CHARACTERS THEREFORE A SHIFT RIGHT
C     OF ONE MUST BE DONE TO REMOVE A POSSIBLE BIT FROM THE SIGN        
C     POSITION THE FOLLOWING ASSIGNS SET THOSE BRANCHES BASED ON MACHINE
C        
      IF (MACH.EQ.2 .OR. DEC) GO TO 120        
      ASSIGN 350  TO MX3        
      ASSIGN 790  TO MY1        
      ASSIGN 820  TO MY2        
      ASSIGN 960  TO MY3        
      ASSIGN 990  TO MY4        
      ASSIGN 1030 TO MY5        
      ASSIGN 840  TO MY6        
      ASSIGN 730  TO MZ1        
      LINF   = 0        
      NSHIFT = 1        
C        
C     SET NSHIFT TO ZERO FOR UNIVAC ASCII VERSION ONLY (NOT FORTRAN 5)  
C        
      IF (MACH .EQ. 3) NSHIFT = 0        
      GO TO 130        
  120 CONTINUE        
      ASSIGN 360  TO MX3        
      ASSIGN 800  TO MY1        
      ASSIGN 970  TO MY2        
      ASSIGN 970  TO MY3        
      ASSIGN 1040 TO MY4        
      ASSIGN 1040 TO MY5        
      ASSIGN 850  TO MY6        
      ASSIGN 740  TO MZ1        
      LINF = ORF(IS,1)        
  130 CONTINUE        
C        
C     START WORKING SORT BUFFER BELOW GINO I/O BUFFERS        
C        
      II     = 5*IBUFSZ + 1        
      IBUFBG = II + 42        
      IBUFLG = KORSZ(BUF) - 21        
      IF (IBUFLG-IBUFBG .LT. 210)        
     1    CALL MESAGE (-8,IBUFBG+210-IBUFLG,NSORT)        
      ITAPE  = ITAPE1        
      JTAPE  = ITAPE2        
C        
C     OPEN ITAPE4 AND ITAPE5        
C     (4 CONTAINS CONTINUATIONS, 5 CONTAINS ALTERS)        
C        
      NBUF3 = 3*IBUFSZ + 1        
      CALL OPEN (*1700,ITAPE4,BUF(NBUF3),1)        
      NBUF4 = 4*IBUFSZ + 1        
      CALL OPEN (*1700,ITAPE5,BUF(NBUF4),1)        
C        
C     A BUFFER LINE IS 20 WORDS OF CARD IMAGE PLUS A 1 WORD POINTER TO  
C     THE NEXT IMAGE IN THE SORT SEQUENCE - A ZERO POINTER INDICATES    
C     THE LAST IMAGE (LARGEST IN SORT)        
C     INITIALIZE WORKING BUFFER - 1ST LINE ZEROS, 2ND LINE ALL BITS     
C        
      K = II + 19        
      DO 140 J = II,K        
      BUF(J) = LINF        
  140 BUF(J+ 21) = INF        
      BUF(II+41) = 0        
C        
C     SET UP UNSORTED HEADING        
C        
      DO 150 J = 1,32        
      HEAD1(J) = HEADU(J)        
  150 HEAD3(J) = HEADN(J)        
      HEAD2(4) = HEADN(1)        
      ICCNT    = 0        
      IF (ECHOU .EQ. 0) GO TO 160        
      CALL PAGE        
C        
C     OPEN ITAPE (LOCATION FOR EACH SORTED CORE LOAD AS ITS FORCED TO   
C     EMPTY        
C        
  160 CALL OPEN (*1700,ITAPE,BUF(1),1)        
  170 BUF(II+20) = 1        
      K    = II        
      NCNT = 2        
C        
C     LOOP TO INPUT AND SORT CARD IMAGES - USES OPEN CORE FOR SORTED    
C     IMAGES        
C        
      DO 550 N1 = IBUFBG,IBUFLG,21        
      N2 = N1 + 19        
      N3 = N2 + 1        
  180 CALL XREAD (*1770,BUF(N1))        
      ICCNT = ICCNT + 1        
      IF (ECHOU .EQ. 0) GO TO 220        
      CALL PAGE2 (-1)        
      WRITE  (OUTTAP,200)(BUF(I),I=N1,N2)        
  200 FORMAT (30X,20A4)        
  210 FORMAT (13X,I8,'-',8X,20A4)        
C        
C     IGNORE BLANK CARDS        
C        
  220 IF (BUF(N1).EQ.BLANX .AND. BUF(N1+1).EQ.BLANX) GO TO 180        
C        
C     LEFT ADJUST FIELD 1        
C        
      CALL XFADJ1 (BUF(N1),LSHIFT,0)        
C        
C     TEST FOR END OF INPUT DATA STREAM (ENDDATA)        
C        
      IIEND(1) = IEND1        
      IIEND(2) = IEND2        
      IF (BUF(N1).EQ.IEND1 .AND. BUF(N1+1).EQ.IEND2) GO TO 560        
      IIEND(1) = IEND3        
      IIEND(2) = IEND4        
      IF (BUF(N1).EQ.IEND3 .AND. BUF(N1+1).EQ.IEND4) GO TO 560        
      IIEND(1) = IEND5        
      IIEND(2) = IEND6        
      IF (BUF(N1).EQ.IEND5 .AND. BUF(N1+1).EQ.IEND6) GO TO 560        
C        
C     IS THIS A CONTINUATION, COMMENT, OR DELETE CARD        
C        
      IF (.NOT.DEC) TST = ANDF(MK(3),BUF(N1))        
      IF (     DEC) TST = KHRFN1(BKMSK2,1,BUF(N1),1)        
C        
C     WRITE CONTINUATIONS ON ITAPE4        
C        
      IF (TST.EQ.STARL .OR. TST.EQ.PLUS) GO TO 530        
C        
C     IGNORE COMMENT CARDS        
C        
      IF (TST .EQ. DOLLAR) GO TO 180        
C        
C     WRITE DELETES ON ITAPE5        
C        
      IF (TST .EQ. SLASH) GO TO 540        
C        
C     IF A STAR IS FOUND IN FIELD 1, MOVE IT TO COLUMN 8        
C        
      NY  = 4        
      DO 240 J = 1,2        
      NX  = N1 + 2 - J        
      TST = BUF(NX)        
      DO 230 I = 1,NY        
      IF (.NOT.DEC) PTST = ANDF(MKA,TST)        
      IF (     DEC) PTST = KHRFN1(BKMSK2,4,TST,4)        
      IF (PTST .NE. BK(1)) GO TO 250        
      IF (.NOT.DEC) TST = RSHIFT(TST,SFTA)        
      IF (     DEC) TST = KHRFN3(BKMSK2,TST,1,0)        
  230 CONTINUE        
  240 NY = 3        
      GO TO 260        
C        
C     STARSW = 0 FOR A SINGLE FIELD CARD (NO STAR)        
C            = 1 FOR A DOUBLE FIELD CARD (W/ STAR)        
C        
  250 STARSW = 0        
      IF (PTST .NE. STAR) GO TO 260        
      STARSW = 1        
      IF (J.EQ.1 .AND. I.EQ.1) GO TO 260        
      IF (DEC) GO TO 258        
      BUF(NX  ) = ORF(ANDF(MK(I),BUF(NX)),BK(I))        
      BUF(N1+1) = ORF(ANDF(MK(1),BUF(N1+1)),STAR)        
      GO TO 260        
  258 BUF(NX  ) = KHRFN1(BUF(NX),5-I,BK(I),5-I)        
      BUF(N1+1) = KHRFN1(BUF(N1+1),4,STAR,4)        
  260 CONTINUE        
      CALL XFADJ (BUF(N1+2),STARSW,NY)        
      CALL EXTINT (BUF(N1))        
C        
C        
C     START SORT LOGIC        
C        
C     WITHOUT THE FOLLOWING CARD, XSORT WILL ASSUME SOME DEGREE OF SORT 
C     EXISTS (I.E.,THE NEXT CARD WILL FOLLOW THE PREVIOUS CARD, MORE    
C     OFTEN THAN NOT)        
C     K  = II  (THIS CARD WILL FORCE SORT TO BEGINNING OF CHAIN)        
C        
      KP = 0        
C        
C     K TYPE SUBSCRIPTS REFER TO POSITIONS AND ITEMS IN THE SORTED      
C     TABLE CURRENTLY BEING BUILT        
C     N TYPE SUBSCRIPTS REFER TO ITEMS ABOUT THE NEWEST CARD IN        
C        
  270 FCNT = 1        
      NI = 0        
      KI = 0        
      NX = N1        
C        
C     THE RIGHT SHIFT IN THE FOLLOWING CODE IS USED TO AVOID THE        
C     NEGATIVE SIGN PROBLEM WHICH WOULD REVERSE THE SORT ORDER ON SOME  
C     MACHINES.        
C     (NOTE THAT THE SORT COMPARES CAN BE MADE BOTH WITH OR WITHOUT     
C     THE SIGN SHIFT DEPENDING ON THE MACHINES CHARACTER CONFIG)        
C        
  300 KX = K        
      GO TO 340        
  330 IF (BUF(NX) .EQ. BUF(KX)) GO TO 400        
      IF (BUF(NX) .EQ. BK(4)  ) GO TO 380        
      IF (BUF(KX) .EQ. BK(4)  ) GO TO 370        
  340 GO TO MX3, (350,360)        
  350 IF (RSHIFT(BUF(NX),NSHIFT)-RSHIFT(BUF(KX),NSHIFT)) 380,400,370    
  360 IF (DEC) GO TO 365        
      IF (BUF(NX) .LT. BUF(KX)) GO TO 380        
      IF (BUF(NX) .GT. BUF(KX)) GO TO 370        
      GO TO 400        
  365 IF (RSHIFT(KHRFN4(BUF(NX)),1)-RSHIFT(KHRFN4(BUF(KX)),1))        
     1    380,366,370        
  366 IF (RSHIFT(LSHIFT(KHRFN4(BUF(NX)),1),1)-        
     1    RSHIFT(LSHIFT(KHRFN4(BUF(KX)),1),1)) 380,400,370        
C        
C     GO ON, LOOK AT NEXT ITEM IN THE SORTED TABLE        
C        
  370 KP = K        
      K  = BUF(K+20)*21 + II        
      IF (NX .EQ. N1) GO TO 300        
      GO TO 270        
C        
C     CARD POSITION FOUND IN SORT, SET THE CHAINING POINTER        
C        
  380 IF (KP .EQ. 0) GO TO 390        
      BUF(N3   ) = BUF(KP+20)        
      BUF(KP+20) = NCNT        
      K    = KP        
      NCNT = NCNT + 1        
      GO TO 550        
  390 K = II        
      GO TO 270        
C        
C     TWO FIELDS EQUAL - SLIDE TO NEXT FIELD ON CARD        
C        
  400 FCNT = FCNT + 1        
      NX   = NX + 1        
      KX   = KX + 1        
      GO TO (1760,410,470,330,510,330,430,330,510,330,520,330,510,330,  
     1        430,330,510,330,380), FCNT        
  410 KTARSW = 0        
      IF (.NOT.DEC) ITST = ANDF(MKA,BUF(K+1))        
      IF (     DEC) ITST = KHRFN1(BKMSK2,4,BUF(K+1),4)        
      IF (ITST .EQ. STAR) KTARSW = 1        
      IF (STARSW .EQ. KTARSW) GO TO 340        
C        
C     IF ONE MEMBER OF THE 2ND FIELD HAS A STAR AND THE OTHER DOES NOT, 
C     DELETE STARS FOR THE COMPARE        
C        
      IF (DEC) GO TO 415        
      IN1 = RSHIFT(ANDF(MKD,BUF(NX)),1)        
      IK2 = RSHIFT(ANDF(MKD,BUF(KX)),1)        
      GO TO 418        
  415 IN1 = RSHIFT(KHRFN4(KHRFN1(BUF(NX),4,BKMSK2,1)),1)        
      IK2 = RSHIFT(KHRFN4(KHRFN1(BUF(KX),4,BKMSK2,1)),1)        
  418 IF (IN1 .NE. IK2) GO TO 428        
      IF (DEC) GO TO 420        
      IN1 = ANDF(MKE,BUF(NX))        
      IK2 = ANDF(MKE,BUF(KX))        
      GO TO 425        
  420 IN1 = RSHIFT(LSHIFT(KHRFN4(KHRFN1(BUF(NX),4,BKMSK2,1)),1),1)      
      IK2 = RSHIFT(LSHIFT(KHRFN4(KHRFN1(BUF(KX),4,BKMSK2,1)),1),1)      
  425 IF (IN1 .EQ. IK2) GO TO 400        
  428 IF (IN1 .LT. IK2) GO TO 380        
      GO TO 370        
C        
C     INCREMENT FIELD LOCATIONS IF FIELD TYPES DID NOT MATCH        
C        
  430 IF (NI-KI) 450,460,440        
  440 NX = NX + NI        
      NI = 0        
      GO TO 460        
  450 KX = KX + KI        
      KI = 0        
C        
C     ADJUST FIELDS RIGHT OR LEFT AS REQUIRED        
C        
  460 CALL XFADJ (BUF(NX),STARSW,K1)        
      CALL XFADJ (BUF(KX),KTARSW,K2)        
      GO TO 480        
  470 IF (STARSW .EQ. KTARSW) GO TO 330        
      K1 = 0        
      K2 = 0        
      IF (DEC) GO TO 472        
      IF (ANDF(MK(3),BUF(NX)) .NE. BKMSK1(4)) K1 = 1        
      IF (ANDF(MK(3),BUF(KX)) .NE. BKMSK1(4)) K2 = 1        
      GO TO 480        
  472 IF (KHRFN1(BKMSK2,1,BUF(NX),1) .NE. BKMSK1(4)) K1 = 1        
      IF (KHRFN1(BKMSK2,1,BUF(KX),1) .NE. BKMSK1(4)) K2 = 1        
  480 IF (STARSW-KTARSW) 500,330,490        
  490 NI = 2        
      IF (K1+K2 .EQ. 2) GO TO 330        
      NX = NX + 2        
      NI = 0        
      GO TO 330        
  500 KI = 2        
      IF (K1+K2 .EQ. 2) GO TO 330        
      KX = KX + 2        
      KI = 0        
      GO TO 330        
  510 IF (STARSW .NE. KTARSW) GO TO 430        
      IF (STARSW .EQ.      0) GO TO 430        
      GO TO 330        
  520 IF (STARSW .EQ. KTARSW) GO TO 430        
      GO TO 380        
C        
C     CONTINUATION CARD - PUT ON ITAPE4        
C        
  530 CALL WRITE (ITAPE4,BUF(N1),20,1)        
      GO TO 180        
C        
C     BULK DATA DELETE CARD - PUT ON ITAPE5        
C        
C     TEST FOR EXTRANEOUS DATA IN FIELD 1 OF DELETE CARD        
C     AND WRITE OUT TO SCRATCH FILE        
C        
  540 IF (.NOT.DEC) ITST1 = ANDF(BUF(N1),BIMSK1(6))        
      IF (     DEC) ITST1 = ANDF(BUF(N1),BIMSK1(1))        
      ITST2 = ANDF(BUF(N1+1),MBIT4)        
      IBK3  = ANDF(BK(3),MBIT4)        
      IBK4  = ANDF(BK(4),MBIT4)        
      IF (ITST1.EQ.IBK3 .AND. ITST2.EQ.IBK4) GO TO 545        
      CALL PAGE2 (2)        
      WRITE  (OUTTAP,541) UFM        
  541 FORMAT (A23,' 221, EXTRANEOUS DATA IN FIELD 1 OF BULK DATA ',     
     1        'DELETE CARD.')        
      NOGO = -2        
  545 CALL XFADJ1 (BUF(N1+2),RSHIFT,0)        
      CALL XBCDBI (BUF(N1+2))        
      CALL XFADJ1 (BUF(N1+4),RSHIFT,0)        
      CALL XBCDBI (BUF(N1+4))        
      BUF(N1+4) = BUF(N1+5)        
      CALL WRITE (ITAPE5,BUF(N1+3),2,1)        
      GO TO 180        
C        
C     END OF BIG SORT LOOP        
C        
  550 CONTINUE        
      GO TO 590        
C        
C        
C     SET (ENDDATA) CARD FOUND FLAG        
C        
  560 IEND = -1        
      IF (ECHOU .NE. 1) GO TO 572        
      CALL PAGE2 (2)        
      WRITE  (OUTTAP,570) ICCNT        
  570 FORMAT (//24X,'TOTAL COUNT=',I5)        
  572 CONTINUE        
C        
C     TEST FOR COLD-START WITH NO BULK DATA        
C        
      IF (ICCNT.GT.1 .OR. IRESTR.GT.0 .OR. KUMF.GT.0) GO TO 590        
      IF (IAPPRC .EQ. 1) GO TO 590        
      IF (ISUBS  .NE. 0) GO TO 590        
      CALL PAGE2 (2)        
      WRITE  (OUTTAP,580) UFM        
  580 FORMAT (A23,' 204, COLD START NO BULK DATA.')        
      NOGO = -2        
      RETURN        
C        
C        
C     IF MODIFIED RESTART - TURN ON SORT ECHO        
C        
  590 CONTINUE        
C        
C     THIS SECTION UNCHAINS THE SORTED TABLE AND WRITES A CORE LOAD,    
C     IN ITS ACTUAL ORDER, ONTO A MERGE SCRATCH TAPE.        
C        
      J = BUF(II+20)        
      J1ST = J*21 + II        
      KEEP = 1        
  610 J  = J*21 + II        
      J1 = BUF(J+20)        
      IF (J1 .EQ. 0) GO TO 620        
C        
C     ITAPE IS PRIMARY CORE UNLOAD TAPE        
C        
      CALL WRITE (ITAPE,BUF(J),20,1)        
      IF (J .LT. KEEP) NOTSOR = 1        
      KEEP = J        
      J    = J1        
      GO TO 610        
  620 ISEQ = ISEQ + 1        
      IF (ISEQ .EQ. 2) GO TO 640        
      IF (ISEQ .GT. 2) GO TO 650        
      IF (IEND .NE. 0) GO TO 630        
      ITAPE = ITAPE2        
      CALL OPEN (*1700,ITAPE,BUF(IBUFSZ+1),1)        
      GO TO 170        
C        
C     NO MERGING IS REQUIRED, ALL CARDS FIT WITHIN ONE WORKING BUFFER   
C     LOAD        
C        
  630 KTAPE = ITAPE        
      CALL CLOSE (KTAPE,1)        
      GO TO 1260        
C        
C     SET UP 1ST MERGE        
C        
  640 CALL CLOSE (ITAPE,1)        
      ITAPE = ITAPE1        
      KOP   = 0        
C        
C     SET UP SUBSEQUENT MERGE TAPES        
C        
  650 IF (MOD(ISEQ,2) .EQ. 0) GO TO 660        
      JTAPE = ITAPE3        
      KTAPE = ITAPE2        
      GO TO 670        
  660 JTAPE = ITAPE2        
      KTAPE = ITAPE3        
  670 CALL CLOSE (ITAPE,1)        
C        
C     SPECIAL LOGIC TO AVOID MERGE IF NEW CORE LOAD FOLLOWS ALL PREVIOUS
C        
      IF (KOP-1) 760,700,680        
  680 DO 690 J = 1,18        
  690 IBUF1(J) = IBUF2(J)        
  700 DO 710 J = 1,18        
      IF (BUF(J1ST) .NE. IBUF1(J)) GO TO 720        
  710 J1ST = J1ST + 1        
      GO TO 760        
  720 GO TO MZ1, (730,740)        
  730 IF (RSHIFT(BUF(J1ST),NSHIFT).LT.RSHIFT(IBUF1(J),NSHIFT)) GO TO 755
      GO TO 750        
  740 IF (DEC) GO TO  745        
      IF (BUF(J1ST) .LT. IBUF1(J)) GO TO 755        
      GO TO 750        
  745 IF (KHRFN4(BUF(J1ST)).LT.KHRFN4(IBUF1(J))) GO TO 755        
  750 TRIAL = KTAPE        
      KTAPE = JTAPE        
      JTAPE = TRIAL        
      ISEQ  = ISEQ - 1        
      CALL OPEN (*1700,ITAPE,BUF(1),0)        
      CALL OPEN (*1700,KTAPE,BUF(IBUFSZ+1),3)        
      GO TO 1210        
C        
C     THIS SECTION PERFORMS A 2 TAPE ALPHANUMERIC MERGE        
C     (ITAPE+JTAPE=KTAPE)        
C     SAME BASIC LOGIC AS ORIGINAL SORT COMPARES (COMMENT CARDS OMITTED)
C        
  755 NOTSOR = 1        
  760 CALL OPEN (*1700,ITAPE,BUF(1),0)        
  770 CALL OPEN (*1700,JTAPE,BUF(IBUFSZ+1),0)        
      NBUF2 = 2*IBUFSZ + 1        
      CALL OPEN (*1700,KTAPE,BUF(NBUF2),1)        
      CCNT = 0        
  780 CALL READ (*1190,*1710,JTAPE,IBUF2,20,1,IFLG)        
      IF (MACH.EQ.2 .AND. (JTAPE.EQ.UMF .OR. JTAPE.EQ.IPTP))        
     1    CALL UMFTRN (IBUF2)        
      IF (MACH.EQ.3 .AND. KIN.EQ.1) CALL UMFFD (IBUF2)        
      LDUP = 0        
      IF (ITAPE .EQ. OPTP) CALL CRDFLG (IBUF2)        
      KTARSW = 0        
      IF (.NOT.DEC) ITST = ANDF(MKA,IBUF2(2))        
      IF (     DEC) ITST = KHRFN1(BKMSK2,4,IBUF2(2),4)        
      IF (ITST .EQ. STAR) KTARSW = 1        
      GO TO MY1, (790,800)        
  790 IBUF2A(1) = RSHIFT(IBUF2(1),NSHIFT)        
      IBUF2A(2) = RSHIFT(IBUF2(2),NSHIFT)        
  800 CALL READ (*1240,*1710,ITAPE,IBUF1,20,1,IFLG)        
      IF (MACH.EQ.2 .AND. (ITAPE.EQ.UMF .OR. ITAPE.EQ.IPTP))        
     1    CALL UMFTRN (IBUF1)        
      IF (MACH.EQ.3 .AND. KIN.EQ.1) CALL UMFFD (IBUF1)        
      STARSW = 0        
      IF (.NOT.DEC) ITST = ANDF(MKA,IBUF1(2))        
      IF (     DEC) ITST = KHRFN1(BKMSK2,4,IBUF1(2),4)        
      IF (ITST .EQ. STAR) STARSW = 1        
      GO TO IBRANB, (830,810)        
  810 GO TO MY2, (820,970)        
  820 IBUF1A(1) = RSHIFT(IBUF1(1),NSHIFT)        
      IBUF1A(2) = RSHIFT(IBUF1(2),NSHIFT)        
      GO TO 970        
C        
C     TEST IF CARD IS TO BE DELETED        
C        
  830 CCNT = CCNT + 1        
      IF (.NOT.DEC) TST = ANDF(MK(3),IBUF1(1))        
      IF (     DEC) TST = KHRFN1(BKMSK2,1,IBUF1(1),1)        
      ICCFLG = -1        
      IF (TST.EQ.PLUS .OR. TST.EQ.STARL) GO TO 860        
      CALL EXTINT (IBUF1(1))        
      GO TO MY6, (840,850)        
  840 IBUF1A(1) = RSHIFT(IBUF1(1),NSHIFT)        
      IBUF1A(2) = RSHIFT(IBUF1(2),NSHIFT)        
  850 ICCFLG    = 0        
      KPARNT(1) = IBUF1(1)        
      KPARNT(2) = IBUF1(2)        
  860 GO TO IBRANC, (870,880,900)        
  870 CALL READ (*920,*1710,ITAPE5,IBUF3,2,1,IFLG)        
      IF (IBUF3(1) .EQ. 0) GO TO 870        
      ASSIGN 880 TO IBRANC        
  880 IF (IBUF3(2) .NE.    0) GO TO 890        
      IF (IBUF3(1) .NE. CCNT) GO TO 900        
      ASSIGN 870 TO IBRANC        
      GO TO 930        
  890 IF (IBUF3(2) .EQ. CCNT) ASSIGN 870 TOIBRANC        
      IF (IBUF3(1).LE.CCNT .AND. IBUF3(2).GE.CCNT) GO TO 930        
C        
C     REMOVE ANY UNDELETED CONTINUATION CARDS DURING RESTART MERGE      
C        
  900 IF (ICCFLG .EQ. 0) GO TO IBRANE, (970,1220)        
      CALL WRITE (ITAPE4,IBUF1(1),20,1)        
  910 GO TO IBRAND, (800,1210)        
  920 ASSIGN 900 TO IBRANC        
      CALL CLOSE (ITAPE5,1)        
      GO TO 900        
C        
C     IF CONTINUATION WAS DELETED, FLAG PARENT        
C        
  930 IF (ICCFLG .EQ. 0) GO TO 940        
      CALL CRDFLG (KPARNT)        
      GO TO 910        
  940 CALL CRDFLG (IBUF1)        
      GO TO 910        
  950 CALL READ (*1190,*1710,JTAPE,IBUF2,20,1,IFLG)        
      IF (MACH.EQ.2 .AND. (JTAPE.EQ.UMF .OR. JTAPE.EQ.IPTP))        
     1    CALL UMFTRN (IBUF2)        
      IF (MACH.EQ.3 .AND. KIN.EQ.1) CALL UMFFD (IBUF2)        
      IF (ITAPE .EQ. OPTP) CALL CRDFLG (IBUF2)        
      KTARSW = 0        
      IF (.NOT.DEC) ITST = ANDF(MKA,IBUF2(2))        
      IF (     DEC) ITST = KHRFN1(BKMSK2,4,IBUF2(2),4)        
      IF (ITST .EQ. STAR) KTARSW = 1        
      GO TO MY3, (960,970)        
  960 IBUF2A(1) = RSHIFT(IBUF2(1),NSHIFT)        
      IBUF2A(2) = RSHIFT(IBUF2(2),NSHIFT)        
  970 J  = 1        
      J1 = 1        
      J2 = 1        
      NI = 0        
      KI = 0        
  980 GO TO MY4, (990,1040)        
  990 IF (IBUF1A(J1)-IBUF2A(J2)) 1050,1070,1060        
 1000 IF (STARSW .EQ. KTARSW) GO TO 980        
      IF (DEC) GO TO 1005        
      IN1 = RSHIFT(ANDF(MKD,IBUF1(J1)),1)        
      IK2 = RSHIFT(ANDF(MKD,IBUF2(J2)),1)        
      GO TO 1008        
 1005 IN1 = RSHIFT(KHRFN4(KHRFN1(IBUF1(J1),4,BKMSK2,1)),1)        
      IK2 = RSHIFT(KHRFN4(KHRFN1(IBUF2(J2),4,BKMSK2,1)),1)        
 1008 IF (IN1 .NE. IK2) GO TO 1018        
      IF (DEC) GO TO 1010        
      IN1 = ANDF(MKE,IBUF1(J1))        
      IK2 = ANDF(MKE,IBUF2(J2))        
      GO TO 1015        
 1010 IN1 = RSHIFT(LSHIFT(KHRFN4(KHRFN1(IBUF1(J1),4,BKMSK2,1)),1),1)    
      IK2 = RSHIFT(LSHIFT(KHRFN4(KHRFN1(IBUF2(J2),4,BKMSK2,1)),1),1)    
 1015 IF (IN1 .EQ. IK2) GO TO 1070        
 1018 IF (IN1 .LT. IK2) GO TO 1050        
      GO TO 1060        
 1020 IF (IBUF1(J1) .EQ. IBUF2(J2)) GO TO 1070        
      IF (IBUF1(J1) .EQ. BK(4)    ) GO TO 1050        
      IF (IBUF2(J2) .EQ. BK(4)    ) GO TO 1060        
      GO TO MY5, (1030,1040)        
 1030 IF (RSHIFT(IBUF1(J1),1)-RSHIFT(IBUF2(J2),1)) 1050,1070,1060       
 1040 IF (DEC) GO TO 1045        
      IF (IBUF1(J1) .LT. IBUF2(J2)) GO TO 1050        
      IF (IBUF1(J1) .GT. IBUF2(J2)) GO TO 1060        
      GO TO 1070        
 1045 IF (KHRFN4(IBUF1(J1))-KHRFN4(IBUF2(J2))) 1050,1070,1060        
 1050 CALL WRITE (KTAPE,IBUF1,20,1)        
      KOP = 1        
      GO TO 800        
 1060 CALL WRITE (KTAPE,IBUF2,20,1)        
      KOP = 2        
      GO TO 950        
 1070 J  = J  + 1        
      J1 = J1 + 1        
      J2 = J2 + 1        
      GO TO (1760,1000,1120,1020,1160,1020,1080,1020,1160,1020,1170,    
     1       1020,1160,1020,1080,1020,1160,1020,1180), J        
 1080 IF (NI-KI) 1100,1110,1090        
 1090 J1 = J1 + NI        
      NI = 0        
      GO TO 1110        
 1100 J2 = J2 + KI        
      KI = 0        
 1110 CALL XFADJ (IBUF1(J1),STARSW,K1)        
      CALL XFADJ (IBUF2(J2),KTARSW,K2)        
      GO TO 1130        
 1120 IF (STARSW .EQ. KTARSW) GO TO 1020        
      K1 = 0        
      K2 = 0        
      IF (DEC) GO TO 1122        
      IF (ANDF(MK(3),IBUF1(J1)) .NE. BKMSK1(4)) K1 = 1        
      IF (ANDF(MK(3),IBUF2(J2)) .NE. BKMSK1(4)) K2 = 1        
      GO TO 1130        
 1122 IF (KHRFN1(BKMSK2,1,IBUF1(J1),1) .NE. BKMSK1(4)) K1 = 1        
      IF (KHRFN1(BKMSK2,1,IBUF2(J2),1) .NE. BKMSK1(4)) K2 = 1        
 1130 IF (STARSW-KTARSW) 1150,1020,1140        
 1140 NI = 2        
      IF (K1+K2 .EQ. 2) GO TO 1020        
      J1 = J1 + 2        
      NI = 0        
      GO TO 1020        
 1150 KI = 2        
      IF (K1+K2 .EQ. 2) GO TO 1020        
      J2 = J2 + 2        
      KI = 0        
      GO TO 1020        
 1160 IF (STARSW .NE. KTARSW) GO TO 1080        
      IF (STARSW .EQ.      0) GO TO 1080        
      GO TO 1020        
C        
C     DUPLICATE CARD        
C        
 1170 IF (STARSW .EQ. KTARSW) GO TO 1080        
 1180 CALL WRITE (KTAPE,IBUF1,20,1)        
      CALL WRITE (KTAPE,IBUF2,20,1)        
      LDUP = -1        
      GO TO 780        
C        
C     ONE OF TWO TAPES BEING MERGED IS EXHAUSTED, OTHER TAPE IS COPIED  
C     ONTO THE MERGE TAPE        
C        
 1190 IF (ITAPE .NE. OPTP) GO TO 1200        
      ASSIGN 1210 TO IBRAND        
      ASSIGN 1220 TO IBRANE        
      ASSIGN 830 TO IBRANF        
      IF (CCNT .EQ. 0) GO TO 1210        
 1200 IF (LDUP .LT. 0) GO TO 1210        
      GO TO 1220        
 1210 CALL READ (*1250,*1710,ITAPE,IBUF1,20,1,IFLG)        
      IF (MACH.EQ.2 .AND. (ITAPE.EQ.UMF .OR. ITAPE.EQ.IPTP))        
     1    CALL UMFTRN (IBUF1)        
      IF (MACH.EQ.3 .AND. KIN.EQ.1) CALL UMFFD (IBUF1)        
      GO TO IBRANF, (830,1220)        
 1220 CALL WRITE (KTAPE,IBUF1,20,1)        
      KOP = 1        
      GO TO 1210        
 1230 CALL READ (*1250,*1710,JTAPE,IBUF2,20,1,IFLG)        
      IF (MACH.EQ.2 .AND. (JTAPE.EQ.UMF .OR. JTAPE.EQ.IPTP))        
     1    CALL UMFTRN (IBUF2)        
      IF (MACH.EQ.3 .AND. KIN.EQ.1) CALL UMFFD (IBUF2)        
 1240 CALL WRITE (KTAPE,IBUF2,20,1)        
      KOP = 2        
      GO TO 1230        
C        
C     CLOSE TAPES ENVOLVED IN MERGE        
C        
 1249 CALL CLOSE (ITAPE,2)        
      GO TO 1251        
 1250 IF (IUEDIT .EQ.   1) GO TO 1249        
      IF (ITAPE .EQ. OPTP) GO TO 1249        
      CALL CLOSE (ITAPE,1)        
 1251 CALL CLOSE (JTAPE,3)        
      CALL CLOSE (KTAPE,3)        
      GO TO IBRANA, (1260,1440)        
C        
C     WAS THIS THE FINAL MERGE (LAST CORE LOAD OF CARDS)        
C        
 1260 IF (IEND .EQ. 0) GO TO 160        
      CALL PAGE2 (2)        
      WRITE (OUTTAP,1660)        
      CALL CLOSE (ITAPE5,1)        
C        
C     PROCESS DELETE CARDS (IF ANY)        
C        
      NBUF4 = 4*IBUFSZ + 1        
      CALL OPEN (*1700,ITAPE5,BUF(NBUF4),0)        
C        
C     IF NOT RESTART - NO DELETES SHOULD EXIST        
C        
      IF (IRESTR.GT.0 .OR. KIN.GT.0) GO TO 1280        
C        
      CALL READ (*1440,*1710,ITAPE5,IBUF3,1,1,IFLG)        
C        
C     NOT RESTART AND DELETES DO EXIST - WARNING        
C        
      CALL CLOSE (ITAPE5,1)        
      CALL PAGE2 (2)        
      WRITE  (OUTTAP,1270) UWM        
 1270 FORMAT (A25,' 205, COLD START,DELETE CARDS IGNORED.')        
      GO TO 1440        
C        
C     FORM DELETE CARD LIST        
C        
 1280 IBUF3(1)  = INF        
      BUF(II  ) = MKB        
      BUF(II+1) = MKB        
      DO 1320 J = II,IBUFLG,2        
      CALL READ (*1330,*1710,ITAPE5,IBUF3,2,1,IFLG)        
      DO 1290 I = II,J,2        
      IF (IBUF3(1) .LE. BUF(I)) GO TO 1300        
 1290 CONTINUE        
C        
C     PUSH DOWN LIST - MAKE DOUBLE WORD SLOT        
C        
 1300 KK = J + 2        
      K1 = (J-I)/2 + 1        
      DO 1310 K = 1,K1        
      BUF(KK+1) = BUF(KK-1)        
      BUF(KK  ) = BUF(KK-2)        
 1310 KK = KK - 2        
      BUF(I  ) = IBUF3(1)        
 1320 BUF(I+1) = IBUF3(2)        
C        
C     IF DELETE CARD LIST WILL NOT FIT        
C        
      CALL MESAGE (-8,0,NSORT)        
C        
C     EOF ON ITAPE5, IF IBUF3(1)= INF, THERE ARE NO DELETE CARDS        
C        
 1330 IF (IBUF3(1) .EQ. INF) GO TO 1400        
      J = J - 1        
C        
C     CHECK FOR AND ELIMINATE OVERLAPS AND REDUNDANCYS IN DELETES       
C        
      IMIN = 0        
      DO 1380 I = II,J,2        
      IF (BUF(I) .EQ. 0) GO TO 1380        
      IF (BUF(I) .LT. BUF(I+1)) GO TO 1340        
      BUF(I+1) = 0        
      IF (BUF(I) .EQ. BUF(I+2)) GO TO 1350        
      IF (IMIN .EQ. 0) GO TO 1380        
      IF (BUF(I) .GT. BUF(IMAX)) GO TO 1370        
      GO TO 1350        
 1340 IF (IMIN .EQ. 0) GO TO 1360        
      IF (BUF(I  ) .GT. BUF(IMAX)) GO TO 1360        
      IF (BUF(I+1) .LT. BUF(IMAX)) GO TO 1350        
      BUF(IMAX) = BUF(I+1)        
 1350 BUF(I) = 0        
      GO TO 1380        
 1360 IMIN = I        
      IMAX = I + 1        
      GO TO 1380        
 1370 IMIN = 0        
 1380 CONTINUE        
      CALL CLOSE (ITAPE5,1)        
C        
C     PUT OUT SORTED DELETE CARD LIST        
C        
      NBUF4 = 4*IBUFSZ + 1        
      CALL OPEN (*1700,ITAPE5,BUF(NBUF4),1)        
      DO 1390 I = II,J,2        
      IF (BUF(I) .EQ. 0) GO TO 1390        
      CALL WRITE (ITAPE5,BUF(I),2,1)        
 1390 CONTINUE        
 1400 CALL CLOSE (ITAPE5,1)        
C        
C     AT THIS POINT, IF THIS IS A RESTART, MERGE OPTP, FINAL KTAPE,     
C     + DELETE        
C        
      ASSIGN 1440 TO IBRANA        
      ASSIGN 830  TO IBRANB        
      ASSIGN 870  TO IBRANC        
      ASSIGN 800  TO IBRAND        
      ASSIGN 970  TO IBRANE        
      NBUF4 = 4*IBUFSZ + 1        
      CALL OPEN (*1700,ITAPE5,BUF(NBUF4),0)        
C        
      IF (KIN .GT. 0) GO TO 1430        
C        
      CALL OPEN (*1740,OPTP,BUF(1),0)        
 1410 CALL READ (*1730,*1710,OPTP,IBUF3,2,1,IFLG)        
      IF (IBUF3(1).EQ.IBLKDA(1) .AND. IBUF3(2).EQ.IBLKDA(2)) GO TO 1420 
      CALL SKPFIL (OPTP,+1)        
      GO TO 1410        
 1420 ITAPE = OPTP        
      TRIAL = JTAPE        
      JTAPE = KTAPE        
      KTAPE = TRIAL        
      IF (ICCNT .EQ. 1) GO TO 1440        
      CALL WRITE (ITAPE4,MKB,20,1)        
      GO TO 770        
C        
 1430 OPTP = UMF        
      CALL OPEN (*50,UMF,BUF(1),2)        
      GO TO 1420        
C        
C     PROCESS CONTINUATION CARDS (IF ANY)        
C        
 1440 CALL CLOSE (ITAPE4,1)        
      NBUF3 = 3*IBUFSZ + 1        
      CALL OPEN (*1700,ITAPE4,BUF(NBUF3),0)        
      IF (ICCNT.EQ.1 .AND. (IRESTR.GT.0 .OR. KIN.GT.0)) KTAPE = OPTP    
      IF (ICCNT.EQ.1 .AND. (IRESTR.GT.0 .OR. KIN.GT.0)) GO TO 1441      
      NBUF2 = 2*IBUFSZ + 1        
      CALL OPEN (*1700,KTAPE,BUF(NBUF2),0)        
C        
C     FORM CONTINUATION CARD DICTIONARY        
C        
 1441 CONTINUE        
      IBUF1(1) = 0        
      DO 1470 J = II,IBUFLG,4        
      CALL READ (*1480,*1710,ITAPE4,IBUF1,20,1,IFLG)        
      IF (MACH.EQ.2 .AND. IBUF1(1).NE.MKB) CALL UMFTRN (IBUF1)        
      IF (MACH.EQ.3 .AND. KIN.EQ.1) CALL UMFFD (IBUF1)        
      IF (IBUF1(1) .NE. MKB) GO TO 1460        
      IF (J .EQ. II) GO TO 1450        
      ICCBRK = J        
 1450 BUF(J) = DOLLAR        
      GO TO 1465        
 1460 IF (.NOT.DEC) BUF(J) = ANDF(MKC,IBUF1(1))        
      IF (     DEC) BUF(J) = KHRFN1(IBUF1(1),1,BKMSK2,1)        
 1465 BUF(J+1) = IBUF1(2)        
      IF (.NOT.DEC) BUF(J+2) = ANDF(MKC,IBUF1(19))        
      IF (     DEC) BUF(J+2) = KHRFN1(IBUF1(19),1,BKMSK2,1)        
      BUF(J+3) = IBUF1(20)        
 1470 CONTINUE        
C        
C        
C     CORE INSUFFICIENT TO ACCOMMODATE 4-WORD PER CARD DICTIONARY       
C     OF CONTINUATION CARDS        
C        
      CALL MESAGE (-8,0,NSORT)        
C        
C     EOF ON ITAPE4, IF IBUF1(1)= 0, THERE ARE NO CONTINUATION CARDS    
C        
 1480 IF (IBUF1(1) .EQ. 0) GO TO 1510        
      CALL REWIND (ITAPE4)        
      JO = 1        
      ICONLG = J - 1        
C        
C     CHECK AND SET FLAGS FOR DUPLICATE CONTINUATION CARDS        
C        
      K = ICONLG - 4        
      IF (K .LE. II) GO TO 1510        
      DO 1500 J = II,K,4        
      IF (BUF(J) .EQ. IDUP) GO TO 1500        
      INDEX = 0        
      M = J + 4        
      DO 1490 JJ = M,ICONLG,4        
      IF (BUF(JJ ) .EQ. IDUP     ) GO TO 1490        
      IF (BUF(J  ) .NE. BUF(JJ)  ) GO TO 1490        
      IF (BUF(J+1) .NE. BUF(JJ+1)) GO TO 1490        
      BUF(JJ) = IDUP        
      INDEX = 1        
 1490 CONTINUE        
      IF (INDEX .EQ. 1) BUF(J) = IDUP        
 1500 CONTINUE        
C        
C     SET UP AND PUT OUT SORTED HEADING        
C        
 1510 IF (NOTSOR .EQ. 0) GO TO 1515        
      CALL PAGE2 (2)        
      WRITE  (OUTTAP,1511) UIM        
 1511 FORMAT (A29,' 207, BULK DATA NOT SORTED, XSORT WILL RE-ORDER ',   
     1       'DECK.')        
 1515 IF (ECHOS .EQ. 0) GO TO 1530        
      DO 1520 J = 1,32        
 1520 HEAD1(J) = HEADS(J)        
      HEAD2(4) = CDCNT(1)        
      HEAD3(4) = CDCNT(2)        
      HEAD3(5) = CDCNT(3)        
      CALL PAGE        
      CCNT = 0        
 1530 CALL CLOSE (ITAPE5,1)        
      J = II        
      NBUF4 = 4*IBUFSZ + 1        
      CALL OPEN (*1750,NPTP,BUF(NBUF4),3)        
      CALL WRITE (NPTP,IBLKDA,2,1)        
      IF (IBUF1(1) .EQ. 0) GO TO 1630        
C        
C     MERGE CONTINUATION CARDS - PRODUCE DATA ON NPTP        
C        
 1540 CALL READ (*1640,*1710,KTAPE,IBUF1,20,1,IFLG)        
      IF (ICCBRK .EQ. 0) GO TO 1550        
      KPARNT(1) = IBUF1(1)        
      KPARNT(2) = IBUF1(2)        
 1550 CALL INTEXT (IBUF1(1))        
      IF (MACH .EQ. 2) CALL UMFTRN (IBUF1)        
      IF (MACH.EQ.3 .AND. KIN.EQ.1) CALL UMFFD (IBUF1)        
      CALL WRITE (NPTP,IBUF1,20,1)        
      IF (ECHOS .EQ. 0) GO TO 1551        
      CALL PAGE2 (-1)        
      CCNT = CCNT + 1        
      CALL XPRETY (IBUF1)        
      WRITE (OUTTAP,210) CCNT,IBUF1        
C        
C      PUNCH OUT DECK        
C        
 1551 IF (ECHOP .EQ. 0) GO TO 1554        
      IF (ECHOS .NE. 0) GO TO 1552        
      CALL XPRETY (IBUF1)        
 1552 WRITE  (LPCH,1553) IBUF1        
 1553 FORMAT (20A4)        
 1554 CONTINUE        
C        
C     SEE IF PREVIOUS CARD HAS A CONTINUATION        
C     IF CONTINUATION FIELD BLANK - CONTINUATION NOT POSSIBLE        
C        
      IF (IBUF1(19).EQ.BK(4) .AND. IBUF1(20).EQ.BK(4)) GO TO 1540       
      IF (.NOT.DEC) TRIAL = ANDF(MKC,IBUF1(19))        
      IF (     DEC) TRIAL = KHRFN1(IBUF1(19),1,BKMSK2,1)        
      JN = 0        
 1571 CONTINUE        
C        
      DO 1601 K = II,ICONLG,4        
C        
C     IGNORE DUPLICATE CONTINUATION CARDS        
C        
      IF (BUF(J) .EQ. IDUP) GO TO 1600        
      IF (IBUF1(20) .NE. BUF(J+1)) GO TO 1600        
      IF (.NOT.DEC) ITST = ANDF(MKC,BUF(J))        
      IF (     DEC) ITST = KHRFN1(BUF(J),1,BKMSK2,1)        
      IF (ITST .NE. TRIAL) GO TO 1600        
C        
C     A CONTINUATION EXISTS, HAS IT ALREADY BEEN USED        
C        
      IF (.NOT.DEC) ITST = ANDF(MK(3),BUF(J))        
      IF (     DEC) ITST = KHRFN1(BKMSK2,1,BUF(J),1)        
      IF (ITST .EQ. DOLLAR) GO TO 1610        
      IF (J .GT. ICCBRK) GO TO 1580        
      CALL CRDFLG (KPARNT)        
 1580 IF (.NOT.DEC) BUF(J) = ORF(BUF(J),DOLLAR)        
      IF (     DEC) BUF(J) = KHRFN1(BUF(J),1,DOLLAR,1)        
      JN = (J-II)/4 + 1        
      CALL XRECPS (JN,JO)        
      CALL READ (*1720,*1710,ITAPE4,IBUF1,20,1,IFLG)        
      IF (MACH .EQ. 2) CALL UMFTRN (IBUF1)        
      IF (MACH.EQ.3 .AND. KIN.EQ.1) CALL UMFFD (IBUF1)        
      CALL WRITE (NPTP,IBUF1,20,1)        
      IF (ECHOS .EQ. 0) GO TO 1581        
      CALL PAGE2 (-1)        
      CCNT = CCNT+ 1        
      WRITE (OUTTAP,210) CCNT,IBUF1        
 1581 IF (ECHOP .EQ. 0) GO TO 1584        
      WRITE (LPCH,1553) IBUF1        
 1584 CONTINUE        
      IF (.NOT.DEC) TRIAL = ANDF(MKC,IBUF1(19))        
      IF (     DEC) TRIAL = KHRFN1(IBUF1(19),1,BKMSK2,1)        
      IF (IBUF1(19).EQ.BK(4) .AND. IBUF1(20).EQ.BK(4)) GO TO 1540       
      GO TO 1571        
 1600 J = J + 4        
      IF (J .GT. ICONLG) J = II        
 1601 CONTINUE        
      GO TO 1540        
C        
C     DUPLICATE PARENT - ERROR        
C        
 1610 NL = 0        
      IF (ECHOS .NE. 0) GO TO 1612        
      NL = 1        
      WRITE (OUTTAP,200) IBUF1        
 1612 NL = NL +2        
      CALL PAGE2 (-NL)        
      WRITE  (OUTTAP,1620) UFM        
 1620 FORMAT (A23,' 208, PREVIOUS CARD IS A DUPLICATE PARENT.')        
      NOGO = -1        
      GO TO 1540        
C        
C     NO CONTINUATION CARDS        
C        
 1630 CALL READ (*1640,*1710,KTAPE,IBUF2,20,1,IFLG)        
      IF (ICCNT .EQ. 1) GO TO 1631        
      CALL INTEXT (IBUF2(1))        
 1631 CONTINUE        
      IF (MACH .EQ. 2) CALL UMFTRN (IBUF2)        
      IF (MACH.EQ.3 .AND. KIN.EQ.1) CALL UMFFD (IBUF2)        
      CALL WRITE (NPTP,IBUF2,20,1)        
      IF (ECHOS .EQ. 0) GO TO 16311        
      CALL PAGE2 (-1)        
      CCNT = CCNT + 1        
      CALL XPRETY (IBUF2)        
      WRITE (OUTTAP,210) CCNT,IBUF2        
16311 IF (ECHOP .EQ. 0) GO TO 1630        
      IF (ECHOS .NE. 0) GO TO 1632        
      CALL XPRETY (IBUF2)        
 1632 WRITE (LPCH,1553) IBUF2        
      GO TO 1630        
C        
C     CLOSE KTAPE AND WRITE (ENDDATA)        
C        
 1640 CALL CLOSE (KTAPE,2)        
      CALL EOF   (NPTP)        
      CALL CLOSE (NPTP,1)        
      IF (ECHOS .EQ. 0) GO TO 1650        
      CALL PAGE2 (-1)        
      WRITE (OUTTAP,200) IIEND        
 1650 IF (IBUF1(1) .EQ. 0) GO TO 1690        
      CALL PAGE2 (2)        
      WRITE  (OUTTAP,1660)        
 1660 FORMAT ('0')        
C        
C     IDENTIFY DUPLICATE OR PARENTLESS CONTINUATION CARDS        
C        
      NCNT = 0        
      DO 1670 J = II,ICONLG,4        
      IF (.NOT.DEC) ITST = ANDF(MK(3),BUF(J))        
      IF (     DEC) ITST = KHRFN1(BKMSK2,1,BUF(J),1)        
      IF (ITST .EQ. DOLLAR) GO TO 1670        
C        
C     CHECK FOR DUPLICATE CONTINUATION CARDS        
C        
      IF (BUF(J) .EQ. IDUP) GO TO 1666        
C        
C     CHECK FOR PARENTLESS CONTINUATION CARDS        
C        
      DO 1664 JJ = II,ICONLG,4        
      IF (J .EQ. JJ) GO TO 1664        
      IF (BUF(J).EQ.BUF(JJ+2) .AND. BUF(J+1).EQ.BUF(JJ+3)) GO TO 1668   
 1664 CONTINUE        
 1666 NCNT = NCNT + 1        
      JN   = (J-II)/4 + 1        
      CALL XRECPS (JN,JO)        
      CALL READ (*1720,*1710,ITAPE4,IBUF2,20,1,IFLG)        
      IF (MACH .EQ. 2) CALL UMFTRN (IBUF2)        
      IF (MACH.EQ.3 .AND. KIN.EQ.1) CALL UMFFD (IBUF2)        
      CALL PAGE2 (-1)        
      WRITE (OUTTAP,200) IBUF2        
      GO TO 1670        
 1668 BUF(J) = IOK        
 1670 CONTINUE        
      IF (NCNT .EQ. 0) GO TO 1690        
      CALL PAGE2 (3)        
      WRITE  (OUTTAP,1680) UFM,NCNT        
 1680 FORMAT (A23,' 209, PREVIOUS',I7,' CONTINUATION MNEMONICS HAVE NO',
     1       ' PARENTS AND/OR ARE DUPLICATES.',/)        
      NOGO = -1        
C        
C     IDENTIFY THOSE CONTINUATION CARDS THAT ARE VALID, BUT YET CANNOT  
C     BE PROCESSED BECAUSE OF ERRORS ON OTHER RELATED CONTINUATION CARDS
C        
      NCNT = 0        
      DO 1684 J = II,ICONLG,4        
      IF (BUF(J) .NE. IOK) GO TO 1684        
      NCNT = NCNT + 1        
      JN   = (J-II)/4 + 1        
      CALL XRECPS (JN,JO)        
      CALL READ (*1720,*1710,ITAPE4,IBUF2,20,1,IFLG)        
      IF (MACH .EQ. 2) CALL UMFTRN (IBUF2)        
      IF (MACH.EQ.3 .AND. KIN.EQ.1) CALL UMFFD (IBUF2)        
      CALL PAGE2 (-1)        
      WRITE (OUTTAP,200) IBUF2        
 1684 CONTINUE        
      IF (NCNT .EQ. 0) GO TO 1690        
      CALL PAGE2 (4)        
      WRITE  (OUTTAP,1686) UFM,NCNT        
 1686 FORMAT (A23,' 206, PREVIOUS',I7,' CONTINUATION CARDS, THOUGH ',   
     1       'VALID, CANNOT BE PROCESSED', /5X,        
     2       'BECAUSE OF ERRORS ON OTHER RELATED CONTINUATION CARDS.',/)
 1690 CALL CLOSE (ITAPE4,1)        
C        
C     REACTIVE DIAG 47 TO PRINT THE CONTENTS OF NTPT        
C        
      L47 = 0        
C     CALL SSWTCH (47,L47)        
      IF (L47 .EQ. 0) GO TO 1699        
      CALL OPEN (*1750,NPTP,BUF(1),0)        
 1691 CALL SKPFIL (NPTP,+1)        
      CALL READ (*1697,*1697,NPTP,IBUF1(1),2,1,J)        
      IF (IBUF1(1).NE.IBLKDA(1) .OR. IBUF1(2).NE.IBLKDA(2)) GO TO 1691  
 1693 CALL READ (*1697,*1697,NPTP,IBUF1(1),20,1,J)        
      WRITE  (OUTTAP,1695) (IBUF1(J),J=1,10),(IBUF1(J),J=17,20)        
 1695 FORMAT (' ==NPTP==>',5(1X,2A4),'...',2(1X,2A4))        
      GO TO 1693        
 1697 CALL CLOSE (NPTP,1)        
 1699 CONTINUE        
C        
C     DISABLE FREE-FIELD INPUT OPTION IN XREAD.        
C        
      FFFLAG = 0        
      RETURN        
C        
C     ERROR MESSAGES        
C        
 1700 WRITE  (OUTTAP,1701) SFM        
 1701 FORMAT (A25,' 210, SCRATCH COULD NOT BE OPENED')        
      GO TO  1800        
 1710 WRITE  (OUTTAP,1711) SFM        
 1711 FORMAT (A25,' 211, ILLEGAL EOR ON SCRATCH')        
      GO TO  1800        
 1720 WRITE  (OUTTAP,1721) SFM        
 1721 FORMAT (A25,' 212, ILLEGAL EOF ON ITAPE4')        
      GO TO  1800        
 1730 WRITE  (OUTTAP,1731) SFM        
 1731 FORMAT (A25,' 213, ILLEGAL EOF ON OPTP')        
      GO TO  1800        
 1740 WRITE  (OUTTAP,1741) SFM        
 1741 FORMAT (A25,' 214, OPTP COULD NOT BE OPENED')        
      GO TO  1800        
 1750 WRITE  (OUTTAP,1751) SFM        
 1751 FORMAT (A25,' 215, NPTP COULD NOT BE OPENED')        
      GO TO  1800        
 1760 WRITE  (OUTTAP,1761) SFM        
 1761 FORMAT (A25,' 216, ILLEGAL INDEX')        
      GO TO  1800        
 1770 WRITE  (OUTTAP,1771) SFM        
 1771 FORMAT (A25,' 219, MISSING ENDDATA CARD.')        
 1800 CALL PAGE2 (2)        
      CALL MESAGE (-37,0,NSORT)        
      RETURN        
      END        
