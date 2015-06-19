      SUBROUTINE IFP1PC (I81,ICONT,POCARD,ORG,PORG)        
C        
C     SUBROUTINE TO PERFORM FIRST-LEVEL CHECKING OF STRUCTURE PLOTTER   
C     CONTROL CARD FORMAT.        
C        
      IMPLICIT INTEGER (A-Z)        
      EXTERNAL        RSHIFT,COMPLF        
      LOGICAL         FLAG(3),BIT64        
      INTEGER         CASE(400),CTYPE(21),IDVPR(3),CAMERA(5),ORIGIN(11),
     1                AXES(3),MAXES(3),CNTUR(20),SETPR(33),SETP2(12),   
     2                COORD(25),LBLPR(5),PLTPR(28),NAST(2),POCARD(1),   
     3                CORE(1),COREY(401)        
      CHARACTER       UFM*23,UWM*25        
      COMMON /XMSSG / UFM,UWM        
      COMMON /SYSTEM/ ISYS,NOUT,NOGO,SKP(16),PLTOPT,SYS21,ILINK,        
     1                SKP63(63),INTRA        
      COMMON /XIFP1 / BLANK,BIT64        
CZZ   COMMON /ZZIFP1/ COREX(1)        
      COMMON /ZZZZZZ/ COREX(1)        
      EQUIVALENCE     (PROJ,CTYPE(11)), (DEFO,IDVPR( 1)),        
     1                (SYMM,PLTPR(13)), (ANTI,PLTPR(14)),        
     2                (MAGN,CNTUR(13)), (THRU,PLTPR(22)),        
     3                (POIN,LBLPR( 2)), (CORE(1),COREY(401)),        
     4                (COREX(1),COREY(1),CASE(1)), (HIDD,PLTPR(24))     
      DATA    CTYPE / 4HPLOT, 4HORTH, 4HPERS, 4HSTER, 4HAXES, 4HVIEW,   
     1                4HMAXI, 4HCSCA, 4HFIND, 4HCONT, 4HPROJ, 4HOCUL,   
     2                4HCAME, 4HPAPE, 4HPEN , 4HPTIT, 4HSCAL, 4HORIG,   
     3                4HVANT, 4HSET , 4HREGI/        
      DATA    CAMERA/ 4HFILM, 4HPAPE, 4HBOTH, 4HBLAN, 4HFRAM/        
      DATA    AXES  / 4HX   , 4HY   , 4HZ   /        
      DATA    MAXES / 4HMX  , 4HMY  , 4HMZ  /        
      DATA    CNTUR / 4HMAJP, 4HMINP, 4HMAXS, 4HXNOR, 4HYNOR, 4HZNOR,   
     1                4HXYSH, 4HXZSH, 4HYZSH, 4HXDIS, 4HYDIS, 4HZDIS,   
     2                4HMAGN, 4HNRM1, 4HNRM2, 4HSH12, 4HSH1Z, 4HSH2Z,   
     3                4HBDSH, 4HSTRA/        
      DATA    SETPR / 4HINCL, 4HEXCL, 4HEXCE, 4HELEM, 4HGRID, 4HALL ,   
     1                4HAERO, 4HAXIF, 4HBAR , 4HCONE, 4HCONR, 4HHEXA,   
     2                4HFLUI, 4HIHEX, 4HPLOT, 4HQDME, 4HQDPL, 4HQUAD,   
     3                4HROD , 4HSHEA, 4HSLOT, 4HTETR, 4HTORD, 4HTRAP,   
     4                4HTRBS, 4HTRIA, 4HTRME, 4HTRPL, 4HTUBE, 4HTWIS,   
     5                4HVISC, 4HWEDG, 4HHBDY/        
      DATA    SETP2 / 4HAX  , 4HRG  , 4H1   , 4H2   , 4H3   , 4H4   ,   
     1                4HD2  , 4HD3  , 4HD4  , 4HM   , 4HM1  , 4HM2  /   
      DATA    PLTPR / 4HSET , 4HSTAT, 4HMODA, 4HCMOD, 4HFREQ, 4HTRAN,   
     1                4HCONT, 4HRANG, 4HTIME, 4HPHAS, 4HMAGN, 4HORIG,   
     2                4HSYMM, 4HANTI, 4HPEN , 4HDENS, 4HSYMB, 4HLABE,   
     3                4HSHAP, 4HVECT, 4HOUTL, 4HTHRU, 4HMAXI, 4HHIDD,   
     4                4HSHRI, 4HNOFI, 4HFILL, 4HOFFS/        
      DATA    IDVPR / 4HDEFO, 4HVELO, 4HACCE/        
      DATA    COORD / 4HYX  , 4HZX  , 4HZY  , 4HXY  , 4HXZ  , 4HYZ  ,   
     1                4HX   , 4HY   , 4HZ   ,        
     2                4HXYZ , 4HRXY , 4HRXZ , 4HRYZ , 4HR   , 4HRN  ,   
     3                4HXN  , 4HYN  , 4HZN  , 4HXYN , 4HXZN , 4HYZN ,   
     4                4HXYZN, 4HRXYN, 4HRXZN, 4HRYZN  /        
      DATA    LBLPR / 4HGRID, 4HPOIN, 4HELEM, 4HBOTH, 4HEPID/        
      DATA    TER   / 4HTER /, PLAN / 4HPLAN/, SEPA / 4HSEPA/        
      DATA    LAG   / 4HLAG /, NAST / 4HSC  , 4HCALC/,ILNK  / 4HNS01/   
C        
C        
C     INITIALIZE        
C        
      IF (INTRA.LE.1 .AND. ILINK.EQ.ILNK) GO TO 15        
      DO 5 I = 1,200        
    5 CORE(I)= POCARD(I)        
   15 ALLON  = COMPLF(0)        
      EOR    = RSHIFT(ALLON,1)        
      ISPLOT = 0        
C     BLANK0 = BLANK        
C     IF (BIT64) CALL MVBITS (0,0,32,BLANK0,0)        
      IWRD   = I81        
C        
C     BRANCH FOR CONTINUATION CARD        
C                                  SET   PLOT  FIND        
      IF (ICONT .NE. 0) GO TO (10, 2111, 2210, 1067), ICONT        
C        
      IF (CORE(IWRD)) 9800,350,20        
   10 IF (CORE(IWRD) .LE.   0) GO TO 320        
   20 IF (CORE(IWRD) .EQ. EOR) GO TO 350        
      MODE = CORE(IWRD)        
      IWRD = IWRD + 1        
C        
C     BRANCH FOR CARD TYPE        
C        
  100 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      DO 300 I = 1,20        
      IF (IWORD .EQ. CTYPE(I))        
     1   GO TO (400,  500,  500,  500,  600,  700,  800,  900, 1000,    
     2         1100, 1200, 1300, 1400,  320,  320,  320, 1800, 1900,    
     3         2000, 2100), I        
C        
C    1         PLOT  ORTH  PERS  STER  AXES  VIEW  MAXI  CSCA  FIND     
C    2         CONT  PROJ  OCUL  CAME  PAPE   PEN  PTIT  SCAL  ORIG     
C    3         VANT   SET        
C        
  300 CONTINUE        
      GO TO 9802        
  320 IF (MODE .LE. 0) GO TO 330        
      IWRD = IWRD + 2        
      MODE = MODE - 1        
      GO TO 320        
  330 IF (CORE(IWRD)) 335,340,340        
  335 IF (CORE(IWRD) .EQ. -4) IWRD = IWRD + 1        
      IWRD = IWRD + 2        
      GO TO 330        
  340 IF (CORE(IWRD).EQ.0 .OR. CORE(IWRD).EQ.EOR) GO TO 350        
      MODE = CORE(IWRD)        
      IWRD = IWRD + 1        
      GO TO 320        
  350 ICONT = 0        
      IF (CORE(IWRD) .EQ. 0) ICONT = 1        
      GO TO 9998        
C        
C     BRANCH TO PLOT OR PLOTTER        
C        
  400 IWORD = CORE(IWRD+1)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD .EQ. TER) GO TO 410        
      ISPLOT = 1        
      GO TO 2200        
C        
C     PLOTTER CARD        
C        
C 410 IF (CORE(IWRD+2) - NAST(1)) 420,9804,420        
C 420 IF (CORE(IWRD+2) - NAST(2)) 320,9804,320        
  410 IWORD = CORE(IWRD+2)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD.EQ.NAST(1) .OR. IWORD.EQ.NAST(2)) GO TO 9804        
      GO TO 320        
C        
C     PROJECTION CARD        
C        
  500 IWRD  = IWRD + 2        
      MODE  = MODE - 1        
      IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD .EQ. PROJ) GO TO 510        
      ASSIGN 510 TO IRTN        
      IPRM = PROJ        
      GO TO 9806        
  510 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 330,330,100        
C        
C     AXES CARD        
C        
  600 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 600        
      DO 605 J = 1,3        
  605 FLAG(J) = .FALSE.        
      I = 0        
      GO TO 607        
  606 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
  607 IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 610        
      DO 608 J = 1,3        
      IF (IWORD.EQ.AXES(J) .OR. IWORD.EQ.MAXES(J)) FLAG(J) = .TRUE.     
  608 CONTINUE        
      I = I + 1        
  610 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (I .LT. 3) GO TO 606        
C        
      ASSIGN 320 TO IRTN        
      IF (.NOT.FLAG(1) .OR. .NOT.FLAG(2) .OR. .NOT.FLAG(3)) GO TO 9810  
  620 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD.EQ.SYMM .OR. IWORD.EQ.ANTI) GO TO 630        
      IF (CORE(IWRD).EQ.0 .OR. CORE(IWRD).EQ.EOR) GO TO 350        
      IF (CORE(IWRD).NE.ALLON .AND. IWORD.NE.BLANK) GO TO 100        
      IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 9812,9812,620        
  630 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 330,330,100        
C        
C     VIEW COMMAND        
C        
  700 NREAL = 3        
      NOPT  = 0        
      GO TO 1310        
C        
C     MAXIMUM DEFORMATION CARD        
C        
  800 NREAL = 1        
      NOPT  = 0        
      IWORD = CORE(IWRD+2)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD .EQ. DEFO) GO TO 1310        
      ASSIGN 320 TO IRTN        
      IPRM = CORE(IWRD+2)        
      GO TO 9808        
C        
C     CSCALE CARD        
C        
  900 ASSIGN 320 TO IRTN        
  910 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 930,930,920        
  920 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 910        
      GO TO 9812        
  930 IF (CORE(IWRD)+1) 960,940,9816        
  940 WRITE  (NOUT,950)        
  950 FORMAT (/5X,'REAL VALUE, NOT INTEGER, IS NOW USED FOR CSCALE')    
      GO TO 9816        
C        
  960 NREAL = 1        
      NOPT  = 0        
      GO TO 1700        
C        
C     FIND COMMAND        
C        
 1000 IWRD = IWRD + 2        
      MODE = MODE - 1        
 1005 IF (CORE(IWRD).EQ.0 .OR. CORE(IWRD).EQ.EOR) GO TO 1080        
      ASSIGN 1070 TO IRTN        
      IF (MODE) 9812,9812,1006        
 1006 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 1000        
      DO 1008 I = 17,21        
      ITYPE = I - 16        
      IF (IWORD .EQ. CTYPE(I))        
     1    GO TO (1020, 1030, 1040, 1030, 1050), ITYPE        
C                SCAL  ORIG  VANT   SET  REGI        
C        
 1008 CONTINUE        
      IPRM = CORE(IWRD)        
      GO TO 9808        
C        
 1020 NREAL = 1        
 1021 IWRD  = IWRD + 2        
      MODE  = MODE - 1        
      IF (MODE .LE. 0) GO TO 1061        
      IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 1021        
      GO TO 1005        
C        
 1030 IPRM = CORE(IWRD)        
      ASSIGN 1005 TO IRTN        
 1031 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 1033,1033,1032        
 1032 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 1031        
      GO TO 9814        
 1033 INTEG = 1        
      IF (CORE(IWRD) .EQ. EOR) GO TO 9814        
      IF (CORE(IWRD) .EQ.  -1) INTEG = 0        
      IF (CORE(IWRD) .EQ.  -4) IWRD  = IWRD + 1        
      IF (ITYPE .NE. 2) GO TO 1034        
      FORG = CORE(IWRD+1)        
      ORG  = ORG + 1        
      ORIGIN(ORG) = FORG        
 1034 IWRD = IWRD + 2        
      IF (PORG .GE. 0) GO TO 1066        
      PORG  = 0        
      PORG1 = FORG        
      GO TO 1066        
C        
 1040 IWRD = IWRD + 2        
      MODE = MODE - 1        
      ASSIGN 1070 TO IRTN        
      IF (MODE) 1041,1041,1042        
 1041 IPRM = POIN        
      GO TO 9806        
 1042 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD .EQ. POIN) GO TO 1000        
      IPRM = CORE(IWRD)        
      GO TO 9808        
C        
 1050 NREAL = 4        
 1060 IWRD  = IWRD + 2        
      MODE  = MODE - 1        
      IF (MODE .LE. 0) GO TO 1062        
      IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 1060        
      GO TO 9818        
 1061 IF (CORE(IWRD).EQ.0 .OR. CORE(IWRD).EQ.EOR) GO TO 1080        
 1062 INTEG = 0        
      ASSIGN 1005 TO IRTN        
      DO 1065 I = 1,NREAL        
      IF (CORE(IWRD) .EQ. -1) INTEG = 1        
      IF (CORE(IWRD) .EQ. -4) IWRD  = IWRD + 1        
      IWRD = IWRD + 2        
 1065 CONTINUE        
 1066 IF (INTEG) 1067,1067,9816        
 1067 IF (CORE(IWRD).EQ.0 .OR. CORE(IWRD).EQ.EOR) GO TO 1080        
      MODE = CORE(IWRD)        
      IWRD = IWRD + 1        
      GO TO 1005        
C        
 1070 IF (CORE(IWRD).EQ.0 .OR. CORE(IWRD).EQ.EOR) GO TO 1080        
      IWRD = IWRD + 1        
      GO TO 1070        
 1080 ICONT = 0        
      IF (CORE(IWRD) .EQ. 0) ICONT = 4        
      GO TO 9998        
C        
C     CONTOUR        
C        
 1100 IWRD = IWRD + 2        
      MODE = MODE - 1        
      ASSIGN 320 TO IRTN        
 1105 IF (CORE(IWRD).EQ.0 .OR. CORE(IWRD).EQ.EOR) GO TO 350        
      IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 1110        
      DO 1108 I = 1,20        
      IF (IWORD .EQ. CNTUR(I)) GO TO 320        
 1108 CONTINUE        
      IPRM = CORE(IWRD)        
      GO TO 9808        
 1110 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 9812,9812,1105        
C        
C     PROJECTION PLANE SEPARATION        
C        
 1200 IWRD = IWRD + 2        
      MODE = MODE - 1        
      ASSIGN 320 TO IRTN        
      IF (MODE) 1210,1210,1220        
 1210 IPRM = PLAN        
      GO TO 9806        
C1220 IF (CORE(IWRD)-PLAN) 1231,1230,1231        
 1220 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD .NE. PLAN) GO TO 1231        
C1230 IF (CORE(IWRD+2) .EQ. SEPA) GO TO 1240        
      IWORD = CORE(IWRD+2)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD .EQ. SEPA) GO TO 1240        
 1231 IPRM = CORE(IWRD)        
      GO TO 9808        
 1240 NREAL = 1        
      NOPT  = 0        
      GO TO 1310        
C        
C     OCULAR SEPARATION        
C        
 1300 NREAL = 1        
      NOPT  = 0        
      IWORD = CORE(IWRD+2)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD .EQ. SEPA) GO TO 1310        
      ASSIGN 320 TO IRTN        
      IPRM = CORE(IWRD+2)        
      GO TO 9808        
C        
 1310 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 1700,1700,1310        
C        
C     CAMERA        
C        
 1400 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE .LE. 0) GO TO 1420        
      IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK ) GO TO 1400        
      IF (CORE(IWRD).EQ.EOR   .OR. CORE(IWRD).EQ.0) GO TO 9820        
      DO 1410 I = 1,4        
      IF (IWORD .EQ. CAMERA(I)) GO TO 1415        
 1410 CONTINUE        
      IPRM = CORE(IWRD)        
      ASSIGN 320 TO IRTN        
      GO TO 9808        
 1415 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE .LE. 0) GO TO 1420        
      IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 1415        
      I = I + 1        
      IF (IWORD.EQ.CAMERA(4) .OR. IWORD.EQ.CAMERA(5)) GO TO 1415        
      ASSIGN 320 TO IRTN        
      IF (I-4) 100,9812,100        
 1420 IF (CORE(IWRD).EQ.EOR .OR. CORE(IWRD).EQ.0) IF (I-3) 350,350,9820 
      ASSIGN 320 TO IRTN        
      IF (CORE(IWRD)+1) 9816,1430,9816        
 1430 IWRD = IWRD + 2        
      GO TO 10        
C        
C     TEST FOR REAL VALUES        
C        
 1700 IRO = 0        
      NRO = NREAL        
 1710 INTEG = 0        
      ASSIGN 320 TO IRTN        
      DO 1720 I = 1,NRO        
      IF (CORE(IWRD).GE.0 .OR. CORE(IWRD).LT.-4) IF (IRO) 9818,9818,1712
 1712 IF (CORE(IWRD) .EQ. -1) INTEG = 1        
      IF (CORE(IWRD) .EQ. -4) IWRD  = IWRD + 1        
      IWRD = IWRD + 2        
 1720 CONTINUE        
      IF (INTEG .EQ. 0) GO TO 1730        
      ASSIGN 1730 TO IRTN        
      GO TO 9816        
 1730 IF (CORE(IWRD)) 1740,350,20        
 1740 IF (IRO.EQ.1 .OR. NOPT.EQ.0) GO TO 9812        
      IRO = 1        
      NRO = NOPT        
      GO TO 1710        
C        
C     SCALE        
C        
 1800 NREAL = 1        
      NOPT  = 1        
      GO TO 1310        
C        
C     ORIGIN        
C        
 1900 NREAL = 3        
      NOPT  = 0        
 1905 IWRD  = IWRD + 2        
      MODE  = MODE - 1        
      IF (MODE) 1907,1907,1906        
 1906 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 1905        
 1907 IF (CORE(IWRD) .EQ. -1) GO TO 1910        
      IPRM = CTYPE(18)        
      ASSIGN 320 TO IRTN        
      GO TO 9814        
 1910 IF (CORE(IWRD) .EQ. -4) IWRD = IWRD + 1        
      IWRD = IWRD + 2        
      ASSIGN 320 TO IRTN        
      IF (CORE(IWRD) .EQ. EOR) GO TO 9818        
      IF (CORE(IWRD) .LT. 0) GO TO 1700        
      MODE  = CORE(IWRD)        
      IWRD  = IWRD + 1        
      IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 1310        
      GO TO 9812        
C        
C     VANTAGE POINT        
C        
 2000 NREAL = 3        
      NOPT  = 1        
      IWORD = CORE(IWRD+2)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD .EQ. POIN) GO TO 1310        
      ASSIGN 320 TO IRTN        
      IPRM = CORE(IWRD+2)        
      GO TO 9808        
C        
C     SET DEFINITION CARD        
C        
 2100 NINT = 0        
      NTHRU= 0        
 2105 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 2106,2106,2108        
 2106 IF (CORE(IWRD) .EQ. -1) GO TO 2110        
      ASSIGN 2107 TO IRTN        
      GO TO 9816        
 2107 IF (CORE(IWRD) .EQ. -4) IWRD = IWRD + 1        
      GO TO 2110        
 2108 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 2105        
      IPRM = CTYPE(20)        
      ASSIGN 2120 TO IRTN        
      GO TO 9814        
C        
 2110 IWRD  = IWRD + 2        
      NREAL = 0        
 2111 IF (CORE(IWRD)) 2112,2113,2114        
 2112 NINT  = NINT + 1        
      IF (CORE(IWRD).EQ.-1 .OR. NREAL.NE.0) GO TO 2110        
      ASSIGN 2110 TO IRTN        
      GO TO 9816        
 2113 ICONT = 2        
      NTHRU = 0        
      GO TO 9998        
 2114 IF (CORE(IWRD) .NE. EOR) GO TO 2115        
      ICONT = 0        
      GO TO 9998        
 2115 MODE  = CORE(IWRD)        
      IWRD  = IWRD + 1        
 2120 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).NE.ALLON .AND. IWORD.NE.BLANK) GO TO 2121        
      IWRD  = IWRD + 2        
      MODE  = MODE - 1        
      IF (MODE) 2111,2111,2120        
 2121 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD .NE. THRU) GO TO 2130        
      NTHRU = NTHRU + 1        
      IF (CORE(IWRD-3).EQ.-1 .AND. CORE(IWRD+2).EQ.-1) GO TO 2122       
      ASSIGN 2123 TO IRTN        
      NREAL = 1        
      GO TO 9822        
 2122 IF (NTHRU .EQ. 1) GO TO 2123        
      IF (NINT.GE.2 .AND. CORE(IWRD-2).GT.CORE(IWRD-4)) GO TO 2123      
      ASSIGN 2123 TO IRTN        
      GO TO 9824        
 2123 NINT = 0        
      IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 2111,2111,2130        
 2130 IF (CORE(IWRD) .EQ.   0) GO TO 2113        
      IF (CORE(IWRD) .EQ. EOR) GO TO 2114        
      IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 2135        
      DO 2132 I = 1,33        
      IF (IWORD .EQ. SETPR(I))        
     1   GO TO (2135, 2135, 2135, 2135, 2138, 2135, 2135, 2142, 2135,   
     2          2135, 2135, 2143, 2144, 2145, 2135, 2146, 2135, 2143,   
     3          2135, 2135, 2147, 2135, 2135, 2148, 2135, 2149, 2135,   
     4          2135, 2135, 2135, 2135, 2135, 2135), I        
C        
C    1          INCL  EXCL  EXCE  ELEM  GRID   ALL  AERO  AXIF   BAR    
C    2          CONE  CONR  HEXA  FLUI  IHEX  PLOT  QDME  QDPL  QUAD    
C    3           ROD  SHEA  SLOT  TETR  TORD  TRAP  TRBS  TRIA  TRME    
C    4          TRPL  TUBE  TWIS VISCX  WEDG  HBDY        
C        
 2132 CONTINUE        
      ASSIGN 2135 TO IRTN        
      IPRM = CORE(IWRD)        
      GO TO 9808        
 2135 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE .LE. 0) GO TO 2136        
      IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 2135        
      GO TO 2130        
 2136 NTHRU = 0        
      GO TO 2111        
C        
 2138 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 2139,2139,2140        
 2139 ASSIGN 2136 TO IRTN        
      IPRM = POIN        
      GO TO 9806        
 2140 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD .EQ. POIN) GO TO 2135        
      ASSIGN 2130 TO IRTN        
      IPRM = CORE(IWRD)        
      GO TO 9808        
C        
 2142 ISTT = 4        
      ISTB = 6        
      GO TO 2150        
C        
 2143 ISTT = 3        
      ISTB = 6        
      GO TO 2150        
C        
 2144 ISTT = 7        
      ISTB = 9        
      GO TO 2150        
C        
 2145 ISTT = 3        
      ISTB = 5        
      GO TO 2150        
C        
 2146 ISTT = 10        
      ISTB = 12        
      GO TO 2150        
C        
 2147 ISTT = 5        
      ISTB = 6        
      GO TO 2150        
C        
 2148 ISTT = 1        
      ISTB = 2        
      GO TO 2150        
C        
 2149 ISTT = 1        
      ISTB = 5        
C        
 2150 IWORD = CORE(IWRD+1)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      DO 2155 I = ISTT,ISTB        
      IF (IWORD .EQ. SETP2(I)) GO TO 2135        
 2155 CONTINUE        
      ASSIGN 2135 TO IRTN        
      IPRM = CORE(IWRD)        
      GO TO 9808        
C        
C     PLOT COMMAND CARD        
C        
 2200 IWRD = IWRD + 2        
      MODE = MODE - 1        
 2202 IF (CORE(IWRD).EQ.0 .OR. CORE(IWRD).EQ.EOR) GO TO 2215        
      IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 2207        
      DO 2205 I = 1,28        
      IF (IWORD .EQ. PLTPR(I))        
     1    GO TO (2208, 2220, 2220, 2220, 2230, 2230, 2207, 2250, 2250,  
     2           2260, 2207, 2208, 2280, 2280, 2208, 2208, 2208, 2290,  
     3           2207, 2281, 2207, 2248, 2240, 2207, 2245, 2207, 2207,  
     4           2208), I        
C        
C    1            SET  STAT  MODA  CMOD  FREQ  TRAN  CONT  RANG  TIME   
C    2           PHAS  MAGN  ORIG  SYMM  ANTI   PEN  DENS  SYMB  LABE   
C    3           SHAP  VECT  OUTL  THRU  MAXI  HIDD  SHRI  NOFI  FILL   
C    4           OFFS        
C        
 2205 CONTINUE        
      ASSIGN 2207 TO IRTN        
      IPRM = CORE(IWRD)        
      GO TO 9808        
C        
 2207 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 2210,2210,2202        
C        
 2208 IPRM = CORE (IWRD)        
      ASSIGN 2202 TO IRTN        
 2209 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE .LE. 0) GO TO 2210        
      IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 2209        
      GO TO 9814        
C        
C2210 IF (CORE(IWRD)  ) 2211,2215,2215        
C         CORE(IWRD)  =    -   0   +        
C2211 IF (CORE(IWRD)+1) 2213,2213,2212        
C         CORE(IWRD)  =   -2  -1   0        
C2212 ASSIGN 2213 TO IRTN        
C     GO TO 9816        
C2213 IWRD = IWRD + 2        
C        
C     COMMENTS FROM G.CHAN/UNISYS, 10/90        
C     THE ABOVE LOGIC CAN NEVER REACH 2212        
C     THE ABOVE LOGIC CAN BE REPLACED BY ONE SIMPLE LINE        
C        
 2210 IF (CORE(IWRD) .GE. 0) GO TO 2215        
      IF (I .NE. 12) GO TO 2214        
      PORG = CORE(IWRD+1)        
      IF (ORG .LE. 0) GO TO 9830        
      DO 2213 I = 1,ORG        
      IF (PORG .EQ. ORIGIN(I)) GO TO 2214        
 2213 CONTINUE        
      GO TO 9830        
 2214 IWRD = IWRD + 2        
      GO TO 2210        
C        
 2215 IF (CORE(IWRD) .NE. 0) GO TO 2216        
      ICONT = 3        
      GO TO 9998        
 2216 IF (CORE(IWRD) .NE. EOR) GO TO 2217        
      ICONT = 0        
      GO TO 9998        
 2217 MODE = CORE(IWRD)        
      IWRD = IWRD + 1        
      GO TO 2202        
C        
 2220 IPR1 = CORE(IWRD  )        
      IPR2 = CORE(IWRD+1)        
      IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 2222,2222,2223        
 2222 IPRM = DEFO        
      ASSIGN 2210 TO IRTN        
      GO TO 9806        
 2223 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      DO 2225 I = 1,3        
C                                     DEFO  VELO  ACCE        
      IF (IWORD .EQ. IDVPR(I)) GO TO (2207, 9826, 9826), I        
 2225 CONTINUE        
      ASSIGN 2207 TO IRTN        
      IPRM = CORE(IWRD)        
      GO TO 9808        
C        
 2230 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 2231,2231,2232        
 2231 ASSIGN 2210 TO IRTN        
      IPRM = DEFO        
      GO TO 9806        
C        
 2232 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      DO 2235 I = 1,3        
      IF (IWORD .EQ. IDVPR(I)) GO TO 2207        
 2235 CONTINUE        
      ASSIGN 2207 TO IRTN        
      IPRM = CORE(IWRD)        
      GO TO 9808        
C        
 2250 NREAL = 2        
      ASSIGN 2202 TO IRTN        
 2251 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE .LE. 0) GO TO 2252        
      IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 2251        
      GO TO 9818        
 2252 INTEG = 0        
      DO 2255 I = 1,NREAL        
      IF (CORE(IWRD) .GE.  0) GO TO 2257        
      IF (CORE(IWRD) .EQ. -1) INTEG = 1        
      IF (CORE(IWRD) .EQ. -4) IWRD  = IWRD + 1        
      IWRD = IWRD + 2        
 2255 CONTINUE        
      IF (INTEG) 2210,2210,2256        
 2256 ASSIGN 2210 TO IRTN        
      GO TO 9816        
 2257 ASSIGN 2215 TO IRTN        
      GO TO 9818        
C        
 2260 IWRD = IWRD + 2        
      MODE = MODE - 1        
      NREAL= 1        
      IF (MODE) 2261,2261,2262        
 2261 ASSIGN 2210 TO IRTN        
      IPRM = LAG        
      GO TO 9806        
 2262 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD .EQ. LAG) GO TO 2251        
      ASSIGN 2251 TO IRTN        
      IPRM = CORE(IWRD)        
      GO TO 9808        
C        
 2280 NCRD = 9        
      ICRD = 1        
      IVC  = 0        
      GO TO 2282        
 2281 NCRD = 25        
      ICRD = 4        
      IVC  = 1        
 2282 ASSIGN 2210 TO IRTN        
      IAX  = 0        
 2283 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 9810,9810,2284        
 2284 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 2283        
      DO 2285 I = ICRD,NCRD        
      IF (IWORD .EQ. COORD(I)) GO TO 2286        
 2285 CONTINUE        
      IF (IAX) 9810,9810,2202        
 2286 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 2215,2215,2287        
 2287 IF (IVC) 2288,2288,2202        
 2288 IF (IAX) 2289,2289,2202        
 2289 IAX = 1        
      GO TO 2284        
C        
 2290 IWRD = IWRD + 2        
      MODE = MODE - 1        
      IF (MODE) 2210,2210,2291        
 2291 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (CORE(IWRD).EQ.ALLON .OR. IWORD.EQ.BLANK) GO TO 2290        
      DO 2292 I = 1,5        
      IF (IWORD .EQ. LBLPR(I)) GO TO (2290, 2207, 2207, 2207, 2207), I  
C                                     GRID  POIN  ELEM  BOTH  EPID      
 2292 CONTINUE        
      GO TO 2202        
C        
 2240 IWRD  = IWRD + 2        
      MODE  = MODE - 1        
      IF (MODE) 2241,2241,2242        
 2241 ASSIGN 2210 TO IRTN        
      GO TO 9812        
 2242 IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD .EQ. DEFO) GO TO 2243        
      ASSIGN 2243 TO IRTN        
      IPRM  = CORE(IWRD)        
      GO TO 9808        
 2243 NREAL = 1        
      GO TO 2251        
C        
 2245 IWRD  = IWRD + 2        
      IWORD = CORE(IWRD)        
      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)        
      IF (IWORD .EQ. HIDD) GO TO 2207        
      MODE  = MODE - 1        
      IF (MODE) 2241,2210,2210        
C        
 2248 IF (CORE(IWRD-3).EQ.-1 .AND. CORE(IWRD+2).EQ.-1) GO TO 2207       
      ASSIGN 2207 TO IRTN        
      GO TO 9822        
C        
C     SET UP ERROR MESSAGE        
C        
 9800 ASSIGN 9900 TO IERR        
      MSGNO = 348        
      GO TO 9890        
 9802 ASSIGN 9902 TO IERR        
      MSGNO = 349        
      GO TO 9890        
 9804 ASSIGN 9904 TO IERR        
      MSGNO = 350        
      GO TO 9895        
 9806 ASSIGN 9906 TO IERR        
      MSGNO = 351        
      GO TO 9890        
 9808 ASSIGN 9908 TO IERR        
      MSGNO = 351        
      GO TO 9890        
 9810 ASSIGN 9910 TO IERR        
      MSGNO = 352        
      GO TO 9890        
 9812 ASSIGN 9912 TO IERR        
      MSGNO = 353        
      GO TO 9890        
 9814 ASSIGN 9914 TO IERR        
      MSGNO = 354        
      GO TO 9895        
 9816 ASSIGN 9916 TO IERR        
      MSGNO = 355        
      GO TO 9890        
 9818 ASSIGN 9918 TO IERR        
      MSGNO = 356        
      GO TO 9890        
 9820 ASSIGN 9920 TO IERR        
      MSGNO = 357        
      GO TO 9895        
 9822 ASSIGN 9922 TO IERR        
      MSGNO = 358        
      GO TO 9890        
 9824 ASSIGN 9924 TO IERR        
      MSGNO = 359        
      GO TO 9890        
 9826 ASSIGN 9926 TO IERR        
      MSGNO = 360        
      GO TO 9890        
 9828 ASSIGN 9928 TO IERR        
      MSGNO = 361        
      GO TO 9895        
 9830 ASSIGN 9930 TO IERR        
      MSGNO = 362        
      GO TO 9895        
C        
 9890 CALL PAGE2 (2)        
      WRITE  (NOUT,9891) UFM,MSGNO        
 9891 FORMAT (A23,I4)        
      IF (PLTOPT .LE. 2) NOGO = 1        
      GO TO 9898        
 9895 CALL PAGE2 (2)        
      WRITE  (NOUT,9896) UWM,MSGNO        
 9896 FORMAT (A25,I4)        
C        
 9898 GO TO IERR, (9900,9902,9904,9906,9908,9910,9912,9914,9916,9918,   
     1             9920,9922,9924,9926,9928,9930)        
C        
 9900 WRITE  (NOUT,9901)        
 9901 FORMAT (5X,'FIRST CHARACTER ON CARD IS NUMERIC. INCORRECT FORMAT',
     1       ' OR INCORRECT CONTINUATION ON PREVIOUS CARD')        
      GO TO 320        
C        
 9902 WRITE  (NOUT,9903) CORE(IWRD)        
 9903 FORMAT (5X,'PLOT COMMAND ',A4,' NOT RECOGNIZED.  CHECK SPELLING ',
     1       'AND FORMAT ON THIS CARD AND CONTINUATION ON PREVIOUS ONE')
      GO TO 320        
 9904 WRITE  (NOUT,9905)        
 9905 FORMAT (1H+,30X,' - ONLY NASTRAN GENERAL PURPOSE PLOTTER IS ',    
     1       'SUPPORTED')        
      GO TO 320        
C        
 9906 WRITE  (NOUT,9907) IPRM        
 9907 FORMAT (1H+,30X,' - KEYWORD ',A4,' NOT FOUND')        
      GO TO IRTN, (320,1070,2110,2136,2210,510)        
C        
 9908 WRITE  (NOUT,9909) IPRM        
 9909 FORMAT (1H+,30X,' - KEYWORD ',A4,' NOT RECOGNIZED')        
      GO TO IRTN, (320,1070,2130,2135,2202,2207,2243,2251)        
C        
 9910 WRITE  (NOUT,9911)        
 9911 FORMAT (1H+,30X,' - COORDINATE AXES INCORRECTLY DEFINED')        
      GO TO IRTN, (320,2210)        
C        
 9912 WRITE  (NOUT,9913)        
 9913 FORMAT (1H+,30X,' - INCORRECT FORMAT')        
      GO TO IRTN, (320,1070,2210)        
C        
 9914 WRITE  (NOUT,9915) IPRM        
 9915 FORMAT (1H+,30X,3H - ,A4,' IDENTIFICATION NUMBER NOT DEFINED')    
      GO TO IRTN, (320,1005,1910,2120,2202)        
C        
 9916 WRITE  (NOUT,9917)        
 9917 FORMAT (1H+,30X,' - DATA TYPE IS INCORRECT')        
      GO TO IRTN, (1005,1730,2107,2110,2210,320)        
C        
 9918 WRITE  (NOUT,9919)        
 9919 FORMAT (1H+,30X,' - ONE OR MORE REQUIRED REAL VALUES MISSING')    
      GO TO IRTN, (320,1005,2202,2215)        
C        
 9920 WRITE  (NOUT,9921)        
 9921 FORMAT (1H+,30X,' - CAMERA OPTION NOT SPECIFIED')        
      GO TO 320        
C        
 9922 WRITE  (NOUT,9923)        
 9923 FORMAT (1H+,30X,' - THRU MUST BE PRECEDED AND FOLLOWED BY INTEGER'
     1,      ' VALUES')        
      GO TO IRTN, (2123,2207)        
C        
 9924 WRITE  (NOUT,9925)        
 9925 FORMAT (1H+,30X,' - THRU RANGE OVERLAPS RANGE OF PREVIOUS THRU')  
      GO TO 2123        
C        
 9926 WRITE  (NOUT,9927) IPR1,IPR2        
 9927 FORMAT (1H+,30X,' - ONLY DEFORMATION VALID WITH ',2A4)        
      GO TO 2207        
C        
 9928 WRITE  (NOUT,9929) FORG,PORG        
 9929 FORMAT (1H+,30X,' - A NEW ORIGIN',I8,' WAS DEFINED IN A FIND ',   
     1        'CARD, BUT IT IS NOT USED BY THE IMMEDIATE PLOT CARD',    
     2        /5X,'(ORIGIN',I8,' WILL BE USED FOR THIS PLOT)',/)        
      GO TO 9999        
C        
 9930 WRITE  (NOUT,9931) PORG        
 9931 FORMAT (1H+,30X,' - ORIGIN',I8,' IS UNDEFINED')        
      GO TO 2207        
C        
 9998 IF (ISPLOT.EQ.0 .OR. PORG.EQ.-1) RETURN        
      IF (PORG .EQ. 0) PORG = PORG1        
      IF (FORG.NE.0 .AND. FORG.NE.PORG) GO TO 9828        
 9999 FORG = 0        
      PORG = 0        
      RETURN        
      END        