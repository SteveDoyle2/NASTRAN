      SUBROUTINE XYOUT (IOPT,BUF,RBUF)        
C        
C     THIS SUBROUTINE IS CALLED BY XYTRAN AND OUTPUTS TO PRINTER AND    
C     PUNCH        
C        
      EXTERNAL        LSHIFT,RSHIFT        
      LOGICAL         PRINT,PUNCH        
      INTEGER         BUF(300),NAMES(44),TYPE(6),PLT(2),IMTD(6),        
     1                ITYPE(4),RSHIFT        
      REAL            RBUF(300)        
      COMMON /MACHIN/ MACH,IHALF        
      COMMON /BLANK / ICOM1,DUM(4),ICARD        
      COMMON /SYSTEM/ SYSBUF,L,D1(6),MAXLNS,D2(2),LINE,D3(78),LPCH      
      COMMON /OUTPUT/ IHEAD(96)        
      DATA    NAMES / 4HDISP ,4HLACE ,4HMENT ,4H     ,        
     1                4HVELO ,4HCITY ,4H     ,4H     ,        
     2                4HACCE ,4HLERA ,4HTION ,4H     ,        
     3                4HS P  ,4HC F  ,4H     ,4H     ,        
     4                4HLOAD ,4H     ,4H     ,4H     ,        
     5                4HELEM ,4HENT- ,4HSTRE ,4HSS   ,        
     6                4HELEM ,4HENT- ,4HFORC ,4HE    ,        
     7                4HS-DI ,4HSPLA ,4HCEME ,4HNT   ,        
     8                4HS-VE ,4HLOCI ,4HTY   ,4H     ,        
     9                4HS-AC ,4HCELE ,4HRATI ,4HON   ,        
     O                4HNONL ,4HINEA ,4HR-FO ,4HRCE                  /  
      DATA    TYPE  / 4HWHOL ,4HE    ,4HUPPE ,4HR    ,4HLOWE ,4HR    /  
      DATA    IRAND / 4HRAND /        
      DATA    IVG   / 4HVG   /        
      DATA    PLT   / 4HNAST ,4HPLT  /        
      DATA    IMTD  / 4HFILM ,1H     ,4HTABL ,1HE    ,4HDRUM ,1H     /  
      DATA    ITYPE / 4HWITH ,4H     ,        
     1                4HWITH ,4HOUT  /        
C        
      IF (ICOM1 .EQ. IVG) GO TO 86        
C        
C     BRANCH ON OPTION        
C        
      IF (IOPT) 10,90,90        
C        
C     PRINT XY-OUTPUT SUMMARY        
C        
C        
C     FILL OUT HEADING        
C        
   10 DO 20 I = 1,96        
   20 IHEAD(I) = BUF(I+50)        
      CALL PAGE1        
      WRITE (L,150)        
      IF (ICOM1 .EQ. IRAND) GO TO 30        
      WRITE (L,170)BUF(1)        
      GO TO 40        
   30 WRITE (L,160) RBUF(1)        
      WRITE (L,161) RBUF(42)        
   40 ITEMPV = 4*BUF(6) - 3        
C        
C     PRINT TYPE OF PLOT        
C        
      IF (BUF(245)-2) 41,42,43        
   41 WRITE (L,460)        
      GO TO 45        
   42 WRITE (L,470)        
      GO TO 45        
   43 WRITE (L,480)        
C        
C     PRINT DATA TYPE AND CURVE        
C        
   45 ICOMP = BUF(5)        
      IF (BUF(6).NE.6 .AND. BUF(6).NE.7) ICOMP = BUF(5) - 2        
      IF (BUF(7)) 70,60,50        
   50 WRITE (L,200) NAMES(ITEMPV),NAMES(ITEMPV+1),NAMES(ITEMPV+2),      
     1              NAMES(ITEMPV+3),BUF(4),ICOMP        
      ITEMP = 3        
      GO TO 72        
   60 WRITE (L,180) NAMES(ITEMPV),NAMES(ITEMPV+1),NAMES(ITEMPV+2),      
     1              NAMES(ITEMPV+3),BUF(4),ICOMP        
      ITEMP = 1        
      GO TO 72        
   70 WRITE (L,190) NAMES(ITEMPV),NAMES(ITEMPV+1),NAMES(ITEMPV+2),      
     1              NAMES(ITEMPV+3),BUF(4),ICOMP        
      ITEMP  = 5        
   72 ICOUNT = ICARD + 1        
      WRITE (L,210)        
      IF (BUF(288) .GT. 0) WRITE (L,230)        
      IF (BUF(290) .GT. 0) WRITE (L,240) ICOUNT        
C        
C     PLOTTER INFORMATION        
C        
      IF (BUF(289) .LE. 0) GO TO 84        
      WRITE (L,220)        
      J = RSHIFT(BUF(284),IHALF)        
      MODEL = BUF(284) - LSHIFT(J,IHALF) - 100        
      M = 1        
      IF (MODEL .LT. 0) M = 3        
C        
C   . NASPLOT...        
C        
      K = 2*IABS(MODEL) - 1        
      WRITE (L,380) PLT(1),PLT(2),IMTD(K),IMTD(K+1),ITYPE(M),ITYPE(M+1) 
      IF (BUF(283) .LE. 0) BUF(283) = 1        
C        
C     WRITE CSCALE DATA OUT        
C        
      WRITE (L,490) RBUF(282)        
      IF (IABS(MODEL)-2) 81,82,82        
C        
C   . CAMERA, DENSITY...        
C        
   81 IF (BUF(287) .GE. 3) WRITE (L,410)        
      IF (BUF(287) .EQ. 2) WRITE (L,430)        
      IF (BUF(287) .LE. 1) WRITE (L,420)        
      WRITE (L,450) BUF(283)        
      GO TO 83        
C        
C   . PAPER SIZE        
C     (THE LOGIC HERE IS SIMILAR TO THAT IN SUBROUTINE PLTSET)        
C        
   82 IF (IABS(MODEL) .EQ. 2) GO TO 822        
C        
C   . DRUM PLOTTERS        
C        
      IF (RBUF(285) .LE. 0.0) RBUF(285) = 30.0        
      IF (RBUF(286) .LE. 0.0) RBUF(286) = 30.0        
      GO TO 824        
C        
C   . TABLE PLOTTERS        
C        
  822 IF (RBUF(285) .LE.  0.0) RBUF(285) = 11.0        
      IF (RBUF(285) .GT. 30.0) RBUF(285) = 30.0        
      IF (RBUF(286) .LE.  0.0) RBUF(286) = 8.5        
  824 IF (RBUF(286) .GT. 30.0) RBUF(286) = 30.0        
      WRITE (L,390) RBUF(285),RBUF(286)        
C        
C   . PEN SIZE        
C        
      WRITE (L,440) BUF(283)        
   83 WRITE (L,250) BUF(3),TYPE(ITEMP),TYPE(ITEMP+1),BUF(2)        
C        
C  .  PAPER PLOT        
C        
   84 IF (BUF(289).GT.0 .AND. BUF(289).NE.2) GO TO 85        
      WRITE (L,400) BUF(281)        
C        
   85 CONTINUE        
      WRITE (L,260) (BUF(J),J=147,174),(BUF(J),J=179,206),        
     1              (BUF(J),J=211,238)        
      WRITE (L,270)        
      WRITE (L,290) RBUF( 11),RBUF( 12)        
      WRITE (L,300) RBUF(293),RBUF(294)        
      WRITE (L,310) RBUF(295),RBUF(296)        
      WRITE (L,280) RBUF(291),RBUF(292)        
      WRITE (L,300) RBUF(297),RBUF(298)        
      WRITE (L,310) RBUF(299),RBUF(300)        
      WRITE (L,320)        
      IF (BUF(288) .GT. 0) WRITE (L,330)        
   86 ITEMPV = 4*BUF(6) - 3        
      IF (BUF(7)) 89,88,87        
   87 ITEMP = 3        
      GO TO 891        
   88 ITEMP = 1        
      GO TO 891        
   89 ITEMP = 5        
  891 IPRINT= 0        
      ID    = BUF(4)        
      ICOMP = BUF(5)        
      IF (BUF(6).NE.6 .AND. BUF(6).NE.7) ICOMP = BUF(5) - 2        
      PRINT = .FALSE.        
      PUNCH = .FALSE.        
      IF (BUF(290) .GT. 0) PUNCH = .TRUE.        
      IF (BUF(288) .GT. 0) PRINT = .TRUE.        
      IF (.NOT.PRINT) RETURN        
      LINE = MAXLNS + 1        
      RETURN        
C        
C     PRINT AND OR PUNCH OUTPUT        
C        
   90 IPRINT = IPRINT + 1        
      IF (.NOT.PUNCH) GO TO 100        
      ICARD = ICARD + 1        
      WRITE (LPCH,370) IPRINT,RBUF(1),RBUF(2),ICARD        
  100 IF (.NOT.PRINT) RETURN        
      IF (LINE .LT. MAXLNS) GO TO 110        
      CALL PAGE1        
      WRITE (L,340) NAMES(ITEMPV),NAMES(ITEMPV+1),NAMES(ITEMPV+2),      
     1              NAMES(ITEMPV+3),ID,ICOMP,TYPE(ITEMP),TYPE(ITEMP+1)  
  110 LINE = LINE + 1        
      IF (.NOT.PUNCH) GO TO 120        
      WRITE (L,350) IPRINT,RBUF(1),RBUF(2),ICARD        
      RETURN        
  120 WRITE (L,350) IPRINT,RBUF(1),RBUF(2)        
      RETURN        
C        
  150 FORMAT (///44X,'X Y - O U T P U T   S U M M A R Y')
  160 FORMAT (//5X,'ROOT MEAN SQUARE VALUE =',1P,E15.6)        
  161 FORMAT (6X,'FREQUENCY OF ZERO CROSSINGS (N ZERO) =',1P,E15.6)    
  170 FORMAT (//5X,'SUBCASE',I10)        
  180 FORMAT (6X,4A4,'CURVE',I9,'(',I2,')')        
  190 FORMAT (6X,4A4,'CURVE',I9,'(--,',I2,')')        
  200 FORMAT (6X,4A4,'CURVE',I9,'(',I2,',--)')        
  210 FORMAT (' ')        
  220 FORMAT (6X,'XY-PAIRS WITHIN FRAME LIMITS WILL BE PLOTTED')       
  230 FORMAT (6X,'XY-PAIRS BETWEEN XMIN AND XMAX WILL BE PRINTED')     
  240 FORMAT (6X,64HXY-PAIRS BETWEEN XMIN AND XMAX WILL BE PUNCHED BIGIN
     1NING ON CARD,I8)        
  250 FORMAT (//5X,'THIS IS CURVE',I4,' OF ',A4,A2,'FRAME',I5)        
  260 FORMAT (//5X,'CURVE  TITLE =',28A4,/6X,'X-AXIS TITLE =',28A4,   
     1        /6X,'Y-AXIS TITLE =',28A4)
  270 FORMAT (/////5X,62HTHE FOLLOWING INFORMATION IS FOR THE ABOVE DEFI
     1NED CURVE ONLY. )        
  280 FORMAT (//5X,'WITHIN THE X-LIMITS OF ALL DATA (X =',1P,E14.6,    
     1        ' TO  X =',1P,E14.6,')')        
  290 FORMAT (///6X,'WITHIN THE FRAME X-LIMITS       (X =',1P,E14.6,   
     1        ' TO  X =',1P,E14.6,')')        
  300 FORMAT (//30X,'THE SMALLEST Y-VALUE =',1P,E14.6,' AT X =',E15.6) 
  310 FORMAT (//30X,'THE LARGEST  Y-VALUE =',1P,E14.6,' AT X =',E15.6, 
     1        //)        
  320 FORMAT (//45X,'E N D   O F   S U M M A R Y')
  330 FORMAT (//25X,69HP R I N T E D   D A T A   F O R   T H I S   C U R
     1 V E   F O L L O W S)        
  340 FORMAT (//5X,4A4,'CURVE   ID =',I9,5X,'COMPONENT =',I3,5X,A4,A2,
     1        'FRAME',///27X,'PRINT NUMBER',10X,'X-VALUE',14X,        
     2        'Y-VALUE',14X, 'CARD NUMBER')
  350 FORMAT (28X,I7,1P,E25.6,E21.6,10X,I8)        
  370 FORMAT (I10,10X,1P,2E20.6,12X,I8)        
  380 FORMAT (6X,'PLOTTER SPECIFIED IS ',3A4,A1,' PLOTTER ',2A4,       
     1        'TYPING CAPABILITY.')
  390 FORMAT (6X,'PAPER SIZE ',F5.2,' X ',F5.2,' INCHES SPECIFIED.')  
  400 FORMAT (6X,'THIS CURVE WILL BE PAPER-PLOTTED FRAME',I5)        
  410 FORMAT (6X,'CAMERA 3 USED. (PAPER AND 35MM FILM)')
  420 FORMAT (6X,'CAMERA 2 USED. (35MM FILM)')
  430 FORMAT (6X,'CAMERA 1 USED. (PAPER)')
  440 FORMAT (6X,'PENSIZE =',I3)        
  450 FORMAT (6X,'DENSITY =',I3)        
  460 FORMAT (6X,'RESPONSE')
  470 FORMAT (6X,'POWER-SPECTRAL-DENSITY-FUNCTION (PSDF)')        
  480 FORMAT (6X,'AUTOCORRELATION')
  490 FORMAT (6X,'CSCALE = ',F5.2)        
      END        
