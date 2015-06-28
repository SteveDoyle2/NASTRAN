      SUBROUTINE OFP1A (LINE)        
C        
C     THIS ROUTINE WAS NAMED OFP1 BEFORE.        
C        
      INTEGER         L123(5),ID(50),OF(6)        
      REAL            FID(50),RT(8,15),SECTN(2)        
      COMMON /SYSTEM/ KSYS(80)        
CZZ   COMMON /ZZOFPX/ CORE(1)        
      COMMON /ZZZZZZ/ CORE(1)        
      COMMON /OFP1ID/ ID22,M        
      EQUIVALENCE     (KSYS(2),L), (KSYS(12),LINET), (KSYS(33),IFLAG),  
     1                (KSYS(3),NOGO), (CORE(1),OF(1),L123(1)),        
     2                (FID(1) ,ID(1), OF(6))        
C        
      DATA   RT/        
     1 4HSING, 4HULAR, 4HITIE, 4HS EN, 4HCOUN, 4HTERE, 4HD.  , 4H    ,  
     2 4H4 SH, 4HIFT , 4HPTS., 4HPER , 4HROOT, 4H EXC, 4HEEDE, 4HD.  ,  
     3 4HALL , 4HEIGE, 4HNVAL, 4HUES , 4HFOUN, 4HD IN, 4H RAN, 4HGE. ,  
     4 4H3X E, 4HST.R, 4HOOTS, 4H IN , 4HRANG, 4HE SP, 4HECIF, 4HIED.,  
     5 4HNO M, 4HORE , 4HEIGE, 4HNVAL, 4HUES , 4HIN P, 4HROBL, 4HEM. ,  
     6 4HNO. , 4HOF R, 4HOOTS, 4H DES, 4HIRED, 4H WER, 4HE FO, 4HUND.,  
     7 4H1 OR, 4H MOR, 4HE RO, 4HOT O, 4HUTSI, 4HDE F, 4HR.RA, 4HNGE.,  
     8 4HINSU, 4HFFIC, 4HIENT, 4H TIM, 4HE FO, 4HR NE, 4HXT R, 4HOOT.,  
     9 4HUNAB, 4HLE T, 4HO CO, 4HNVER, 4HGE. , 4H    , 4H    , 4H    ,  
     O 4HNORM, 4HAL T, 4HERMI, 4HNATI, 4HON  , 4H    , 4H    , 4H    ,  
     1 4HEIGE, 4HNVAL, 4HUES , 4HOUTS, 4HIDE , 4HFREQ, 4H. RA, 4HNGE ,  
     2 4HINSU, 4HFFIC, 4HIENT, 4H TIM, 4HE RE, 4HMAIN, 4HING , 4H    ,  
     3 4HFEWE, 4HR TH, 4HAN R, 4HEQUE, 4HSTED, 4H ROO, 4HTS F, 4HOUND,  
     4 4HROOT, 4HS FO, 4HUND , 4HWITH, 4H REQ, 4H. AC, 4HCURA, 4HCY  ,  
     5 4HNO R, 4HOOTS ,4H FOU, 4HND, , 4HNONE, 4H PAS, 4HSED , 4HTEST/  
      DATA     SECTN / 4H.3.3, 4H.7.3  /

      TWOPI = 6.283185307
C        
      LOCAL = LINE - 100        
      IF (LOCAL) 2003,2003,2004        
 2003 GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,  
     1 23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,  
     2 44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,  
     3 65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,  
     4 86,87,88,89,90,91,92,93,94,95,96,97,98,99,100), LINE        
 2004 GO TO (  101,102,103,104,105,106,107,108,109,110,111,112,113,114, 
     1 115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130, 
     2 131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146, 
     3 147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162, 
     4 163,164,165,166,167,168,169,170,171,172,173,174), LOCAL        
C        
    1 WRITE (L,501)        
      GO TO 1000        
    2 WRITE (L,502)        
      GO TO 1000        
    3 WRITE (L,503)        
      GO TO 1000        
    4 WRITE (L,504)        
      GO TO 1000        
    5 WRITE (L,505)        
      IF (ID22 .EQ. -999) GO TO 1000        
      IF (ID22) 2053,2052,2051        
 2051 WRITE (L,5050)        
      WRITE (L,5051) ID22        
      GO TO 2054        
 2052 IF (.NOT.((M.GE.3 .AND. M.LE.7) .OR. M.EQ.10 .OR. M.EQ.13))       
     1   GO TO 1000        
      WRITE (L,5050)        
      WRITE (L,5052)        
      GO TO 2054        
 2053 IF (M.NE.8 .AND. M.NE.12) GO TO 1000        
      WRITE (L,5050)        
      WRITE (L,5053)        
 2054 WRITE (L,5054)        
      LINET = LINET + 10        
      ID22  =-999        
      GO TO 1000        
    6 WRITE (L,506) ID(5)        
      GO TO 1000        
    7 WRITE (L,507)        
      GO TO 1000        
    8 WRITE (L,508)        
      GO TO 1000        
    9 WRITE (L,509)        
      GO TO 1000        
   10 WRITE (L,510)        
      GO TO 1000        
   11 WRITE (L,511)        
      GO TO 1000        
   12 WRITE (L,512)        
      GO TO 1000        
   13 WRITE (L,513)        
      GO TO 1000        
   14 WRITE (L,514)        
      GO TO 1000        
   15 WRITE (L,515)        
      GO TO 1000        
   16 WRITE (L,516)        
      GO TO 1000        
   17 WRITE (L,517)        
      GO TO 1000        
   18 WRITE (L,518)        
      GO TO 1000        
   19 WRITE (L,519)        
      GO TO 1000        
   20 WRITE (L,520)        
      GO TO 1000        
   21 WRITE (L,521)        
      GO TO 1000        
   22 WRITE (L,522)        
      GO TO 1000        
   23 WRITE (L,523)        
      GO TO 1000        
   24 WRITE (L,524)        
      GO TO 1000        
   25 WRITE (L,525)        
      GO TO 1000        
   26 WRITE (L,526)        
      GO TO 1000        
   27 WRITE (L,527)        
      GO TO 1000        
   28 WRITE (L,528)        
      GO TO 1000        
   29 WRITE (L,529)        
      GO TO 1000        
C        
C     PROCESS SPC AND MPC SET IDS PROPERLY TO ACCOUNT FOR AXISYMMETRIC  
C     PROBLEMS        
C        
   30 DO 3010 J = 3,4        
      IF (ID(J) .LT.  100000000) GO TO 3010        
      ID(J) = ID(J) - 100000000        
      IF (ID(J) .LT.  100000000) GO TO 3010        
      ID(J) = ID(J) - 100000000        
 3010 CONTINUE        
      WRITE (L,530) ID(3),ID(4)        
      LINET = LINET + 1        
      GO TO 1000        
   31 WRITE (L,531)        
      GO TO 1000        
   32 WRITE (L,532)        
      GO TO 1000        
   33 WRITE (L,533)        
      GO TO 1000        
   34 WRITE (L,534)        
      GO TO 1000        
   35 WRITE (L,535)        
      GO TO 1000        
   36 WRITE (L,536)        
      GO TO 1000        
   37 WRITE (L,537)        
      GO TO 1000        
   38 WRITE (L,538)        
      GO TO 1000        
   39 WRITE (L,539)        
      GO TO 1000        
   40 WRITE (L,540)        
      GO TO 1000        
   41 WRITE (L,541)        
      GO TO 1000        
   42 WRITE (L,542)        
      GO TO 1000        
   43 WRITE (L,543)        
      GO TO 1000        
   44 WRITE (L,544)        
      GO TO 1000        
   45 WRITE (L,545)        
      GO TO 1000        
   46 WRITE (L,546)        
      GO TO 1000        
   47 WRITE (L,547)        
      GO TO 1000        
   48 WRITE (L,548)        
      GO TO 1000        
   49 WRITE (L,549)        
      GO TO 1000        
   50 WRITE (L,550)        
      GO TO 1000        
   51 WRITE (L,551)        
      GO TO 1000        
   52 WRITE (L,552)        
      GO TO 1000        
   53 WRITE (L,553)        
      GO TO 1000        
   54 WRITE (L,554)        
      GO TO 1000        
   55 WRITE (L,555)        
      GO TO 1000        
   56 WRITE (L,556)        
      GO TO 1000        
   57 WRITE (L,557)        
      GO TO 1000        
   58 WRITE (L,558)        
      GO TO 1000        
   59 WRITE (L,559)        
      GO TO 1000        
   60 WRITE (L,560)        
      GO TO 1000        
   61 WRITE (L,561)        
      GO TO 1000        
   62 WRITE (L,562)        
      GO TO 1000        
   63 WRITE (L,563)        
      GO TO 1000        
   64 WRITE (L,564)        
      GO TO 1000        
   65 WRITE (L,565)        
      GO TO 1000        
   66 WRITE (L,566)        
      GO TO 1000        
   67 WRITE (L,567)        
      GO TO 1000        
   68 WRITE (L,568)        
      GO TO 1000        
   69 WRITE (L,569)        
      GO TO 1000        
   70 WRITE (L,570)        
      GO TO 1000        
   71 WRITE (L,571)        
      GO TO 1000        
   72 WRITE (L,572)        
      GO TO 1000        
   73 WRITE (L,573)        
      GO TO 1000        
   74 WRITE (L,574)        
      GO TO 1000        
   75 WRITE (L,575)        
      GO TO 1000        
   76 WRITE (L,576)        
      GO TO 1000        
   77 WRITE (L,577)        
      GO TO 1000        
   78 WRITE (L,578)        
      GO TO 1000        
   79 WRITE (L,579)        
      GO TO 1000        
   80 WRITE (L,580)        
      GO TO 1000        
   81 WRITE (L,581)        
      GO TO 1000        
   82 WRITE (L,582)        
      GO TO 1000        
   83 WRITE (L,583)        
      GO TO 1000        
   84 WRITE (L,584)        
      GO TO 1000        
   85 WRITE (L,585)        
      GO TO 1000        
   86 WRITE (L,586)        
      GO TO 1000        
   87 WRITE (L,587)        
      GO TO 1000        
   88 WRITE (L,588)        
      GO TO 1000        
   89 WRITE (L,589)        
      GO TO 1000        
   90 IF (ID(16) .EQ. 1) GO TO 905        
      WRITE (L,590)        
      GO TO 1000        
  905 WRITE (L,5905)        
      GO TO 1000        
   91 WRITE (L,591) (ID(K),K=11,15),ID(17),FID(18),(ID(K),K=19,21)      
      M = ID(17)        
      IF (M .GE. 8) NOGO = 14        
      IF (ID(16) .NE. 1) GO TO 911        
      IF (M.EQ.2 .OR. M.GT.3) NOGO = 14        
      IF (M .EQ. 0) M = 10        
      IF (M .EQ. 1) M = 13        
      IF (M .EQ. 3) M = 14        
  911 IF (M .GT. 0) WRITE (L,5911) (RT(K,M),K=1,8),SECTN(1)        
      ID22 = ID(22)        
      GO TO 1000        
   92 WRITE (L,592)        
      GO TO 1000        
   93 WRITE (L,593) (ID(K),K=11,17),FID(18),(ID(K),K=19,21)        
      M = ID(17)        
  933 IF (M .GE. 3) NOGO = 14        
      IF (M .EQ. 1) M = 6        
      IF (M .EQ. 2) M = 11        
      IF (M .EQ. 3) M = 8        
      IF (M .EQ. 4) M = 1        
      GO TO 911        
   94 WRITE (L,594)        
      GO TO 1000        
   95 WRITE (L,595)        
      GO TO 1000        
   96 WRITE (L,596)        
      SECTN(1) = SECTN(2)        
      GO TO 1000        
   97 WRITE (L,597)        
      GO TO 1000        
   98 WRITE (L,598) (ID(K),K=11,18)        
      M = ID(18)        
      GO TO 933        
   99 IF (ID(3) .EQ. 4) GO TO 1007        
      WRITE (L,599) (ID(K),K=11,16)        
      M = ID(16)        
      IF (ID(17) .NE. 1) GO TO 911        
      IF (M .GT. 2) NOGO = 14        
      IF (M .EQ. 0) M = 10        
      IF (M .EQ. 1) M = 13        
      IF (M .EQ. 2) M = 15        
      GO TO 911        
C        
C     ID(3)=2, ID(17)=0, METHOD IS COMPLEX INV        
C     ID(3)=2, ID(17)=1, METHOD IS COMPLEX FEER        
C     ID(3)=4, ID(17)=0, METHOD IS COMPLEX HESS        
C        
  100 SECTN(1) = SECTN(2)        
      IF (ID(17) .EQ. 1) GO TO 1005        
      IF (ID( 3) .EQ. 4) GO TO 1006        
      WRITE (L,600)        
      GO TO 1000        
 1005 WRITE (L,6005)        
      GO TO 1000        
 1006 WRITE (L,6006)        
      GO TO 1000        
 1007 WRITE (L,6007) ID(11),ID(12),ID(18)        
      M = ID(18)        
      GO TO 933        
  101 F = SQRT(ABS(FID(6)))/TWOPI        
      WRITE (L,601) FID(6),F        
      LINET = LINET + 1        
      GO TO 1000        
  102 F = SQRT(ABS(FID(6)))/TWOPI        
      WRITE (L,602) FID(6),F        
      LINET = LINET + 1        
      GO TO 1000        
  103 WRITE (L,603) FID(5)        
      GO TO 1000        
  104 CONTINUE        
      WRITE (L,604) FID(5)        
      GO TO 1000        
  105 WRITE (L,605) FID(5)        
      GO TO 1000        
  106 CONTINUE        
      WRITE (L,606) FID(5)        
      GO TO 1000        
  107 WRITE (L,607) ID(5)        
      GO TO 1000        
  108 WRITE (L,608) ID(5)        
      GO TO 1000        
  109 WRITE (L,609) FID(6),FID(7)        
      GO TO 1000        
  110 CONTINUE        
      WRITE (L,610) FID(6),FID(7)        
      GO TO 1000        
  111 WRITE (L,611)        
      GO TO 1000        
  112 WRITE (L,612)        
      GO TO 1000        
  113 WRITE (L,613)        
      GO TO 1000        
  114 WRITE (L,614)        
      GO TO 1000        
  115 WRITE (L,615)        
      GO TO 1000        
  116 WRITE (L,616)        
      GO TO 1000        
  117 WRITE (L,617)        
      GO TO 1000        
  118 WRITE (L,618)        
      GO TO 1000        
  119 WRITE (L,619)        
      GO TO 1000        
  120 WRITE (L,620)        
      GO TO 1000        
  121 WRITE (L,621)        
      GO TO 1000        
  122 WRITE (L,622)        
      GO TO 1000        
  123 WRITE (L,623)        
      GO TO 1000        
  124 WRITE (L,624) ID(5)        
      GO TO 1000        
  125 WRITE (L,625)        
      GO TO 1000        
  126 WRITE (L,626)        
      GO TO 1000        
  127 WRITE (L,627)        
      GO TO 1000        
  128 WRITE (L,628)        
      GO TO 1000        
  129 WRITE (L,629)        
      GO TO 1000        
  130 WRITE (L,630)        
      GO TO 1000        
  131 WRITE (L,631)        
      GO TO 1000        
  132 WRITE (L,632)        
      GO TO 1000        
  133 WRITE (L,633)        
      GO TO 1000        
  134 WRITE (L,634)        
      GO TO 1000        
  135 WRITE (L,635)        
      GO TO 1000        
  136 WRITE (L,636)        
      GO TO 1000        
  137 WRITE (L,637)        
      GO TO 1000        
  138 WRITE (L,638)        
      GO TO 1000        
  139 WRITE (L,639)        
      GO TO 1000        
  140 WRITE (L,640)        
      GO TO 1000        
  141 CONTINUE        
      GO TO 1000        
  142 WRITE (L,642)        
      GO TO 1000        
  143 WRITE (L,643)        
      GO TO 1000        
  144 WRITE (L,644)        
      GO TO 1000        
  145 WRITE (L,645)        
      GO TO 1000        
  146 WRITE (L,646)        
      GO TO 1000        
  147 CONTINUE        
      GO TO 1000        
  148 WRITE (L,648)        
      GO TO 1000        
  149 WRITE (L,649)        
      GO TO 1000        
  150 WRITE (L,650)        
      GO TO 1000        
  151 WRITE (L,651)        
      GO TO 1000        
  152 WRITE (L,652)        
      GO TO 1000        
  153 WRITE (L,653)        
      GO TO 1000        
  154 WRITE (L,654)        
      GO TO 1000        
  155 WRITE (L,655)        
      GO TO 1000        
  156 WRITE (L,656)        
      GO TO 1000        
  157 WRITE (L,657)        
      GO TO 1000        
  158 WRITE (L,658)        
      GO TO 1000        
  159 WRITE (L,659)        
      GO TO 1000        
  160 WRITE (L,660)        
      GO TO 1000        
  161 WRITE (L,661)        
      GO TO 1000        
  162 WRITE (L,662)        
      GO TO 1000        
  163 WRITE (L,663)        
      GO TO 1000        
  164 WRITE (L,664)        
      GO TO 1000        
  165 WRITE (L,665)        
      GO TO 1000        
  166 WRITE (L,666)        
      GO TO 1000        
  167 WRITE (L,667)        
      GO TO 1000        
  168 WRITE (L,668)        
      GO TO 1000        
  169 WRITE (L,669)        
      GO TO 1000        
  170 WRITE (L,670)        
      GO TO 1000        
  171 WRITE (L,671)        
      GO TO 1000        
  172 WRITE (L,672)        
      GO TO 1000        
  173 WRITE (L,673)        
      GO TO 1000        
  174 WRITE (L,674)        
C     GO TO 1000        
 1000 CONTINUE        
      RETURN        
C        
C        
  501 FORMAT (45X,'D I S P L A C E M E N T   V E C T O R')
  502 FORMAT (6X,'POINT ID.   TYPE',10X,'T1',13X,'T2',13X,'T3',13X,
     1       'R1',13X,'R2',13X,'R3')        
  503 FORMAT (46X, 'R E A L   E I G E N V A L U E S',/)
  504 FORMAT (3X, 'MODE', 4X, 'EXTRACTION', 7X, 'EIGENVALUE', 12X,    
     1       'RADIAN', 14X, 'CYCLIC', 2X, 2(9X, 'GENERALIZED'))
  505 FORMAT (4X,'NO.', 7X, 'ORDER', 30X, 'FREQUENCY', 11X, 'FREQUENCY',
     1       12X,'MASS', 14X, 'STIFFNESS',/)        
 5050 FORMAT (/37X,16('****'), /37X,'*',62X,'*', /37X,'*')
 5051 FORMAT ('+',45X,'NASTRAN INFORMATION MESSAGE 3307, POTENTIALLY',
     1       9X,'*', /37X,'*',I10,' EIGENVALUE(S) AT LOW FREQ. END NOT',
     2       ' FOUND',11X,'*')        
 5052 FORMAT ('+',39X,'NASTRAN INFORMATION MESSAGE 3308, LOWEST EIGEN',
     1       'VALUE FOUND',3X,'*', /37X,'*',2X,'AS INDICATED BY THE ',
     2       'STURM''S SEQUENCE OF THE DYNAMIC MATRIX',2X,'*')
 5053 FORMAT ('+',42X,'NASTRAN WARNING MESSAGE 3309, ALL LOWER EIGEN',
     1       'VALUES',6X,'*', /37X,'*', 5X,'NOT NECESSARY FOUND.',37X,
     2       '*')        
 5054 FORMAT (37X,'*',62X,'*', /37X,'*',8X,
     1       '(THIS MESSAGE CAN BE SUPPRESSED BY DIAG 37)',11X,'*',
     1       /37X,16('****'),/)        
  506 FORMAT (41X,'R E A L   E I G E N V E C T O R   N O .',I11)       
  507 FORMAT (7X,'ELEMENT',11X,'AXIAL',37X,'ELEMENT',11X,'AXIAL')       
  508 FORMAT (9X,'ID.',13X,'FORCE',10X,'TORQUE',23X,'ID.',13X,'FORCE',  
     1       10X,'TORQUE')        
  509 FORMAT ('0    ELEMENT',9X,'BEND-MOMENT END-A',12X,
     1       'BEND-MOMENT END-B',16X,'- SHEAR -',15X,'AXIAL')        
  510 FORMAT (4X,'   ID.   ',3(6X,'PLANE 1',7X,'PLANE 2',2X),7X,'FORCE',
     1       9X,'TORQUE')
  511 FORMAT (7X,'ELEMENT',11X,'FORCE',10X,'FORCE',22X,'ELEMENT',11X,   
     1       'FORCE',10X,'FORCE')        
  512 FORMAT (9X,'ID.',12X,'PTS 1,3',8X,'PTS 2,4',23X,'ID.',12X,        
     1       'PTS 1,3',8X,'PTS 2,4')
  513 FORMAT (7X,'ELEMENT',10X,'MOMENT',9X,'MOMENT',22X,'ELEMENT',10X,  
     1       'MOMENT',9X,'MOMENT')        
  514 FORMAT ('0',8X,'ELEMENT',2(11X,'BEND-MOMENT'),10X,'TWIST-MOMENT'
     1,      2(13X,'SHEAR',4X))        
  515 FORMAT (11X,'ID.',17X,'X',21X,'Y',43X,'X',21X,'Y')
  516 FORMAT (6X,3('ELEMENT',9X,'FORCE',12X),'ELEMENT',9X,'FORCE')      
  517 FORMAT (8X,3('ID.',30X),'ID.')        
  518 FORMAT (2(7X,'ELEMENT',7X,'AXIAL',7X,'SAFETY',6X,'TORSIONAL',5X,  
     1       'SAFETY'))        
  519 FORMAT (2(9X,'ID.',8X,'STRESS',7X,'MARGIN',8X,'STRESS',6X,        
     1       'MARGIN'))        
  520 FORMAT (2X,'ELEMENT',8X,'SA1',12X,'SA2',12X,'SA3',15X,'S',14X,
     1       'SA-MAX',9X,'SA-MIN',11X,'M.S.-T')        
  521 FORMAT (4X,'ID.',10X,'SB1',12X,'SB2',12X,'SB3',30X,'SB-MAX',9X,   
     1       'SB-MIN',11X,'M.S.-C')        
  522 FORMAT (2(9X,'ELEMENT',12X,'MAX',12X,'AVG',8X,'SAFETY'))        
  523 FORMAT (2(11X,'ID.',13X,'SHEAR',10X,'SHEAR',7X,'MARGIN'))        
  524 FORMAT (2(11X,'ID.',40X,'MARGIN'))        
  525 FORMAT (2X,'ELEMENT',11X,'STRESSES IN ELEMENT COORD SYSTEM',12X, 
     1       'PRINCIPAL',11X,'PRINCIPAL STRESSES',12X,'MAX')        
  526 FORMAT (4X,'ID.',11X,'NORMAL-X',7X,'NORMAL-Y',7X,'SHEAR-XY',6X,   
     1       'STRESS ANGLE',9X,'MAJOR',10X,'MINOR',10X,'SHEAR')
  527 FORMAT (2X,'ELEMENT',6X,'FIBRE',15X,'STRESSES IN ELEMENT COORD ', 
     1       'SYSTEM',13X,'PRINCIPAL STRESSES (ZERO SHEAR)',12X,'MAX')  
  528 FORMAT (4X,'ID.',7X,'DISTANCE',11X,'NORMAL-X',7X,'NORMAL-Y',6X,   
     1       'SHEAR-XY',7X,'ANGLE',9X,'MAJOR',11X,'MINOR',10X,'SHEAR')  
  529 FORMAT (6X,3('ELEMENT',9X,'STRESS',11X),'ELEMENT',9X,'STRESS')    
  530 FORMAT (30X,'G R I D   P O I N T   S I N G U L A R I T Y   ',     
     1       'T A B L E',6X,'SPC',I9,3X,'MPC',I9)        
  531 FORMAT (8X,'POINT',10X,'SINGULARITY',18X,'LIST OF COORDINATE ',
     1       'COMBINATIONS THAT WILL REMOVE SINGULARITY')        
  532 FORMAT (9X,'ID.',3X,'TYPE',7X,'ORDER',7X,'STRONGEST COMBINATION',
     1       15X,'WEAKER COMBINATION',17X,'WEAKEST COMBINATION')      
  533 FORMAT (53X,'L O A D   V E C T O R')
  534 FORMAT (2X,'ELEMENT',8X,'SA1',12X,'SA2',12X,'SA3',12X,'SA4',11X,  
     1       'AXIAL',10X,'SA-MAX',9X,'SA-MIN     M.S.-T')        
  535 FORMAT (4X,'ID.',10X,'SB1',12X,'SB2',12X,'SB3',12X,'SB4',11X,     
     1       'STRESS',9X,'SB-MAX',9X,'SB-MIN     M.S.-C')
  536 FORMAT (43X,'F O R C E S   I N   R O D   E L E M E N T S',5X,     
     1       '( C R O D )')        
  537 FORMAT (33X,'F O R C E S   I N   B E A M   E L E M E N T S',8X,   
     1       '( C B E A M )')        
  538 FORMAT (27X,'F O R C E S   A C T I N G   O N   S H E A R   ',     
     1       'P A N E L   E L E M E N T S   ( C S H E A R )')        
  539 FORMAT (37X,'F O R C E S   I N   T W I S T   P A N E L S',6X,     
     1       '( C T W I S T )')        
  540 FORMAT (21X,'F O R C E S   I N   B A S I C   B E N D I N G   ',   
     1       'T R I A N G L E S',7X,'( C T R B S C )')        
  541 FORMAT (30X,'F O R C E S   I N   S C A L A R   S P R I N G S',8X, 
     1       '( C E L A S 1 )')        
  542 FORMAT (30X,'F O R C E S   I N   S C A L A R   S P R I N G S',8X, 
     1       '( C E L A S 2 )')        
  543 FORMAT (30X,'F O R C E S   I N   S C A L A R   S P R I N G S',8X, 
     1       '( C E L A S 3 )')        
  544 FORMAT (30X,'F O R C E S   I N   S C A L A R   S P R I N G S',8X, 
     1       '( C E L A S 4 )')        
  545 FORMAT (31X,'F O R C E S   O F   S I N G L E - P O I N T   ',     
     1       'C O N S T R A I N T')        
  546 FORMAT (43X,'F O R C E S   I N   R O D   E L E M E N T S',5X,     
     1       '( C O N R O D )')        
  547 FORMAT (33X,'F O R C E S   I N   B A R   E L E M E N T S',9X,     
     1       '( C B A R )')        
  548 FORMAT (17X,'F O R C E S   I N   B E N D I N G   Q U A D R I L A',
     1       ' T E R A L S',9X,'( C Q D P L T )')        
  549 FORMAT (17X,'F O R C E S   I N   G E N E R A L   Q U A D R I L A',
     1       ' T E R A L   E L E M E N T S     ( C Q U A D 1 )')        
  550 FORMAT (17X,'F O R C E S   I N   G E N E R A L   Q U A D R I L A',
     1       'T E R A L   E L E M E N T S     ( C Q U A D 2 )')        
  551 FORMAT (21X,'F O R C E S   I N   G E N E R A L   T R I A N G U L',
     1       ' A R   E L E M E N T S',8X,'( C T R I A 1 )')        
  552 FORMAT (21X,'F O R C E S   I N   G E N E R A L   T R I A N G U L',
     1       ' A R   E L E M E N T S',8X,'( C T R I A 2 )')        
  553 FORMAT (27X,'F O R C E S   I N   B E N D I N G   T R I A N G L E',
     1       ' S       ( C T R P L T )')        
  554 FORMAT (33X,'F O R C E S   I N   R O D   E L E M E N T S     ',   
     1       '( C T U B E )')        
  555 FORMAT (37X,'S T R E S S E S   I N   R O D   E L E M E N T S',6X, 
     1       '( C R O D )')        
  556 FORMAT (34X,'S T R E S S E S   I N   B E A M   E L E M E N T S',  
     1       8X,'( C B E A M )')        
  557 FORMAT (40X,'S T R E S S E S   I N   S H E A R   P A N E L S',6X, 
     1       '( C S H E A R )')        
  558 FORMAT (40X,'S T R E S S E S   I N   T W I S T   P A N E L S',7X, 
     1       '( C T W I S T )')        
  559 FORMAT (22X,'S T R E S S E S   I N   T R I A N G U L A R   ',     
     1       'M E M B R A N E S      ( C T R M E M )')        
  560 FORMAT (19X,'S T R E S S E S   I N   B A S I C   B E N D I N G  ',
     1       ' T R I A N G L E S',8X,'( C T R B S C )')        
  561 FORMAT (30X,'S T R E S S E S   I N   S C A L A R   S P R I N G S',
     1       8X,'( C E L A S 1 )')        
  562 FORMAT (30X,'S T R E S S E S   I N   S C A L A R   S P R I N G S',
     1       8X,'( C E L A S 2 )')        
  563 FORMAT (30X,'S T R E S S E S   I N   S C A L A R   S P R I N G S',
     1       8X,'( C E L A S 3 )')        
  564 FORMAT (33X,'S T R E S S E S   I N   B A R   E L E M E N T S',10X,
     1       '( C B A R )')        
  565 FORMAT (37X,'S T R E S S E S   I N   R O D   E L E M E N T S',6X, 
     1       '( C O N R O D )')        
  566 FORMAT (21X,'S T R E S S E S   I N   Q U A D R I L A T E R A L  ',
     1       ' M E M B R A N E S      ( C Q D M E M )')        
  567 FORMAT (18X,'S T R E S S E S   I N   B E N D I N G   Q U A D R I',
     1       ' L A T E R A L S',13X,'( C Q D P L T )')        
  568 FORMAT (18X,'S T R E S S E S   I N   G E N E R A L   Q U A D R I',
     1       ' L A T E R A L   E L E M E N T S',6X,'( C Q U A D 1 )')   
  569 FORMAT (18X,'S T R E S S E S   I N   G E N E R A L   Q U A D R I',
     1       ' L A T E R A L   E L E M E N T S',6X,'( C Q U A D 2 )')   
  570 FORMAT (18X,'S T R E S S E S   I N   G E N E R A L   T R I A N G',
     1       ' U L A R   E L E M E N T S',7X,'( C T R I A 1 )')        
  571 FORMAT (18X,'S T R E S S E S   I N   G E N E R A L   T R I A N G',
     1       ' U L A R   E L E M E N T S',7X,'( C T R I A 2 )')        
  572 FORMAT (24X,'S T R E S S E S   I N   B E N D I N G   T R I A N G',
     1       ' L E S',8X,'( C T R P L T )')        
  573 FORMAT (36X,'S T R E S S E S   I N   R O D   E L E M E N T S',6X, 
     1       '( C T U B E )')        
  574 FORMAT (20X,'S T R E S S E S   F O R   T H E   T R I A N G U L A',
     1       ' R   R I N G S',5X,'( C T R I A R G )')        
  575 FORMAT (5X,'EL ',13X,'RADIAL',20X,'CIRCUMFERENTIAL',20X,'AXIAL', 
     1       25X,'SHEAR')        
  576 FORMAT (5X,'ID ',15X,'(X)',25X,'(THETA)',25X,'(Z)',27X,'(ZX)')    
  577 FORMAT (18X,'S T R E S S E S   F O R   T H E   T R A P E Z O I D',
     1       ' A L   R I N G S',5X,'( C T R A P R G )')        
  578 FORMAT (5X,'EL ',5X,'STRESS',15X,'RADIAL',16X,'CIRCUMFERENTIAL', 
     1       16X,'AXIAL',21X,'SHEAR')        
  579 FORMAT (5X,'ID ',6X,'POINT',17X,'(X)',21X,'(THETA)',21X,'(Z)',23X,
     1       '(ZX)')        
  580 FORMAT (11X,'S T R E S S   R E S U L T A N T S   F O R   T H E  ',
     1       ' T O R O I D A L   R I N G S     ( C T O R D R G )')      
  581 FORMAT (5X, 'EL ', '  STRESS', 15X, 'MEMBRANE (FORCES)', 26X,    
     1       'FLEXURE (MOMENTS)', 23X, 'SHEAR')        
  582 FORMAT (5X,'ID','    POINT',8X,'HTANGENTIAL',10X,'CIRCUMFERENTIAL'
     1,      8X,'TANGENTIAL',11X,'CIRCUMFERENTIAL',10X,'(FORCE)')     
  583 FORMAT (22X,'F O R C E S   F O R   T H E   T R I A N G U L A R  ',
     1       ' R I N G S     ( C T R I A R G )')        
  584 FORMAT (5X,'EL    CORNER',18X,'RADIAL',26X,'CIRCUMFERENTIAL',   
     1       26X,'AXIAL')        
  585 FORMAT (5X,'ID     POINT',20X,'(X)',31X,'(THETA)',31X,'(Z)')     
  586 FORMAT (21X,'F O R C E S   F O R   T H E   T R A P E Z O I D A L',
     1       '   R I N G S     ( C T R A P R G )')        
  587 FORMAT (23X,'F O R C E S   F O R   T H E   T O R O I D A L   ',   
     1       'R I N G S     ( C T O R D R G )')        
  588 FORMAT (5X,'EL    CORNER',9X,'RADIAL',8X,'CIRCUMFERENTIAL',7X,  
     1       'AXIAL',13X,'MOMENT',9X,'DIRECT STRAIN',7X,'CURVATURE')   
  589 FORMAT (5X,'ID     POINT',11X,'(X)',13X,'(THETA)',12X,'(Z)',15X, 
     1       '(ZX)',14X,'(XI)',13X,'(XI,XI)')        
  590 FORMAT (30X,'E I G E N V A L U E   A N A L Y S I S   S U M M A R',
     1       ' Y     (INVERSE POWER METHOD)')        
 5905 FORMAT (30X,'E I G E N V A L U E   A N A L Y S I S   S U M M A R',
     1       ' Y',9X,'(FEER METHOD)')        
  591 FORMAT ('0', /'0',39X,'NUMBER OF EIGENVALUES EXTRACTED ',6(' .'),
     1       I10,/'0',39X,'NUMBER OF STARTING POINTS USED',7(' .'),I10,
     2       /'0',39X,'NUMBER OF STARTING POINT MOVES',7(' .'),I10,    
     3       /'0',39X,'NUMBER OF TRIANGULAR DECOMPOSITIONS ',4(' .'),  
     4       I10,/'0',39X,'TOTAL NUMBER OF VECTOR ITERATIONS ',5(' .'),
     4       I10,//'0',39X,'REASON FOR TERMINATION',11(' .'),I10,'*',/,
     5       /'0',39X,'LARGEST OFF-DIAGONAL MODAL MASS TERM',4(' .'),  
     6       E10.2, /'0',77X,3(' .'),I10, /50X,'MODE PAIR' ,10(' .'),   
     7       /78X,3(' .'),I10, /'0',39X,'NUMBER OF OFF-DIAGONAL MODAL ',
     8       'MASS', /45X,'TERMS FAILING CRITERION',8(' .'),I10)       
 5911 FORMAT (/'0',39X,'(* ',8A4, /41X,'SEE NASTRAN U.M. VOL II, ',     
     1       'SECTION 2',A4,')')        
  592 FORMAT (26X,'E I G E N V A L U E   A N A L Y S I S   S U M M A R',
     1       ' Y       (DETERMINANT METHOD)')        
  593 FORMAT ('0', /'0',39X,'NUMBER OF EIGENVALUES EXTRACTED ',6(' .'),
     1       I9,/'0',39X,'NUMBER OF PASSES THROUGH STARTING POINTS . .',
     2       I9,/'0',39X,'NUMBER OF CRITERIA CHANGES',9(' .'),I9,      
     3       /'0',39X,'NUMBER OF STARTING POINT MOVES',7(' .'),I9,     
     4       /'0',39X,'NUMBER OF TRIANGULAR DECOMPOSITIONS ',4(' .'),  
     5       I9,/'0',39X,'NUMBER OF FAILURES TO ITERATE TO A ROOT  . .',
     6       I9, //'0',39X,'REASON FOR TERMINATION',11(' .'),I9,'*',   
     7       //,'0',39X,'LARGEST OFF-DIAGONAL MODAL MASS TERM',4(' .'),
     8       E9.2,/'0',77X,3(' .'),I9,/50X, 'MODE PAIR' ,10(' .'), /78X,
     9       3(' .'),I9, /'0',39X,'NUMBER OF OFF-DIAGONAL MODAL MASS', 
     O       /45X,'TERMS FAILING CRITERION',8(' .'),I9)
  594 FORMAT (10X,'STARTING POINT',6X,'LAMBDA',9X,'RADIAN FREQUENCY  ',
     1       '  CYCLIC FREQUENCY    DETERMINANT',9X,'SCALE FACTOR',/)   
  595 FORMAT ('0',40X,'S W E P T   D E T E R M I N A N T   F U N C T I',
     1       ' O N',/)        
  596 FORMAT (20X,'C O M P L E X   E I G E N V A L U E   A N A L Y S I',
     1       ' S   S U M M A R Y     (DETERMINANT METHOD)')        
  597 FORMAT (42X,'- P -',35X,'- DET(P) -', /10X,'STARTING POINT',10X,
     1       'REAL',13X,'IMAG',20X,'MAGNITUDE',9X,'PHASE',5X,        
     2       'SCALE FACTOR')        
  598 FORMAT ('0', /'0',39X,'NUMBER OF EIGENVALUES EXTRACTED ',6(' .'),
     1       I9,/'0',39X,'NUMBER OF PASSES THROUGH STARTING POINTS . .',
     2       I9,/'0',39X,'NUMBER OF CRITERIA CHANGES',9(' .'),I9,      
     3       /'0',39X,'NUMBER OF STARTING POINT MOVES',7(' .'),I9,     
     4       /'0',39X,'NUMBER OF TRIANGULAR DECOMPOSITIONS ',4(' .'),  
     5       I9,/'0',39X,'NUMBER OF FAILURES TO ITERATE TO A ROOT  . .',
     6       I9,/'0',39X,'NUMBER OF PREDICTIONS OUTSIDE REGION',4(' .'),
     7       I9,/'0',/'0',39X,'REASON FOR TERMINATION',11(' .'),I9,'*')
  599 FORMAT ('0', /'0', /'0',35X,'NUMBER OF EIGENVALUES EXTRACTED ',  
     1       9(' .'),I9, /'0',35X,'NUMBER OF STARTING POINTS USED',
     2       10(' .'),I9, /'0',35X,        
     3       'NUMBER OF STARTING POINT OR SHIFT POINT MOVES  . .',I9,  
     4       /'0',35X,'TOTAL NUMBER OF TRIANGULAR DECOMPOSITIONS ',
     5       4(' .'),I9, /'0',35X,'TOTAL NUMBER OF VECTOR ITERATIONS ',
     6       8(' .'),I9, /'0', /'0',35X,'REASON FOR TERMINATION',
     7       14(' .'),I9,'*')        
  600 FORMAT (19X,'C O M P L E X   E I G E N V A L U E   A N A L Y S I',
     1       ' S   S U M M A R Y   (INVERSE POWER METHOD)')        
 6005 FORMAT (23X,'C O M P L E X   E I G E N V A L U E   A N A L Y S I',
     1       ' S   S U M M A R Y     (FEER METHOD)')        
 6006 FORMAT (20X,'C O M P L E X   E I G E N V A L U E   A N A L Y S I',
     1       ' S   S U M M A R Y     (HESSENBERG METHOD)')        
 6007 FORMAT ('0', /'0', /,'0',35X,'NUMBER OF EIGENVALUES EXTRACTED ',
     1       9(' .'),I9, /,'0',35X,'NUMBER OF EIGENVALUES DESIRED ',
     2       10(' .'),I9, /,'0',35X,'REASON FOR TERMINATION',14(' .'),
     3       I9,'*')        
  601 FORMAT (6X,'EIGENVALUE =',E14.6,  '  (FREQ =',E12.5,' HZ)',/)
  602 FORMAT (6X,'EIGENVALUE =',1P,E14.6,'  (FREQ =',1P,E12.5,' HZ)',/)
  603 FORMAT (6X,'FREQUENCY =',E14.6)
  604 FORMAT (6X,'FREQUENCY =',1P,E14.6)
  605 FORMAT (6X,'TIME =',E14.6)
  606 FORMAT (6X,'TIME =',1P,E14.6)
  607 FORMAT (6X,'POINT-ID =',I8)
  608 FORMAT (6X,'ELEMENT-ID =',I8,/)
  609 FORMAT (6X,'COMPLEX EIGENVALUE =',E14.6,',',E14.6)
  610 FORMAT (6X,'COMPLEX EIGENVALUE =',1P,E14.6,',',1P,E14.6)
  611 FORMAT (6X,'FREQUENCY   TYPE',10X,'T1',13X,'T2',13X,'T3',13X,
     1       'R1',13X,'R2',13X,'R3')
  612 FORMAT (6X,' TIME       TYPE',10X,'T1',13X,'T2',13X,'T3',13X,
     1       'R1',13X,'R2',13X,'R3')
  613 FORMAT (48X,'V E L O C I T Y    V E C T O R' )
  614 FORMAT (44X,'A C C E L E R A T I O N    V E C T O R' )
  615 FORMAT (41X,'N O N - L I N E A R - F O R C E   V E C T O R' )
  616 FORMAT (40X,'C O M P L E X   E I G E N V A L U E   S U M M A R Y')
  617 FORMAT ('0',16X,'ROOT     EXTRACTION',18X,'EIGENVALUE',21X,
     1       'FREQUENCY',14X,'DAMPING')        
  618 FORMAT (18X,'NO.',8X,'ORDER',13X,'(REAL)',11X,'(IMAG)',16X,
     1       '(CYCLES)',12X,'COEFFICIENT')
  619 FORMAT (39X,'C O M P L E X   D I S P L A C E M E N T   V E C T O R
     1'      )
  620 FORMAT (43X,'C O M P L E X   V E L O C I T Y   V E C T O R')
  621 FORMAT (39X,'C O M P L E X   A C C E L E R A T I O N   V E C T O R
     1'      )
  622 FORMAT (25X,'C O M P L E X   F O R C E S   O F   S I N G L E   ',
     1       'P O I N T   C O N S T R A I N T')
  623 FORMAT (47X,'C O M P L E X   L O A D   V E C T O R')
  624 FORMAT (39X,'C O M P L E X   E I G E N V E C T O R   NO.',I11)
  625 FORMAT (58X,'(REAL/IMAGINARY)')
  626 FORMAT (57X,'(MAGNITUDE/PHASE)')
  627 FORMAT (27X,'C O M P L E X   S T R E S S E S   I N   B A R   E L',
     1       ' E M E N T S   ( C B A R )')
  628 FORMAT (23X,'C O M P L E X   S T R E S S E S   I N   S C A L A R',
     1       '   S P R I N G S   ( C E L A S 1 )')
  629 FORMAT (23X,'C O M P L E X   S T R E S S E S   I N   S C A L A R',
     1       '   S P R I N G S   ( C E L A S 2 )')
  630 FORMAT (23X,'C O M P L E X   S T R E S S E S   I N   S C A L A R',
     1       '   S P R I N G S   ( C E L A S 3 )')
  631 FORMAT (25X,'C O M P L E X   S T R E S S E S   I N   R O D   E L',
     1       ' E M E N T S   ( C O N R O D )')
  632 FORMAT (14X,'C O M P L E X   S T R E S S E S   I N   Q U A D R I',
     1       ' L A T E R A L   M E M B R A N E S   ( C Q D M E M )')
  633 FORMAT (16X,'C O M P L E X   S T R E S S E S   I N   B E N D I N',
     1       ' G   Q U A D R I L A T E R A L S   ( C Q D P L T )')
  634 FORMAT (6X,'C O M P L E X   S T R E S S E S   I N   G E N E R A L'
     1,      '   Q U A D R I L I A T E R A L   E L E M E N T S   ',
     2       '( C Q U A D 1)')
  635 FORMAT (6X,'C O M P L E X   S T R E S S E S   I N   G E N E R A L'
     1,      '   Q U A D R I L I A T E R A L   E L E M E N T S   ',
     2       '( C Q U A D 2 )')
  636 FORMAT (27X,'C O M P L E X   S T R E S S E S   I N   R O D   E L',
     1       ' E M E N T S   ( C R O D )')
  637 FORMAT (25X,'C O M P L E X   S T R E S S E S   I N   S H E A R  ',
     1       ' P A N E L S   ( C S H E A R )')
  638 FORMAT (14X,'C O M P L E X   S T R E S S E S   I N   B A S I C  ',
     1       ' B E N D I N G   T R I A N G L E S   ( C T R B S C )')
  639 FORMAT (10X,'C O M P L E X   S T R E S S E S   I N   G E N E R A',
     1       ' L   T R I A N G U L A R   E L E M E N T S   ',
     2       '( C T R I A 1 )')
  640 FORMAT (11X,'C O M P L E X   S T R E S S E S   I N   G E N E R A',
     1       ' L  T R I A N G U L A R   E L E M E N T S   ',
     2       '( C T R I A 2 )')
  642 FORMAT (17X,'C O M P L E X   S T R E S S E S   I N   T R I A N G',
     1       ' U L A R   M E M B R A N E S   ( C T R M E M )')        
  643 FORMAT (20X,'C O M P L E X   S T R E S S E S   I N   B E N D I N',
     1       ' G   T R I A N G L E S   ( C T R P L T )')        
  644 FORMAT (26X,'C O M P L E X   S T R E S S E S   I N   R O D   ',   
     1       'E L E M E N T S   ( C T U B E )')        
  645 FORMAT (25X,'C O M P L E X   S T R E S S E S   I N   T W I S T  ',
     1       ' P A N E L S   ( C T W I S T )')        
  646 FORMAT (29X,'C O M P L E X   F O R C E S   I N   B A R   E L E M',
     1       ' E N T S   ( C B A R )')        
  648 FORMAT (25X,'C O M P L E X   F O R C E S   I N   S C A L A R   ', 
     1       'S P R I N G S   ( C E L A S 1 )')        
  649 FORMAT (25X,'C O M P L E X   F O R C E S   I N   S C A L A R   ', 
     1       'S P R I N G S   ( C E L A S 2 )')
  650 FORMAT (25X,'C O M P L E X   F O R C E S   I N   S C A L A R   ', 
     1       'S P R I N G S   ( C E L A S 3 )')
  651 FORMAT (25X,'C O M P L E X   F O R C E S   I N   S C A L A R   ', 
     1       'S P R I N G S   ( C E L A S 4 )')
  652 FORMAT (27X,'C O M P L E X   F O R C E S   I N   R O D   E L E M',
     1       ' E N T S   ( C O N R O D )')
  653 FORMAT (17X,'C O M P L E X   F O R C E S   I N   B E N D I N G  ',
     1       ' Q U A D R I L A T E R A L S   ( C Q D P L T )')
  654 FORMAT (9X,'C O M P L E X   F O R C E S   I N   G E N E R A L   ',
     1       'Q U A D R I L A T E R A L   E L E M E N T S   ',
     2       '( C Q U A D 1 )')
  655 FORMAT (9X,'C O M P L E X   F O R C E S   I N   G E N E R A L   ',
     1       'Q U A D R I L A T E R A L   E L E M E N T S   ',
     2       '( C Q U A D 2 )')
  656 FORMAT (29X,'C O M P L E X   F O R C E S   I N   R O D   E L E M',
     1       ' E N T S   ( C R O D )')
  657 FORMAT (7X,'C O M P L E X   F O R C E S   A C T I N G   O N   ',
     1       'S H E A R   P A N E L   E L E M E N T S   (C S H E A R)')
  658 FORMAT (16X,'C O M P L E X   F O R C E S   I N   B A S I C   B E',
     1       ' N D I N G   T R I A N G L E S   ( C T R B S C )')
  659 FORMAT (12X,'C O M P L E X   F O R C E S   I N   G E N E R A L  ',
     1       ' T R I A N G U L A R   E L E M E N T S   ( C T R I A 1 )')
  660 FORMAT (12X,'C O M P L E X   F O R C E S   I N   G E N E R A L  ',
     1       ' T R I A N G U L A R   E L E M E N T S   ( C T R I A 2 )')
  661 FORMAT (22X, 'C O M P L E X   F O R C E S   I N   B E N D I N G ',
     1       '  T R I A N G L E S   ( C T R P L T )')
  662 FORMAT (28X,'C O M P L E X   F O R C E S   I N   R O D   E L E M',
     1       ' E N T S   ( C T U B E )')
  663 FORMAT (27X,'C O M P L E X   F O R C E S   I N   T W I S T   P A',
     1       ' N E L S   ( C T W I S T )')
  664 FORMAT (12X,'ELEMENT',20X,4('LOCATION',7X ),6X,'AVERAGE', /14X,   
     1       'ID.',26X,'1',14X,'2',14X,'3',14X,'4',13X,'AXIAL STRESS')
  665 FORMAT (17X,'ELEMENT',29X,'AXIAL',39X,'TORSIONAL', /19X,'ID.',30X,
     1       'STRESS',41X,'STRESS')
  666 FORMAT (17X,'ELEMENT',28X,'MAXIMUM',39X,'AVERAGE', /19X,'ID.',31X,
     1       'SHEAR',41X,'SHEAR')
  667 FORMAT (17X,'ELEMENT',29X,'AXIAL',41X,'TORQUE', /19X,'ID.',31X,   
     1       'FORCE')
  668 FORMAT ('  ELEMENT',7X,'FIBRE',37X,'- STRESSES IN ELEMENT COORDI',
     1       'NATE SYSTEM -', /4X,'ID.',8X,'DISTANCE',18X,'NORMAL-X',
     2       26X,'NORMAL-Y',25X,'SHEAR-XY')
  669 FORMAT (13X,'ELEMENT',33X,'- STRESSES IN ELEMENT COORDINATE SYST',
     1       'EM -', /15X,'ID.',18X,'NORMAL-X',26X,'NORMAL-Y',26X,
     2       'SHEAR-XY')
  670 FORMAT (2(16X,'ELEMENT',35X), /2(18X,'ID.',20X,'FORCE',12X))
  671 FORMAT (2(16X,'ELEMENT',35X), /2(18X,'ID.',19X,'STRESS',12X))
  672 FORMAT (17X,'ELEMENT',29X,'FORCE',42X,'FORCE')
  673 FORMAT (19X,'ID.',30X,'PTS 1,3',40X,'PTS 2,4')
  674 FORMAT (17X,'ELEMENT',28X,'MOMENT',41X,'MOMENT')
C        
      END        
