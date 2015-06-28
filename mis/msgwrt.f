      SUBROUTINE MSGWRT        
C        
C     MSGWRT WILL PRINT THE INDICATED ERROR MESSAGES ON THE OUTPUT TAPE 
C        
      INTEGER         OUTTAP,NAME(2),POS(2),NEG(2),PNG(2)        
      DIMENSION       XMSG(4,1),IPAG(2)        
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25,SWM*27        
      COMMON /XMSSG / UFM,UWM,UIM,SFM,SWM        
      COMMON /MACHIN/ MACH        
      COMMON /SYSTEM/ SYSBUF,OUTTAP        
      COMMON /MSGX  / N,M,MSG(4,1)        
      EQUIVALENCE     (XMSG(1,1),MSG(1,1))        
      DATA   POS,NEG/ 4HWARN,4HING ,4HFATA,4HL    /        
      DATA   IPAG / 4H PAG,4HE2   /
      NMSGS = 117
C        
      DO 99 I = 1,N        
      L = IABS(MSG(1,I))        
      IF (MACH.EQ.3 .AND. L.GE.1125 .AND. L.LE.1320) GO TO 80        
C        
C     *** NOTE ***  CHANGE IF STATEMENT WHEN YOU CHANGE GO TO        
C                   MAKE SURE MESSAGE NO. IS WITHIN GO TO RANGE        
C        
      IF (L .GT. NMSGS) GO TO 98        
      IF (L .EQ.    30) GO TO 30        
      IF (MSG(3,I).NE.IPAG(1) .AND. MSG(4,I).NE.IPAG(2)) CALL PAGE2 (4) 
      IF (L.GE.71 .AND. L.LE.NMSGS) GO TO 210        
C        
C     --- NOTE --- INCREASE THE UPPER LIMIT TO ADD MORE MESSAGES        
C        
      LPLUS = L + 3000        
      DO 205 J = 1,2        
      PNG(J) = POS(J)        
      IF (MSG(1,I) .LT. 0) PNG(J) = NEG(J)        
  205 CONTINUE        
      CALL FNAME (MSG(2,I),NAME)        
C        
      GO TO (  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,        
     1        11, 12, 13, 14, 15, 16, 17, 18, 19, 20,        
     2        21, 22, 23, 24, 25, 26, 27, 28, 29, 30,        
     3        31, 32, 33, 34, 35, 36, 37, 38, 39, 40,        
     4        41, 42, 43, 44, 45, 46, 47, 48, 49, 50,        
     5        51, 52, 53, 54, 55, 56, 57, 58, 59, 60,        
     6        61, 62, 63, 64, 65, 66, 67, 68, 69, 70), L        
C        
C     *** CHANGE L INTO CORRECT GINO.NASTIO.PACKUNPK ERROR NUMBER       
C        
  210 LPLUS = L + 1055        
      WRITE (OUTTAP,2005) SFM,LPLUS        
C        
C     *** BRANCH TO PRINT APPROPRIATE ERROR MESSAGE        
C     --- NOTE - EACH NEW MESSAGE REQUIRES A NEW PRINT STATEMENT        
C        
      LPLUS = L - 70        
      GO TO ( 226,  227,  228,  229,  230,  231,  232,  233,  234,      
     9  235,  236,  237,  238,  239,  240,  241,  242,  243,  244,      
     8  245,  246,  247,  248,  249,  250,  251,  252,  253,  254,      
     7  255,  256,  257,  258,  259,  260,  261,  262,  263,  264,      
     6  265,  266,  267,  268,  269,  270,  271,  272), LPLUS        
C        
C     *** GINO FORMAT NUMBERS MATCH THE MESSAGE NUMBER.        
C        
  226 WRITE (OUTTAP,1126)        
      GO TO 99        
  227 WRITE (OUTTAP,1127)        
      GO TO 99        
  228 WRITE (OUTTAP,1128)        
      GO TO 99        
  229 WRITE (OUTTAP,1129)        
      GO TO 99        
  230 WRITE (OUTTAP,1130)        
      GO TO 99        
  231 WRITE (OUTTAP,1131)        
      GO TO 99        
  232 WRITE (OUTTAP,1132)        
      GO TO 99        
  233 WRITE (OUTTAP,1133)        
      GO TO 99        
  234 WRITE (OUTTAP,1134)        
      GO TO 99        
  235 WRITE (OUTTAP,1135)        
      GO TO 99        
  236 WRITE (OUTTAP,1136)        
      GO TO 99        
  237 WRITE (OUTTAP,1137)        
      GO TO 99        
  238 WRITE (OUTTAP,1138)        
      GO TO 99        
  239 WRITE (OUTTAP,1139)        
      GO TO 99        
  240 WRITE (OUTTAP,1140)        
      GO TO 99        
  241 WRITE (OUTTAP,1141)        
      GO TO 99        
  242 WRITE (OUTTAP,1142)        
      GO TO 99        
  243 WRITE (OUTTAP,1143)        
      GO TO 99        
  244 WRITE (OUTTAP,1144)        
      GO TO 99        
  245 WRITE (OUTTAP,1145)        
      GO TO 99        
  246 WRITE (OUTTAP,1146)        
      GO TO 99        
  247 WRITE (OUTTAP,1147)        
      GO TO 99        
  248 WRITE (OUTTAP,1148)        
      GO TO 99        
  249 WRITE (OUTTAP,1149)        
      GO TO 99        
  250 WRITE (OUTTAP,1150)        
      GO TO 99        
  251 WRITE (OUTTAP,1151)        
      GO TO 99        
  252 WRITE (OUTTAP,1152)        
      GO TO 99        
  253 WRITE (OUTTAP,1153)        
      GO TO 99        
  254 WRITE (OUTTAP,1154)        
      GO TO 99        
  255 WRITE (OUTTAP,1155)        
      GO TO 99        
  256 WRITE (OUTTAP,1156)        
      GO TO 99        
  257 WRITE (OUTTAP,1157)        
      GO TO 99        
  258 WRITE (OUTTAP,1158)        
      GO TO 99        
  259 WRITE (OUTTAP,1159)        
      GO TO 99        
  260 WRITE (OUTTAP,1160)        
      GO TO 99        
  261 WRITE (OUTTAP,1161)        
      GO TO 99        
  262 WRITE (OUTTAP,1162) SFM        
      GO TO 99        
  263 WRITE (OUTTAP,1163) SFM        
      GO TO 99        
  264 WRITE (OUTTAP,1164)        
      GO TO 99        
  265 WRITE (OUTTAP,1165)        
      GO TO 99        
  266 WRITE (OUTTAP,1166) SFM        
      GO TO 99        
  267 WRITE (OUTTAP,1167) SFM        
      GO TO 99        
  268 WRITE (OUTTAP,1168)        
      GO TO 99        
  269 WRITE (OUTTAP,1169) SFM        
      GO TO 99        
  270 WRITE (OUTTAP,1170)        
      GO TO 99        
  271 WRITE (OUTTAP,1171)        
      GO TO 99        
  272 WRITE (OUTTAP,1172) SFM        
      GO TO 99        
C        
C    *** END OF GINO ERRORS SECTION        
C        
    1 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,101 ) MSG(2,I),MSG(3,I),MSG(4,I)        
      GO TO 99        
    2 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,102 ) NAME,MSG(2,I),MSG(3,I),MSG(4,I)        
      GO TO 99        
    3 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,103 ) NAME,MSG(2,I),MSG(3,I),MSG(4,I)        
      GO TO 99        
    4 WRITE (OUTTAP,2005) SFM,LPLUS        
      WRITE (OUTTAP,104 ) NAME        
      GO TO 99        
    5 WRITE (OUTTAP,2001) PNG,LPLUS        
      WRITE (OUTTAP,105 ) NAME,MSG(3,I),MSG(4,I)        
      GO TO 99        
    6 WRITE (OUTTAP,106 ) SFM,MSG(3,I),MSG(4,I),MSG(2,I)        
      GO TO 99        
    7 WRITE (OUTTAP,2005) SFM,LPLUS        
      WRITE (OUTTAP,107 ) MSG(3,I),MSG(4,I)        
      IF (MSG(1,I) .GE. 0) GO TO 99        
C     WRITE (OUTTAP,100)        
      CALL ERRTRC ('MSGWRT  ',0)        
      GO TO 99        
    8 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,108 ) MSG(3,I),MSG(4,I)        
      IF (MSG(2,I) .GT.0) WRITE (OUTTAP,1081) MSG(2,I)        
      J = -MSG(2,I)        
      IF (J .GT. 0) WRITE (OUTTAP,1082) J        
      IF (MACH.EQ.3 .OR. MACH.GE.5) WRITE (OUTTAP,1083)        
      GO TO 99        
    9 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,109 ) NAME,MSG(2,I)        
      GO TO 99        
   10 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,110 ) NAME,MSG(2,I)        
      GO TO 99        
   11 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,111 ) MSG(2,I)        
      GO TO 99        
   12 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,112 ) NAME,MSG(2,I)        
      GO TO 99        
   13 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,113 ) NAME,MSG(2,I)        
      GO TO 99        
   14 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,114 ) NAME,MSG(2,I)        
      GO TO 99        
   15 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,115 ) NAME,MSG(2,I)        
      GO TO 99        
   16 WRITE (OUTTAP,2005) SFM,LPLUS        
      WRITE (OUTTAP,116 ) NAME,MSG(3,I),MSG(4,I)        
      GO TO 99        
   17 WRITE (OUTTAP,2010) UWM,LPLUS        
      IF (MSG(2,I) .EQ. 0) WRITE (OUTTAP,117 )        
      IF (MSG(2,I) .NE. 0) WRITE (OUTTAP,1175)        
      GO TO 99        
   18 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,118 ) MSG(3,I),MSG(4,I),MSG(2,I)        
      GO TO 99        
   19 WRITE (OUTTAP,2015) UFM,LPLUS        
      WRITE (OUTTAP,119 ) MSG(3,I),MSG(4,I),MSG(2,I)        
      GO TO 99        
   20 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,120 ) MSG(2,I),MSG(3,I),MSG(4,I)        
      GO TO 99        
   21 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,121 ) MSG(2,I)        
      GO TO 99        
   22 WRITE (OUTTAP,2025) SWM,LPLUS        
      WRITE (OUTTAP,2026)        
      WRITE (OUTTAP,122 ) MSG(3,I),MSG(4,I)        
      GO TO 99        
   23 CONTINUE        
      WRITE (OUTTAP,123 ) UIM,MSG(2,I),MSG(3,I),MSG(4,I)        
      GO TO 99        
   24 WRITE (OUTTAP,2020) UIM,LPLUS        
      WRITE (OUTTAP,124 ) NAME,MSG(3,I)        
      GO TO 99        
   25 WRITE (OUTTAP,2005) SFM,LPLUS        
      WRITE (OUTTAP,125 ) MSG(3,I),MSG(4,I)        
      GO TO 99        
   26 WRITE (OUTTAP,2005) SFM,LPLUS        
      WRITE (OUTTAP,126 ) NAME,MSG(3,I),MSG(4,I)        
      GO TO 99        
   27 WRITE (OUTTAP,2020) UIM,LPLUS        
      WRITE (OUTTAP,127 ) MSG(2,I)        
      GO TO 99        
   28 CONTINUE        
      WRITE (OUTTAP,128 ) UIM,MSG(2,I),MSG(3,I),MSG(4,I)        
      GO TO 99        
   29 WRITE (OUTTAP,129 ) SFM,NAME,MSG(2,I)        
      GO TO 99        
   30 CALL USRMSG (I)        
      GO TO 99        
   31 CONTINUE        
   32 WRITE (OUTTAP,2015) UFM,LPLUS        
      WRITE (OUTTAP,132 ) MSG(2,I),MSG(3,I),MSG(4,I)        
      GO TO 99        
   33 WRITE (OUTTAP,133 ) UFM,MSG(2,I)        
      GO TO 99        
   34 WRITE (OUTTAP,2010) UWM,LPLUS        
      WRITE (OUTTAP,134 ) XMSG(2,I),XMSG(3,I)        
      GO TO 99        
   35 WRITE (OUTTAP,2020) UIM,LPLUS        
      WRITE (OUTTAP,135 ) MSG(2,I),XMSG(3,I)        
      GO TO 99        
   36 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,136 ) MSG(3,I),MSG(4,I)        
      GO TO 99        
   37 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,137 ) MSG(3,I),MSG(4,I)        
      GO TO 99        
   38 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,138 ) MSG(2,I)        
      GO TO 99        
   39 WRITE (OUTTAP,139 ) SFM        
      GO TO 99        
   40 WRITE (OUTTAP,2000) PNG,LPLUS        
      WRITE (OUTTAP,140 ) NAME,MSG(2,I)        
      GO TO 99        
   41 WRITE (OUTTAP,2010) UWM,LPLUS        
      WRITE (OUTTAP,141 ) MSG(2,I)        
      GO TO 99        
   42 WRITE (OUTTAP,2010) UWM,LPLUS        
      WRITE (OUTTAP,142 ) XMSG(2,I)        
      GO TO 99        
   43 WRITE (OUTTAP,2015) UFM,LPLUS        
      WRITE (OUTTAP,143 ) MSG(2,I),MSG(3,I),MSG(4,I)        
      GO TO 99        
   44 WRITE (OUTTAP,2015) UFM,LPLUS        
      WRITE (OUTTAP,144 ) MSG(2,I),MSG(3,I)        
      GO TO 99        
   45 WRITE (OUTTAP,145 ) UWM,MSG(2,I),MSG(3,I),MSG(4,I)        
      GO TO 99        
   46 WRITE (OUTTAP,146 ) UFM        
      GO TO 99        
   47 WRITE (OUTTAP,147 ) UFM        
      GO TO 99        
   48 WRITE (OUTTAP,148 ) SFM,MSG(2,I),MSG(3,I),MSG(4,I)        
      GO TO 99        
   49 WRITE (OUTTAP,149 ) SFM,MSG(3,I),MSG(4,I),MSG(2,I)        
      GO TO 99        
   50 WRITE (OUTTAP,150 ) SFM,MSG(3,I),MSG(4,I),MSG(2,I)        
      GO TO 99        
   51 WRITE (OUTTAP,151 ) UFM,MSG(2,I)        
      GO TO 99        
   52 WRITE (OUTTAP,152 ) UWM,MSG(2,I),MSG(3,I),MSG(4,I)        
      GO TO 99        
   53 WRITE (OUTTAP,153 ) UWM,MSG(2,I),MSG(3,I)        
      GO TO 99        
   54 WRITE (OUTTAP,154 ) UWM,MSG(2,I),XMSG(3,I)        
      GO TO 99        
   55 WRITE (OUTTAP,2001) PNG,LPLUS        
      WRITE (OUTTAP,155 ) MSG(3,I),MSG(4,I)        
      GO TO 99        
   56 WRITE (OUTTAP,2001) PNG,LPLUS        
      WRITE (OUTTAP,156 )        
      GO TO 99        
   57 WRITE (OUTTAP,2001) PNG,LPLUS        
      WRITE (OUTTAP,157 ) NAME        
      GO TO 99        
   58 WRITE (OUTTAP,2001) PNG,LPLUS        
      WRITE (OUTTAP,158 ) XMSG(2,I),MSG(3,I)        
       GO TO 99        
   59 WRITE (OUTTAP,2001) PNG,LPLUS        
      WRITE (OUTTAP,159 ) MSG(2,I),MSG(3,I),MSG(4,I)        
      GO TO 99        
   60 WRITE (OUTTAP,160 ) UFM        
      GO TO 99        
   61 CONTINUE        
      GO TO 99        
   62 WRITE (OUTTAP,162 ) SFM        
      GO TO 99        
   63 WRITE (OUTTAP,163 ) SFM        
      GO TO 99        
   64 WRITE (OUTTAP,164 ) SFM        
      GO TO 99        
   65 WRITE (OUTTAP,165 ) SFM        
      GO TO 99        
   66 WRITE (OUTTAP,166 ) SFM        
      GO TO 99        
   67 WRITE (OUTTAP,167 ) SFM        
      GO TO 99        
   68 WRITE (OUTTAP,168 ) SFM        
      GO TO 99        
   69 WRITE (OUTTAP,169 ) SFM        
      GO TO 99        
   70 WRITE (OUTTAP,170 ) SFM        
      GO TO 99        
   80 CALL MSGUNI (L)        
      GO TO 99        
   98 WRITE (OUTTAP,198 ) MSG(1,I),MSG(2,I),MSG(3,I),MSG(4,I)        
   99 CONTINUE        
      IF (N .GE. M) WRITE (OUTTAP,199) UWM,M        
      I = N        
      N = 0        
      IF (MSG(1,I) .GE. 0) GO TO 1000        
C     WRITE (OUTTAP,100)        
      CALL ERRTRC ('WRTMSG  ',100)        
 1000 RETURN        
C        
C        
C 100 FORMAT ('0FATAL ERROR')        
  101 FORMAT ('0ATTEMPT TO OPEN DATA SET',I4,' IN SUBROUTINE ',A4,A2,   
     1        ', WHICH WAS NOT DEFINED IN THE FIST')        
  102 FORMAT ('0EOF ENCOUNTERED WHILE READING DATA SET ',2A4,'(FILE',   
     1        I4,') IN SUBROUTINE ',2A4)        
  103 FORMAT ('0ATTEMPT TO READ PAST THE END OF A LOGICAL RECORD IN ',  
     1        'DATA SET ',2A4,'(FILE',I4,') IN SUBROUTINE ',2A4)        
  104 FORMAT ('0INCONSISTENT TYPE FLAGS ENCOUNTERED WHILE PACKING DATA',
     1        'SET ',2A4)        
  105 FORMAT ('0ATTEMPT TO OPERATE ON THE SINGULAR MATRIX ',2A4,        
     1        ' IN SUBROUTINE ',2A4)        
  106 FORMAT (A25,' 3006, BUFFER ASSIGNED WHEN OPENING DATA BLOCK ',2A4,
     1        6H,FILE ,I5,1H,, /5X,'CONFLICTS WITH BUFFERS CURRENTLY ', 
     2        'OPEN.')        
  107 FORMAT ('0ILLEGAL INPUT TO SUBROUTINE ',2A4)        
  108 FORMAT ('0INSUFFICIENT CORE AVAILABLE FOR SUBROUTINE ',2A4)       
 1081 FORMAT (' ADDITIONAL CORE REQUIRED =',I10,' WORDS.')        
 1082 FORMAT (' PRESENT OPEN CORE SIZE =',I10,' WORDS.')        
 1083 FORMAT (' USE NASTRAN HICORE CARD TO INCREASE CORE SIZE')        
  109 FORMAT ('0DATA TRANSMISSION ERROR ON DATA SET ',2A4,'(FILE',I4,   
     1        1H))        
  110 FORMAT ('0ATTEMPT TO MANIPULATE DATA SET ',2A4,'(FILE',I4,        
     1        ' BEFORE OPENING THE FILE')        
  111 FORMAT ('0ATTEMPT TO WRITE A TRAILER ON FILE',I4,        
     1        ' WHEN IT HAS BEEN PURGED')        
  112 FORMAT ('0ATTEMPT TO OPEN DATA SET ',2A4,'(FILE',I4,        
     1        ') WHICH HAS ALREADY BEEN OPENED')        
  113 FORMAT ('0ATTEMPT TO READ DATA SET ',2A4,'(FILE',I4,        
     1        ') WHEN IT WAS OPENED FOR OUTPUT')        
  114 FORMAT ('0ATTEMPT TO WRITE DATA SET ',2A4,'(FILE',I4,        
     1        ') WHEN IT WAS OPENED FOR INPUT')        
  115 FORMAT ('0ATTEMPT TO FWDREC ON DATA SET ',2A4,'(FILE',I4,        
     1        ') WHEN IT WAS OPENED FOR OUTPUT')        
  116 FORMAT (1H0,2A4,' MATRIX IS NOT IN PROPER FORM IN SUBROUTINE ',   
     1        2A4)        
  117 FORMAT ('0    ONE OR MORE POTENTIAL SINGULARITIES HAVE NOT BEEN ',
     1        'REMOVED BY SINGLE OR MULTI-POINT CONSTRAINTS.', /5X,     
     2        '(USER COULD REQUEST NASTRAN AUTOMATIC SPC GENERATION ',  
     3        'VIA A ''PARAM AUTOSPC'' BULK DATA CARD)')        
 1175 FORMAT ('0    ONE OR MORE POTENTIAL SINGULARITIES HAVE NOT BEEN ',
     1        'REMOVED BY SINGLE OR MULTI-POINT CONSTRAINTS.')        
  118 FORMAT ('0MODULE ',2A4,', SEQUENCE NO.',I5,        
     1        ', REQUIREMENTS EXCEED AVAILABLE FILES')        
  119 FORMAT ('0MAXIMUM LINE COUNT EXCEEDED IN SUBROUTINE ',2A4,        
     1        ' LINE COUNT EQUALS',I8)        
  120 FORMAT ('0GNFIST OVERFLOWED FIST TABLE AT SEQUENCE NO.',I5,       
     1        '  DATA SET ',2A4)        
  121 FORMAT ('0FILE',I4,' NOT DEFINED IN FIST')        
  122 FORMAT (5X,'DATA BLOCK ',2A4,' MAY BE REQUIRED AS INPUT AND IS ', 
     1        'NOT OUTPUT BY A PREVIOUS MODULE IN THE CURRENT DMAP ',   
     2        'ROUTE.')        
  123 FORMAT (A29,' 3028   B =',I5,' C =',I5,' R =',I5)        
  124 FORMAT ('0THE BANDWIDTH OF MATRIX ',2A4,' EXCEEDS THE MAXIMUM ',  
     1        'BANDWIDTH. A MAXIMUM BANDWIDTH OF',I5,' WILL BE USED')   
  125 FORMAT ('0ILLEGAL INDEX IN ACTIVE ROW OR COLUMN CALCULATION IN ', 
     1        2A4)        
  126 FORMAT ('0MATRIX ',2A4,' EXCEEDS MAXIMUM ALLOWABLE SIZE FOR BAND',
     1        'WIDTH PLUS ACTIVE COLUMNS. BMAX =',I6,' CMAX =',I6)      
  127 FORMAT ('0DECOMPOSITION TIME ESTIMATE IS',I6)        
  128 FORMAT (A29,' 3028, BBAR =',I5,' CBAR =',I5,' R =',I5)        
  129 FORMAT (A25,' 3029, PHYSICAL EOF ENCOUNTERED ON DATA SET ',2A4,   
     1        ' (FILE',I4,3H ).)        
  132 FORMAT ('0UNABLE TO FIND SELECTED SET (',I8,') IN TABLE (',A4,    
     1        ') IN SUBROUTINE (',A4,2H).)        
  133 FORMAT (A23,' 3033, SUBCASE ID',I9,' IS REFERENCED ON ONE OR MORE'
     1,       ' RANDPS CARDS', /5X,'BUT IS NOT A CURRENT SUBCASE ID.')  
  134 FORMAT ('0ORTHOGONALITY CHECK FAILED, LARGEST TERM = ',1P,E14.7,  
     1        ', EPSILON = ',1P,E14.7)        
  135 FORMAT (5X,'FOR SUBCASE NUMBER',I6,', EPSILON SUB E = ',1P,E15.7) 
  136 FORMAT ('0DATA SET ',2A4,' IS REQUIRED AS INPUT BUT HAS NOT ',    
     1        'BEEN GENERATED OR PURGED')        
  137 FORMAT ('0JOB TERMINATED IN SUBROUTINE ',2A4)        
  138 FORMAT ('0DATA SET ',A4,' DOES NOT HAVE MULTI-REEL CAPABILITY')   
  139 FORMAT (A25,' 3039, ENDSYS CANNOT FIND SAVE FILE.')        
  140 FORMAT ('0ATTEMPT TO WRITE DATA SET ',2A4,'(FILE',I4,        
     1        ') WHEN IT IS AN INPUT FILE')        
  141 FORMAT ('0EXTERNAL GRID POINT',I9,' DOES NOT EXIST OR IS NOT A ', 
     1        'GEOMETRIC GRID POINT.',        
     2        /5X,'THE BASIC ORIGIN WILL BE USED.')        
  142 FORMAT ('0INCONSISTENT SCALAR MASSES HAVE BEEN USED.  EPSILON/',  
     1        'DELTA = ',1P,E15.7)        
  143 FORMAT ('0UNCONNECTED EXTRA POINT (MODAL COORDINATE =',I9,        
     1        ') HAS BEEN DETECTED BY SUBROUTINE ',2A4)        
  144 FORMAT ('0A POINT ON NON-LINEAR LOAD SET',I9,' NOLIN',I1,        
     1        ' IS NOT AN EXTRA POINT.', /5X,'ONLY EXTRA POINTS MAY ',  
     2        'HAVE NON-LINEAR LOADS IN A MODAL FORMULATION.')        
  145 FORMAT (A25,' 3045, INSUFFICIENT TIME TO COMPLETE THE REMAINING', 
     1        I6,' SOLUTION(S) IN MODULE ',2A4)        
  146 FORMAT (A23,' 3046, YOUR SELECTED LOADING CONDITION, INITIAL ',   
     1        'CONDITION, AND NON-LINEAR FORCES ARE NULL', /5X,        
     2        'A ZERO SOLUTION WILL RESULT.')        
  147 FORMAT (A23,' 3047, NO MODES WITHIN RANGE AND LMODES = 0. A MODAL'
     1,       ' FORMULATION CANNOT BE MADE.')        
  148 FORMAT (A25,' 3048, BUFFER CONTROL WORD INCORRECT FOR GINO ',A4,  
     1        ' OPERATION ON DATA BLOCK ',2A4)        
  149 FORMAT (A25,' 3049, GINO UNABLE TO POSITION DATA BLOCK ',2A4,     
     1        ' CORRECTLY DURING ',A4,' OPERATION.')        
  150 FORMAT (A25,' 3050, INSUFFICIENT TIME REMAINING FOR ',2A4,        
     1        '.  TIME ESTIMATE IS',I9,' SECONDS.')        
  151 FORMAT (A23,' 3051, INITIAL CONDITION SET',I9,' WAS SELECTED FOR',
     1        ' A MODAL TRANSIENT PROBLEM.' ,/5X,        
     2        'INITIAL CONDITIONS ARE NOT ALLOWED IN SUCH A PROBLEM.')  
  152 FORMAT (A25,' 3052, A RANDOM REQUEST FOR CURVE TYPE - ',A4,       
     1        ' -, POINT -',I9, /5X,'COMPONENT -',I4,        
     2        ' -, SPECIFIES TOO LARGE A COMPONENT ID.  THE LAST ',     
     3        'COMPONENT WILL BE USED.')        
  153 FORMAT (A25,' 3053, THE ACCURACY OF EIGENVALUE',I6,' IS IN DOUBT.'
     1,       ' GIVENS-QR FAILED TO CONVERGE IN',I4,' ITERATIONS.')     
  154 FORMAT (A25,' 3054, THE ACCURACY OF EIGENVECTOR',I6,' CORRESPOND',
     1        'ING TO THE EIGENVALUE ',1P,E15.7,' IS IN DOUBT.')        
  155 FORMAT ('0AN ATTEMPT TO MULTIPLY OR MULTIPLY AND ADD NON-CONFOR', 
     1        'MABLE MATRICES TOGETHER WAS MADE IN SUBROUTINE ',2A4)    
  156 FORMAT ('0NO MASS MATRIX IS PRESENT BUT MASS DATA IS REQUIRED')   
  157 FORMAT ('0MATRIX ',2A4,' IS NOT POSITIVE DEFINITE.')        
  158 FORMAT ('0EPSILON IS LARGER THAN ',1P,E14.7,' FOR SUBCASE',I5)    
  159 FORMAT ('0SET IDENTIFIER ',A4,' DOES NOT EXIST. ERROR DETECTED ', 
     1        'IN SUBROUTINE ',2A4)        
  160 FORMAT (A23,' 3060, READ MODULE FINDS THAT THE INPUT STIFFNESS ', 
     1        'AND/OR MASS MATRIX IS NULL.')        
C 161 FORMAT (9H CODES = ,2I20)        
  162 FORMAT (A25,' 3062, NO MESSAGE.')        
  163 FORMAT (A25,' 3063, NO MESSAGE.')        
  164 FORMAT (A25,' 3064, NO MESSAGE.')        
  165 FORMAT (A25,' 3065, NO MESSAGE.')        
  166 FORMAT (A25,' 3066, NO MESSAGE.')        
  167 FORMAT (A25,' 3067, NO MESSAGE.')        
  168 FORMAT (A25,' 3068, NO MESSAGE.')        
  169 FORMAT (A25,' 3069, NO MESSAGE.')        
  170 FORMAT (A25,' 3070, NO MESSAGE.')        
  198 FORMAT ('0NO MESSAGE FOR MESSAGE NO. =',I5 ,/5X,'PARAMETERS = ',  
     1        3I20)        
  199 FORMAT (A25,' 3199, NON-FATAL MESSAGES MAY HAVE BEEN LOST BY ',   
     1        'ATTEMPTING TO QUEUE MORE THAN',I5,' MESSAGES')        
 1126 FORMAT ('0ADDRESS OF BUFFER LESS THAN ADDRESS OF /XNSTRN/.')      
 1127 FORMAT ('0BUFFER ASSIGNED EXTENDS INTO MASTER INDEX AREA.')       
 1128 FORMAT ('0ON AN OPEN CALL WITHOUT REWIND, THE BLOCK NUMBER READ ',
     1        'DOES NOT MATCH EXPECTED VALUE.')        
 1129 FORMAT ('0ON A CALL WRITE THE WORD COUNT IS NEGATIVE.')        
 1130 FORMAT ('0ON A CALL READ THE CONTROL WORD AT WHICH THE FILE IS ', 
     1        'POSITIONED IS NOT ACCEPTABLE.')        
 1131 FORMAT ('0LOGICAL RECORD TRAILER NOT RECOGNIZABLE AS SUCH.')      
 1132 FORMAT ('0UNRECOGNIZABLE CONTROL WORD DURING PROCESSING OF A ',   
     1        'BCKREC CALL.')        
 1133 FORMAT ('0AFTER A POSITIONING CALL TO IO6600, DURING PROCESSING ',
     1        'OF A BCKREC CALL THE BLOCK READ WAS NOT THE EXPECTED ',  
     2        'ONE.')        
 1134 FORMAT ('0CALL SKPFIL IN A FORWARD DIRECTION ON A FILE NOT ',     
     1        'OPENED FOR OUTPUT IS NOT SUPPORTED.')        
 1135 FORMAT ('0FILPOS WAS CALLED ON A FILE OPENED FOR OUTPUT.')        
 1136 FORMAT ('0ENDPUT WAS CALLED WITH BLOCK(8) EQUAL TO -1.')        
 1137 FORMAT ('0MORE TERMS WRITTEN IN STRING THAN WERE AVAILABLE TO ',  
     1        'WRITE.')        
 1138 FORMAT ('0CURRENT BUFFER POINTER EXCEEDS LAST DATA WORD IN BLOCK')
 1139 FORMAT ('0ON AN INITIAL CALL TO GETSTR, THE RECORD IS NOT ',      
     1        'POSITIONED AT THE COLUMN HEADER.')        
 1140 FORMAT ('0STRING DEFINITION WORD NOT RECOGNIZABLE.')        
 1141 FORMAT ('0FIRST WORD OF A DOUBLE PRECISION STRING IS NOT ON A ',  
     1        'DOUBLE PRECISION BOUNDARY.')        
 1142 FORMAT ('0CURRENT BUFFER POINTER IS BEYOND RANGE OF INFORMATION ',
     1        'IN BUFFER.')        
 1143 FORMAT ('0ON AN INITIAL CALL TO GETSTB, THE FILE IS NOT ',        
     1        'POSITIONED AT AN ACCEPTABLE POINT.')        
 1144 FORMAT ('0END-OF-SEGMENT CONTROL WORD SHOULD HAVE IMMEDIATELY ',  
     1        'PRECED CURRENT POSITION AND IT DID NOT.')        
 1145 FORMAT ('0COLUMN TRAILER NOT FOUND.')        
 1146 FORMAT ('0PREVIOUS RECORD TO BE READ BACKWARDS WAS NOT WRITTEN ', 
     1        'WITH STRING TRAILERS.')        
 1147 FORMAT ('0STRING RECOGNITION WORD NOT RECOGNIZED.')        
 1148 FORMAT ('0RECORD CONTROL WORD NOT IN EXPECTED POSITION.')        
 1149 FORMAT ('0RECTYP WAS CALLED FOR A FILE OPENED FOR OUTPUT.')       
 1150 FORMAT ('0RECTYP MUST BE CALLED WHEN THE FILE IS POSITIONED AT ', 
     1        'THE BEGINNING OF A RECORD.')        
 1151 FORMAT ('ON A CALL TO OPEN THE BUFFER ASSIGNED OVERLAPS A ',      
     1        'PREVIOUSLY ASSIGNED BUFFER.')        
 1152 FORMAT ('0A CALL TO OPEN FOR AN ALREADY OPEN FILE.')        
 1153 FORMAT ('0FILE NOT OPEN.')        
 1154 FORMAT ('0GINO REFERENCE NAME NOT IN FIST OR FILE NOT OPEN.')     
 1155 FORMAT ('0A CALL TO GETSTR OCCURRED WHEN THE FILE WAS POSITIONED',
     1        'AT END-OF-FILE.')        
 1156 FORMAT ('0ATTEMPTED TO WRITE ON AN INPUT FILE.')        
 1157 FORMAT ('0ATTEMPTED TO READ FROM AN OUTPUT FILE.')        
 1158 FORMAT ('0A CALL TO BLDPK OR PACK IN WHICH EITHER TYPIN OR ',     
     1        'TYPOUT IS OUT OF RANGE.')        
 1159 FORMAT ('0ROW POSITIONS OF ELEMENTS FURNISHED TO ZBLPKI OR ',     
     1        'BLDPKI ARE NOT IN A MONOTONIC INCREASING SEQUENCE.', /,  
     2        ' (POSSIBLY DUE TO ROW OR COLUMN INDEX ERROR)')        
 1160 FORMAT ('0ON A CALL TO BLDPKN, FILE NAME DOES NOT MATCH PREVIOUS',
     1        'CALLS.')        
 1161 FORMAT ('0A CALL TO INTPK OR UNPACK IN WHICH TYPOUT IS OUT OF ',  
     1        'RANGE.')        
 1162 FORMAT (A25,' 1162, NO MESSAGE.')        
 1163 FORMAT (A25,' 1163, NO MESSAGE.')        
 1164 FORMAT ('0 FOLLOWING A READ ATTEMPT ON AN INDEXED FILE, EITHER ', 
     1        'AN END-OF-FILE WAS ENCOUNTERED OR THE NUMBER OF WORDS ', 
     2        'READ WAS INCORRECT.')        
 1165 FORMAT ('0ON AN ATTEMPT TO READ A SEQUENTIAL FILE, AN END-OF-',   
     1        'FILE OR AN END-OF-INFORMATION WAS ENCOUNTERED.')        
 1166 FORMAT (A25,' 1166, NO MESSAGE.')        
 1167 FORMAT (A25,' 1167, NO MESSAGE.')        
 1168 FORMAT ('0A CALL TO IO6600 WITH OPCODE=5 (FORWARD SPACE) IS NOT ',
     1        'SUPPORTED.')        
 1169 FORMAT (A25,' 1169, NO MESSAGE.')        
 1170 FORMAT ('0ILLEGAL CALL TO NASTIO, LOGIC ERROR IN IO6600.')        
 1171 FORMAT ('0ON A POSITION CALL, THE BLOCK NUMBER REQUESTED IS NOT ',
     1        'FOUND IN CORE WHEN IT IS EXPECTED THERE.')        
 1172 FORMAT (A25,' 1172, NO MESSAGE.')        
 2000 FORMAT (12H0*** SYSTEM ,2A4,8H MESSAGE,I5)        
 2001 FORMAT (10H0*** USER ,2A4,9H MESSAGE ,I5)        
 2005 FORMAT (A25,I5)        
 2010 FORMAT (A25,I5)        
 2015 FORMAT (A23,I5)        
 2020 FORMAT (A29,I5)        
 2025 FORMAT (A27,I5)        
 2026 FORMAT (1H+,33X,'(SEE PROG. MANUAL SEC. 4.9.7, OR ',7HUSERS' ,    
     1        'MANUAL P. 6.5-3)')        
      END        
