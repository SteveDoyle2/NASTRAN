      SUBROUTINE XRGDFM (NEWSOL,OLDSOL,IAPP,IUFILE,IOPEN,ISIZE,ISCR,    
     1                   NOGO)        
C        
C     XRGDFM READS AND PROCESSES RIGID FORMATS        
C        
C     WRITTEN BY  RPK CORPORATION; DECEMBER, 1983        
C        
C     INPUT        
C       ARGUMENTS        
C         IAPP        =1, FOR DMAP APPROACH; =2, DISPLACEMENT APPRAOCH  
C                     =3, HEAT APPROACH    ; =4, AERO APPROACH        
C         IOPEN       ARRAY FROM OPEN CORE TO CONTAIN THE MODULE        
C                     EXECUTION DECISION TABLE        
C         ISIZE       NUMBER OF WORDS AVAILABLE IN THE IOPEN ARRAY      
C         IUFILE      NAME OF USER'S FILE CONTAINING THE RIGID FORMAT   
C         NEWSOL      ARRAY CONTAINING THE SOLUTION NUMBER FOLLOWED     
C                     BY ALL SUBSET NUMBERS GIVEN BY THE USER        
C         OLDSOL      SOLUTION ON PREVIOUS RUN IF THIS IS A RESTART     
C       OTHER        
C       /XRGDXX/        
C         IRESTR      RESTART FLAG - NON-ZERO IF RUN IS A RESTART       
C         NSUBST      NUMBER OF SUBSETS GIVEN BY THE USER        
C         RECORD      ARRAY CONTAINING THE CARD IMAGE IN 20A4 FORMAT    
C       /SYSTEM/        
C         IDATE       ARRAY CONTAINING MONTH AND YEAR OF NASTRAN LEVEL  
C         OPTAPE      UNIT USED FOR THE OUTPUT PRINT FILE        
C       /TWO/        
C         TWO         ARRAY CONTAINING THE VALUES OF THE POWERS OF 2.   
C       /MEDMSK/        
C         N1          NUMBER OF WORDS USED FOR THE CARD NAME RESTART    
C                     TABLE        
C         N2          NUMBER OF WORDS USED FOR THE FILE NAME RESTART    
C                     TABLE        
C         N3          NUMBER OF WORDS USED FOR THE RIGID FORMAT        
C                     CHANGE RESTART TABLE        
C        
C     OUTPUT        
C       ARGUMENTS        
C         IOPEN       ARRAY CONTAINING THE MODULE EXECUTION DECISION    
C                     TABLE        
C       OTHER        
C         /MEDMSK/        
C           MEDMSK    MODULE EXECUTION DECISION MASK - SET IF SOLUTION  
C                     CHANGE OCCURRED ON A RESTART        
C         /SYSTEM/        
C           ITHRML    SET TO NON-ZERO FOR A HEAT APPROACH        
C         /PHAS11/        
C           IPAS11    ARRAY FOR SUBSTRUCTURE CONTROLS-SET TO ZERO       
C         /PHAS25/        
C           IPAS25    SAME AS IPAS11        
C         /PHAS28/        
C           IPAS28    SAME AS IPAS11        
C         /PHAS31/        
C           IPAS31    SAME AS IPAS11        
C         /PHAS37/        
C           IPAS37    SAME AS IPAS11        
C         /XRGDXX/        
C           IDMAP     DMAP SEQUENCE NUMBER        
C           IGNORE    FLAG SET TO IGNORE ANY CONTROL CARDS FOR THE      
C                     CURRENT DMAP STATEMENT - IS SET WHEN THE DMAP     
C                     STATEMENT IS TO BE DELETED BY THE SUBSET        
C           IPHASE    PHASE NUMBER ASSOCIATED WITH THE ****PHS-        
C                     CONTROL CARD        
C           ITYPE     SET TO 'FILE' OR 'CARD' FOR TYPE OF CONTROL CARD  
C           LIMIT     LOWER/UPPER LIMITS ASSOCIATED WITH THE VALUES     
C                     OF A PARTICULAR CARD TYPE        
C           MEMBER    NAME OF USER'S FILE CONTAINING A RIGID FORMAT     
C                     THIS IS A 2-WORD ARRAY IN 2A4 FORMAT        
C           NUMENT    NUMBER OF WORDS PER ENTRY IN THE MODULE EXECUTION 
C                     DECISION TABLE        
C        
C        
C     LOCAL VARIABLES        
C       ASTERS        VARIABLE CONTAINING THE VALUE OF 4H****        
C       CARD          VARIABLE CONTAINING THE VALUE OF 4HCARD        
C       COMENT        VARIABLE CONTAINING THE VALUE OF 4H$$$$        
C       DOLACR        VARIABLE CONTAINING THE VALUE OF 4H$*CA        
C       DOLAFL        VARIABLE CONTAINING THE VALUE OF 4H$*FI        
C       FILE          VARIABLE CONTAINING THE VALUE OF 4HFILE        
C       FILTYP        ARRAY CONTAINING ACRONYMS FOR APPROACH        
C       IBIT          BIT NUMBER TO SET IN THE MEDMSK        
C       IFILL         VALUE TO BE USED TO INITIALIZE THE MODULE        
C                     EXECUTION DECISION TABLE; =0, IF RESTART;        
C                     =1, OTHERWISE        
C       LU            FORTRAN LOGICAL UNIT NUMBER AS RETURN FROM RFOPEN 
C                     =0, IF OPEN IS NOT SUCCESSFUL        
C       INDEX         INDEX INTO CURRENT ENTRY OF MODULE EXEC.        
C                     DECISION TABLE        
C       ISOL          SOLUTION NUMBER        
C       IWORD         WORD IN MEDMSK TO BE SET FOR RESTART FLAG        
C       NEXT          FLAG INDICATING THAT A NEW DMAP STATEMENT IS      
C                     TO BE PROCESSED; =0, IF NEW DMAP STATEMENT;       
C                     =1, IF PROCESSING THE SAME DMAP STATEMENT        
C       NUMSOL        ARRAY CONTAINING THE RESTART BITS ASSOCIATED      
C                     WITH A RIGID FORMAT SWITCH DURING RESTART        
C       MAXSOL        MAX. SOLUTION NUMBER        
C       PHASE         ARRAY CONTAINING 'PHS1', PHS2', AND 'PHS3'        
C       RFMT          VARIABLE CONTAINING THE VALUE 4HRFMT        
C       SOLNUM        ARRAY CONTAINING THE ALPHA REPRESENTATIONS OF     
C                     THE SOLUTION NUMBERS        
C        
C     FUNCTIONS        
C       1. INITIALIZES SUBSTRUCTURE CONTROLS TO ZERO        
C       2. CHECKS FOR USER SUPPLIED RIGID FORMAT        
C       3. IF STANDARD RIGID FORMAT, VALIDATES SOLUTION NUMBER,        
C          SETS MEDMSK IF A RESTART OCCURRED ON A DIFFERENT        
C          RIGID FORMAT        
C       4. SETS NUMENT=1 AND IFILL=1 IS NO RESTART - OTHERWISE        
C          NUMENT=N1+N2+N3 AND IFILL=0        
C       5. CALLS RFOPEN TO OPEN THE RIGID FORMAT        
C       6. READS A CARD IMAGE FROM THE RIGID FORMAT FILE -        
C          THE DATE AND YEAR OF THE RIGID FORMAT IS VALIDATED AGAINST   
C          THAT THE LEVEL OF NASTRAN        
C          RE-DEFINE NO. OF LINES PER OUTPUT PAGE IF 4TH WORD IS        
C          PRESENT, .GT.20 .AND. .LE.99,  NO DATE CHECK IF THE ORD WROD 
C          IS ****        
C       7. READS A CARD FROM THE RIGID FORMAT FILE AND DOES THE        
C          FOLLOWING DEPENDING ON THE TYPE OF CARD READ:        
C          - FOR '$$$$' COMMENT CARDS, NEXT IS RESET        
C          - FOR '****SBST' CARDS SUBROUTINE XRGSUB IS CALLED        
C          - FOR '****CARD' CARDS SUBROUTINE XRGDCF IS CALLED        
C          - FOR '****FILE' CARDS SUBROUTINE XRGDCF IS CALLED        
C          - FOR '****RFMT' CARDS SUBROUTINE XRGDCF IS CALLED        
C          - FOR '****PHS-' CARDS SUBROUTINE XRGSST IS CALLED        
C          - OTHERWISE, THE CARD IS A DMAP AND WRITEN TO SCRATCH 315    
C          (NOTE- FOR NON RESTARTS, THE ****CARD,****FILE,****RFMT      
C          CARDS ARE BYPASSED.  FOR DMAP STATEMENTS THAT ARE        
C          DELETED BY SUBSET CONTROLS, NO CONTROL CARDS ARE        
C          PROCESSED EXCEPT FOR ****PHS- CARDS UNTIL THE NEXT        
C          DMAP STATEMENT IS ENCOUNTERED)        
C       8. WHEN A '$*CA' OR A '$*FI' CARD IS READ, PROCESSING OF        
C          DMAP STATEMENTS TERMINATES - IF THE JOB IS NOT A RESTART     
C          XRGDFM RETURNS.  OTHERWISE, A CHECK IS MADE TO ENSURE        
C          THAT THE CARD NAME TABLE IS GIVEN FIRST FOLLOWED BY        
C          THE FILE NAME TABLE.  SUBROUTINE XRGDTB IS CALLED TO        
C          PROCESS BOTH TABLES.  AFTER THESE TABLES ARE PROCESSED,      
C          XRGDFM RETURNS.        
C        
C     SUBROUTINES CALLED - RFOPEN,READ,WRITE,XRGSUB,XRGDCF,XRGSST,      
C                          XRGDTB,MESAGE,RFCLOS        
C        
C     COMMENTS FROM G.C./UNISYS - ALL THE MACHINE DEPENDENT DSX* SUB-   
C     ROUTINES ARE NO LONGER USED. SEE RFOPEN.  10/1990        
C        
C     CALLING SUBROUTINE - XCSA        
C        
C     ERROR MESSAGES 8023,504,8025,8026,8024,8037 MAY BE ISSUED        
C        
      EXTERNAL        ORF        
      INTEGER         RECORD, BLANK, ORF, TWO,  ASTERS, SUB(2),        
     1                OPTAPE, CARD, FILE, RFMT, COMENT, SUBSET, DOLAFL, 
     2                DOLACR, IUFILE(2),  IOPEN(100),   IDATE(3),       
     3                FILTYP(4),  SOLNUM(20), NUMSOL(50),OLDNUM,        
     4                NEWSOL(12), OLDSOL(12), PHASE(3), OLDIND        
      CHARACTER       UFM*23        
      COMMON /XMSSG / UFM        
      COMMON /XRGDXX/ IRESTR, NSUBST, IPHASE, ICOL   , NUMBER, ITYPE  , 
     1                ISTATE, IERROR, NUM(2), IND    , NUMENT         , 
     2                RECORD(20)    , ICHAR(80)      , LIMIT(2)       , 
     3                ICOUNT, IDMAP , ISCRX , NAME(2), MEMBER(2)      , 
     4                IGNORE        
      COMMON /SYSTEM/ KSYSTM(100)        
      COMMON /TWO   / TWO(31)        
      COMMON /XMDMSK/ N1,N2,N3,MEDMSK(7)        
      COMMON /PHAS11/ IPAS11(8)        
      COMMON /PHAS25/ IPAS25(14)        
      COMMON /PHAS28/ IPAS28(14)        
      COMMON /PHAS31/ IPAS31(2)        
      COMMON /PHAS37/ IPAS37(6)        
      EQUIVALENCE     (KSYSTM( 2), OPTAPE), (KSYSTM(56), ITHRML) ,      
     1                (KSYSTM(42), IDATE(1)),        
     2                (KSYSTM(69), ISUBAL), (KSYSTM( 9), NLPP  )        
      DATA    FILTYP/ 4HDMAP, 4HDISP, 4HHEAT, 4HAERO    /        
      DATA    SOLNUM/ 1H1 ,1H2 ,1H3 ,1H4 ,1H5 ,1H6 ,1H7 ,        
     1                1H8 ,1H9 ,2H10,2H11,2H12,2H13,2H14,        
     2                2H15,2H16,2H17,2H18,2H19,2H20     /        
      DATA    CARD  / 4HCARD /, FILE   / 4HFILE /        
      DATA    RFMT  / 4HRFMT /, BLANK  / 4H     /        
      DATA    ASTERS/ 4H**** /, COMENT / 4H$$$$ /        
      DATA    SUBSET/ 4HSBST /, DOLACR / 4H$*CA /        
      DATA    DOLAFL/ 4H$*FI /        
      DATA    PHASE / 4HPHS1,   4HPHS2, 4HPHS3  /        
      DATA    SUB   / 4HXRGD,   4HFM            /        
      DATA    NAS   / 4HNAS  /, MAXSOL / 19     /        
C        
C     IN THE FOLLOWING TABLE, VALUES 187-209 ARE FOR STATICS,        
C     210-213 ARE FOR HEAT, AND 214-217 ARE FOR AERO -        
C     THIS PROVIDES FOR 31 DIFFERENT VALUES IN TOTAL (1 WORD)        
C        
      DATA    NUMSOL/        
     1                187, 188, 189, 190, 191, 192, 193, 194,        
     2                195, 196, 197, 198, 199, 200, 201, 202,        
     3                203, 204, 205,  -1,  -1,  -1,  -1, 210,        
     4                 -1, 211,  -1,  -1,  -1,  -1,  -1, 212,        
     5                 -1,  -1,  -1,  -1,  -1,  -1, 216, 214,        
     6                215, 9*-1 /        
C WAS:        
C     DATA    NUMSOL/        
C    1                187, 188, 189, 190, 191, 192, 193, 194,        
C    2                195, 196, 197, 198, 199, 200, 201, 202,        
C    3                 -1,  -1,  -1,  -1, 207,  -1, 208,  -1,        
C    4                 -1,  -1,  -1,  -1, 209,  -1,  -1,  -1,        
C    5                 -1,  -1,  -1,  -1,  -1,  -1, 216, 214,        
C    6                215, 9*-1 /        
C        
      ISCRX = ISCR        
      IDMAP = 0        
      DO 10 K = 1,8        
 10   IPAS11(K) = 0        
      DO 20 K = 1,14        
      IPAS25(K) = 0        
 20   IPAS28(K) = 0        
      DO 30 K = 1,2        
 30   IPAS31(K) = 0        
      DO 40 K = 1,6        
 40   IPAS37(K) = 0        
      IF (IUFILE(1) .EQ. 0) GO TO 100        
      MEMBER(1) = IUFILE(1)        
      MEMBER(2) = IUFILE(2)        
      GO TO 210        
 100  ISOL = NEWSOL(1)        
      GO TO (700,120,130,140), IAPP        
 120  IF (ISOL.GE.1 .AND. ISOL.LE.MAXSOL) GO TO 200        
      GO TO 700        
 130  ITHRML = 1        
      ISOL = ISOL - 23        
      IF (ISOL.EQ.1 .OR. ISOL.EQ.3 .OR. ISOL.EQ.9) GO TO 200        
      GO TO 700        
 140  ISOL = ISOL - 30        
      IF (ISOL.EQ.9 .OR. ISOL.EQ.10 .OR. ISOL.EQ.11) GO TO 200        
      GO TO 700        
 200  MEMBER(1) = FILTYP(IAPP)        
      MEMBER(2) = SOLNUM(ISOL)        
 210  CONTINUE        
C        
      OLDIND = OLDSOL(1)        
      IF (OLDIND.EQ.0 .OR. OLDIND.EQ.NEWSOL(1)) GO TO 270        
C        
C     MAKE SURE CHECKPOINT TAPE FROM OLDER VERSION IS COMPATIBLE WITH   
C     NEW CHANGE MADE IN 1991.        
C        
      IF (OLDIND.NE.21 .AND. OLDIND.NE.23 .AND. OLDIND.NE.29) GO TO 220 
      OLDIND = OLDIND + 3        
      OLDSOL(1) = OLDIND        
      IF (OLDIND .EQ. NEWSOL(1)) GO TO 270        
C        
 220  OLDNUM = NUMSOL(OLDIND)        
      IF (OLDNUM .LE. 0) GO TO 270        
      IWORD = ((OLDNUM-1)/31) + 1        
      IBIT  = OLDNUM - 31*(IWORD-1) + 1        
      MEDMSK(IWORD) = ORF(MEDMSK(IWORD),TWO(IBIT))        
      WRITE  (OPTAPE,240) OLDSOL(1),NEWSOL(1),OLDNUM        
 240  FORMAT ('0*** SWITCHED SOLUTION FOR RESTART - OLD SOLUTION =',I4,
     1        ', NEW SOLUTION =',I4,', BIT NUMBER =',I4)        
 270  IF (IRESTR .NE. 0) GO TO 280        
      NUMENT = 1        
      IFILL  = 1        
      GO TO 290        
 280  NUMENT = N1 + N2 + N3        
      IFILL  = 0        
 290  CONTINUE        
      IDMAP = 0        
      DO 300 KB = 1,NUMENT        
      IOPEN(KB) = IFILL        
 300  CONTINUE        
      INDEX = 1 - NUMENT        
      NEXT  = 0        
C     CALL DSXOPN (MEMBER,2,IDDNAM,2,IFLAG)        
C     IGNORE = 0        
C     IF (IFLAG .NE. 0) GO TO 720        
C     CALL DSXREA (*730,RECORD)        
      CALL RFOPEN (MEMBER,LU)        
      IGNORE = 0        
      IF (LU .EQ. 0) GO TO 790        
      READ (LU,305,ERR=720,END=730) RECORD        
 305  FORMAT (20A4)        
C        
C     BLANK OUT THE 19TH AND 20TH WORDS AS THEY        
C     MAY CONTAIN SEQUENCE INFORMATION        
C        
      RECORD(19) = BLANK        
      RECORD(20) = BLANK        
C        
C     ALLOW OPTIONS TO CHANGE NLPP LOCALLY, AND NOT TO CHECK RF DATE.   
C     (THE NLPP OPTION HERE IS OBSOLETE. CAN BE EASILY DONE VIA NASINFO 
C     FILE - 7/90)        
C        
C     RECORD(5) = BLANK        
C     IF (RECORD(4) .NE. BLANK) CALL A82INT (*780,RECORD(4),J)        
C     IF (J.GE.20 .AND. J.LE.99) NLPP = J        
      IF (RECORD(3) .EQ. ASTERS) GO TO 310        
C     IF (RECORD(1).NE.IDATE(1) .OR. RECORD(2).NE.IDATE(3)) GO TO 770   
      IF (                           RECORD(2).NE.IDATE(3)) GO TO 770   
C310  CALL DSXREA (*730,RECORD)        
 310  READ (LU,305,ERR=720,END=730) RECORD        
C        
C     BLANK OUT THE 19TH AND 20TH WORDS AS THEY        
C     MAY CONTAIN SEQUENCE INFORMATION        
C        
      RECORD(19) = BLANK        
      RECORD(20) = BLANK        
      IF (RECORD(1) .NE. COMENT) GO TO 315        
      IF (NEXT      .EQ. 0     ) GO TO 310        
      NEXT = 0        
      IF (INDEX .LE. ISIZE) GO TO 310        
      GO TO 740        
 315  IF (RECORD(1) .EQ. ASTERS) GO TO 330        
      IF (RECORD(1).EQ.DOLACR .OR. RECORD(1).EQ.DOLAFL) GO TO 400       
      IF (NEXT .EQ. 1) GO TO 325        
      NEXT   = 1        
      IDMAP  = IDMAP + 1        
      INDEX  = INDEX + NUMENT        
      DO 320 KB = 1,NUMENT        
      IOPEN(KB+INDEX-1) = IFILL        
 320  CONTINUE        
 325  CONTINUE        
      CALL WRITE (ISCR,RECORD,18,0)        
      IGNORE = 0        
      GO TO 310        
 330  IF (RECORD(2) .NE. SUBSET) GO TO 340        
      IF (NSUBST .EQ. 0) GO TO 310        
      CALL XRGSUB (IOPEN(INDEX),NEWSOL(2))        
      IF (IERROR .NE. 0) NOGO = 3        
      GO TO 310        
 340  IF (RECORD(2) .NE. CARD) GO TO 350        
      IF (IRESTR.EQ.0 .OR. IGNORE.EQ.1) GO TO 310        
      LIMIT(1) = 1        
      LIMIT(2) = N1*31        
      CALL XRGDCF (IOPEN(INDEX))        
      IF (IERROR .NE. 0) NOGO = 3        
      GO TO 310        
 350  IF (RECORD(2) .NE. FILE) GO TO 360        
      IF (IRESTR.EQ.0 .OR. IGNORE.EQ.1) GO TO 310        
      LIMIT(1) =  N1*31 + 1        
      LIMIT(2) = (N1+N2)*31        
      CALL XRGDCF (IOPEN(INDEX))        
      IF (IERROR .NE. 0) NOGO = 3        
      GO TO 310        
 360  IF (RECORD(2) .NE. RFMT) GO TO 365        
      IF (IRESTR.EQ.0 .OR. IGNORE.EQ.1) GO TO 310        
      LIMIT(1) = (N1+N2)*31 + 1        
      LIMIT(2) = (N1+N2+N3)*31        
      CALL XRGDCF (IOPEN(INDEX))        
      IF (IERROR .NE. 0) NOGO = 3        
      GO TO 310        
 365  DO 370 K = 1,3        
      IF (RECORD(2) .NE. PHASE(K)) GO TO 370        
      IPHASE = K        
      CALL XRGSST (NEWSOL)        
      IF (IERROR .NE. 0) NOGO = 3        
      GO TO 310        
 370  CONTINUE        
      GO TO 750        
 400  CALL WRITE (ISCR,0,0,1)        
      CALL WRITE (ISCR,IOPEN(1),INDEX+NUMENT-1,1)        
      IF (IRESTR. EQ. 0) GO TO 800        
      ITYPE = CARD        
      IF (RECORD(1) .NE. DOLACR) GO TO 760        
      LIMIT(1) = 1        
      LIMIT(2) = N1*31        
      CALL XRGDTB (LU)        
      IF (IERROR .NE. 0) NOGO = 3        
      ITYPE = FILE        
      IF (RECORD(1) .NE. DOLAFL) GO TO 760        
      LIMIT(1) =  N1*31 + 1        
      LIMIT(2) = (N1+N2)*31        
      CALL XRGDTB (LU)        
      IF (IERROR .NE. 0) NOGO = 3        
      GO TO 800        
C        
C     ERRORS        
C        
 700  WRITE  (OPTAPE,710) UFM,ISOL,FILTYP(IAPP)        
 710  FORMAT (A23,' 8023, SOLUTION NUMBER',I4,' IS ILLEGAL FOR APPROACH'
     1,       A4)        
 720  WRITE  (OPTAPE,725) UFM,MEMBER        
 725  FORMAT (A23,' 8025, READ ERROR ON FILE ',2A4)        
      GO TO 790        
 730  WRITE  (OPTAPE,735) UFM,MEMBER        
 735  FORMAT (A23,' 8025, UNEXPECTED EOF ENCOUNTERED ON FILE ',2A4)     
      GO TO 790        
 740  CALL MESAGE (-8,0,SUB)        
      GO TO 800        
 750  WRITE  (OPTAPE,755) UFM,RECORD        
 755  FORMAT (A23,' 8026, THE FOLLOWING CARD HAS AN UNIDENTIFIED ',     
     1       'FUNCTION AFTER ',6H'****', //20X,20A4)        
      NOGO = 3        
      GO TO 310        
 760  WRITE  (OPTAPE,765) UFM,ITYPE,RECORD        
 765  FORMAT (A23,' 8024, EXPECTED A ',3H'$*,A4,1H',' CARD.',        
     1        ' INSTEAD THE FOLLOWING CARD IS READ', //20X,20A4)        
      GO TO 790        
 770  WRITE  (OPTAPE,775) UFM,IDATE(1),IDATE(3),RECORD(1),RECORD(2)     
 775  FORMAT (A23,' 8037, NASTRAN IS LEVEL ',2A4,        
     1        ' BUT THE RIGID FORMAT DATA BASE IS LEVEL ',2A4)        
C     GO TO 790        
C780  WRITE  (OPTAPE,785) RECORD(4)        
C785  FORMAT ('0*** NLPP IN FIRST DMAP LINE ERROR ',A4)        
 790  NOGO = 3        
C800  CALL DSXCLS        
 800  CALL RFCLSE (LU)        
      RETURN        
      END        
