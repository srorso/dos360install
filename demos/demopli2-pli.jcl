// JOB DEMOPLI SAMPLE PL/I PROGRAM
// OPTION LINK,SYM,LISTX
// EXEC PL/I
* PROCESS STMT  (PRINT STATEMENT NUMBER IF ABEND)
 /* SAMPLE BUSINESS ORIENTED PL/I PROGRAM */
 /* DEMONSTRATES RECORD ORIENTED I/O */
 REVIEW: PROC OPTIONS (MAIN);
         DCL GETLC ENTRY,
             1 CARDIN,
               5 IDEPNUM    PIC 'XX',
               5 ISALNUM    PIC 'XXX',
               5 ISALNAM    PIC '(20)X',
               5 IYTDSAL    PIC '99999V99',
               5 ICURSAL    PIC '99999V99',
               5 FILLER1    PIC 'XX',
               5 IMONEMP    PIC '99',
               5 IYTDRET    PIC '9999V99',
               5 ICURRET    PIC '9999V99',
               5 FILLER2    PIC '(25)X',
             CARD CHAR(80) DEFINED CARDIN,
             READER FILE INPUT RECORD
                  ENV(MEDIUM(SYS007,2501) BUFFERS(2) F(80)),
             PRINTR FILE OUTPUT STREAM PRINT
                  ENV(MEDIUM(SYS009,1403) BUFFERS(2) F(133)),
             SYSLC FIXED DEC(3,0),
             LINES FIXED DEC(3,0),
             PAGE FIXED DEC(5,0) STATIC INIT (0),
             DATE BUILTIN,
             TODAY CHAR(8) STATIC INIT ('  /  /  '),
            (WYTDSAL, WCURSAL, WYTDRET, WCURRET) FIXED DEC (7,2),
             WMONEMP FIXED DEC (3,0),
            (WSALTOT, WAVESAL) FIXED DEC (7,2),
             WFINTOT FIXED DEC (9,2) STATIC INIT (0);
         SUBSTR(TODAY,1,2) = SUBSTR(DATE,3,2);
         SUBSTR(TODAY,4,2) = SUBSTR(DATE,5,2);
         SUBSTR(TODAY,7,2) = SUBSTR(DATE,1,2);
         CALL GETLC (SYSLC);
         OPEN FILE (READER), FILE (PRINTR);
         /* IN THIS PL/I SUBSET COMPILER THE ON STATEMENT WILL ONLY
            ALLOW A 'GO TO'.  'ENDPAGE' WILL LOOK FOR CHANNEL 12.
            THIS WILL IGNORE IT; WE'LL USE A LINE COUNTER INSTEAD. */
         ON ENDFILE (READER)  GO TO ENDJOB;
         ON ENDPAGE (PRINTR);
         ON CONVERSION  GO TO INVALID;
         CALL HEDING;
 MAINLOP: READ FILE (READER) INTO (CARDIN);
         IF LINES > SYSLC THEN CALL HEDING;
         /* IF THE INPUT RECORD HAS NON-NUMERIC DATA THE 'CONVERSION'
            SIGNAL WILL BE RAISED IN THE FOLLOWING GET. */
         GET STRING (CARD) EDIT
             (WYTDSAL, WCURSAL, WMONEMP, WYTDRET, WCURRET)
             (X(25), 2 F(7,2), X(2), F(2,0), 2 F(6,2));
         WSALTOT = WYTDSAL - WYTDRET + WCURSAL - WCURRET;
         WFINTOT = WFINTOT + WSALTOT;
         IF IMONEMP > 0
            THEN DO; WAVESAL = WSALTOT / WMONEMP;
                     PUT FILE (PRINTR) SKIP EDIT
                         (IDEPNUM, ISALNUM, ISALNAM, WSALTOT, WMONEMP,
                          WAVESAL)
                         (3(X(5), A), X(4), F(11,2), X(3), F(4,0),
                          X(5), F(11,2));
                     LINES = LINES + 1; /* INCREMENT LINE COUNTER */
                 END;
            ELSE DO; PUT FILE (PRINTR) SKIP EDIT
                         (IDEPNUM, ISALNUM, ISALNAM, WSALTOT, '0')
                         (3(X(5), A), X(4), F(11,2), X(6), A);
                     LINES = LINES + 1; /* INCREMENT LINE COUNTER */ 
                 END;
          GO TO MAINLOP;          
 INVALID: PUT FILE (PRINTR) SKIP EDIT
              (IDEPNUM, ISALNUM, ISALNAM, SUBSTR(CARD, 26, 30),
               '**NON-NUMERIC DATA**')
              (4(X(5), A), X(2), A);
          LINES = LINES + 1; /* INCREMENT LINE COUNTER */ 
          GO TO MAINLOP;          
 ENDJOB:  PUT FILE (PRINTR) SKIP(2) EDIT
              ('FINAL TOTAL $', WFINTOT, '*')
              (X(30), A, F(12,2), X(1), A);
          CLOSE FILE (READER), FILE (PRINTR);
 HEDING:  PROC;
          DCL HEAD1 CHAR(37) STATIC INIT ('A COMPUTER CENTER SALES STATU
 S REPORT'),
              HEAD2 CHAR(77) STATIC INIT ('    DEPT. SALESMAN    SALESMA
 N                  TOTAL      MONTHS     AVERAGE'),
              HEAD3 CHAR(76) STATIC INIT ('     NO.    NO.       NAME
                    SALES       EMP.       SALES');
          PAGE = PAGE + 1;  /* INCREMENT PAGE COUNTER */
          IF PAGE > 1  THEN PUT FILE (PRINTR) PAGE;
          PUT FILE (PRINTR) EDIT (TODAY, HEAD1, 'PAGE', PAGE)
                                 (X(4), A, X(6), A, X(8), A, F(4,0));
          PUT FILE (PRINTR) LINE(3) EDIT (HEAD2) (A);
          PUT FILE (PRINTR) LINE(4) EDIT (HEAD3) (A);
          PUT FILE (PRINTR) SKIP;
          LINES = 5;  /* RESET LINE COUNTER */
 END HEDING;
 END REVIEW;
/*
// EXEC ASSEMBLY
         TITLE 'GET SYSTEM LINE COUNT FOR PL/I'
GETLC    CSECT
R1       EQU   1
R2       EQU   2
R9       EQU   9
R15      EQU   15
         USING *,R15
         SAVE  (14,12)          SAVE PL/I'S REGISTERS
         L     R9,0(R1)         GET ADDRESS OF PL/I'S PASSED FIELD
         COMRG
         SR    R2,R2
         IC    R2,78(R1)        GET SYSTEM LINE COUNT
         CVD   R2,DOUBLE
         ZAP   0(2,R9),DOUBLE      AND MOVE TO PL/I
         RETURN (14,12)         RESTORE PL/I'S REGISTERS AND RETURN
DOUBLE   DS    D
         END
/*
// EXEC LNKEDT
// ASSGN SYS007,X'00C'
// ASSGN SYS009,X'00E'
// EXEC
10004ACHER, WILLIAM C.   1100000006752410043679874321120000034
40027ALHOUER, ELAINE E.  1100000002206600066381469783079500022
/*
/&
