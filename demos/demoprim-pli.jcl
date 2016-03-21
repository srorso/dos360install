// JOB DEMOPRIM PRIME NUMBERS
// OPTION LINK,SYM,LISTX
 PHASE PRIMES,S
// EXEC PL/I
* PROCESS STMT (PRINT STATEMENT NUMBER IF ABEND)
 /* SHOW PL/I ADVANCED FEATURES */
 /* ADAPTED FOR PL/I FROM Z.FO1 */
 PRIMES: PROC OPTIONS (MAIN);
         DCL
            (PLISLC, PLIUPS) ENTRY,
             PRINTR FILE OUTPUT STREAM PRINT
                  ENV(MEDIUM(SYS009,1403) BUFFERS(2) F(121)),
             SYSLC FIXED DEC(3,0),
             LINES FIXED DEC(3,0),
             PAGE FIXED DEC(5,0) STATIC INIT (0),
             UPSI STATIC BIT(8),
             DATE BUILTIN,
             TODAY CHAR(8) STATIC INIT ('  /  /  '),
            (I,J,K,NPRIME,CTLN) FIXED BIN(31);
         SUBSTR(TODAY,1,2)=SUBSTR(DATE,3,2);
         SUBSTR(TODAY,4,2)=SUBSTR(DATE,5,2);
         SUBSTR(TODAY,7,2)=SUBSTR(DATE,1,2);
         OPEN FILE (PRINTR);
         ON ENDPAGE (PRINTR); /* IGNORE CHANNEL 12 */
         CALL OVERLAY ('PRIMSLC');
         CALL PLISLC (SYSLC);
         CALL OVERLAY ('PRIMUPS');
         CALL PLIUPS (UPSI);
         CALL DYNDUMP (UPSI); /* DISPLAY UPSI IN HEX FORMAT */
 /*      CALL IJKTRON; */
         GET EDIT (NPRIME) (F(4,0)); /* READ CONTROL CARD */
         CALL HEDING;
         PUT FILE (PRINTR) EDIT (1, 2) (2 F(10,0)); 
         CTLN = 2;
         DO I = 3 TO NPRIME BY 2;
            K = SQRT(I);
            DO J = 3 TO K BY 2;
               IF MOD(I,J) = 0 THEN GOTO NEXT;
            END;
            IF LINES > SYSLC THEN CALL HEDING;
            PUT FILE (PRINTR) EDIT (I) (F(10,0));
            CTLN = CTLN + 1;
            IF ^(((UPSI & '10000000'B) = '00000000'B) & (CTLN < 10) 
               THEN DO; PUT FILE (PRINTR) SKIP; /* FORCE NEW LINE */
                        LINES = LINES + 1;
                        CTLN = 0;
                    END;
 NEXT:
         END;
         CLOSE FILE (PRINTR);
         DISPLAY ('JOB IS FINISHED');
 HEDING: PROC;
         DCL HEAD1 CHAR(24) STATIC INIT ('PRIME NUMBERS FROM 1 TO ');
         PAGE=PAGE+1;  /* INCREMENT PAGE COUNTER */
         IF PAGE > 1  THEN PUT FILE (PRINTR) PAGE;
         PUT FILE (PRINTR) EDIT (TODAY, HEAD1, NPRIME, 'PAGE', PAGE)
                  (X(4), A, X(6), A, F(4,0), X(8), A, F(4,0));
         PUT FILE (PRINTR) SKIP;
         LINES=2;  /* RESET LINE COUNTER */
 END HEDING;
 END PRIMES;
/*
 PHASE PRIMSLC,*
 INCLUDE ,(PLISLC)
 PHASE PRIMUPS,PRIMSLC
 INCLUDE ,(PLIUPS)
// EXEC ASSEMBLY
         TITLE 'GET SYSTEM LINE COUNTER FOR PL/I'
         PRINT NOGEN
PLISLC   CSECT
R1       EQU   1
R2       EQU   2
R9       EQU   9
R12      EQU   12
R15      EQU   15
         USING *,R12
         SAVE  (14,12)         SAVE PL/I REGISTERS
         LR    R12,R15
         L     R9,0(R1)
         COMRG
         SR    R2,R2
         IC    R2,78(R1)       GET SYSTEM LINE COUNT,
         CVD   R2,DOUBLE         CONVERT TO PACKED, AND
         ZAP   0(2,R9),DOUBLE    MOVE TO PL/I
         RETURN (14,12)        RESTORE PL/I REGISTERS AND RETURN
DOUBLE   DS    D
         TITLE 'GET UPSI BYTE FOR PL/I'
PLIUPS   CSECT
         USING *,R12
         SAVE  (14,12)         SAVE PL/I REGISTERS
         LR    R12,R15
         L     R9,0(R1)
         COMRG
         MVC   0(1,R9),23(R1)  MOVE UPSI BYTE TO PL/I
         RETURN (14,12)        RESTORE PL/I REGISTERS AND RETURN
         END
/*
// EXEC LNKEDT
// ASSGN SYS009,X'00E'
// UPSI 1        0=1 PER LINE, 1=10 PER LINE
// EXEC
5000
/*
/&
