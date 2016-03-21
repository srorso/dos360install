// JOB DEMO3270 SUN LOCATION - SAMPLE DOS 3270 PROGRAM
/* -----Physical IOCS with channel appendage routine-----
/*     The 360 architecture is interrupt driven.  In input and output
/* operations the main processor starts a channel processor and then
/* waits for it to finish.  When the channel processor is done it
/* signals or interrupts the main processor so the resulting interrupt
/* can be processed.  DOS uses a 'channel queue' to keep track of
/* which I/O operations are running on which devices.  It connects a
/* CCB to the device (PUB) with a channel queue entry when I/O starts
/* and removes the channel queue entry when the I/O interrupt is
/* processed.  This works fine until we come to an unsolicitied
/* attention interrupt from a local terminal, originally a 2260
/* and then the 3270 family.  Interrupts from devices without a CCB
/* in the channel queue will be ignored.  So for DOS to process the
/* attention interrupts each terminal needs to have an entry in the
/* channel queue.  If you have 24 terminals, for example, you'll need
/* 24 channel queue entries.  Other non-IBM techniques of the time
/* would modify the supervisor so no special channel queue entries
/* would be needed for local terminals.
/*     Rather than put all the I/O interrupt processing instructions
/* in the supervisor DOS has the option of placing some of the
/* instructions in the application program, especially for unique
/* devices like terminals.  This is a channel appendage routine.
/* It resides in the application program but runs with a key of zero
/* and in supervisor state.  Don't even try and think of the security
/* and reliability ramifications of that.  In those days the computing
/* environment was more easily controlled.
/*     So what we'll do is issue an EXCP with a NOP CCW for each
/* terminal.  This will assign it a channel queue entry.  Normally
/* a NOP would generate a CE,DE interrupt and then remove the channel
/* queue entry.  But we'll intercept the CE,DE and change it to a PCI
/* and then let the supervisor have it which will keep the entry in
/* the channel queue expecting a future interrupt when the operation
/* is finished.  See the discussion on Program Controlled Interruptions
/* in the Principles of Operation manual.  When the terminal operator
/* pushes ENTER an attention interrupt is created.  The device has an
/* entry in the channel queue so our appendage routine will get
/* control.  We'll change the attention interrupt to a CE,DE and let
/* the supervisor deque and finish that previous I/O operation.  We'll
/* then issue a new I/O operation to read the terminal's data.  We
/* just have to make sure the terminal is queued if we are expecting
/* an attention interrupt sometime and it is not queued if we are
/* going to use it in an actual read or write operation.
/*     If it all seems a little complicated it is but this is how IBM
/* decided to handle local terminal operations in the DOS world.
/* While not covering every possibility to make it even more
/* complicated, this example is designed to give you the basics
/* of what is involved.
// OPTION LINK,DUMP
 ACTION CLEAR
 PHASE DEMO3270,S
// EXEC ASSEMBLY
         TITLE 'LOCAL 3270 SAMPLE PROGRAM - THE SUN''S LOCATION'
* THIS PROGRAM WILL CALCULATE AN APPROXIMATE LOCATION FOR THE SUN
*   BASED ON AN INPUTTED DATE AND TIME.
*
* SAMPLE DOS LOCAL 3270 APPLICATION USING PIOCS, UNFORMATTED SCREENS,
*   FLOATING-POINT OPERATIONS, TABLE SEARCH, AND THE FORTRAN
*   SINE/COSINE FUNCTIONS.  IT WILL SUPPORT A MAXIMUM OF TEN
*   TERMINALS (0-9) AS INDICATED BY THE NUMBER OF PROGRAMMER
*   LOGICAL UNITS.
*   TERM. 0 IS CONSIDERED A MASTER TERMINAL WITH ADDITIONAL AUTHORITY.
*      DISABLE1 - PREVENT ANYBODY FROM USING TERMINAL 1
*      ENABLE1 - ALLOW THE USE OF TERMINAL 1 AND NOTIFY USER
*      SPYTERM1 - WATCH WHAT TERMINAL 1 IS DOING
*   PF3 - TO END THE APPLICATION FROM THE MASTER TERMINAL
*   EXTERNAL INTERRUPT - TO END THE APPLICATION FROM THE CONSOLE
*   MAKES USE OF SUPERVISOR CHANNEL APPENDAGE ROUTINE
*
ECLIPTIC CSECT
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9       RELATIVE LINE NUMBER (RLN)
R10      EQU   10
R11      EQU   11      TEMPORARY BASE FOR OC
R12      EQU   12      BASE
R13      EQU   13
R14      EQU   14
R15      EQU   15
FR0      EQU   0
FR2      EQU   2
FR4      EQU   4
FR6      EQU   6
         BALR  R12,0
         USING *,R12
         USING TERMDS,R1
         ST    R12,SAVEBASE
         LA    R1,=CL8'$$BAPNDG' ALLOW APPENDAGE ROUTINES
         SVC   2
         LA    R2,NUMTERMS
         BCTR  R2,0            REDUCE BY ONE 
         STH   R2,MAXRLN         TO GET THE MAX RLN ALLOWED
         BAL   R2,BLANK        CLEAR OUTPUT BUFFER
         LH    R6,MAXRLN
         LA    R6,1(R6)        GET NUMBER OF TERMINALS
         XR    R9,R9           START WITH RLN 0
INITLOOP BAL   R4,WRITINIT     WRITE INITAL MESSAGE ON TERMINAL
         BAL   R2,BLANK
         LA    R9,1(R9)        POINT TO NEXT RLN
         BCT   R6,INITLOOP
         STXIT OC,OCEXIT,OCSAVE    ACCEPT EXTERNAL INTERRUPTS
*
READATA  MVI   INPDATA,C' '    CLEAR INPUT BUFFER
         MVC   INPDATA+1(39),INPDATA
         BAL   R4,READ         WAIT FOR ENTER AND READ TERMINAL
         BAL   R2,BLANK        CLEAR OUTPUT BUFFER
         OC    INPDATA(7),BLANKS  TRAN. LC TO UC
         LTR   R9,R9           IS IT MASTER TERMINAL
         BNZ   IGNMAST
         CLI   AIDCA,C'3'      PF3 TO END APPLICATION
         BE    ENDOFJOB
         CLC   INPDATA(7),=C'SPYTERM'
         BE    SPYTERM
         CLC   INPDATA(7),=C'DISABLE'
         BE    DEACTIVE
         CLC   INPDATA(6),=C'ENABLE'
         BE    REACTIVE
IGNMAST  LA    R5,MONTHS           VALIDATE MONTH
         LA    R6,12
         USING MONTHTAB,R5
LOOP     CLC   INPDATA(3),MONTH    IS THIS THE MONTH
         BE    FOUND
         LA    R5,L'MONTHS(R5)     TRY NEXT MONTH
         BCT   R6,LOOP             LOOP FOR 12 MONTHS
BADATA   EQU   *
         MVC   LINE2(6),INPDATA
         MVI   LINE2+3,C' '
         MVC   LINE2+10(4),INPDATA+10
         MVC   LINE3(L'INVALID),INVALID
         OC    LINE2(14),BLANKS
         BAL   R4,WRITINIT         WRITE ERROR MESSAGE
         B     READATA
FOUND    CLC   INPDATA+4(2),DAYS   VALIDATE MAX DAYS IN MONTH
         BH    BADATA
         CLC   INPDATA+4(2),=C'01'
         BL    BADATA
         CLI   INPDATA+5,C'0'
         BL    BADATA
         CLC   INPDATA+10(4),=C'2359' VALIDATE TIME
         BH    BADATA
         CLI   INPDATA+10,C'0'
         BL    BADATA
         CLI   INPDATA+11,C'0'
         BL    BADATA
         CLI   INPDATA+12,C'0'
         BL    BADATA
         CLI   INPDATA+12,C'5'
         BH    BADATA
         CLI   INPDATA+13,C'0'
         BL    BADATA
         PACK  DOUBLE,YTD          CALC DAYS SINCE SPRING EQUINOX
         DROP  R5
         PACK  PACKWORK,INPDATA+4(2)
         AP    DOUBLE,PACKWORK
         CP    DOUBLE,=P'366'      IF NECESSARY,
         BL    *+10
         SP    DOUBLE,=P'366'        ADJUST FOR WRAPAROUND 
         MVC   LINE5(6),INPDATA
         MVI   LINE5+3,C' '
         MVC   LINE6(4),INPDATA+10
         BAL   R2,PKFLT            CONVERT DAYS TO FLT.PT.
         SDR   FR0,FR0             CLEAR LAST HALF OF FR0
         LE    FR0,DOUBLE          LOAD DAYS IN FIRST HALF OF FR0
         ME    FR0,=E'360'         ADJUST TO PCT. OF CIRCLE (DEGREES)
         DE    FR0,=E'366'           FROM PCT. OF YEAR (DAYS)
         LDR   FR6,FR0             SAVE DEGREES SINCE SPRING EQUINOX
         DE    FR0,=E'15'          CONVERT DEGREES TO HOURS (/360*24)
         ME    FR0,=E'60'          CONVERT HOURS TO MINUTES
         STD   FR0,DOUBLE
         LA    R6,LINE5+10         MOVE R.A. (HH:MM) TO OUTPUT BUFFER
         BAL   R2,EDIT
         DE    FR6,=E'180'         CONVERT DEGREES TO RADIANS
         ME    FR6,=E'3.14159'
         STE   FR6,DOUBLE
         BAL   R2,SIN              GET SINE
         ME    FR0,=E'23.445'      ADJUST FOR EARTH'S TILT (AMPLITUDE)
         LDR   FR6,FR0             SAVE DECLINATION
         ME    FR0,=E'60'          CONVERT TO DEG.MIN.
         STD   FR0,DOUBLE
         LA    R6,LINE5+21         MOVE DECL. (DDD:MM) TO OUTPUT BUFFER
         BAL   R2,EDIT
         PACK  DOUBLE,INPDATA+10(2)    GET INPUT HOURS
         MP    DOUBLE,=P'60'           CONVERT TO MINUTES
         PACK  PACKWORK,INPDATA+12(2)
         AP    DOUBLE,PACKWORK         ADD INPUT MINUTES
         BAL   R2,PKFLT                CONVERT TO FLOATING-POINT
         LE    FR0,DOUBLE
         DE    FR0,=E'60'          CONVERT MINUTES TO HOURS
         ME    FR0,=E'15'          CONVERT HOURS TO DEGREES (/24*360)
         AE    FR0,=E'2'       ADJUST FOR DISTANCE FROM HOUR MERIDIAN
         LER   FR4,FR0             SAVE AZIMUTH DEGREES
         ME    FR0,=E'60'          CONVERT TO DEGREE MINUTES
         STD   FR0,DOUBLE
         LA    R6,LINE5+33     MOVE AZIMUTH (DDD:MM) TO OUTPUT BUFFER
         BAL   R2,EDIT
         LER   FR0,FR4             RESTORE AZIMUTH DEGREES
         DE    FR0,=E'180'         CONVERT DEGREES TO RADIANS
         ME    FR0,=E'3.14159'
         STE   FR0,DOUBLE
         BAL   R2,COS              GET COSINE
         LCER  FR0,FR0             TAKE COMPLEMENT
         ME    FR0,=E'48'          90-LATITUDE (AMPLITUDE)
         AER   FR0,FR6             ADD TO DECLINATION
         ME    FR0,=E'60'          CONVERT TO DEGREE MINUTES
         STD   FR0,DOUBLE
         LA    R6,LINE5+44     MOVE ALTITUDE (DD:MM) TO OUTPUT BUFFER
         BAL   R2,EDIT
         MVC   LINE2+12(L'MESS22),MESS22   MOVE INSTRUCTIONS
         MVC   LINE3+10(L'MESS23),MESS23
         MVC   LINE4+12(L'MESS24),MESS24
         BAL   R4,WRITINIT     WRITE RESULTS TO SCREEN
         B     READATA
*
OCEXIT   BALR  R11,0
         USING *,R11
         L     R12,SAVEBASE
         DROP  R11
         BAL   R2,BLANK        CLEAR OUTPUT BUFFER
         MVC   LINE1(L'MESSCLS),MESSCLS    CLOSED BY OPERATOR
         MVC   LINE1+L'MESSCLS+1(L'MESSCLSC),MESSCLSC
         B     CLOSTERM
ENDOFJOB BAL   R2,BLANK        CLEAR OUTPUT BUFFER
         MVC   LINE1(L'MESSCLS),MESSCLS    CLOSED BY MASTER TERM.
         MVC   LINE1+L'MESSCLS+1(L'MESSCLSM),MESSCLSM
CLOSTERM LH    R6,MAXRLN
         LA    R6,1(R6)        GET NUMBER OF TERMINALS
         XR    R9,R9           START AT RLN 0
CLOSLOOP BAL   R4,ERSEWRIT     WRITE CLOSED MESSAGE
         LA    R9,1(R9)        GET NEXT RLN
         BCT   R6,CLOSLOOP
         EOJ
CANCEL   STM   R0,R15,SAVEREGS SAVE REGS. FOR DUMP ANALYSIS
         DUMP
***************
* BASIC FORTRAN IV FUNCTION SIN,COS FROM IJTSSCN
* IF AVAILABLE COULD ALSO ' INCLUDE IJTSSCN' AND
*   LA   R13,REGSAVE  (18F)
*   CALL SIN,(DOUBLE)
*   CALL COS,(DOUBLE)
*  INPUT IS SINGLE PRECISION VALUE AT DOUBLE
*  OUTPUT IS FR0 AS USED IN A FORTRAN FUNCTION
* COS(X)=SIN(PI/2+X)
* SIN(-X)=SIN(PI+X)
COS      MVI   SINCOSC+3,2     FOR COS, OCTANT CRANK IS 2
         B     SINCOS1         GO TO COMMON ROUTINE
SIN      MVI   SINCOSC+3,0     FOR SINE, OCTANT CRANK IS 0 IF +ARG
         TM    DOUBLE,X'80'    IS IT NEGATIVE NUMBER
         BZ    SINCOS1
         MVI   SINCOSC+3,4     FOR SINE OCTANT CRANK IS 4 IF -ARG
SINCOS1  SDR   FR0,FR0         CLEAR FR0
         SDR   FR2,FR2         CLEAR FR2
         LE    FR0,DOUBLE      GET ARGUMENT
         LPER  FR0,FR0         CONSIDER ARGUMENT TO BE POSITIVE
         CE    FR0,SINCOSD     IF GE PI*2**18
         BC    10,CANCEL         NOT ALLOWED
         MD    FR0,SINCOSA     TIMES 4/PI
         CE    FR0,SINCOSH     IF PRODUCT LT ONE
         BL    SINCOS2           BRANCH
         AW    FR0,SINCOSB     GIVE PROD CHAR. OF 46, UNNORMALIZED
         LER   FR2,FR0     INTEGER PART OF PROD TO FR2, UNNORMALIZED
         SDR   FR0,FR2     FRACTION PART OF PROD TO FR0, NORMALIZED
SINCOS2  AU    FR2,SINCOSC     ADD OCTANT CRANK TO FR2, UNNORMALIZED
         STE   FR2,SINCOSI     SAVE IT, LAST 3 BITS ARE MODIF.OCTANT
         TM    SINCOSI+3,1     IF ODD OCTANT, TAKE COMPLEMEMENT
         BZ    SINCOS3           OF FRACTION TO OBTAIN THE MODIFIED
         SE    FR0,SINCOSH       FRACTION R
         LPER  FR0,FR0
SINCOS3  SR    R1,R1           GR1=0 FOR COSINE POLYNOMIAL
         TM    SINCOSI+3,3       FOR OCTANT 2,3,6,7
         BM    *+8             IF OCTANT 1,4,5,8
         LA    R1,4              GR1=4 FOR SINE POLYNOMIAL
         LER   FR4,FR0         SAVE R
         MER   FR0,FR0         COMPUTE SIN OR COS OF MODIFIED
         LER   FR2,FR0           FRACTION USING PROPER CHEBYSHEV
         ME    FR0,SINCOSE(R1)   INTERPOLATION POLYNOMIAL
         AE    FR0,SINCOSF(R1)
         MER   FR0,FR2
         AE    FR0,SINCOSG(R1)
         MER   FR0,FR2
         AE    FR0,SINCOSH(R1) SIN(R)/R OR COS(R) READY
         LTR   R1,R1           IF SINE POLYNOMIAL,
         BZ    *+6
         MER   FR0,FR4           MULTIPLY BY R
         TM    SINCOSI+3,4     IF MODIFIED OCTANT IS IN LOWER PLANE
         BCR   8,R2  BE
         LNER  FR0,FR0           SIGN IS NEGATIVE
         BR    R2
         DC    0D'0'
SINCOSA  DC    X'41145F306DC9C830'     4/PI
SINCOSB  DC    X'4600000000000000'
SINCOSC  DC    X'46000000'
SINCOSD  DC    X'45C90FDB'     MAX ALLOWED-PI*2**18
SINCOSE  DC    X'BE14E5E0'     -0.00031888 C3
         DC    X'BD25B368'     -0.00003595 S3
SINCOSF  DC    X'3F40EBD6'      0.01594991 C2
         DC    X'3EA32F62'      0.00249001 S2
SINCOSG  DC    X'C04EF4E5'     -0.30842425 C1 + FUDGE 1
         DC    X'C014ABBC'     -0.08074543 S1
SINCOSH  DC    E'1'             1.0        C0
         DC    X'40C90FDB'      0.78539816 S0
SINCOSI  DC    F'0'             OCTANT
***************
* CONVERT PACKED TO FLOATING-POINT
*        PKFLT DOUBLE,0,DOUBLE
         CNOP  0,8
PKFLT    CVB   R1,DOUBLE
         LPR   R1,R1           ASSUME POSITIVE
         ST    R1,*+46
         LD    FR0,*+38
         XC    *+38(4),*+38    CLEAR FOR NEXT TIME
         AD    FR0,*+28        NORMALIZE
         ZAP   DOUBLE,DOUBLE   IF PACKED NEGATIVE,
         BNM   *+6
         LNDR  FR0,FR0           MAKE FP NEGATIVE
         DD    FR0,=D'1E0'     NO DECIMAL PLACES
         STD   FR0,DOUBLE      STORE RESULT
         B     *+12
         DC    X'4E00000000000000'
*
         BR    R2
***************
* CONVERT FLOATING-POINT TO PACKED AND EDIT
*        FLTPK DOUBLE,0,DOUBLE
         CNOP  6,8
EDIT     LD    FR0,DOUBLE
         LDR   FR2,FR0         SAVE VALUE FOR SIGN
         MD    FR0,=D'1E0'     NO DECIMAL PLACES
         LPDR  FR0,FR0         ENSURE POSITIVE
         AD    FR0,=D'.5'      ROUND OFF
         AW    FR0,*+34        UNNORMALIZE
         STD   FR0,*+30
         L     R1,*+30         GET INTEGER PORTION
         LTDR  FR2,FR2         IF FP NEGATIVE,
         BNM   *+6
         LNR   R1,R1             MAKE BINARY NEGATIVE
         CVD   R1,DOUBLE
         MVC   *+10(8),=X'4E00000000000000' RESET FOR NEXT TIME
         B     *+12
         DC    X'4E00000000000000'
*
         DP    DOUBLE,=P'60'
         MVO   DOUBLE(7),DOUBLE(6)
         MVO   DOUBLE(7),DOUBLE(6)
         MVC   0(L'EDITWORD,R6),EDITWORD
         ED    0(L'EDITWORD,R6),DOUBLE+5
         BR    R2
***************
BLANK    MVI   BUFFER,C' '
         MVC   BUFFER+1(256),BUFFER
         MVC   BUFFER+257(256),BUFFER+256
         MVC   BUFFER+513(256),BUFFER+512
         MVC   BUFFER+769(190),BUFFER+768
         BR    R2
***************
* MASTER TERMINAL ROUTINES
SPYTERM  EQU   *
         CLI   INPDATA+7,C'1'  VALIDATE TERMINAL NUMBER
         BL    BADRLN
         CLI   INPDATA+7,C'9'
         BH    BADRLN
         PACK  DOUBLE,INPDATA+7(1)
         CVB   R9,DOUBLE
         CH    R9,MAXRLN
         BH    BADRLN
         BAL   R4,READBUFF     READ TERMINAL SCREEN
         XR    R9,R9           WRITE ON MASTER TERMINAL
         BAL   R4,ERSEWRIT
         B     READATA
DEACTIVE EQU   *
         CLI   INPDATA+7,C'1'  VALIDATE TERMINAL NUMBER
         BL    BADRLN
         CLI   INPDATA+7,C'9'
         BH    BADRLN
         PACK  DOUBLE,INPDATA+7(1)
         CVB   R9,DOUBLE
         CH    R9,MAXRLN
         BH    BADRLN
         MVC   LINE1(L'MESSCLS),MESSCLS    CLOSED BY MASTER TERM.
         MVC   LINE1+L'MESSCLS+1(L'MESSCLSM),MESSCLSM
         BAL   R4,ERSEWRIT
         OI    TERMSW,X'40'    MARK AS DISABLED
         XR    R9,R9           POINT TO MASTER TERMINAL
         MVC   LINE3(20),=C'TERMINAL DEACTIVATED'
         B     MASTWR
REACTIVE EQU   *
         CLI   INPDATA+6,C'1'  VALIDATE TERMINAL NUMBER
         BL    BADRLN
         CLI   INPDATA+6,C'9'
         BH    BADRLN
         PACK  DOUBLE,INPDATA+6(1)
         CVB   R9,DOUBLE
         CH    R9,MAXRLN
         BH    BADRLN
         LR    R1,R9
         MH    R1,=Y(TERMLEN)
         LA    R1,TERMS(R1)
         NI    TERMSW,255-X'40'    MARK AS ENABLED
         BAL   R4,WRITINIT     WRITE INITAL MESSAGE
         XR    R9,R9           POINT TO MASTER TERMINAL
         MVC   LINE3(18),=C'TERMINAL ACTIVATED'
         B     MASTWR
BADRLN   MVC   LINE3(19),=C'BAD TERMINAL NUMBER'
MASTWR   BAL   R4,WRITINIT     WRITE RESULTS TO SCREEN
         B     READATA
***************
* TERMINAL I/O SUBROUTINES
WRITINIT EQU   *
         MVC   LINE1(L'MESS11),MESS11  MOVE INTRO. MESSAGES
         MVC   LINE8(L'MESS18),MESS18
         MVC   LINE9(L'MESS19),MESS19
         MVC   LINE10(L'MESS1A),MESS1A
         LTR   R9,R9           IS IT MASTER TERMINAL
         BNZ   *+10
         MVC   LINE12(L'MESS1C),MESS1C
ERSEWRIT EQU   *
         MVI   WCC,X'C3'       SET WRITE CONTROL CHARACTER
         LR    R1,R9           POINT TO CORRECT CCB
         MH    R1,=Y(TERMLEN)
         LA    R1,TERMS(R1)
         LA    R2,OUTPUT       SETUP ERASE/WRITE CCW
         ST    R2,TERMCCW
         MVI   TERMCCW,X'05'
         LA    R2,L'OUTPUT
         STH   R2,TERMCCW+6
         TM    TERMSW,X'80'    IF QUEUED,
         BZ    WRITEIT    
         NI    TERMSW,255-X'80'  RESET QUEUED AND
         SVC   25                DEQUE CHANNEL QUEUE (HIO)
WRITEIT  EXCP  (1)             ERASE/WRITE ON TERMINAL
         WAIT  (1)
         CLC   TERM+4(2),=X'0C00' CE,DE
         BNE   CANCEL
         BR    R4
READBUFF EQU   *
         LR    R1,R9           POINT TO CORRECT CCB
         MH    R1,=Y(TERMLEN)
         LA    R1,TERMS(R1)
         LA    R2,RDBFBUFF     SETUP READ BUFFER CCW
         ST    R2,TERMCCW
         MVI   TERMCCW,X'02'
         LA    R2,L'RDBFBUFF
         STH   R2,TERMCCW+6
         TM    TERMSW,X'80'    IF QUEUED,
         BZ    READIT    
         NI    TERMSW,255-X'80'  RESET QUEUED AND
         SVC   25                DEQUE CHANNEL QUEUE (HIO)
READIT   EXCP  (1)             READ BUFFER
         WAIT  (1)
         CLC   TERM+4(2),=X'0C00'  CE,DE
         BNE   CANCEL
         BR    R4
*
READ     EQU   *
* MAKE SURE ALL ENABLED TERMINALS ARE IN CHANNEL QUEUE
         LA    R3,NUMTERMS
         LA    R1,TERMS
QUELOOP  TM    TERMSW,X'80'        IS IT ALREADY QUEUED
         BO    QUENEXT
         TM    TERMSW,X'40'        IS IT DISABLED
         BO    QUENEXTR
         MVI   TERMCCW,X'03'       SETUP NOP CCW
         LA    R2,1
         STH   R2,TERMCCW+6
         EXCP  (1)                 PUT IN CHANNEL QUEUE
         OI    TERMSW,X'80'        MARK AS QUEUED
QUENEXTR NI    TERM+2,255-X'80'    MAKE SURE WAIT BIT IS OFF
QUENEXT  LA    R1,TERMLEN(R1)      POINT TO NEXT TERMINAL
         BCT   R3,QUELOOP
* WAIT FOR ONE OF THEM TO RESPOND WITH 'ATTN', ADDR. OF CCB IN R1
* IF ONLY ONE TERMINAL YOU COULD USE A SIMPLE 'WAIT' MACRO.
         WAITM TERM1,TERM2
         NI    TERMSW,255-X'80'    MARK AS NOT QUEUED
         LR    R9,R1               CALCULATE RLN FROM CCB ADDR.
         LA    R2,TERMS
         SR    R9,R2
         LA    R2,TERMLEN
         SR    R8,R8
         DR    R8,R2
* READ FROM THAT TERMINAL
         LA    R2,INPUT            SETUP CCW FOR READ
         ST    R2,TERMCCW
         MVI   TERMCCW,X'06'
         MVI   TERMCCW+4,X'20'     SUPPRESS WLR
         LA    R2,L'INPUT
         STH   R2,TERMCCW+6
         EXCP  (1)                 READ FROM TERMINAL
         WAIT  (1)
         CLC   TERM+4(2),=X'0C00'  CE,DE
         BNE   CANCEL
         BR    R4
***************
* CHANNEL APPENDAGE ROUTINE
* - RUNS IN SUPERVISOR STATE, KEY OF ZERO, ALL INTERRUPTS MASKED
APPNDAGE EQU   *
* R1   CCB ADDR
* R2   CHANNEL AND DEVICE NO.
* R3   PUB ADDR
* R4   CHANNEL QUE ENTRY ADDR
* R5   LUBID ENTRY ADDR
* R6   WORK REG
* R7   RETURN TO SUPERVISOR
* R8   BASE FOR APPENDAGE ROUTINE
         USING APPNDAGE,R8
         ST    R6,SAVESUPR         SAVE SUPVR. REGISTER
* IF 'CE,DE' AND 'NOP' LEAVE IN CHANNEL QUEUE WITH 'PCI'
         CLC   X'44'(2),=X'0C00'   IS IT CE,DE
         BE    APCEDE
* IF 'ATTN' CHANGE TO 'CE,DE' AND DEQUE
         TM    X'44',X'80'         IS IT ATTN
         BO    APATTN
         B     APEXIT4
APCEDE   L     R6,8(R1)            GET CCW ADDR.
         CLI   0(R6),X'03'         IS IT NOOP
         BNE   APEXIT4
         MVC   X'44'(2),=X'0080'   CHANGE TO PCI, STAY QUEUED
         B     APEXIT4
APATTN   MVC   X'44'(2),=X'0C00'   CHANGE TO CE,DE, DEQUE
APEXIT4  L     R6,SAVESUPR         RESTORE SUPVR. REGISTER
         B     4(R7)               RETURN TO SUPERVISOR
SAVESUPR DS    F
***************
DOUBLE   DS    D
SAVEBASE DS    F
OCSAVE   DS    18F
MAXRLN   DS    H
PACKWORK DS    PL2
MONTHTAB DSECT
MONTH    DS    CL3     MONTH
DAYS     DS    CL2     MAX DAYS IN MONTH
YTD      DS    CL3     DAYS SINCE 3/21
ECLIPTIC CSECT
MONTHS   DC    C'JAN31285'
         DC    C'FEB29316'
         DC    C'MAR31345'
         DC    C'APR30010'
         DC    C'MAY31040'
         DC    C'JUN30071'
         DC    C'JUL31101'
         DC    C'AUG31132'
         DC    C'SEP30163'
         DC    C'OCT31193'
         DC    C'NOV30224'
         DC    C'DEC31254'
         LTORG
EDITWORD DC    X'402021207A202060'
BLANKS   DC    CL14' '
MESS11   DC    C'XXX 00 AT 0000 HOURS  (24 HOUR CLOCK, STANDARD TIME)'
MESS18   DC    C'TO GET APPROXIMATE LOCATION INFORMATION ABOUT THE SUN -
               FOR A PARTICULAR TIME'
MESS19   DC    C'ENTER THE FIRST THREE LETTERS OF THE MONTH, THE DAY, A-
               ND THE TIME ABOVE'
MESS1A   DC    C'E. G. -- JUL 04 AT 1359'
MESS1C   DC    C'ENTER PF3 TO END APPLICATION'
MESS22   DC    C'RIGHT                   AT CHICAGO, IL'
MESS23   DC    C'ASCENSION DECLINATION   AZIMUTH   ALTITUDE'
MESS24   DC    C'HH:MM      DD:MM      DDD:MM      DD:MM'
INVALID  DC    C'PREVIOUS DATE AND/OR TIME INVALID'
MESSCLS  DC    C'TERMINAL CLOSED BY'
MESSCLSM DC    C'MASTER TERMINAL'
MESSCLSC DC    C'CONSOLE OPERATOR'
*
INPUT    DS    0CL43 INPUT BUFFER
AIDCA    DS    CL3   ATTN.ID, CURSOR ADDRESS
INPDATA  DS    CL40  INPUT DATA
RDBFBUFF DS    0CL963
         DS    CL2
OUTPUT   DS    0CL961  OUTPUT BUFFER
WCC      DS    X       WRITE CONTROL CHAR.
BUFFER   DS    0CL960  OUTPUT DATA (12 LINES)
LINE1    DS    CL80
LINE2    DS    CL80           
LINE3    DS    CL80
LINE4    DS    CL80
LINE5    DS    CL80
LINE6    DS    CL80           
LINE7    DS    CL80
LINE8    DS    CL80
LINE9    DS    CL80
LINE10   DS    CL80           
LINE11   DS    CL80
LINE12   DS    CL80
*
TERMDS   DSECT
TERM     CCB   SYS000,0
TERMCCW  CCW   0,0,0,0
TERMSW   DS    X
* BIT 0 - DEVICE IS QUEUED
* BIT 1 = DEVICE IS DISABLED
         DS    0D
TERMLEN  EQU   *-TERMDS
ECLIPTIC CSECT
         DC    0D'0'
TERMS    EQU   *
TERM1    CCB   SYS020,TERM1CCW
         ORG   TERM1+12
         DC    X'40',AL3(APPNDAGE)
         ORG   ,
TERM1CCW CCW   0,0,0,0
TERM1SW  DC    X'00'
         DC    0D'0'
TERM2    CCB   SYS021,TERM2CCW
         ORG   TERM2+12
         DC    X'40',AL3(APPNDAGE)
         ORG   ,
TERM2CCW CCW   0,0,0,0
TERM2SW  DC    X'00'
         DC    0D'0'
NUMTERMS EQU   (*-TERMS)/TERMLEN
SAVEREGS DS    18F
         END
/*
/* To use a DOS Channel Appendage routine a bit must be turned on
/*   in the Program Information Block (PIB) in the supervisor.
/* To do this we need a key of zero so we need to call a transient
/*   to turn the bit on.
/* A way to avoid needing the transient is to put a DTFBT in the
/*   program, OPEN it, and then forget it.  The OPEN will turn
/*   the bit on.
 PHASE $$BAPNDG,+0
// EXEC ASSEMBLY
$$BDEMO  CSECT
R1       EQU   1
R3       EQU   3
R15      EQU   15   BASE
         USING *,R15
         DC    CL8'$$BAPNDG'
* THIS ASSUMES WE ARE RUNNING AS A MAIN TASK, NOT A SUBTASK
         COMRG
         LH    R3,90(R1)    GET PIB ADDR.
         AH    R3,46(R1)    ADD PIK
         OI    12(R3),X'40' SET APPENDAGE ALLOWED FLAG
         SVC   11
         END
/*
// EXEC LNKEDT
// PAUSE CREATE TWO 3270 TERMINALS
// ASSGN SYS020,X'0A0'
// ASSGN SYS021,X'0A1'
// EXEC
/&
