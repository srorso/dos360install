// JOB DEMOLOAN PRINT LOAN REPAYMENT SCHEDULE
// OPTION LINK,SYM,LISTX
// EXEC FCOBOL
 CBL BUF=1024,SUPMAP,NOTRUNC
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOAN.
       AUTHOR. A PROGRAMMER.
       REMARKS. THIS IS A COBOL PROGRAM TO PRINT A LOAN
                REPAYMENT SCHEDULE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-360.
       OBJECT-COMPUTER. IBM-360.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE  ASSIGN TO SYS007-UR-2501-S.
           SELECT PRINT-FILE ASSIGN TO SYS009-UR-1403-S.
      
       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE
           LABEL RECORDS ARE OMITTED
           RECORDING MODE IS F
           RECORD CONTAINS 47 CHARACTERS
           DATA RECORD IS EMPLOYEE-RECORD.
       01  LN-RECORD.
           02  LN-AMOUNT-X.
               05  LN-AMOUNT       PIC S9(6)V99.
           02  LN-RATE-X.
               05  LN-RATE         PIC S99V99.
           02  LN-TIME-X.
               05  LN-TIME         PIC S9(3).
           02  LN-TITLE            PIC X(32).
       FD  PRINT-FILE
           LABEL RECORDS ARE OMITTED
           RECORDING MODE IS F
           RECORD CONTAINS 81 CHARACTERS
           DATA RECORD IS PRINTOUT.
       01  PRINTOUT.
           02  ASA                 PIC X.
           02  FILLER              PIC X(80).
       01  HEADING1.
           02  FILLER              PIC X.
           02  H1-TODAY            PIC X(8).
           02  FILLER              PIC X(12).
           02  H1-TITLE            PIC X(32).
           02  FILLER              PIC X(20).
           02  H1-PAGE             PIC X(4).
           02  FILLER              PIC X.
           02  H1-PAGENUM          PIC ZZ9.
       01  HEADING2.
           02  FILLER              PIC X.
           02  FILLER              PIC X(20).
           02  H2-L-R-S-F          PIC X(27).
           02  FILLER              PIC X.
           02  H2-AMOUNT           PIC $ZZZ,ZZZ.99-.
           02  FILLER              PIC X(20).
       01  HEADING3.
           02  FILLER              PIC X.
           02  FILLER              PIC X(20).
           02  H3-AT               PIC XX.
           02  H3-RATE             PIC ZZ.99.
           02  H3-P-A-R            PIC X(13).
           02  FILLER              PIC X(40).
       01  HEADING4.
           02  FILLER              PIC X.
           02  FILLER              PIC X(20).
           02  H4-W-M-P-O          PIC X(24).
           02  FILLER              PIC X.
           02  H4-PAYMNT           PIC $ZZ,ZZ9.99-.
           02  FILLER              PIC X(24).
       01  HEADING5.
           02  FILLER              PIC X.
           02  FILLER              PIC X(11).
           02  H5-MONTHLY          PIC X(7).
           02  FILLER              PIC X(7).
           02  H5-INTEREST         PIC X(8).
           02  FILLER              PIC X(6).
           02  H5-PRINCIPAL        PIC X(9).
           02  FILLER              PIC X(10).
           02  H5-BALANCE          PIC X(7).
           02  FILLER              PIC X(15).
       01  HEADING6.
           02  FILLER              PIC X.
           02  H6-MONTH            PIC X(5).
           02  FILLER              PIC X(6).
           02  H6-PAYMENT          PIC X(7).
           02  FILLER              PIC X(21).
           02  H6-REDUCTION        PIC X(9).
           02  FILLER              PIC X(10).
           02  H6-OF-LOAN          PIC X(7).
           02  FILLER              PIC X(15).
       01  ERROR-LINE.
           02  FILLER              PIC X.
           02  EL-NOT-NUMERIC      PIC X(11).
           02  FILLER              PIC X.
           02  EL-RECORD           PIC X(40).
           02  FILLER              PIC X(28).
       01  DETAIL-LINE.
           02  FILLER              PIC X.
           02  FILLER              PIC X.
           02  DL-TIME             PIC ZZ9.
           02  FILLER              PIC X(5).
           02  DL-PAYMNT           PIC ZZ,ZZ9.99-.
           02  FILLER              PIC X(5).
           02  DL-PDINT            PIC ZZ,ZZ9.99-.
           02  FILLER              PIC X(5).
           02  DL-PRIN-RE          PIC ZZ,ZZ9.99-.
           02  FILLER              PIC X(5).
           02  DL-BALANCE          PIC ZZZZ,ZZ9.99-.
           02  FILLER              PIC X(14).
       01  TOTAL-LINE.
           02  FILLER              PIC X.
           02  TL-T-I-P            PIC X(20).
           02  TL-TOT-INT          PIC $Z,ZZZ,ZZ9.99-.
           02  FILLER              PIC X(46).
       WORKING-STORAGE SECTION.
       01  FILLER.
           02  FILLER       COMP-3.
               05  WS-PAGENUM      PIC S999.
               05  WS-TIME         PIC S999.
               05  WS-TOT-INT      PIC S9(7)V99.
               05  WS-RATEM        PIC S9V9(14).
               05  WS-X1           PIC S999V9(14).
               05  WS-PAYMNT       PIC S9(5)V99.
               05  WS-PRIN-RE      PIC S9(5)V99.
               05  WS-BALANCE      PIC S9(7)V99.
               05  WS-PDINT        PIC S9(5)V99.
               05  WS-QUOT         PIC S999.
               05  WS-REMAINDER    PIC S999.
           02  FILLER.
               05  WS-TODAY        PIC X(8).

       PROCEDURE DIVISION.
           OPEN INPUT LOAN-FILE, OUTPUT PRINT-FILE.
           MOVE SPACES TO PRINTOUT.
           MOVE CURRENT-DATE TO WS-TODAY.
       MAIN-LOOP.
           READ LOAN-FILE AT END GO TO END-OF-JOB.
           MOVE ZERO TO WS-PAGENUM, WS-TOT-INT.
           EXAMINE LN-AMOUNT-X REPLACING LEADING SPACES BY ZEROS.
           EXAMINE LN-RATE-X   REPLACING LEADING SPACES BY ZEROS.
           EXAMINE LN-TIME-X   REPLACING LEADING SPACES BY ZEROS.
           PERFORM HEADING1-ROUTINE THRU HEADING1-EXIT.
           IF LN-AMOUNT IS NUMERIC AND
              LN-RATE   IS NUMERIC AND
              LN-TIME   IS NUMERIC
                    PERFORM DATA-IS-GOOD THRU DATA-IS-GOOD-EXIT
               ELSE PERFORM DATA-IS-BAD  THRU DATA-IS-BAD-EXIT.
           GO TO MAIN-LOOP.
       END-OF-JOB.
           CLOSE LOAN-FILE, PRINT-FILE.
           STOP RUN.

       HEADING1-ROUTINE.
           MOVE WS-TODAY TO H1-TODAY
           MOVE LN-TITLE TO H1-TITLE
           MOVE 'PAGE' TO H1-PAGE
           ADD 1 TO WS-PAGENUM
           MOVE WS-PAGENUM TO H1-PAGENUM
           MOVE '1' TO ASA
           PERFORM PRINT-LINE THRU PRINT-LINE-EXIT.
       HEADING1-EXIT.
           EXIT.

       HEADINGR-ROUTINE.
           MOVE 'LOAN REPAYMENT SCHEDULE FOR' TO H2-L-R-S-F
           MOVE LN-AMOUNT TO H2-AMOUNT.
           MOVE ZERO TO ASA
           PERFORM PRINT-LINE THRU PRINT-LINE-EXIT.
           MOVE 'AT' TO H3-AT
           MOVE LN-RATE TO H3-RATE
           MOVE '% ANNUAL RATE' TO H3-P-A-R
           PERFORM PRINT-LINE THRU PRINT-LINE-EXIT.
           MOVE 'WITH MONTHLY PAYMENTS OF' TO H4-W-M-P-O
           MOVE WS-PAYMNT TO H4-PAYMNT
           PERFORM PRINT-LINE THRU PRINT-LINE-EXIT.
           MOVE 'MONTHLY'   TO H5-MONTHLY
           MOVE 'INTEREST'  TO H5-INTEREST
           MOVE 'PRINCIPAL' TO H5-PRINCIPAL
           MOVE 'BALANCE'   TO H5-BALANCE
           MOVE ZERO TO ASA.
           PERFORM PRINT-LINE THRU PRINT-LINE-EXIT.
           MOVE 'MONTH'     TO H6-MONTH
           MOVE 'PAYMENT'   TO H6-PAYMENT
           MOVE 'REDUCTION' TO H6-REDUCTION
           MOVE 'OF LOAN'   TO H6-OF-LOAN
           PERFORM PRINT-LINE THRU PRINT-LINE-EXIT.
           MOVE ZERO TO ASA.
       HEADINGR-EXIT.
           EXIT.

       DATA-IS-GOOD.
           COMPUTE WS-RATEM = LN-RATE / 12 / 100
           COMPUTE WS-X1 = (1 + WS-RATEM) ** (- LN-TIME)
           COMPUTE WS-PAYMNT ROUNDED = LN-AMOUNT * WS-RATEM /
               (1 - WS-X1)
           PERFORM HEADINGR-ROUTINE THRU HEADINGR-EXIT.
           MOVE LN-AMOUNT TO WS-BALANCE.
           PERFORM DETAIL-LOOP THRU DETAIL-LOOP-EXIT
               VARYING WS-TIME FROM 1 BY 1
               UNTIL WS-TIME GREATER THAN LN-TIME.
           MOVE 'TOTAL INTEREST PAID-' TO TL-T-I-P.
           MOVE WS-TOT-INT TO TL-TOT-INT.
           MOVE ZERO TO ASA
           PERFORM PRINT-LINE THRU PRINT-LINE-EXIT.
       DATA-IS-GOOD-EXIT.
           EXIT.
       DATA-IS-BAD.
           MOVE 'NOT NUMERIC' TO EL-NOT-NUMERIC
           MOVE LN-RECORD TO EL-RECORD
           MOVE ZERO TO ASA
           PERFORM PRINT-LINE THRU PRINT-LINE-EXIT.
       DATA-IS-BAD-EXIT.
           EXIT.

       DETAIL-LOOP.
           MULTIPLY WS-BALANCE BY WS-RATEM GIVING WS-PDINT ROUNDED.
           IF WS-TIME = LN-TIME
               ADD WS-BALANCE, WS-PDINT GIVING WS-PAYMNT.
           SUBTRACT WS-PDINT FROM WS-PAYMNT GIVING WS-PRIN-RE.
           SUBTRACT WS-PRIN-RE FROM WS-BALANCE.
           ADD WS-PDINT TO WS-TOT-INT.
           MOVE WS-TIME TO DL-TIME
           MOVE WS-PAYMNT TO DL-PAYMNT
           MOVE WS-PDINT TO DL-PDINT
           MOVE WS-PRIN-RE TO DL-PRIN-RE
           MOVE WS-BALANCE TO DL-BALANCE
           PERFORM PRINT-LINE THRU PRINT-LINE-EXIT.
           DIVIDE 12 INTO WS-TIME GIVING WS-QUOT
               REMAINDER WS-REMAINDER.
           IF WS-REMAINDER = 0 
               PERFORM PRINT-LINE THRU PRINT-LINE-EXIT.
           DIVIDE 48 INTO WS-TIME GIVING WS-QUOT
               REMAINDER WS-REMAINDER.
           IF WS-REMAINDER = 0 
               PERFORM HEADING1-ROUTINE THRU HEADING1-EXIT
               PERFORM HEADINGR-ROUTINE THRU HEADINGR-EXIT.
       DETAIL-LOOP-EXIT.

       PRINT-LINE.
           WRITE PRINTOUT AFTER POSITIONING ASA.
           MOVE SPACES TO PRINTOUT.
       PRINT-LINE-EXIT.
           EXIT.
/*
// EXEC LNKEDT
// ASSGN SYS007,X'00C'
// ASSGN SYS009,X'00E'
// EXEC
 1500000 650 60AUTOMOBILE
/*
/&
