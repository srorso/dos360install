* $$JOB DEMOISAC,,,BG
// JOB DEMOISAC DEMONSTRATE ISAM ASSEMBLER CREATE
// OPTION LINK
// EXEC ASSEMBLY
         TITLE 'ISAM DEMONSTRATION-CREATE'
* THIS PROGRAM GIVES AN EXAMPLE OF USAGE OF THE INDEXED-
* SEQUENTIAL ACCESS METHOD.  IT BUILDS A FILE FROM A SEQUENTIAL FILE.
* IT ALSO SHOWS AN EXAMPLE OF A SELF-RELOCATING PROGRAM USING
* REGISTER NOTATION IN THE MACROS AND USES THE SYSTEM LINE COUNTER
* FOR PAGE OVERFLOW.
*        PRINT NOGEN
DEMOISAC CSECT
R0       EQU   0                   WORK REGISTER                        C1R10400
R1       EQU   1                   WORK, ALSO POINTER TO I/O CCB        C1R10500
R2       EQU   2                   LINKAGE FOR INTERNAL SUBROUTINES     C1R10600
R3       EQU   3                   CYLINDER COUNT REMAINING             C1R10700
R4       EQU   4                   CURRENT CYLINDER NR                  C1R10800
R5       EQU   5                   TRACK COUNT REMAINING                C1R10900
R6       EQU   6                   CURRENT TRACK NR                     C1R11000
R7       EQU   7                   RECORD COUNT REMAINING               C1R11100
R8       EQU   8                   CURRENT RECORD NR                    C1R11200
R9       EQU   9                                                        C1R11300
R10      EQU   10                                                       C1R11400
R11      EQU   11                  PROGRAM BASE REGISTER 1              C1R11500
R12      EQU   12                  PROGRAM BASE REGISTER 2              C1R11600
R13      EQU   13                                                       C1R11700
R14      EQU   14                  WORK REGISTER                        C1R11800
R15      EQU   15                  WORK REGISTER                        C1R11900
         BALR  R12,0
         USING *,R12
         USING PRDSECT,R10
         OPENR READER,ISFILE,PRINTER   OPEN FILES
         MVI   PRTLINE,C' '        CLEAR PRINT LINE
         MVC   PRTLINE+1(132),PRTLINE
         COMRG
         MVC   TODAY,0(R1)         GET SYSTEM DATE
         SR    R2,R2                 AND SYSTEM LINE COUNTER
         IC    R2,78(R1)
         CVD   R2,DOUBLE
         ZAP   SYSLC,DOUBLE
         BAL   R3,HEADING          PRINT FIRST PAGE HEADING
         LA    R0,ISFILE           FORMAT EXTENTS FOR ISAM
         SETFL (0)
         TM    30(R1),X'FF'        IF ANY ERRORS,
         BNZ   CANCEL                CANCEL
*
MAINLOOP LA    R1,READER           READ A CARD
         GET   (1)
         CLI   EOPSW,C'1'          IF BOTTOM OF PAGE,
         BNE   *+8
         BAL   R3,HEADING            GO TO NEXT PAGE
         MVC   PRTLINE+5(3),CEMPNO
         MVC   PRTLINE+11(L'ISREC),CARDIN
         AP    LINCT,=P'1'
         BAL   R2,PRINT
         MVC   ISREC,CARDIN        MOVE DATA TO RECORD
         LA    R1,ISFILE           WRITE RECORD
         WRITE (1),NEWKEY
         TM    30(R1),X'FF'        IF ANY ERRORS,
         BNZ   CANCEL                CANCEL
         B     MAINLOOP
*
ENDOFJOB LA    R1,ISFILE           WRITE LAST BLOCK, EOF, ETC.
         ENDFL (1)
         TM    30(R1),X'FF'        IF ANY ERRORS,
         BNZ   CANCEL                CANCEL
         CLOSER READER,ISFILE,PRINTER  CLOSE FILES
         EOJ
CANCEL   UNPK  MERR1C(3),30(2,R1)  MAKE ERRORS PRINTABLE
         TR    MERR1C(2),TRTABLE-X'F0'
         MVC   PRTLINE+10(L'MERR1-1),MERR1
         BAL   R2,PRINT            PRINT ISAM STATUS BYTE
         CANCEL
*
HEADING  MVC   PRTLINE+1(L'TODAY),TODAY
         MVC   PRTLINE+30(L'MHEAD1),MHEAD1
         AP    PAGENO,=P'1'
         MVC   PRTLINE+111(4),=C'PAGE'
         MVC   PRTLINE+115(6),=X'402020202120'
         ED    PRTLINE+115(6),PAGENO
         MVI   PRTLINE,C'1'        SKIP TO NEW PAGE
         BAL   R2,PRINT
         MVI   PRTLINE,C'0'        DOUBLE SPACE NEXT TIME
         ZAP   LINCT,=P'2'         RESET LINE COUNTER
         MVI   EOPSW,C'0'          RESET END-OF-PAGE SWITCH
         BR    R3
PRINT    LA    R1,PRINTER
         PUT   (1)
         CP    LINCT,SYSLC         CHECK FOR BOTTOM OF PAGE
         BL    *+8
         MVI   EOPSW,C'1'          INDICATE BOTTOM OF PAGE
         MVI   PRTLINE,C' '        CLEAR PRINT LINE
         MVC   PRTLINE+1(132),PRTLINE
         BR    R2
TRTABLE  DC    C'0123456789ABCDEF'
MERR1    DC    C'ISAM ERROR-XX '
MERR1C   EQU   MERR1+L'MERR1-3
MHEAD1   DC    C'ISAM DEMONSTRATION-CREATE'
PAGENO   DC    PL3'0'
LINCT    DC    PL2'0'
SYSLC    DS    PL2
DOUBLE   DS    D
TODAY    DS    CL8
EOPSW    DS    C
CARDIN   DS    0CL80
         DS    CL2
CEMPNO   DS    CL3
         DS    CL75
ISREC    DS    CL75
PRDSECT  DSECT
PRTLINE  DS    CL133
DEMOISAC CSECT
         LTORG
READER   DTFDI DEVADDR=SYSIPT,EOFADDR=ENDOFJOB,                        X
               IOAREA1=CARDIN,RECSIZE=80
ISFILE   DTFIS DSKXTNT=3,IOROUT=LOAD,CYLOFL=2,VERIFY=YES,              X
               DEVICE=2314,HINDEX=2314,                                X
               RECFORM=FIXBLK,RECSIZE=75,NRECDS=4,                     X
               KEYLOC=3,KEYLEN=3,                                      X
               IOAREAL=ISAMIOL1,WORKL=ISREC
PRINTER  DTFPR DEVADDR=SYSLST,BLKSIZE=133,CTLCHR=ASA,                  X
               IOAREA1=PRNTOUT1,IOAREA2=PRNTOUT2,IOREG=(10)
ISAMIOL1 DS    CL311
PRNTOUT1 DS    CL133
PRNTOUT2 DS    CL133
         END
/*
// LBLTYP NSD(3)
// EXEC LNKEDT
// ASSGN SYS004,X'192'
// DLBL ISFILE,'DEMO.ISAM.FILE',0,ISC
// EXTENT SYS004,WRK14A,4,1,600,10
// EXTENT SYS004,WRK14A,1,2,620,20
// EXTENT SYS004,WRK14A,2,3,610,10
// EXEC
10004ACHER, WILLIAM C.          00675241004            3679874321120000034
40027ALHOUER, ELAINE E.         00220660006            6381469783079500022
30030ALLOREN, RUTH W.           00000000002            7647982367130000055
50040ATKINSON, CHARLES          00675241004            3679874321120000034
50060BASEL, DEBORAH L           00900191003            1043781234130000040
50105BETTINARDI, RONALD J       01500500003            1125666601110000022
20111CARTOLER, VIOLET B.        00750060004            3455667381140000032
40171COSTA, NAN S.              00560000005            1241963182122000046
30181DELBERT, EDWARD D.         01305541015            6419182773110000051
10185DONNEMAN, THOMAS M.        00900191003            1043781234130000040
10300FELDMAN, MIKE R.           00300000004            2156278643115000026
20304FROMM, STEVE V.            01200005002            2300267832122500037
40317HANBEE, ALETTA O.          00395000008            1136719674139000050
30318HANEY, CAROL S.            01450005008            5533266791100000058
10325HATFIELD, MARK I.          00205390002            2225723628090000030
30487KING, MILDRED J.           01804290010            8711322487090500033
20590NEIL, CLARENCE N.          00950230001            4016787112135000040
40721RASSMUSEN, JOHN J.         01000000004            2064397865129600040
10730REEDE, OWEN W.             01051440001            2115897234105000021
20801SCHEIBER, HARRY T.         00325080002            6198842236112500046
30834TRAWLEY, HARRIS T.         00550000009            7623561471100250032
20956WANGLEY, THEO. A.          00150000003            1723456124120000050
10960WINGLAND, KEITH E.         00350000003            4215679672085000026
/*
/&
// JOB DEMOISAS DEMONSTRATE ISAM ASSEMBLER SEQ. RETRIEVE
// OPTION LINK,NOXREF
// EXEC ASSEMBLY
         TITLE 'ISAM DEMONSTRATION-SEQUENTIAL RETRIEVAL'
*         PRINT NOGEN
DEMOISAS CSECT
R0       EQU   0                   WORK REGISTER                        C1R10400
R1       EQU   1                   WORK, ALSO POINTER TO I/O CCB        C1R10500
R2       EQU   2                   LINKAGE FOR INTERNAL SUBROUTINES     C1R10600
R3       EQU   3                   CYLINDER COUNT REMAINING             C1R10700
R4       EQU   4                   CURRENT CYLINDER NR                  C1R10800
R5       EQU   5                   TRACK COUNT REMAINING                C1R10900
R6       EQU   6                   CURRENT TRACK NR                     C1R11000
R7       EQU   7                   RECORD COUNT REMAINING               C1R11100
R8       EQU   8                   CURRENT RECORD NR                    C1R11200
R9       EQU   9                                                        C1R11300
R10      EQU   10                                                       C1R11400
R11      EQU   11                  PROGRAM BASE REGISTER 1              C1R11500
R12      EQU   12                  PROGRAM BASE REGISTER 2              C1R11600
R13      EQU   13                                                       C1R11700
R14      EQU   14                  WORK REGISTER                        C1R11800
R15      EQU   15                  WORK REGISTER                        C1R11900
         BALR  R12,0
         USING *,R12
         USING PRDSECT,R10
         OPENR ISFILE,PRINTER
         MVI   PRTLINE,C' '        CLEAR PRINT LINE
         MVC   PRTLINE+1(132),PRTLINE
         COMRG
         MVC   TODAY,0(R1)         GET SYSTEM DATE
         SR    R2,R2                 AND SYSTEM LINE COUNTER
         IC    R2,78(R1)
         CVD   R2,DOUBLE
         ZAP   SYSLC,DOUBLE
         BAL   R3,HEADING          PRINT FIRST PAGE HEADING
         LA    R1,ISFILE
         SETL  (1),BOF             START AT BEGINNING OF FILE
         TM    30(R1),X'FE'        IF ANY ERRORS,
         BNZ   CANCEL                CANCEL
*
MAINLOOP LA    R1,ISFILE           READ A RECORD
         LA    R0,ISREC
         GET   (1),(0)
         TM    30(R1),X'DE'        IF ANY ERRORS,
         BNZ   CANCEL                CANCEL
         TM    30(R1),X'20'        IS IT END OF FILE
         BO    ENDOFJOB
         CLI   EOPSW,C'1'          IF BOTTOM OF PAGE,
         BNE   *+8
         BAL   R3,HEADING            GO TO NEXT PAGE
         MVC   PRTLINE+5(3),ISREC+2
         MVC   PRTLINE+11(L'ISREC),ISREC
         AP    LINCT,=P'1'
         BAL   R2,PRINT            PRINT RECORD
         B     MAINLOOP
*
ENDOFJOB LA    R1,ISFILE           END SEQUENTIAL MODE
         ESETL (1)
         TM    30(R1),X'FE'        IF ANY ERRORS,
         BNZ   CANCEL                CANCEL
         CLOSER ISFILE,PRINTER
         EOJ
CANCEL   UNPK  MERR1C(3),30(2,R1)  MAKE ERRORS PRINTABLE
         MVI   MERR1C+2,C' '
         TR    MERR1C(2),TRTABLE-X'F0'
         MVC   PRTLINE+10(L'MERR1),MERR1
         BAL   R2,PRINT            PRINT ISAM STATUS BYTE
         CANCEL
*
HEADING  MVC   PRTLINE+1(L'TODAY),TODAY
         MVC   PRTLINE+30(L'MHEAD1),MHEAD1
         AP    PAGENO,=P'1'
         MVC   PRTLINE+111(4),=C'PAGE'
         MVC   PRTLINE+115(6),=X'402020202120'
         ED    PRTLINE+115(6),PAGENO
         MVI   PRTLINE,C'1'        SKIP TO NEW PAGE
         BAL   R2,PRINT
         MVI   PRTLINE,C'0'        DOUBLE SPACE NEXT TIME
         ZAP   LINCT,=P'2'         RESET LINE COUNTER
         MVI   EOPSW,C'0'          RESET BOTTOM-OF-PAGE SWITCH
         BR    R3
PRINT    LA    R1,PRINTER
         PUT   (1)
         CP    LINCT,SYSLC         CHECK FOR BOTTOM OF PAGE
         BL    *+8
         MVI   EOPSW,C'1'          INDICATE BOTTOM OF PAGE
         MVI   PRTLINE,C' '        CLEAR PRINT LINE
         MVC   PRTLINE+1(132),PRTLINE
         BR    R2
TRTABLE  DC    C'0123456789ABCDEF'
MERR1    DC    C'ISAM ERROR-XX '
MERR1C   EQU   MERR1+L'MERR1-3
MHEAD1   DC    C'ISAM DEMONSTRATION-SEQUENTIAL RETRIEVAL'
PAGENO   DC    PL3'0'
LINCT    DC    PL2'0'
SYSLC    DS    PL2
DOUBLE   DS    D
TODAY    DS    CL8
EOPSW    DS    C
ISKEY    DS    CL3
ISREC    DS    CL75
PRDSECT  DSECT
PRTLINE  DS    CL133
DEMOISAS CSECT
         LTORG
ISFILE   DTFIS DSKXTNT=3,IOROUT=RETRVE,TYPEFLE=SEQNTL,                 X
               DEVICE=2314,HINDEX=2314,                                X
               RECFORM=FIXBLK,RECSIZE=75,NRECDS=4,                     X
               KEYLOC=3,KEYLEN=3,KEYARG=ISKEY,                         X
               IOAREAS=ISAMIOS1,WORKS=YES
PRINTER  DTFPR DEVADDR=SYSLST,BLKSIZE=133,CTLCHR=ASA,                  X
               IOAREA1=PRNTOUT1,IOAREA2=PRNTOUT2,IOREG=(10)
ISAMIOS1 DS    CL300
PRNTOUT1 DS    CL133
PRNTOUT2 DS    CL133
         END
/*
// LBLTYP NSD(3)
// EXEC LNKEDT
// ASSGN SYS004,X'192'
// DLBL ISFILE,'DEMO.ISAM.FILE',0,ISC
// EXTENT SYS004,WRK14A,4,1,600,10
// EXTENT SYS004,WRK14A,1,2,620,20
// EXTENT SYS004,WRK14A,2,3,610,10
// EXEC
/&
// JOB DEMOISAR DEMONSTRATE ISAM ASSEMBLER RANDOM RETRIEVAL
// OPTION LINK
// EXEC ASSEMBLY
         TITLE 'ISAM DEMONSTRATION-RANDOM RETRIEVAL'
*        PRINT NOGEN
DEMOISAR CSECT
R0       EQU   0                   WORK REGISTER                        C1R10400
R1       EQU   1                   WORK, ALSO POINTER TO I/O CCB        C1R10500
R2       EQU   2                   LINKAGE FOR INTERNAL SUBROUTINES     C1R10600
R3       EQU   3                   CYLINDER COUNT REMAINING             C1R10700
R4       EQU   4                   CURRENT CYLINDER NR                  C1R10800
R5       EQU   5                   TRACK COUNT REMAINING                C1R10900
R6       EQU   6                   CURRENT TRACK NR                     C1R11000
R7       EQU   7                   RECORD COUNT REMAINING               C1R11100
R8       EQU   8                   CURRENT RECORD NR                    C1R11200
R9       EQU   9                                                        C1R11300
R10      EQU   10                                                       C1R11400
R11      EQU   11                  PROGRAM BASE REGISTER 1              C1R11500
R12      EQU   12                  PROGRAM BASE REGISTER 2              C1R11600
R13      EQU   13                                                       C1R11700
R14      EQU   14                  WORK REGISTER                        C1R11800
R15      EQU   15                  WORK REGISTER                        C1R11900
         BALR  R12,0
         USING *,R12
         USING PRDSECT,R10
         OPENR READER,ISFILE,PRINTER
         MVI   PRTLINE,C' '        CLEAR PRINT LINE
         MVC   PRTLINE+1(132),PRTLINE
         COMRG
         MVC   TODAY,0(R1)         GET SYSTEM DATE
         SR    R2,R2                 AND SYSTEM LINE COUNTER
         IC    R2,78(R1)
         CVD   R2,DOUBLE
         ZAP   SYSLC,DOUBLE
         BAL   R3,HEADING          PRINT FIRST PAGE HEADING
*
MAINLOOP LA    R1,READER           READ A RECORD
         GET   (1)
         CLI   EOPSW,C'1'          IF BOTTOM OF PAGE,
         BNE   *+8
         BAL   R3,HEADING            GO TO NEXT PAGE
         MVC   ISKEY,CEMPNO        SET KEY FIELD FOR SEARCH
         LA    R1,ISFILE
         READ  (1),KEY             READ RECORD
         WAITF (1)
         TM    30(R1),X'EE'        IF ANY ERRORS,
         BNZ   CANCEL                CANCEL
         TM    30(R1),X'10'        WAS RECORD FOUND
         BO    NORECFND
FOUND    MVC   PRTLINE+5(3),CEMPNO
         MVC   PRTLINE+11(L'ISREC),ISREC
         AP    LINCT,=P'1'
         BAL   R2,PRINT
         B     MAINLOOP
NORECFND MVC   PRTLINE+5(3),ISKEY
         MVC   PRTLINE+20(9),=C'NOT FOUND'
         AP    LINCT,=P'1'
         BAL   R2,PRINT
         B     MAINLOOP
*
ENDOFJOB CLOSER READER,ISFILE,PRINTER
         EOJ
CANCEL   UNPK  MERR1C(3),30(2,R1)  MAKE ERRORS PRINTABLE
         MVI   MERR1C+2,C' '
         TR    MERR1C(2),TRTABLE-X'F0'
         MVC   PRTLINE+10(L'MERR1),MERR1
         BAL   R2,PRINT            PRINT ISAM STATUS BYTE
         CANCEL
*
HEADING  MVC   PRTLINE+1(L'TODAY),TODAY
         MVC   PRTLINE+30(L'MHEAD1),MHEAD1
         AP    PAGENO,=P'1'
         MVC   PRTLINE+111(4),=C'PAGE'
         MVC   PRTLINE+115(6),=X'402020202120'
         ED    PRTLINE+115(6),PAGENO
         MVI   PRTLINE,C'1'        SKIP TO NEW PAGE
         BAL   R2,PRINT
         MVI   PRTLINE,C'0'        DOUBLE SPACE NEXT TIME
         ZAP   LINCT,=P'2'         RESET LINE COUNTER
         MVI   EOPSW,C'0'          RESET BOTTOM-OF-PAGE SWITCH
         BR    R3
PRINT    LA    R1,PRINTER
         PUT   (1)
         CP    LINCT,SYSLC         CHECK FOR BOTTOM OF PAGE
         BL    *+8
         MVI   EOPSW,C'1'          INDICATE BOTTOM OF PAGE
         MVI   PRTLINE,C' '        CLEAR PRINT LINE
         MVC   PRTLINE+1(132),PRTLINE
         BR    R2
TRTABLE  DC    C'0123456789ABCDEF'
MERR1    DC    C'ISAM ERROR-XX '
MERR1C   EQU   MERR1+L'MERR1-3
MHEAD1   DC    C'ISAM DEMONSTRATION-RANDOM RETRIEVAL'
PAGENO   DC    PL3'0'
LINCT    DC    PL2'0'
SYSLC    DS    PL2
DOUBLE   DS    D
TODAY    DS    CL8
EOPSW    DS    C
CARDIN   DS    0CL80
CEMPNO   DS    CL3
         DS    CL77
ISKEY    DS    CL3
ISREC    DS    CL75
PRDSECT  DSECT
PRTLINE  DS    CL133
DEMOISAR CSECT
         LTORG
READER   DTFDI DEVADDR=SYSIPT,EOFADDR=ENDOFJOB,                        X
               IOAREA1=CARDIN,RECSIZE=80
ISFILE   DTFIS DSKXTNT=3,IOROUT=RETRVE,TYPEFLE=RANDOM,                 X
               DEVICE=2314,HINDEX=2314,                                X
               RECFORM=FIXBLK,RECSIZE=75,NRECDS=4,                     X
               KEYLOC=3,KEYLEN=3,KEYARG=ISKEY,                         X
               IOAREAR=ISAMIOR,WORKR=ISREC,                            X
               INDAREA=ISAMIDX,INDSIZE=45
* CYL.IDX. IN CORE IS (CYL+1+3)*(KL+6)  CYL=1,KL=3
PRINTER  DTFPR DEVADDR=SYSLST,BLKSIZE=133,CTLCHR=ASA,                  X
               IOAREA1=PRNTOUT1,IOAREA2=PRNTOUT2,IOREG=(10)
ISAMIOR  DS    CL300
ISAMIDX  DS    CL45
PRNTOUT1 DS    CL133
PRNTOUT2 DS    CL133
         END
/*
// LBLTYP NSD(3)
// EXEC LNKEDT
// ASSGN SYS004,X'192'
// DLBL ISFILE,'DEMO.ISAM.FILE',0,ISC
// EXTENT SYS004,WRK14A,4,1,600,10
// EXTENT SYS004,WRK14A,1,2,620,20
// EXTENT SYS004,WRK14A,2,3,610,10
// EXEC
304
030
031
105
960
060
/*
/&
// JOB DEMOISRR DEMONSTRATE ISAM RPG RANDOM RETRIEVE
// OPTION LINK
// EXEC RPG
     H                                                                    ISAMRR
     FREADER  IPE F  80  80    2      EREAD01 SYSIPT
     FISFILE  IC  F 300  75R 3KI     3 DISK14 SYS000S
     FREPORT  O   V 132 132     OF    LPRINTERSYSLST
     EAAC1READER  ISFILE
     LREPORT  001010070201303019040250503106037070430804609049100531105712
     IREADER  AA  01
     I                                        1   3 CEMPNO  C1
     IISFILE  AA  02 
     I                                        1  75 ISREC
     C                     SETOF                     H0
     OREPORT  H  2 1   1P
     O       OR        OF
     O                         UDATE      8 ' 0/  /  '
     O                                   33 'ISAM'
     O                                   47 'DEMONSTRATION'
     O                                   55 'RANDOM-'
     O                                   64 'RETRIEVAL'
     O                                  116 'PAGE'
     O                         PAGE  Z  120
     O        H  2     1P
     O       OR        OF
     O                                    7 'KEY'
     O                                   20 'RECORD'
     O        D  1     01
     O                         CEMPNO     5
     O                 02      ISREC     86
     O                N02                15 'NOT FOUND'
/*
// LBLTYP NSD(3)
// EXEC LNKEDT
// ASSGN SYS004,X'192'
// DLBL ISFILE,'DEMO.ISAM.FILE',0,ISC
// EXTENT SYS004,WRK14A,4,1,600,10
// EXTENT SYS004,WRK14A,1,2,620,20
// EXTENT SYS004,WRK14A,2,3,610,10
// EXEC
304
030
031
105
960
060
/*
/&
// JOB DEMOISCR DEMONSTRATE ISAM FCOBOL RANDOM RETRIEVE
// OPTION LINK SYM,LISTX NOXREF
// EXEC FCOBOL
 CBL BUF=1024,SUPMAP,NOTRUNC
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ISAMRR.
       AUTHOR. A PROGRAMMER.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-360.
       OBJECT-COMPUTER. IBM-360.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT READER ASSIGN TO SYS007-UR-2501-S.
           SELECT ISFILE ASSIGN TO SYS000-DA-2314-I-ISFILE
               ACCESS IS RANDOM
               NOMINAL KEY IS WS-KEY
               RECORD KEY IS ISKEY.
           SELECT PRINT-FILE ASSIGN TO SYS009-UR-1403-S.
       I-O-CONTROL.
           APPLY CORE-INDEX TO ISFILE-CORE-INDEX ON ISFILE.
       DATA DIVISION.
       FILE SECTION.
       FD  READER
           LABEL RECORDS ARE OMITTED
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS CARDIN.
       01  CARDIN.
           02  CD-KEY              PIC X(3).
           02  FILLER              PIC X(77).
       FD  ISFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 75 CHARACTERS
           BLOCK CONTAINS 4 RECORDS
           DATA RECORD IS ISREC.
       01  ISREC.
           05  FILLER              PIC XX.
           05  ISKEY               PIC XXX.
           05  FILLER              PIC X(70).
       FD  PRINT-FILE
           LABEL RECORDS ARE OMITTED
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS PRINTOUT.
       01  PRINTOUT.
           02  ASA                 PIC X.
           02  FILLER              PIC X(132).
       01  HEADING1.
           02  FILLER              PIC X.
           02  H1-TODAY            PIC X(8).
           02  FILLER              PIC X(25).
           02  H1-TITLE            PIC X(35).
           02  FILLER              PIC X(44).
           02  H1-PAGE             PIC X(4).
           02  FILLER              PIC X.
           02  H1-PAGENUM          PIC ZZ9.
           02  FILLER              PIC X(12).
       01  DETAIL-LINE-LISTING.
           02  FILLER              PIC X. 
           02  FILLER              PIC X.
           02  DL-KEY              PIC X(3).
           02  FILLER              PIC X(5).
           02  DL-REC              PIC X(75).
           02  FILLER              PIC X(48).
       WORKING-STORAGE SECTION.
       01  FILLER.
           05  WS-KEY              PIC XXX.
           05  WS-TODAY            PIC X(8).
           05  WS-SYSLC    COMP-3  PIC S999.
           05  WS-LC       COMP-3  PIC S999 VALUE ZERO.
           05  WS-PAGENUM  COMP-3  PIC S999 VALUE ZERO.
           05  WS-EOF              PIC X VALUE 'N'.
           05  WS-EOP              PIC X.
           05  WS-NRF              PIC X.
      * CYL.IDX. IN CORE IS (CYL+1+3)*(KL+6)  CYL=1,KL=3
           05  ISFILE-CORE-INDEX   PIC X(45).
       PROCEDURE DIVISION.
           OPEN INPUT READER, ISFILE
                OUTPUT PRINT-FILE.
           MOVE SPACES TO PRINTOUT.
           MOVE CURRENT-DATE TO WS-TODAY
           CALL 'FCOBSLC' USING WS-SYSLC
           PERFORM HEADING-ROUTINE THRU HEADING-ROUTINE-EXIT.
           READ READER AT END MOVE 'Y' TO WS-EOF.
           PERFORM DETAIL-ROUTINE THRU DETAIL-ROUTINE-EXIT
               UNTIL WS-EOF = 'Y'
           CLOSE READER, ISFILE, PRINT-FILE
           STOP RUN.

       DETAIL-ROUTINE.
           IF WS-EOP = 'Y'
               PERFORM HEADING-ROUTINE THRU HEADING-ROUTINE-EXIT.
           MOVE CD-KEY TO WS-KEY
           MOVE 'N' TO WS-NRF
           READ ISFILE
              INVALID KEY MOVE 'Y' TO WS-NRF.
           IF WS-NRF = 'N'
                   MOVE ISKEY TO DL-KEY
                   MOVE ISREC TO DL-REC
           ELSE
               MOVE ISKEY                TO DL-KEY
               MOVE '  RECORD NOT FOUND' TO DL-REC. 
           ADD 1 TO WS-LC
           PERFORM PRINT-LINE THRU PRINT-LINE-EXIT.
           READ READER AT END MOVE 'Y' TO WS-EOF.
       DETAIL-ROUTINE-EXIT.
           EXIT.

       HEADING-ROUTINE.
           MOVE WS-TODAY   TO H1-TODAY
           MOVE 'ISAM DEMONSTRATION-RANDOM RETRIEVAL' TO H1-TITLE
           MOVE 'PAGE'     TO H1-PAGE
           ADD 1 TO WS-PAGENUM
           MOVE WS-PAGENUM TO H1-PAGENUM
           MOVE '1' TO ASA
           PERFORM PRINT-LINE THRU PRINT-LINE-EXIT
           MOVE '0' TO ASA
           MOVE 2 TO WS-LC
           MOVE 'N' TO WS-EOP.
       HEADING-ROUTINE-EXIT.
           EXIT.

       PRINT-LINE.
           WRITE PRINTOUT AFTER POSITIONING ASA.
           IF WS-LC NOT LESS THAN WS-SYSLC
               THEN MOVE 'Y' TO WS-EOP.
           MOVE SPACES TO PRINTOUT.
       PRINT-LINE-EXIT.
           EXIT.
/*
// EXEC ASSEMBLY
         TITLE 'GET SYSTEM LINE COUNT FOR FCOBOL'
         PRINT NOGEN
FCOBSLC  CSECT
R0       EQU   0                   WORK REGISTER                        C1R10400
R1       EQU   1                   WORK, ALSO POINTER TO I/O CCB        C1R10500
R2       EQU   2                   LINKAGE FOR INTERNAL SUBROUTINES     C1R10600
R3       EQU   3                   CYLINDER COUNT REMAINING             C1R10700
R4       EQU   4                   CURRENT CYLINDER NR                  C1R10800
R5       EQU   5                   TRACK COUNT REMAINING                C1R10900
R6       EQU   6                   CURRENT TRACK NR                     C1R11000
R7       EQU   7                   RECORD COUNT REMAINING               C1R11100
R8       EQU   8                   CURRENT RECORD NR                    C1R11200
R9       EQU   9                                                        C1R11300
R10      EQU   10                                                       C1R11400
R11      EQU   11                  PROGRAM BASE REGISTER 1              C1R11500
R12      EQU   12                  PROGRAM BASE REGISTER 2              C1R11600
R13      EQU   13                                                       C1R11700
R14      EQU   14                  WORK REGISTER                        C1R11800
R15      EQU   15                  WORK REGISTER                        C1R11900
         USING *,R12
         SAVE  (14,12)             SAVE COBOL REGISTERS
         LR    R12,R15
         L     R9,0(R1)            GET ADDR. OF COBOL FIELD
         COMRG
         SR    R2,R2               GET SYSTEM LINE COUNT
         IC    R2,78(R1)
         CVD   R2,DOUBLE           CONVERT TO PACKED
         ZAP   0(2,R9),DOUBLE      MOVE LINE COUNT TO COBOL
         RETURN (14,12)            RESTORE COBOL REGISTERS AND RETURN
DOUBLE   DS    D
         END
/*
// LBLTYP NSD(3)
// EXEC LNKEDT
// ASSGN SYS007,X'00C'
// ASSGN SYS009,X'00E'
// ASSGN SYS004,X'192'
// DLBL ISFILE,'DEMO.ISAM.FILE',0,ISC
// EXTENT SYS004,WRK14A,4,1,600,10
// EXTENT SYS004,WRK14A,1,2,620,20
// EXTENT SYS004,WRK14A,2,3,610,10
// EXEC
304
030
031
105
960
060
/*
/&
// JOB DEMOISPR ISAM PL/I RANDOM RETRIEVE
// OPTION LINK SYM,LISTX,DUMP
// EXEC PL/I
* PROCESS STMT
 /* ISAM DEMONSTRATION-RANDOM RETRIEVAL */
 ISAMRR: PROC OPTIONS (MAIN);
         DCL PLISLC ENTRY,
             1 CARDIN,
               5 CDKEY      PIC '(3)X',
               5 FILLER2    PIC '(77)X',
             1 ISREC,
               5 FILLER1    PIC '(75)X',
             READER FILE INPUT RECORD
                  ENV(MEDIUM(SYS007,2501) BUFFERS(2) F(80)),
             ISFILE FILE INPUT RECORD DIRECT KEYED
                  ENV(MEDIUM(SYS000,2314) F(300,75) INDEXED
                      KEYLENGTH(3) KEYLOC(3)
                      EXTENTNUMBER(3) INDEXAREA(45)),
             PRINTR FILE OUTPUT STREAM PRINT
                  ENV(MEDIUM(SYS009,1403) BUFFERS(2) F(133)),
             SYSLC FIXED DEC(3,0),
             LINES FIXED DEC(3,0) STATIC INIT (0),
             PAGE FIXED DEC(5,0) STATIC INIT (0),
             DATE BUILTIN,
             TODAY CHAR(8) STATIC INIT ('  /  /  '),
             WNRF CHAR(1);
         SUBSTR(TODAY,1,2) = SUBSTR(DATE,3,2);
         SUBSTR(TODAY,4,2) = SUBSTR(DATE,5,2);
         SUBSTR(TODAY,7,2) = SUBSTR(DATE,1,2);
         CALL PLISLC(SYSLC);
         OPEN FILE (READER), FILE (ISFILE), FILE (PRINTR);
         ON ENDFILE (READER) GO TO ENDJOB;
         ON KEY (ISFILE) GO TO NRF;
         ON ENDPAGE (PRINTR);
         CALL HEDING;
 MAINLP: READ FILE (READER) INTO (CARDIN);
         WNRF = 'N';
         READ FILE (ISFILE) INTO (ISREC) KEY(CDKEY);
         GO TO READOK;
 NRF:    WNRF = 'Y';
 READOK: IF LINES >= SYSLC THEN CALL HEDING;
         IF WNRF = 'N'
            THEN PUT FILE (PRINTR) SKIP EDIT
                         (CDKEY, ISREC)
                         (X(5), A, X(3), A);
            ELSE PUT FILE (PRINTR) SKIP EDIT
                         (CDKEY, '  RECORD NOT FOUND')
                         (X(1), A, X(12), A);
         LINES = LINES + 1;
         GO TO MAINLP;          

 ENDJOB: CLOSE FILE (READER), FILE (ISFILE), FILE (PRINTR);

 HEDING: PROC;
         PAGE = PAGE + 1;  /* INCREMENT PAGE COUNTER */
         IF PAGE > 1  THEN PUT FILE (PRINTR) PAGE;
         PUT FILE (PRINTR) EDIT
             (TODAY, 'ISAM DEMONSTRATION-RANDOM RETRIEVAL',
                     'PAGE', PAGE)
             (A, X(6), A, X(44), A, F(4,0), SKIP);
         PUT FILE (PRINTR) SKIP;
         LINES = 2;  /* RESET LINE COUNTER */
         RETURN;
 END HEDING;
 END ISAMRR;
/*
// EXEC ASSEMBLY
         TITLE 'GET SYSTEM LINE COUNT FOR PL/I'
         PRINT NOGEN
PLISLC   CSECT
R0       EQU   0                   WORK REGISTER                        C1R10400
R1       EQU   1                   WORK, ALSO POINTER TO I/O CCB        C1R10500
R2       EQU   2                   LINKAGE FOR INTERNAL SUBROUTINES     C1R10600
R3       EQU   3                   CYLINDER COUNT REMAINING             C1R10700
R4       EQU   4                   CURRENT CYLINDER NR                  C1R10800
R5       EQU   5                   TRACK COUNT REMAINING                C1R10900
R6       EQU   6                   CURRENT TRACK NR                     C1R11000
R7       EQU   7                   RECORD COUNT REMAINING               C1R11100
R8       EQU   8                   CURRENT RECORD NR                    C1R11200
R9       EQU   9                                                        C1R11300
R10      EQU   10                                                       C1R11400
R11      EQU   11                  PROGRAM BASE REGISTER 1              C1R11500
R12      EQU   12                  PROGRAM BASE REGISTER 2              C1R11600
R13      EQU   13                                                       C1R11700
R14      EQU   14                  WORK REGISTER                        C1R11800
R15      EQU   15                  WORK REGISTER                        C1R11900
         USING *,R12
         SAVE  (14,12)             SAVE PL/I REGISTERS
         LR    R12,R15
         L     R9,0(R1)            GET ADDR. OF PL/I FIELD
         COMRG
         SR    R2,R2               GET SYSTEM LINE COUNT
         IC    R2,78(R1)
         CVD   R2,DOUBLE           CONVERT TO PACKED
         ZAP   0(2,R9),DOUBLE      MOVE LINE COUNT TO PL/I
         RETURN (14,12)            RESTORE PL/I REGISTERS AND RETURN
DOUBLE   DS    D
         END
/*
// LBLTYP NSD(3)
// EXEC LNKEDT
// ASSGN SYS007,X'00C'
// ASSGN SYS009,X'00E'
// ASSGN SYS004,X'192'
// DLBL ISFILE,'DEMO.ISAM.FILE',0,ISC
// EXTENT SYS004,WRK14A,4,1,600,10
// EXTENT SYS004,WRK14A,1,2,620,20
// EXTENT SYS004,WRK14A,2,3,610,10
// EXEC
304
030
031
105
960
060
/*
/&
* $$EOJ