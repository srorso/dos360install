* $$JOB DEMODAMC,,,BG
// JOB DEMODAMC DEMONSTRATE DIRECT ACCESS METHOD-CREATE
/* Direct Access Method is for direct access to records and is not
/*   designed for sequential access, though it is possible with some
/*   extra effort.  The goal is to retrieve (or update) a record
/*   directly with one SIO instruction.  SAM is designed for
/*   sequential access but not direct (though it is possible with
/*   some extra effort); ISAM can do sequential or direct access
/*   but uses an index structure to find records resulting in
/*   additional overhead and I/O operations.
/* One disadvantage with DAM is you need an algorithm or formula to
/*   convert a key to a specific track (or record ID), then access
/*   the one track.  There are many ways of doing this.  If you had
/*   numeric keys one way is to use modulo arithmetic, i.e. use the
/*   remainder from dividing a key by the number of tracks in the file.
/*   If the keys in the file are randomly distributed then each key
/*   will have an equal chance of being on any given track and all
/*   the tracks will fill up at the same rate.
/* A second disadvantage of DAM is you have to decide what to do when
/*   a track or block fills up.  In our example if a track or block
/*   overflows we'll have to redesign our structure with more tracks
/*   or bigger blocks.
/* A third disadvantage is any blocking of records you have to do
/*   yourself, it's not built into the access method like SAM and ISAM.
/* One advantage, at least in this case, is that the input file does
/*   not have to be in key order as in ISAM in order to create the
/*   DAM file.
/* You could preformat each track with empty records (blocks) with
/*   a clear disk utility or start with a empty track and just add
/*   blocks as needed.  In this example we'll erase each track and
/*   just add blocks as needed.
/* In this example we have a three digit key.  The first two digits
/*   indicate a department and the third indicates an employee in
/*   that department but at present there are no more than four
/*   employees in any department.  So we'll use the department to
/*   calculate a track and all the employees for that department
/*   would be in a single physical record or block.
/* While multiple extents are permitted in DAM (and SAM and ISAM)
/*   this example assumes only one extent.
// OPTION LINK
 PHASE DEMODAMC,+0
// EXEC ASSEMBLY
         TITLE 'DEMONSTRATE DIRECT ACCESS METHOD-CREATE'
* THIS PROGRAM GIVES AN EXAMPLE OF USAGE OF THE DIRECT ACCESS
* METHOD.  IT BUILDS A DAM FILE FROM A SEQUENTIAL FILE.
* IT ALSO SHOWS AN EXAMPLE OF A SELF-RELOCATING PROGRAM USING
* REGISTER NOTATION IN THE MACROS. IT ALSO USES CHANNEL 12
* ON THE CARRIAGE TAPE FOR END OF PAGE.
*        PRINT NOGEN
DEMODAMC CSECT
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         BALR  R12,0
         USING *,R12
         LA    R2,READER           OPEN FILES
         LA    R3,DAFILE
         LA    R4,PRINTER
         OPENR (R2),(R3),(R4)
         COMRG
         MVC   TODAY,0(R1)         GET TODAY'S DATE
         BAL   R3,HEADING          PRINT FIRST PAGE HEADING
* FORMAT THE DISK FILE (ERASE THE TRACKS)
CLERLOOP LA    R1,DAFILE           ERASE TRACK
         WRITE (1),RZERO
         WAITF (1)
         TM    DAERRS,X'FE'        IF ANY ERRORS,
         BNZ   CANCEL                 CANCEL
         TM    DAERRS+1,X'FF'      IF ANY ERRORS,
         BNZ   CANCEL                 CANCEL
         TM    DAERRS,X'01'        IF OUTSIDE OF EXTENT,
         BO    MAINLOOP               DONE ERASING
         L     R2,NUMTRKS          BUMP TRACK COUNT
         LA    R2,1(R2)
         ST    R2,NUMTRKS
         SLL   R2,8                TTTTTT00
         ST    R2,DATRK            SET TRACK NUMBER
         B     CLERLOOP            GO TRY ERASE NEXT TRACK
*
MAINLOOP LA    R1,READER           READ A CARD
         GET   (1)
         CLI   EOPSW,C'1'          IF BOTTOM OF PAGE,
         BNE   *+8
         BAL   R3,HEADING             GO TO NEXT PAGE
         PACK  DOUBLE,CDEPTNO      CALCULATE TRACK NUMBER
         CVB   R7,DOUBLE
         SR    R6,R6
         D     R6,NUMTRKS          REM = R6 = TTTTTTTT
         SLL   R6,8                TTTTTT00
         ST    R6,DATRK            SET TRACK NUMBER
         LA    R1,DAFILE           OPTIONAL SEEK
         CNTRL (1),SEEK
         MVC   DAKEY,CDEPTNO       SET KEY SEARCH ARGUMENT
         LA    R1,DAFILE           READ BLOCK
         READ  (1),KEY
         WAITF (1)
         TM    DAERRS,X'FF'        IF ANY ERRORS,
         BNZ   CANCEL                 CANCEL
         TM    DAERRS+1,X'F7'       IF ANY ERRORS,
         BNZ   CANCEL                 CANCEL
         TM    DAERRS+1,X'08'      WAS BLOCK FOUND
         BO    NORECFND
         LA    R2,NDATA            SEARCH BLOCK FOR AVAIL.RECORD
         LA    R6,DATA
FINDNULL OC    0(L'DATA,R6),0(R6)  IS THIS NULL RECORD
         BZ    RECOK
         LA    R6,L'DATA(R6)       POINT TO NEXT RECORD
         BCT   R2,FINDNULL
         B     CANCEL
RECOK    MVC   0(L'DATA,R6),CARDIN MOVE DATA TO RECORD
         MVC   PRTLINE+5(2),CDEPTNO
         MVC   PRTLINE+11(L'DATA),CARDIN
         BAL   R2,PRINT
         LA    R1,DAFILE           REWRITE BLOCK
         WRITE (1),KEY
         WAITF (1)
         OC    DAERRS,DAERRS       IF ANY ERRORS,
         BNZ   CANCEL                 CANCEL
         B     MAINLOOP
* IF BLOCK NOT FOUND, ADD A NEW BLOCK TO THE TRACK
NORECFND MVC   DAKEY,CDEPTNO       SET KEY FOR NEW BLOCK
         MVC   DATA,CARDIN         MOVE DATA TO BLOCK
         XC    DATA+L'DATA(L'DATA*3),DATA+L'DATA
         MVC   PRTLINE+5(2),CDEPTNO
         MVI   PRTLINE+8,C'N'
         MVC   PRTLINE+11(L'DATA),CARDIN
         BAL   R2,PRINT
         LA    R1,DAFILE           WRITE NEW BLOCK
         WRITE (1),AFTER
         WAITF (1)
         OC    DAERRS,DAERRS       IF ANY ERRORS,
         BNZ   CANCEL                 CANCEL
         B     MAINLOOP            GO DO NEXT CARD
* TERMINATION - NORMAL AND ABNORMAL
ENDOFJOB LA    R2,READER           CLOSE FILES
         LA    R3,DAFILE
         LA    R4,PRINTER
         CLOSER (R2),(R3),(R4)
         EOJ
CANCEL   UNPK  MERR1C(5),DAERRS(3)  MAKE ERRORS PRINTABLE
         TR    MERR1C(4),TRTABLE-X'F0'
         MVC   PRTLINE+10(L'MERR1-1),MERR1
         BAL   R2,PRINT
         CANCEL
* SUBROUTINES
HEADING  MVC   PRTLINE+1(L'TODAY),TODAY
         MVC   PRTLINE+30(L'MHEAD1),MHEAD1
         MVI   PRTLINE,C'1'         SKIP TO NEW PAGE
         BAL   R2,PRINT
         MVI   PRTLINE,C'0'         DOUBLE SPACE NEXT TIME
         MVI   EOPSW,C'0'           RESET END-OF-PAGE SWITCH
         BR    R3
PRINT    LA    R0,PRTLINE
         LA    R1,PRINTER
         PUT   (1),(0)
         LA    R0,EOP               
         PRTOV (1),12,(0)           CHECK FOR BOTTOM OF PAGE
         MVI   PRTLINE,C' '         CLEAR PRINT LINE
         MVC   PRTLINE+1(132),PRTLINE
         BR    R2
EOP      MVI   EOPSW,C'1'           INDICATE BOTTOM OF PAGE
         BR    R14
*
TRTABLE  DC    C'0123456789ABCDEF'
MHEAD1   DC    C'DIRECT ACCESS METHOD DEMONSTRATION'
MERR1    DC    C'DAM ERROR-XXXX '
MERR1C   EQU   MERR1+L'MERR1-5
TODAY    DS    CL8
EOPSW    DS    C
DOUBLE   DS    D
CARDIN   DS    0CL80
         DS    CL2
CDEPTNO  DS    CL2
         DS    CL76
NUMTRKS  DC    F'0'    CONTAINS LENGTH OF FILE (TRACKS)
DATRK    DC    F'0'    DAM RELATIVE TRACK FIELD
DAERRS   DS    BL2     DAM STATUS BYTES
DAIO     DS    0CL310  DAM I/O AREA
         DS    CL8       COUNT
DAKEY    DS    CL2       KEY
DATA     DS    4CL75     DATA
NDATA    EQU   (*-DATA)/L'DATA  BLOCKING FACTOR
PRTLINE  DC    CL133' '
         LTORG
READER   DTFDI DEVADDR=SYSIPT,EOFADDR=ENDOFJOB,                        X
               IOAREA1=CARDIN,RECSIZE=80
DAFILE   DTFDA AFTER=YES,BLKSIZE=310,CONTROL=YES,DEVICE=2314,          X
               DSKXTNT=1,ERRBYTE=DAERRS,IOAREA1=DAIO,KEYARG=DAKEY,     X
               KEYLEN=2,READKEY=YES,RECFORM=FIXUNB,RELTYPE=HEX,        X
               SEEKADR=DATRK,TYPEFLE=OUTPUT,VERIFY=YES,WRITEKY=YES
PRINTER  DTFPR DEVADDR=SYSLST,BLKSIZE=133,IOAREA1=PRNTOUT1,            X
               WORKA=YES,PRINTOV=YES,CTLCHR=ASA
PRNTOUT1 DS    CL133
         PRMOD WORKA=YES,CTLCHR=ASA,PRINTOV=YES
         END
/*
// EXEC LNKEDT
// DLBL DAFILE,'DEMO.DAM.FILE',0,DA
// EXTENT SYS004,WRK14A,1,0,1100,20
* // DLBL IJSYSIN,'DEMO.DAM.FILE.BACKUP'  TO RECREATE THE FILE FROM BACKUP
* // EXTENT SYSIPT,WRK14A
* ASSGN SYSIPT,X'192'
// LBLTYP NSD(1)
// EXEC
10004ACHER, WILLIAM C.          00675241004            3679874321120000034
10185DONNEMAN, THOMAS M.        00900191003            1043781234130000040
10300FELDMAN, MIKE R.           00300000004            2156278643115000026
10325HATFIELD, MARK I.          00205390002            2225723628090000030
10730REEDE, OWEN W.             01051440001            2115897234105000021
10960WINGLAND, KEITH E.         00350000003            4215679672085000026
20111CARTOLER, VIOLET B.        00750060004            3455667381140000032
20304FROMM, STEVE V.            01200005002            2300267832122500037
20590NEIL, CLARENCE N.          00950230001            4016787112135000040
20801SCHEIBER, HARRY T.         00325080002            6198842236112500046
20956WANGLEY, THEO. A.          00150000003            1723456124120000050
30030ALLOREN, RUTH W.           00000000002            7647982367130000055
30181DELBERT, EDWARD D.         01305541015            6419182773110000051
30318HANEY, CAROL S.            01450005008            5533266791100000058
30487KING, MILDRED J.           01804290010            8711322487090500033
30834TRAWLEY, HARRIS T.         00550000009            7623561471100250032
40171COSTA, NAN S.              00560000005            1241963182122000046
40027ALHOUER, ELAINE E.         00220660006            6381469783079500022
40317HANBEE, ALETTA O.          00395000008            1136719674139000050
40721RASSMUSEN, JOHN J.         01000000004            2064397865129600040
50040ATKINSON, CHARLES          00675241004            3679874321120000034
50060BASEL, DEBORAH L           00900191003            1043781234130000040
50105BETTINARDI, RONALD J       01500500003            1125666601110000022
/*
* CLOSE SYSIPT,X'00C'
* DISPLAY THE FILE TO SEE HOW IT WAS BUILT
// UPSI 1
// EXEC DITTO
$$DITTO  DDU   INPUT=SYS004,BEGIN=05500,END=05519
$$DITTO  EOJ
/&
// JOB DEMODAMB DAM-SEQUENTIAL RETRIEVE (BACKUP)-LIOCS
/* Any file needs to be backed up.  But DAM files are not designed
/*   to be accessed sequentially so how do you know when you've
/*   retrieved all the records?  In this first method we'll use DAM
/*   and start reading at record one for each track and continue to
/*   read one block at a time until there are no more blocks on the
/*   track (no record found).  Do this for each track in the file.
/* Write out the records to a sequential disk file (card image).
/*   This file can then be used to feed back into the create program
/*   above to recreate the DAM file if needed.
// OPTION LINK
 PHASE DEMODAMB,+0
// EXEC ASSEMBLY
         TITLE 'DAM-SEQUENTIAL RETRIEVE (BACKUP)'
* READ THE FILE SEQUENTIALLY USING LIOCS TO BACKUP THE FILE
* THE PROGRAM IS SELF-RELOCATING
*        PRINT NOGEN
DEMODAMB CSECT
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         BALR  R12,0
         USING *,R12
         LA    R2,DAFILE
         LA    R3,SDFILE
         LA    R4,PRINTER
         OPENR (R2),(R3),(R4)
         MVI   HITRK,9                 ASSUME 2311, USE 9
         CLI   DAFILE+29,X'01'         IF 2314,
         BNE   *+8
         MVI   HITRK,19                   USE 19
         COMRG
         MVC   TODAY,0(R1)             GET TODAY'S DATE
         BAL   R3,HEADING              PRINT FIRST PAGE HEADING
         MVC   DARECID+3(4),XTNTBGCH   SET FIRST TRACK
         MVI   DARECIDR,1              SET RECORD NUMBER
MAINLOOP CLC   DARECID+3(4),XTNTENCH   IS IT END OF FILE
         BH    ENDOFJOB
         LA    R1,DAFILE
         READ  (1),ID                  READ RECORD
         WAITF (1)
         TM    DAERRS,X'FF'            CHECK FOR ERRORS
         BNZ   CANCEL
         TM    DAERRS+1,X'F7'          CHECK FOR ERRORS
         BNZ   CANCEL
         TM    DAERRS+1,X'08'          WAS IT NRF
         BO    NOBLKFND
         LA    R5,NDATAIN              BLOCKING FACTOR
         LA    R4,DATAIN
BLKLOOP  OC    0(L'DATAIN,R4),0(R4)    IS THERE A RECORD
         BZ    NOREC
         CLI   EOPSW,C'1'              IF BOTTOM OF PAGE,
         BNE   *+8
         BAL   R3,HEADING                 GO TO NEXT PAGE
         MVC   SDOUT(L'DATAIN),0(R4)
         LA    R1,SDFILE
         LA    R0,SDOUT
         PUT   (1),(0)
         MVC   PRTLINE+1(L'DATAIN),0(R4)
         BAL   R2,PRINT
NOREC    LA    R4,L'DATAIN(R4)
         BCT   R5,BLKLOOP
         IC    R2,DARECIDR             INCREMENT RECORD NUMBER
         LA    R2,1(R2)
         STC   R2,DARECIDR 
         B     MAINLOOP
NOBLKFND IC    R2,DARECIDH+1           INCREMENT FOR NEXT TRACK
         LA    R2,1(R2)
         STC   R2,DARECIDH+1
         MVI   DARECIDR,1              RESET RECORD TO ONE
         CLC   DARECIDH+1(1),HITRK     IF END OF CYLINDER,
         BNH   MAINLOOP
         LH    R2,DARECIDC             INCREMENT FOR NEXT CYLINDER
         LA    R2,1(R2)
         STH   R2,DARECIDC
         MVI   DARECIDH+1,0            RESET HEAD TO ZERO
         MVI   DARECIDR,1              RESET RECORD TO ONE
         B     MAINLOOP
ENDOFJOB LA    R2,DAFILE
         LA    R3,SDFILE
         LA    R4,PRINTER
         CLOSER (R2),(R3),(R4)
         EOJ
CANCEL   UNPK  MERR1C(5),DAERRS(3)     MAKE ERRORS PRINTABLE
         TR    MERR1C(4),TRTABLE-X'F0'
         MVC   PRTLINE+10(L'MERR1-1),MERR1
         BAL   R2,PRINT
         CANCEL
HEADING  MVC   PRTLINE+1(L'TODAY),TODAY
         MVC   PRTLINE+30(L'MHEAD1),MHEAD1
         MVI   PRTLINE,C'1'            SKIP TO NEW PAGE
         BAL   R2,PRINT
         MVI   PRTLINE,C'0'            DOUBLE SPACE NEXT TIME
         MVI   EOPSW,C'0'              RESET BOTTOM-OF-PAGE SWITCH
         BR    R3
PRINT    LA    R0,PRTLINE
         LA    R1,PRINTER
         PUT   (1),(0)
         LA    R0,EOP
         PRTOV (1),12,(0)              CHECK FOR BOTTOM OF PAGE
         MVI   PRTLINE,C' '
         MVC   PRTLINE+1(132),PRTLINE
         BR    R2
EOP      MVI   EOPSW,C'1'              INDICATE BOTTOM OF PAGE
         BR    R14
XTNTEXIT MVC   XTNTINFO(4*2),2(R1)     GET EXTENT INFO
         LBRET 2
TRTABLE  DC    C'0123456789ABCDEF'
MHEAD1   DC    C'DIRECT ACCESS METHOD DEMONSTRATION'
MERR1    DC    C'DAM ERROR-XXXX '
MERR1C   EQU   MERR1+L'MERR1-5
TODAY    DS    CL8
EOPSW    DS    C
DAERRS   DS    BL2
HITRK    DS    X           HIGH TRACK NUMBER
XTNTINFO DS    0XL8        EXTENT INFO
XTNTBGCH DS    XL4           BEG.CCHH
XTNTENCH DS    XL4           END.CCHH
*
         DS    0H
         DS    X
DARECID  DS    0XL8      MBBCCHHR
DARECIDM DC    X'0'
DARECIDB DC    H'0'
DARECIDC DC    H'0'
DARECIDH DC    H'0'
DARECIDR DC    X'0'
*
DATAIN   DS    4CL75
NDATAIN  EQU   (*-DATAIN)/L'DATAIN
SDOUT    DC    CL80' '
PRTLINE  DC    CL133' '
         LTORG
DAFILE   DTFDA BLKSIZE=300,DEVICE=2314,ERRBYTE=DAERRS,IOAREA1=DATAIN,  X
               SEEKADR=DARECID,TYPEFLE=INPUT,READID=YES,               X
               XTNTXIT=XTNTEXIT
SDFILE   DTFSD DEVICE=2314,TYPEFLE=OUTPUT,VERIFY=YES,BLKSIZE=88,       X
               IOAREA1=SDO1,IOAREA2=SDO2,WORKA=YES
PRINTER  DTFPR DEVADDR=SYSLST,BLKSIZE=133,IOAREA1=PRNTOUT1,            X
               WORKA=YES,PRINTOV=YES,CTLCHR=ASA
SDO1     DS    CL88
SDO2     DS    CL88
PRNTOUT1 DS    CL133
         PRMOD WORKA=YES,CTLCHR=ASA,PRINTOV=YES
         END
/*
// EXEC LNKEDT
// DLBL DAFILE,'DEMO.DAM.FILE',,DA
// EXTENT SYS004,WRK14A,1,0,1100,20
// DLBL SDFILE,'DEMO.DAM.FILE.BACKUP',0
// EXTENT SYS004,WRK14A,1,0,1120,20
// LBLTYP NSD(1)
// EXEC
/&
// JOB DEMODAMB DAM-SEQUENTIAL RETRIEVE (BACKUP)-PIOCS
/* In this next backup example we'll use PIOCS and read each block
/*   until end of cylinder.  Do this for each cylinder.  With this
/*   way we don't have to worry about false 'no record found'
/*   indicators because of malfunctioning disk drives which I realize
/*   can't happen in Hercules but I did see happen on real 2311 drives.
/* While we could access the file as a direct access file this method
/*   actually uses the file as a sequential file.  Note the DLBL.
// OPTION LINK
// EXEC ASSEMBLY
         TITLE 'DAM-SEQUENTIAL RETRIEVE (BACKUP)-PIOCS'
* READ THE FILE SEQUENTIALLY TO BACKUP THE FILE
*        PRINT NOGEN
DEMODAMB CSECT
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         BALR  R12,0
         USING *,R12
         OPEN  DAFILE,SDFILE,PRINTER
         OI    DAFILE+3,X'04' DISALLOW NRF
         COMRG
         MVC   TODAY,0(R1)             GET TODAY'S DATE
         BAL   R3,HEADING              PRINT FIRST PAGE HEADING
MAINLOOP CLC   DAFILE4(4),DAFILE2      IS IT END OF FILE
         BH    ENDOFJOB
         CLC   DAFILE4+3(2),=X'0001'   IF BEGINNING OF CYLINDER
         BNE   NTNEWCYL
         MVC   CCWREAD+6(2),=H'8'        SET FOR RECORD 0
         MVI   DAFILE4+4,0
         LA    R1,DAFILE
         EXCP  (1)                     READ RECORD
         WAIT  (1)
         TM    DAFILE+3,X'20'          IS IT EMPTY CYLINDER
         BO    NONXTID
         MVC   DAFILE4(5),NXTID
         MVC   CCWREAD+6(2),=H'300'
NTNEWCYL LA    R1,DAFILE
         EXCP  (1)                     READ RECORD
         WAIT  (1)
         LA    R5,NDATAIN              BLOCKING FACTOR
         LA    R4,DATAIN
BLKLOOP  OC    0(L'DATAIN,R4),0(R4)    IS THERE A RECORD
         BZ    NOREC
         CLI   EOPSW,C'1'              IF BOTTOM OF PAGE,
         BNE   *+8
         BAL   R3,HEADING                 GO TO NEXT PAGE
         MVC   SDOUT(L'DATAIN),0(R4)
         LA    R1,SDFILE
         LA    R0,SDOUT
         PUT   (1),(0)
         MVC   PRTLINE+1(L'DATAIN),0(R4)
         BAL   R2,PRINT
NOREC    LA    R4,L'DATAIN(R4)
         BCT   R5,BLKLOOP
         TM    DAFILE+3,X'20'          IS IT END OF CYLINDER (EOC)
         BO    NONXTID
         MVC   DAFILE4(5),NXTID
         B     MAINLOOP
NONXTID  LH    R2,DAFILE4              INCREMENT CYLINDER 
         LA    R2,1(R2)
         STH   R2,DAFILE4
         MVI   DAFILE4+3,0             RESET HEAD NUMBER
         MVI   DAFILE4+4,1             RESET RECORD NUMBER
         B     MAINLOOP
ENDOFJOB CLOSE DAFILE,SDFILE,PRINTER
         EOJ
HEADING  MVC   PRTLINE+1(L'TODAY),TODAY
         MVC   PRTLINE+30(L'MHEAD1),MHEAD1
         MVI   PRTLINE,C'1'            SKIP TO NEW PAGE
         BAL   R2,PRINT
         MVI   PRTLINE,C'0'            DOUBLE SPACE NEXT TIME
         MVI   EOPSW,C'0'              RESET BOTTOM-OF-PAGE SWITCH
         BR    R3
PRINT    LA    R0,PRTLINE
         LA    R1,PRINTER
         PUT   (1),(0)
         LA    R0,EOP
         PRTOV (1),12,(0)              CHECK FOR BOTTOM OF PAGE
         MVI   PRTLINE,C' '
         MVC   PRTLINE+1(132),PRTLINE
         BR    R2
EOP      MVI   EOPSW,C'1'              INDICATE BOTTOM OF PAGE
         BR    R14
MHEAD1   DC    C'DIRECT ACCESS METHOD DEMONSTRATION'
TODAY    DS    CL8
EOPSW    DS    C
NXTID    DS    XL8
DATAIN   DS    4CL75
NDATAIN  EQU   (*-DATAIN)/L'DATAIN
SDOUT    DC    CL80' '
PRTLINE  DC    CL133' '
         LTORG
CCWSEEK  CCW   X'07',DAFILE3,X'40',6    SEEK
         CCW   X'31',DAFILE4,X'40',5    SIDE
         CCW   X'08',*-8,0,0            TIC
CCWREAD  CCW   X'06',DATAIN,X'40',8     READ
         CCW   X'92',NXTID,0,8          RDCT-MT
DAFILE   DTFPH TYPEFLE=INPUT,DEVICE=2314,MOUNTED=SINGLE,CCWADDR=CCWSEEK
SDFILE   DTFSD DEVICE=2314,TYPEFLE=OUTPUT,VERIFY=YES,BLKSIZE=88,       X
               IOAREA1=SDO1,IOAREA2=SDO2,WORKA=YES
PRINTER  DTFPR DEVADDR=SYSLST,BLKSIZE=133,IOAREA1=PRNTOUT1,            X
               WORKA=YES,PRINTOV=YES,CTLCHR=ASA
SDO1     DS    CL88
SDO2     DS    CL88
PRNTOUT1 DS    CL133
         PRMOD WORKA=YES,CTLCHR=ASA,PRINTOV=YES
         END
/*
// EXEC LNKEDT 
// DLBL DAFILE,'DEMO.DAM.FILE'
// EXTENT SYS004,WRK14A
// DLBL SDFILE,'DEMO.DAM.FILE.BACKUP',0
// EXTENT SYS004,WRK14A,1,0,1120,20
// EXEC
/&
// JOB DEMODAMR DEMONSTRATE DIRECT ACCESS METHOD-RAN.RETRIEVE
// OPTION LINK
// EXEC ASSEMBLY
         TITLE 'DEMONSTRATE DIRECT ACCESS METHOD-RAN.RETRIEVE'
* DO A RANDOM (DIRECT) RETRIEVAL FROM THE FILE CREATED IN THE FIRST JOB
* THE PROGRAM IS SELF-RELOCATING.
*        PRINT NOGEN
DEMODAMR CSECT
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         BALR  R12,0
         USING *,R12
         OPENR READER,DAFILE,PRINTER
         LA    R2,10               ASSUME 2311
         CLI   DAFILE+29,X'01'     IS IT 2314
         BNE   *+8
         LA    R2,20
         STH   R2,TRKCYL
         LH    R2,XTNTENCC         GET NUM. OF CYL.
         SH    R2,XTNTBGCC           SUB. END-BEG.
         LA    R2,1(R2)              PLUS 1
         MH    R2,TRKCYL           TIMES TRK. PER CYL
         SH    R2,XTNTBGHH         SUB. UNUSED TRACKS ON FIRST CYL.
         LH    R1,TRKCYL
         SH    R1,XTNTENHH         GET UNUSED TRACKS ON LAST CYL.
         BCTR  R1,0
         SR    R2,R1                 AND SUBTRACT
         CVD   R2,DOUBLE           STORE NUM. OF TRACKS FOR MODULO
         ZAP   NUMTRKS,DOUBLE
         COMRG
         MVC   TODAY,0(R1)         GET TODAY'S DATE
         BAL   R3,HEADING          PRINT FIRST PAGE HEADING
MAINLOOP LA    R1,READER
         GET   (1)
         CLI   EOPSW,C'1'          IF BOTTOM OF PAGE,
         BNE   *+8
         BAL   R3,HEADING             GO TO NEXT PAGE
         PACK  DOUBLE,CDEPTNO      CALCULATE TRACK ADDRESS
         DP    DOUBLE,NUMTRKS
         UNPK  DATRK(8),DOUBLE+6(2)   MOVE REMAINDER AND
         OI    DATRK+7,X'F0'            CORRECT SIGN
         LA    R1,DAFILE           OPTIONAL SEEK
         CNTRL (1),SEEK
         MVC   DAKEY,CDEPTNO       SET KEY FIELD FOR SEARCH
         LA    R1,DAFILE
         READ  (1),KEY             READ BLOCK
         WAITF (1)
         TM    DAERRS,X'FF'        IF ANY ERRORS,
         BNZ   CANCEL                 CANCEL
         TM    DAERRS+1,X'F7'      IF ANY ERRORS,
         BNZ   CANCEL                 CANCEL
         TM    DAERRS+1,X'08'      WAS BLOCK FOUND
         BO    NORECFND
         LA    R3,DAIN             SET REGISTERS FOR SEARCH
         LA    R4,L'DATA               OF BLOCK
         LA    R5,DAIN+L'DAIN-1
SERCLOOP CLC   2(3,R3),CEMPNO      IS THIS THE EMPLOYEE
         BE    FOUND
         BXLE  R3,R4,SERCLOOP       
         B     NORECFND
FOUND    MVC   PRTLINE+5(3),CEMPNO
         MVC   PRTLINE+11(L'DATA),0(R3)
         BAL   R2,PRINT
*    UPDATE THE BLOCK
*         LA    R1,DAFILE
*         WRITE (1),KEY             REWRITE BLOCK
*         WAITF (1)
*         OC    DAERRS,DAERRS       IF ANY ERRORS,
*         BNZ   CANCEL                 CANCEL
         B     MAINLOOP
NORECFND MVC   PRTLINE+5(3),CEMPNO
         MVC   PRTLINE+20(9),=C'NOT FOUND'
         BAL   R2,PRINT
         B     MAINLOOP
ENDOFJOB CLOSER READER,DAFILE,PRINTER
         EOJ
CANCEL   UNPK  MERR1C(5),DAERRS(3)  MAKE ERRORS PRINTABLE
         TR    MERR1C(4),TRTABLE-X'F0'
         MVC   PRTLINE+10(L'MERR1-1),MERR1
         BAL   R2,PRINT
         CANCEL
HEADING  MVC   PRTLINE+1(L'TODAY),TODAY
         MVC   PRTLINE+30(L'MHEAD1),MHEAD1
         MVI   PRTLINE,C'1'        SKIP TO NEW PAGE
         BAL   R2,PRINT
         MVI   PRTLINE,C'0'        DOUBLE SPACE NEXT TIME
         MVI   EOPSW,C'0'          RESET BOTTOM-OF-PAGE SWITCH
         BR    R3
PRINT    LA    R0,PRTLINE
         LA    R1,PRINTER
         PUT   (1),(0)
         LA    R0,EOP
         PRTOV (1),12,(0)          CHECK FOR BOTTOM OF PAGE
         MVI   PRTLINE,C' '
         MVC   PRTLINE+1(132),PRTLINE
         BR    R2
EOP      MVI   EOPSW,C'1'          INDICATE BOTTOM OF PAGE
         BR    R14
XTNTEXIT MVC   XTNTINFO(4*2),2(R1) GET EXTENT INFO
         LBRET 2
TRTABLE  DC    C'0123456789ABCDEF'
MHEAD1   DC    C'DIRECT ACCESS METHOD DEMONSTRATION'
MERR1    DC    C'DAM ERROR-XXXX '
MERR1C   EQU   MERR1+L'MERR1-5
TODAY    DS    CL8
EOPSW    DS    C
DOUBLE   DS    D
CARDIN   DS    0CL80
CEMPNO   DS    0CL3
CDEPTNO  DS    CL2
         DS    CL78
NUMTRKS  DS    PL2          
TRKCYL   DS    H           TRACKS PER CYLINDER
XTNTINFO DS    0H          EXTENT INFO
XTNTBGCC DS    H             BEG.CC
XTNTBGHH DS    H             BEG.HH
XTNTENCC DS    H             END.CC
XTNTENHH DS    H             END.HH
DATRK    DS    0ZL10       RELATIVE TRACK
         DC    10C'0'
DAERRS   DS    BL2         DAM ERROR BYTES
DAKEY    DS    CL2         DAM KEY FIELD
DAIN     DS    0CL300      DAM INPUT AREA
DATA     DS    4CL75
PRTLINE  DC    CL133' '
         LTORG
READER   DTFDI DEVADDR=SYSIPT,EOFADDR=ENDOFJOB,                        X
               IOAREA1=CARDIN,RECSIZE=80
DAFILE   DTFDA BLKSIZE=300,CONTROL=YES,DEVICE=2314,DSKXTNT=1,          X
               ERRBYTE=DAERRS,IOAREA1=DAIN,KEYARG=DAKEY,               X
               KEYLEN=2,READKEY=YES,RECFORM=FIXUNB,RELTYPE=DEC,        X
               SEEKADR=DATRK,TYPEFLE=INPUT,XTNTXIT=XTNTEXIT
*              WRITEKEY=YES,VERIFY=YES
PRINTER  DTFPR DEVADDR=SYSLST,BLKSIZE=133,IOAREA1=PRNTOUT1,            X
               WORKA=YES,PRINTOV=YES,CTLCHR=ASA
PRNTOUT1 DS    CL133
         PRMOD WORKA=YES,CTLCHR=ASA,PRINTOV=YES
         END
/*
// LBLTYP NSD(1)
// EXEC LNKEDT
// DLBL DAFILE,'DEMO.DAM.FILE',,DA
// EXTENT SYS004,WRK14A,1,0,1100,20
// EXEC
304
030
031
105
960
060
/*
/&
// JOB DEMODRPG DEMONSTRATE DIRECT ACCESS METHOD-RAN.RETRIEVE-RPG
/* RPG is not able to create a DA file but it can retrieve and
/*   optionally update a block using a key.  The key needs to go
/*   through the algorithm to come up with the actual disk location
/*   of the physical record desired.
// OPTION LINK,DUMP
// EXEC RPG
     H
     FREADER  IPE F  80  80    2      EREAD01 SYSIPT
     F* TO UPDATE RECORDS CHANGE THE 'I' IN COL.15 TO 'U'
     FDAFILE  IC  F 300 300R 2KD       DISK14 SYS000S      DAMEXT
     FREPORT  O   F 132 132     OF     PRINTERSYSLST
     EAAC1READER  DAFILE  CONVT
     IREADER  AA  01
     I                                        1   3 EMPNO
     I                                        1   2 CDKEY   C1
     IDAFILE  AA  02
     I                                        1  75 REC1
     I                                        3   5 EMP1
     I                                       76 150 REC2
     I                                       78  80 EMP2
     I                                      151 225 REC3
     I                                      153 155 EMP3
     I                                      226 300 REC4
     I                                      228 233 EMP4
     C           CONVT     EXTCVASMCVT    TRKADR  8
     C                     KEYCV          CDKEY
     C                     SETOF                     H015    
     C   02      EMPNO     COMP EMP1                     11
     C   02      EMPNO     COMP EMP2                     12
     C   02      EMPNO     COMP EMP3                     13
     C   02      EMPNO     COMP EMP4                     14
     C   02 11             MOVE REC1      REC    75
     C   02 12             MOVE REC2      REC
     C   02 13             MOVE REC3      REC
     C   02 14             MOVE REC4      REC
     C   02 11             SETON                     15    
     C   02N15 12          SETON                     15    
     C   02N15 13          SETON                     15    
     C   02N15 14          SETON                     15    
     O* CAN BE USED TO UPDATE EXISTING RECORDS
     O*DAFILE  D        01 02
     O*                         REC1      75
     O*                         REC2     150
     O*                         REC3     225
     O*                         REC4     300
     OREPORT  H  2 1   1P
     O       OR        OF
     O                         UDATE      8 ' 0/  /  '
     O                                   75 'DAM-RETRIEVAL'
     O                                  116 'PAGE'
     O                         PAGE  Z  120
     O        D  1     01
     O                         EMPNO      7
     O                 02 15   REC       85
     O                 02N15             35 'NO RECORD FOUND'
     O                N02                35 'NO RECORD FOUND'
/*
// EXEC ASSEMBLY
         TITLE 'DTFDA EXTENT EXIT'
DAMEXT   CSECT
         ENTRY BEGTRK,NUMTRKS,XTNTINFO,TRKSCYL
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
* EXIT CALLED BY $$B TRANSIENTS AS EXTENT(S) BEING PROCESSED.
*
* ENTRY IS VIA SVC 8 FROM THE TRANSIENT, AND EXIT RECEIVES CONTROL
* WITH REGISTERS AND CONTEXT OF THE PROGRAM THAT ISSUED THE OPEN MACRO 
* THAT RESULTED IN THE EXTENT EXIT BEING CALLED.  
*
* THIS MEANS WE HAVE NO ADDRESSABILITY, AND A VERY LIMITED SET OF 
* AVAILABLE REGISTERS WITH WHICH TO ESTABLISH * ADDRESSABILITY AND
* SAVE THE OTHER REGISTERS.  NOR DO WE HAVE DOS RPG DOCUMENTATION 
* THAT WOULD LET US UNDERSTAND THE RPG PROGRAM CONTEXT WELL ENOUGH 
* TO KNOW WHICH REGISTERS COULD BE USED.  
*
* NB--R13 DOES NOT POINT TO A SAVEAREA.
*
* SO WE'LL USE REGISTERS CLAIMED BY OPEN (R0&R1) AND HOPE FOR THE
* BEST.  THIS APPROACH SHOULD WORK FOR OTHER LANGUAGES.  
*
         LR    R0,R1               SAVE POINTER TO EXTENT DESCRIPTOR
         BALR  R1,0                CREATE A BASE REGISTER
         USING DAMEXTAD,R1         ESTABLISH TEMPORARY ADDRESSABILITY
DAMEXTAD STM   R0,R15,SAVERPGX     SAVE RPG REGISTERS
         LR    R12,R1              TRANSFER ADDRESSING
         DROP  R1
         USING DAMEXTAD,R12        MAKE PROGRAM ADDRESSABLE
         LR    R1,R0               MAKE EXTENT INFO ADDRESSABLE
         MVC   XTNTINFO,0(R1)      GET 14-BYTE EXTENT INFO TABLE
*
*  DETERMINE EXECUTION PARTITION, FIND THE LUB FOR SYS004, THEN
*  FIND THE PUB SO WE CAN DETERMINE THE DEVICE TYPE.  
*
         L     R1,X'14'             GET ADDR OF THIS PART'N COMRG
         LH    R2,X'2E'(,R1)        GET PIK/PID OF THIS PARTITION
         SRL   R2,4                 CONVERT PIK/PID INTO FICL INDEX
         AH    R2,X'48'(,R1)        ADD INDEX TO ADDR OF FICL
         XR    R11,R11              CLEAR R11 FOR IC
         IC    R11,0(,R2)           GET LUBTAB INDEX FOR THIS PART'N
         XR    R2,R2                CLEAR R2 FOR IC
         IC    R2,XTNTSNR           GET LOGICAL UNIT NR W/IN PART'N
         AR    R11,R2               ADD TO LUBTAB INDEX
         SLL   R11,1                CHG INDEX INTO DISPLACEMENT
         AH    R11,X'4C'(,R1)       ADD START ADDR OF LUBTAB
         IC    R2,0(,R11)           GET PUB INDEX FOR SYS004
         SLL   R2,3                 CHG INDEX INTO DISPLACEMENT
         AH    R2,X'40'(,R1)        GET ADDRESS OF SYS004 PUB
         MVC   PUBENT,0(R2)         MAKE A COPY OF THE PUB ENTRY
*
*  WE HAVE THE PUB.  CHECK DEVICE TYPE
*
         MVC   TRKSCYL,=F'10'       SET TRACK GEOMETRY FOR 2311
         CLI   PUBENT+4,X'60'       IS DEVICE TYPE = 2311
         BE    GEODONE              ..YES, TRACK GEOMETRY SET
         MVC   TRKSCYL,=F'20'       SET TRACK GEOMETRY FOR 2314
         CLI   PUBENT+4,X'62'           IS DEVICE TYPE = 2314
         BE    GEODONE              ..YES, TRACK GEOMETRY SET
         CANCEL ,                   INVALID DEVICE ASSIGNMENT
GEODONE  DS    0H                   TRACKS/CYLINDER SET
*
* CALCULATE NUMBER OF TRACKS IN THE EXTENT
*
         LH    R2,XTNTENCC         GET NUM. OF CYL.
         SH    R2,XTNTBGCC           SUB. END-BEG.
         LA    R2,1(R2)              PLUS 1
         MH    R2,TRKSCYL+2        2311-10/2314-20 TRK. PER CYL
         SH    R2,XTNTBGHH         SUB. UNUSED TRACKS ON FIRST CYL.
         L     R1,TRKSCYL
         SH    R1,XTNTENHH         GET UNUSED TRACKS ON LAST CYL.
         BCTR  R1,0
         SR    R2,R1                 AND SUBTRACT
         ST    R2,NUMTRKS          STORE NUM. OF TRACKS FOR MODULO
         LH    R2,XTNTBGCC         TAKE BEGINNING CYLINDER,
         MH    R2,TRKSCYL+2        TIMES 10/20 TRKS/CYL AND
         AH    R2,XTNTBGHH           ADD BEGINNING HEAD
         ST    R2,BEGTRK           STORE BEGINNING TRACK NUMBER
         LM    R0,R15,SAVERPGX     RESTORE RPG REGISTERS
         LR    R1,R0               RESTORE POINTER TO EXTENT INFO
         LBRET 2                   BACK TO B-TRANSIENT
         LTORG
SAVERPGX DS    16F         RPG REGISTER SAVE AREA
BEGTRK   DS    F           BEGINNING TRACK NUMBER
NUMTRKS  DS    F           NUMBER OF TRACKS IN FILE
TRKSCYL  DS    F           TRACKS PER CYLINDER
XTNTINFO DS    0XL14       EXTENT INFO FROM $$B TRANS
XTNTTYPE DS    X           ..EXTENT TYPE FROM DLBL
XTNTSEQ  DS    X           ..EXTENT SEQUENCE NR FROM DLBL
XTNTBGCC DS    H             BEG.CC
XTNTBGHH DS    H             BEG.HH
XTNTENCC DS    H             END.CC
XTNTENHH DS    H             END.HH
XTNTSCLS DS    X           CLASS OF SYSNNN 0=SYS, 1=PROG
XTNTSNR  DS    X           LOGICAL UNIT NR WITHIN CLASS
XTNTOBIN DS    X           OLD BIN NR (2321 ONLY)
XTNTCBIN DS    X           CURRENT BIN NR (2321 ONLY)
PUBENT   DS    CL8         SAVEAREA FOR PUB ENTRY
         DROP  R12
         TITLE 'SETUP MBBCCHHR USING KEY'
ASMCVT   CSECT
         USING ASMCVT,R15
         STM   R0,R15,SAVERPGC     SAVE RPG REGISTERS
         LM    R5,R10,=V(CDKEY,BEGTRK,NUMTRKS,XTNTINFO,TRKADR,TRKSCYL)
         XC    0(8,R9),0(R9)       CLEAR MBBCCHHR
         PACK  DOUBLE,0(2,R5)      TRACK NUMBER EQUALS
         CVB   R3,DOUBLE              KEY (DEPT.)
         SR    R2,R2                  MODULO
         D     R2,0(,R7)              NUMBER OF TRACKS
         A     R2,0(,R6)              PLUS BEGINNING TRACK
         LR    R3,R2               CONVERT TRACK NUMBER
         SR    R2,R2                  TO CYL. AND HEAD
         D     R2,0(,R10)
         STC   R3,4(,R9)           SET CYLINDER
         STC   R2,6(,R9)           SET HEAD
         LM    R0,R15,SAVERPGC     RESTORE RPG REGISTERS
         BR    R14                 RETURN TO RPG
         LTORG
DOUBLE   DS    D
SAVERPGC DS    16F          RPG REGISTER SAVE AREA
         END
/*
// LBLTYP NSD(1)
// EXEC LNKEDT
// DLBL DAFILE,'DEMO.DAM.FILE',,DA
// EXTENT SYS004,WRK14A,1,0,1100,20
// EXEC
304
030
031
105
960
060
/*
/&
// JOB DEMODCOB DEMONSTRATE DIRECT ACCESS METHOD-RAN.RETRIEVE-COBOL
/* ANS COBOL likewise cannot really create a DA file but can
/*   retrieve and optionally update existing records.
// OPTION LINK,LISTX,DUMP
// EXEC FCOBOL
 CBL BUF=1024,SUPMAP,NOTRUNC
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DMDCOB.
       AUTHOR. WILLIAM CARLBORG.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-360.
       OBJECT-COMPUTER. IBM-360.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT READER ASSIGN TO SYS007-UR-2501-S.
           SELECT DAFILE ASSIGN TO SYS000-DA-2314-D-DAFILE
               ACCESS IS RANDOM
               ACTUAL KEY IS ACT-KEY.
           SELECT PRINT-FILE ASSIGN TO SYS009-UR-1403-S
               RESERVE NO ALTERNATE AREA.
      *I-O-CONTROL.
      *    APPLY WRITE-VERIFY ON DAFILE.
       DATA DIVISION.
       FILE SECTION.
       FD  READER
           LABEL RECORDS ARE OMITTED
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS CARDIN.
       01  CARDIN.
           02  CD-EMPNO.
               05  CD-DEPT         PIC 99.
               05  FILLER          PIC X.
           02  FILLER              PIC X(77).
       FD  DAFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 300 CHARACTERS
           DATA RECORD IS DA-BLK.
       01  DA-BLK.
           02  REC OCCURS 4 TIMES.
               05  FILLER          PIC XX.
               05  REC-EMPNO       PIC XXX.
               05  FILLER          PIC X(70).
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
           02  H1-TITLE            PIC X(13).
           02  FILLER              PIC X(66).
           02  H1-PAGE             PIC X(4).
           02  FILLER              PIC X.
           02  H1-PAGENUM          PIC ZZ9.
           02  FILLER              PIC X(12).
       01  DETAIL-LINE-LISTING.
           02  FILLER              PIC X. 
           02  FILLER              PIC X(5).
           02  DL-EMPNO            PIC X(3).
           02  FILLER              PIC X(2).
           02  DL-REC              PIC X(75).
           02  FILLER              PIC X(47).
       WORKING-STORAGE SECTION.
       01  ACT-KEY.
           02  ACT-KEY-TRK    COMP PIC S9(8) SYNC.
           02  ACT-KEY-DEPT        PIC XX.
       01  FILLER.
           02  NUMTRKS        COMP PIC S9(8) SYNC.
           02  QUOTIENT       COMP PIC S9(8) SYNC.
           02  WS-TODAY            PIC X(8).
           02  WS-PAGENUM   COMP-3 PIC S999 VALUE ZERO.
           02  WS-EOF              PIC X VALUE 'N'.
           02  WS-EOP              PIC X.
           02  WS-NRF              PIC X.
       PROCEDURE DIVISION.
           OPEN INPUT READER
                INPUT DAFILE
      *         I-O   DAFILE
                OUTPUT PRINT-FILE.
           MOVE SPACES TO PRINTOUT.
           CALL 'DAMEXT' USING DAFILE, NUMTRKS.
           MOVE CURRENT-DATE TO WS-TODAY
           PERFORM HEADING-ROUTINE THRU HEADING-ROUTINE-EXIT.
           READ READER AT END MOVE 'Y' TO WS-EOF.
           PERFORM DETAIL-ROUTINE THRU DETAIL-ROUTINE-EXIT
               UNTIL WS-EOF = 'Y'
           CLOSE READER, DAFILE, PRINT-FILE
           STOP RUN.

       DETAIL-ROUTINE.
           IF WS-EOP = 'Y'
               PERFORM HEADING-ROUTINE THRU HEADING-ROUTINE-EXIT.
           DIVIDE NUMTRKS INTO CD-DEPT GIVING QUOTIENT
               REMAINDER ACT-KEY-TRK.
           MOVE CD-DEPT TO ACT-KEY-DEPT
           MOVE 'N' TO WS-NRF
           READ DAFILE
              INVALID KEY MOVE 'Y' TO WS-NRF.
           IF WS-NRF = 'N'
      *    UPDATE THE BLOCK AT THIS POINT
      *    REWRITE DA-BLK
               IF      CD-EMPNO = REC-EMPNO (1)
                   MOVE REC-EMPNO (1) TO DL-EMPNO
                   MOVE REC (1)       TO DL-REC
               ELSE IF CD-EMPNO = REC-EMPNO (2)
                   MOVE REC-EMPNO (2) TO DL-EMPNO
                   MOVE REC (2)       TO DL-REC
               ELSE IF CD-EMPNO = REC-EMPNO (3)
                   MOVE REC-EMPNO (3) TO DL-EMPNO
                   MOVE REC (3)       TO DL-REC
               ELSE IF CD-EMPNO = REC-EMPNO (4)
                   MOVE REC-EMPNO (4) TO DL-EMPNO
                   MOVE REC (4)       TO DL-REC
               ELSE
                   MOVE CD-EMPNO                TO DL-EMPNO
                   MOVE '     RECORD NOT FOUND' TO DL-REC 
           ELSE
               MOVE CD-EMPNO                TO DL-EMPNO
               MOVE '     RECORD NOT FOUND' TO DL-REC. 
           PERFORM PRINT-LINE THRU PRINT-LINE-EXIT.
           READ READER AT END MOVE 'Y' TO WS-EOF.
       DETAIL-ROUTINE-EXIT.
           EXIT.

       HEADING-ROUTINE.
           MOVE WS-TODAY        TO H1-TODAY
           MOVE 'DAM-RETRIEVAL' TO H1-TITLE
           MOVE 'PAGE'          TO H1-PAGE
           ADD 1 TO WS-PAGENUM
           MOVE WS-PAGENUM      TO H1-PAGENUM
           MOVE '1' TO ASA
           PERFORM PRINT-LINE THRU PRINT-LINE-EXIT
           MOVE '0' TO ASA
           MOVE 'N' TO WS-EOP.
       HEADING-ROUTINE-EXIT.
           EXIT.

       PRINT-LINE.
           WRITE PRINTOUT AFTER POSITIONING ASA
               EOP MOVE 'Y' TO WS-EOP.
           MOVE SPACES TO PRINTOUT.
       PRINT-LINE-EXIT.
           EXIT.
/*
// EXEC ASSEMBLY
         TITLE 'GET LENGTH OF FILE (TRACKS) FROM DTFDA EXTENT TABLE'
* SINCE WE DON'T HAVE ACCESS TO THE DTFDA EXTENT EXIT IN COBOL WE CAN
*   GET THE LENGTH OF THE EXTENT FROM THE DTFDA EXTENT TABLE AFTER 
*   THE FILE IS OPENED.
DAMEXT   CSECT
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         SAVE  (14,12)             SAVE COBOL REGISTERS
         USING DAMEXT,R15
         LM    R8,R9,0(R1)         RETRIEVE PASSED PARAMETERS
         LA    R7,272(R8)          POINT TO EXTENT TABLE
NEXTEXT  L     R2,0(R7)            GET NO. OF TRACKS
         SRL   R2,8
         ST    R2,0(R9)            STORE NO. OF TRACKS FOR MODULO
         LA    R7,8(R7)            POINT TO NEXT EXTENT
         CLC   0(2,R7),=X'FFFF'    IS THERE ANOTHER EXTENT
         BNE   NEXTEXT
         RETURN (14,12)            RESTORE COBOL REGISTERS AND RETURN
         LTORG
         END
/*
// LBLTYP NSD(1)
// EXEC LNKEDT
// ASSGN SYS007,X'00C'
// ASSGN SYS009,X'00E'
// DLBL DAFILE,'DEMO.DAM.FILE',,DA
// EXTENT SYS004,WRK14A,1,0,1100,20
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