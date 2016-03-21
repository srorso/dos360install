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
* ENTRY IS VIA SVC 8 FROM THE TRANSIENT, AND EXIT RECEIVES 
* CONTROL WITH REGISTERS AND CONTEXT OF THE PROGRAM THAT 
* ISSUED THE OPEN MACRO THAT RESULTED IN THE * EXTENT EXIT 
* BEING CALLED.  
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
*234567890123456789012345678901234567890123456789012345678901234567890
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
*  DETERMINE EXECUTION PARTITION
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
.NOMODS  ANOP                      BYPASS ADDED CODE ASSEMBLY
*
***      MVC   TRKSCYL,=F'20'      SET TRACK GEOMETRY FOR 2314
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
