// JOB DEMORASF SAMPLE RPG-ASSEMBLY-FORTRAN
/* Demonstrate linkage conventions for RPG-ASSEMBLY and
/*   ASSEMBLY-FORTRAN.
/* Uses the report capabilities of RPG and
/*   the mathematical capabilities of FORTRAN
/*   to use the strengths of both languages.
// OPTION LINK,LISTX
// EXEC RPG
      * DEMONSTRATES RPG LINKAGE CONVENTIONS TO AN
      *   ASSEMBLER PROGRAM
     H
     FCARDS   IPE F  80  80    2       READ01 SYSIPT
     FOUTPUT  O   F  60  60     OF     PRINTERSYSLST
     ICARDS   AA  01
     I                                        1   20SCORE
     C   01                EXIT ASSEM
     C                     RLABL          SCORE
     C                     ULABL          PSQRE   50
     C                     ULABL          PCUBE   50
     C                     ULABL          PSQRTR  53
     C                     ULABL          PSQRTI  53
     OOUTPUT  H  2     1P
     O       OR        OF
     O                                   20 'MATH STATS.'
     O                                   45 'PAGE'
     O                         PAGE  Z   50
     O        H  2     1P
     O       OR        OF
     O                                   10 'NUMBER'
     O                                   20 'SQUARE'
     O                                   30 'CUBE'
     O                                   40 'SQ.ROOT-R'
     O                                   50 '+SQ.ROOT-I'
     O        D  1     01
     O                         SCORE     10 '    0-'
     O                         PSQRE     20 '    0-'
     O                         PCUBE     30 '    0-'
     O                         PSQRTR    40 ' -0.   '
     O                         PSQRTI    50 ' -0.   '
     O                                   51 'I'
/*
// EXEC ASSEMBLY
         TITLE 'ASSEMBLER SUBROUTINE FROM RPG'
         MACRO
&NAME    FLTPK &FROM,&DEC,&TO
         LCLC  &DECA
         AIF   (N'&SYSLIST NE 3).ERROR1
         AIF   ('&DEC' LT '0').ERROR2
         AIF   ('&DEC' GT '75').ERROR3
         AIF   ('&FROM'(1,1) EQ '(' AND '&FROM(1)' EQ '1').ERROR4
&DECA    SETC  '&DEC'
.TRY     CNOP  6,8
         AIF   ('&FROM'(1,1) EQ '(').REG1
&NAME    LD    0,&FROM
         AGO   .NREG1
.REG1    ANOP
&NAME    LD    0,0(&FROM(1))
.NREG1   LDR   2,0
         MD    0,=D'1E&DECA'
         LPDR  0,0   
         AD    0,=D'.5'    
         AW    0,*+34
         STD   0,*+30
         L     1,*+30
         LTDR  2,2
         BNM   *+6
         LNR   1,1
         AIF   ('&TO'(1,1) EQ '(').REG2
         CVD   1,&TO
         AGO   .NREG2
.REG2    CVD   1,0(&TO(1))
.NREG2   MVC   *+10(8),=X'4E00000000000000'
         B     *+12
         DC    XL8'4E00000000000000'
         MEXIT
.ERROR1  MNOTE 255,'NUMBER OF OPERANDS NOT 3, GENERATION TERMINATED'
         MEXIT
.ERROR2  MNOTE 1,DECIMAL POINTS NOT NUMERIC, ''0'' ASSUMED'
&DECA    SETC  '0'
         AGO   .TRY
.ERROR3  MNOTE 1,'DECIMAL POINTS TOO LARGE, ''75'' ASSUMED'
&DECA    SETC  '75'
         AGO   .TRY
.ERROR4  MNOTE 255,'USE OF REG. 1 NOT ALLOWED', GENERATION TERMINATED'
         MEND
ASSEM    CSECT
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
         USING *,R15
         ENTRY PSQRE,PCUBE,PSQRTR,PSQRTI MAKE ADDRS. AVAIL. TO RPG
         STM   R0,R15,SAVERPG  SAVE RPG'S REGISTERS
         LR    R12,R15         USE R12 FOR OUR BASE
         DROP  R15
         USING ASSEM,R12
*
         L     R3,=V(SCORE)    GET ADDR. OF RPG'S FIELD
         ZAP   DOUBLE,0(2,R3)  GET THE PACKED ARGUMENT
         CVB   R4,DOUBLE       CONVERT TO BINARY FOR FORTRAN
         ST    R4,ISCORE
*
         LA    R13,SAVEAREA    FOR STANDARD LINKAGE CONVENTIONS
         CALL  FORT,(ISCORE,ISQRE,ICUBE,CSQRT)
* 
         L     R4,ISQRE        CONVERT SQUARE TO PACKED FOR RPG
         CVD   R4,DOUBLE
         ZAP   PSQRE,DOUBLE    LEAVE IT SO RPG CAN GET IT LATER
*
         L     R4,ICUBE        CONVERT CUBE TO PACKED FOR RPG
         CVD   R4,DOUBLE
         ZAP   PCUBE,DOUBLE    LEAVE IT SO RPG CAN GET IT LATER
*
         XC    DOUBLE,DOUBLE   CONVERT SQUARE ROOT REAL PART TO PACKED
         MVC   DOUBLE(4),CSQRT
         FLTPK DOUBLE,3,DOUBLE
         ZAP   PSQRTR,DOUBLE   LEAVE IT SO RPG CAN GET IT LATER
*
         XC    DOUBLE,DOUBLE   CONVERT SQUARE ROOT IMAG. PART TO PACKED
         MVC   DOUBLE(4),CSQRT+4
         FLTPK DOUBLE,3,DOUBLE
         ZAP   PSQRTI,DOUBLE   LEAVE IT SO RPG CAN GET IT LATER
*
         LM    R0,R15,SAVERPG  RESTORE RPG'S REGISTERS
         BR    R14             RETURN TO RPG
         LTORG
DOUBLE   DS    D       WORK AREA
SAVEAREA DS    9D      STANDARD LINKAGE SAVE AREA FOR FORTRAN
SAVERPG  DS    16F     RPG REGISTER SAVE AREA
ISCORE   DS    F
ISQRE    DS    F
ICUBE    DS    F
CSQRT    DS    2E
PSQRE    DS    PL3
PCUBE    DS    PL3
PSQRTR   DS    PL3
PSQRTI   DS    PL3
         END
/*
// EXEC FFORTRAN
      SUBROUTINE FORT (ISCORE,ISQRE,ICUBE,COSQRT)
      COMPLEX COSQRT
      ISQRE=ISCORE*ISCORE
      ICUBE=ISQRE*ISCORE
      COSQRT=CSQRT(CMPLX(FLOAT(ISCORE),0.0))
      RETURN
      END
/*
// EXEC LNKEDT
// EXEC
11
1J     (-11)
19
/*
/&
