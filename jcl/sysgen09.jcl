// JOB SYSGEN9A   FIXISMOD CORRECT DOS R26.2 ISAM BUG FOR RAN.RET. NRF
// EXEC MAINT
 UPDATE A.ISMOD4,B.ISMOD4,,NO 
) DEL 0197
) ADD 0203
         L     IJHRG10,IJHCRARA  GET ADDR. OF IOAREAR
) END
/*
// DLBL IJSYSPH,'DOS.BG.WORKFILE.004',0,SD
// EXTENT SYSPCH,WRK14A,1,1,2600,300
ASSGN SYSPCH,X'192'
/&
// JOB SYSGEN9B   RECATALR IBM SUPPLIED ISAM MODULES
// OPTION DECK,NOXREF
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=BOTH,IOROUT=ADDRTR,TYPEFLE=RANSEQ,  X
               CORINDX=YES,CORDATA=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=BOTH,IOROUT=ADDRTR,TYPEFLE=RANSEQ,  X
               CORINDX=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=BOTH,IOROUT=ADDRTR,TYPEFLE=RANSEQ,  X
               CORDATA=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=BOTH,IOROUT=ADDRTR,TYPEFLE=RANSEQ
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=BOTH,IOROUT=ADDRTR,TYPEFLE=RANDOM,  X
               CORINDX=YES,CORDATA=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=BOTH,IOROUT=ADDRTR,TYPEFLE=RANDOM,  X
               CORINDX=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=BOTH,IOROUT=ADDRTR,TYPEFLE=RANDOM,  X
               CORDATA=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=BOTH,IOROUT=ADDRTR,TYPEFLE=RANDOM
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXBLK,IOROUT=ADDRTR,TYPEFLE=RANSEQ,X
               CORINDX=YES,CORDATA=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXBLK,IOROUT=ADDRTR,TYPEFLE=RANSEQ,X
               CORINDX=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXBLK,IOROUT=ADDRTR,TYPEFLE=RANSEQ,X
               CORDATA=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXBLK,IOROUT=ADDRTR,TYPEFLE=RANSEQ
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXBLK,IOROUT=ADDRTR,TYPEFLE=RANDOM,X
               CORINDX=YES,CORDATA=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXBLK,IOROUT=ADDRTR,TYPEFLE=RANDOM,X
               CORINDX=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXBLK,IOROUT=ADDRTR,TYPEFLE=RANDOM,X
               CORDATA=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXBLK,IOROUT=ADDRTR,TYPEFLE=RANDOM
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXUNB,IOROUT=ADDRTR,TYPEFLE=RANSEQ,X
               CORINDX=YES,CORDATA=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXUNB,IOROUT=ADDRTR,TYPEFLE=RANSEQ,X
               CORINDX=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXUNB,IOROUT=ADDRTR,TYPEFLE=RANSEQ,X
               CORDATA=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXUNB,IOROUT=ADDRTR,TYPEFLE=RANSEQ
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXUNB,IOROUT=ADDRTR,TYPEFLE=RANDOM,X
               CORINDX=YES,CORDATA=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXUNB,IOROUT=ADDRTR,TYPEFLE=RANDOM,X
               CORINDX=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXUNB,IOROUT=ADDRTR,TYPEFLE=RANDOM,X
               CORDATA=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,RECFORM=FIXUNB,IOROUT=ADDRTR,TYPEFLE=RANDOM
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,IOROUT=RETRVE,TYPEFLE=RANSEQ,CORINDX=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,IOROUT=RETRVE,TYPEFLE=RANSEQ
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,IOROUT=RETRVE,TYPEFLE=RANDOM,CORINDX=YES
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         ISMOD SEPASMB=YES,IOROUT=RETRVE,TYPEFLE=RANDOM
         END
/*
// EXEC ASSEMBLY
         PRINT NOGEN
         PUNCH '/*    '
         END
/*
CLOSE SYSPCH,X'00D'
/* STRIP STACKER SELECT CHARACTER FROM SYSPCH FILE
/*   (DOS SYSTEM FILES ON DISK WAS NOT VERY SOPHISTICATED)
// DLBL UIN,'DOS.BG.WORKFILE.004',0,SD
// EXTENT SYS004,WRK14A
// DLBL UOUT,'DOS.BG.WORKFILE.001',0,SD
// EXTENT SYS005,WRK14A,1,1,1700,300
// ASSGN SYS004,X'192'       INPUT SYSPCH FILE 81-BYTE RECORDS
// ASSGN SYS005,X'192'       OUTPUT SYSIPT FILE 80-BYTE RECORDS
/* NOTE THAT CARD INPUT STARTS IN COLUMN TWO AND IS SHIFTED LEFT 1 
/*   BY OPERATION OF THE UTILITY // FS STATEMENT
// EXEC DKDK                 DISK TO DISK                            
// UDD TF,FF,A=(81,81),B=(80,80),ON,R1,E=(2314)
// FS 2,80,1
// END
// DLBL IJSYSIN,'DOS.BG.WORKFILE.001',0,SD
// EXTENT SYSIPT,WRK14A,1,1,1700,300
ASSGN SYSIPT,X'192'
// EXEC MAINT
CLOSE SYSIPT,X'00C'
/&
ASSGN SYSPCH,X'00D'   OR CLOSE SYSPCH,X'00D' 
ASSGN SYSIPT,X'00C'   OR CLOSE SYSPCH,X'00C' 