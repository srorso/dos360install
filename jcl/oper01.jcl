* $$ JOB STARTBG,,,BG
// JOB STARTBG START UP BG PARTITION
NOLOG
// OPTION NODUMP
ASSGN SYSPCH,X'00D'
ASSGN SYSLST,X'00E'
/&
* $$ EOJ
* $$ JOB STARTF2,,,F2
// JOB STARTF2 START F2 PARTITION
// OPTION PARSTD
/*   PRIVATE CORE IMAGE LIB FOR F2 PROGRAMS
// DLBL IJSYSCL,'DOS.F2.PRIVATE.CORE.LIB',,SD
// EXTENT SYSCLB,WRK14B,1,1,20,1000
// DLBL IJSYSPC,'DOS.F2.PRIVATE.CORE.LIB',99/365,SD
// EXTENT SYS003,WRK14B,1,1,20,1000
/*   WORKFILES FOR ASSEMBLY AND LNKEDIT
// DLBL IJSYSLN,'DOS.F2.WORKFILE.LNK',0,SD
// EXTENT SYSLNK,WRK14B,1,1,1400,300
// DLBL IJSYS01,'DOS.F2.WORKFILE.001',0,SD
// EXTENT SYS001,WRK14B,1,1,1700,300
// DLBL IJSYS02,'DOS.F2.WORKFILE.002',0,SD
// EXTENT SYS002,WRK14B,1,1,2000,300
// DLBL IJSYS03,'DOS.F2.WORKFILE.003',0,SD
// EXTENT SYS003,WRK14B,1,1,2300,300
// DLBL IJSYS04,'DOS.F2.WORKFILE.004',0,SD
// EXTENT SYS004,WRK14B,1,1,2600,300
/*   WORKFILES FOR SORT (OVERLAPS COMPILER WORKFILES)
// DLBL SORTWK1,'DOS.F2.WORKFILE.001',0,SD
// EXTENT SYS001,WRK14B,1,1,1700,300
// DLBL SORTWK2,'DOS.F2.WORKFILE.002',0,SD
// EXTENT SYS002,WRK14B,1,1,2000,300
// DLBL SORTWK3,'DOS.F2.WORKFILE.003',0,SD
// EXTENT SYS003,WRK14B,1,1,2300,300
// DLBL SORTWK4,'DOS.F2.WORKFILE.004',0,SD
// EXTENT SYS004,WRK14B,1,1,2600,300
// OPTION NODUMP,USRLABEL
ASSGN SYSCLB,X'193'  ASSIGN PRIVATE CORE
ASSGN SYSLST,X'02E'
ASSGN SYSPCH,X'02D'
ASSGN SYSLNK,X'193'  LINKEDIT WORK FILE
ASSGN SYS001,X'193'  COMPILER / LINK / SORT WORK FILES
ASSGN SYS002,X'193'  COMPILER / SORT WORK FILES
ASSGN SYS003,X'193'  COMPILER / SORT WORK FILES
ASSGN SYS004,X'193'  COMPILER / SORT WORK FILES
/&
* $$ EOJ
