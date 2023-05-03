//RODDI01A   JOB (40600000),CLASS=A,MSGCLASS=X                          00010000
//**************************************************************        00020000
//* DELETE IF OUTPUT DATASET EXISTS                            *        00030000
//**************************************************************        00040000
//STEP0100 EXEC PGM=IEFBR14                                             00050000
//DD01     DD  DSN=&SYSUID..SDSF.OUTPUT,                                00060000
//             DISP=(MOD,DELETE),UNIT=SYSDA,                            00070000
//             SPACE=(TRK,(1,0),RLSE)                                   00080000
//*                                                                     00090000
//**************************************************************        00100000
//* CANCEL ALL JOBS IN DA FOR PREFIX 'ACLA2*'                  *        00110000
//* USING SDSF BLOCK COMMANDS (//C - //)                       *        00120000
//**************************************************************        00130000
//STEP0200 EXEC PGM=SDSF,PARM='++24,80'                                 00140000
//ISFOUT   DD SYSOUT=*                                                  00150000
//SYSUDUMP DD SYSOUT=*                                                  00160000
//SYSPRINT DD SYSOUT=*                                                  00170000
//SYSTSPRT DD SYSOUT=*                                                  00180000
//SYSOUT   DD SYSOUT=*                                                  00190000
//DATAOUT  DD DSN=&SYSUID..SDSF.OUTPUT,                                 00200000
//            DISP=(NEW,CATLG,DELETE),                                  00210000
//            SPACE=(CYL,(25,25),RLSE),                                 00220000
//            RECFM=FB,LRECL=133,BLKSIZE=0                              00230000
//ISFIN    DD *                                                         00240000
 SET CONFIRM OFF                                                        00250000
 PRE GSV2*                                                              00260000
 H                                                                      00270000
 FIND 'GSV2'                                                            00280000
 ++//P                                                                  00290000
 FIND 'GSV2' LAST                                                       00300000
 ++//                                                                   00310000
 END                                                                    00320000
//*                                                                     00330000
//                                                                      00340000
PRINT FILE DATAOUT                                                      00350000
PRINT CLOSE                                                             00360000
