//RODDI01X JOB (C36C2,CHM00,OSN,'VPCHM'),'HERRA.-PROD',
//         MSGLEVEL=(1,1),              CMNEX008
//         MSGCLASS=X
//*------------------------------------------------------
//*------------------------------------------------------
//STEP1  EXEC PGM=IDCAMS
//*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
  DEFINE CLUSTER (NAME(RODDI01.COPY.KSDS)      -
  INDEXED                                 -
  KEYS(6 1)                               -
  RECSZ(80 80)                            -
  TRACKS(1,1)                             -
  CISZ(4096)                              -
  FREESPACE(3 3) )                        -
  DATA (NAME(RODDI01.COPY.KSDS.DATA))          -
  INDEX (NAME(RODDI01.COPY.KSDS.INDEX))
/*
//* another comment
//SERXMLBC EXEC PGM=SERXMLBC,COND=(4,LT)
//STEPLIB  DD DISP=SHR,DSN=CHM.ZMF.LOAD
//         DD DISP=SHR,DSN=CHM.COMC.LOAD
//SERQPARM DD DISP=SHR,DSN=CHM.TCPIPORT
//SYSPRINT DD SYSOUT=*
//SERPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//XMLOUT   DD SYSOUT=*
//SYSOUT   DD *
//XMLIN    DD *
<?xml version='1.0'?>
<service name='PACKAGE'>
 <scope name='SERVICE'>
 <message name='PROMOTE'>
 <header>
 <subsys>8</subsys>
 <product>CMN</product>
 </header>
 <request>
 <package>ABCD000010</package>
 <promotionSiteName>SERT8</promotionSiteName>
 <promotionLevel>10</promotionLevel>
 <promotionName>C001AUT</promotionName>
 <jobCards01>//XMLX029B JOB (RWM,T),DUMP,CLASS=A,MSGCLASS=A</jobCard01>
 <jobCards02>//* JOBCARD2</jobCard02>
 </request>
 </message>
 </scope>
</service>


