//RODDI01A JOB (V1),TIGERTEAM,
//         CLASS=B,
//         MSGCLASS=X
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD   *
    DEFINE CLUSTER -
       (NAME (Name) -
       STORAGECLASS (StorageClass) -
       MANAGEMENTCLASS (ManagementClass) -
       DATACLASS (DataClass))
/*
//STEP2    EXEC PGM=IEBCOPY
//SYSIN    DD   SYSIN
//SYSPRINT DD   SYSPRINT
//SYSUT1   DD   DSN=DATA.ONE,DISP=(SHR)
//SYSUT2   DD   DSN=DATA.TWO,DISP=(SHR)
//*
//StepName EXEC PGM=IDCAMS
//INPUT    DD   DSN=DataSetName,DISP=SHR,DCB=(DataControlBlock)
//SYSPRINT DD   SYSOUT=SYSOUT
//SYSIN    DD   *
    REPRO -
       INFILE (Input) -
       OUTDATASET (OutDataSet) -
       ERRORLIMIT (ErrorLimit)
/*
