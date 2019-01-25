//REPLSTUB  JOB 'REXX','REPLIKSTUB',CLASS=T,REGION=0M,                  JOB09909
//        MSGLEVEL=(1,1),MSGCLASS=A,NOTIFY=&SYSUID
//**********************************************************************
//STEP01    EXEC  PGM=IKJEFT1B
//**********************************************************************
//SYSEXEC  DD  DISP=SHR,DSN=DES.XXX.JOBLIB.REXX
//*
//SYSOUT   DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//*
//*********************************************************************
//* Replicador Stub (ONLINE, BATCH)                                   *
//*********************************************************************
//* Parametros de entrada:                                            *
//* - Subrotina                         (XXXS99)                      *
//* - Usercode                          (XXX999A)                     *
//* - Ambiente a qual se destina a stub (DES,HML,PRD)                 *
//*********************************************************************
//*
//SYSTSIN     DD  *
  %REPLSTU  CDFS01 CDF001P BRB12
/*
//
