/* REXX */
/* =========================================================== */
/* ESTA REXX TEM POR OBJETIVO REPLICAR CODIGOS FONTES DE STUBS */
/* EM UM ARQUIVO SEQUENCIAL. A PARTIR DE UM MODELO BASE        */
/* PARAMETRIZADO COM ESTA FINALIDADE.                          */
/* =========================================================== */
/* PARAMETROS:                                                 */
/* SUBROTINA = A SUBROTINA A QUAL SERAH CRIADA A STUB          */
/* USERCODE  = USERCODE DO SISTEMA DESTA SUBROTINA             */
/* AMBIENTE DE RESIDENCIA = QUE EH O AMBIENTE DESTINO DA STUB  */
/* =========================================================== */
/* EX:ABCS01 ABC201D DES                                       */
/* =========================================================== */
/* O MODELOS ATUAIS SE ENCONTRAM NO DSN PREFIX (USER-RADILSON) */
/* ==> USER.DES.SIS.SRCLIB(XXXS99SC)                           */
/* ==> USER.DES.SIS.SRCLIB(XXXS99ST)                           */
/* =========================================================== */
/* A ALOCACAO DOS FONTES EH FEITA EM UM ARQUIVO SEQUENCIAL,ONDE*/
/* ==> XXX.SIS.STUB.XXXS99SC.AMBIENTE                          */
/* ==> XXX.SIS.STUB.XXXS99ST.AMBIENTE                          */
/* Autor: Joao Luis                                            */
/* =========================================================== */
 Parse Arg Sbrot User Amb
 
/* Configuracoes Iniciais ---------------------------------- */

Srot    = Substr(Sbrot,1,6)
Sistema = Substr(Srot,1,3)
User    = Substr(User,1,7)
Ambiente= Substr(Amb,1,5)
Prefixo = Substr(Srot,3,1)||'S'||Substr(Srot,5,2)||'01'
SL      = Substr(Srot,1,4)||'L'||Substr(Srot,5,2)
Obj1    = Substr(Srot,1,2)||'/'||Substr(Srot,3,1)
Obj2    = '/SC/'||Substr(Srot,5,2)
PC      = Obj1||Obj2

/* Os Stubs serao criados a partir das replicas a seguir:    */
MDLSC   = 'USER.DES.SIS.SRCLIB(XXXS99SC)'
MDLST   = 'USER.DES.SIS.SRCLIB(XXXS99ST)'

/* Nome DSN                                                  */
DSNSC   = 'DES.SIS.SRCLIB.'||Srot||'SC.'||Ambiente
DSNST   = 'DES.SIS.SRCLIB.'||Srot||'ST.'||Ambiente

Select
   When Amb        = 'DES' then
        Do
          IP       = "'10.1.0.010.'."
          ServPort = 08099
          Direcao  = 'UnisysDES'
        End

   When Amb        = 'HML' then
        Do
          IP       = "'10.1.0.020.'."
          ServPort = 09099
          Direcao  = 'UnisysHML'
        End

   When Amb        = 'PRD' then
        Do
          IP       = "'10.1.0.030.'."
          ServPort = 10099
          Direcao  = 'UnisysPRD'
        End

   Otherwise
        Do
          IP       =  "'10.1.0.010.'."
          say '*==================================================*'
          say '!!!!!! Não foi informado nos parametros Entrada!!!!!'
          say '!!!!!!   É necessário alterar no cod.Fonte     !!!!!'
          say '!!!!!!          (MANUALMENTE)                  !!!!!'
          say '*==================================================*'
        End
END

Say ''
Say ''
Say '*---------------------------------------------------------*'
Say '*--             Parametros Carregados                     *'
Say '*---------------------------------------------------------*'
Say 'Sistema            : ' Sistema
Say 'Nome Subrotina     : ' Srot
Say 'USERCODE           : ' User
Say 'Ambiente Destino   : ' Ambiente
Say 'Ambiente Direcao   : ' Direcao
Say 'Libary  Name       : ' SL
Say 'Libary  Prefixo    : ' Prefixo
Say 'Nome Obj Unisys    : ' PC
SAY 'IP                 : ' IP
SAY 'ServPort           : ' ServPort
Say '*---------------------------------------------------------*'
Say 'Nome Fonte Online  : ' DSNSC
Say 'Nome Fonte Batch   : ' DSNST
Say '*---------------------------------------------------------*'

/* =========================================================== */
/*                 INICIO REPLICA ONLINE                       */
/* =========================================================== */

/* Testa Existencia do modelo fonte (ONLINE) */
Call TestExist MDLSC


/* Le Membro a ser copiado - fonte    (Online)     */
Call AllocDsn MDLSC


Say ' '
say '======================================================='
say 'Linhas com Alteracao (ONLINE): ' DSNSC
say '======================================================='
x    = 0
Resp = 1
SrotXX    = 'XXXS99'
PrefixoXX = 'XS9901'
SLXX      = 'XXXSL99'
PCXX      = 'XX/X/SC/99'
UserXX    = 'USERCOD'
PortXX    = 08811

Do until x = RegR.0

   Do until Resp = 0
      CALL TestaNomeSrot
   End

   Do until Resp = 0
      CALL TestaPrefixo
   End

   Do until Resp = 0
      CALL TestaLibSubrotina
   End

   x = x + 1

End

say '*-------------- Parametros Especificos ----------------*'

/* Linha do fonte onde se encontra SERVER-PORT   */
x = 161
CALL PortConfig

/* Linha do fonte onde se encontra o IP          */
x = 170
CALL IpConfig

/* Linha do fonte onde se encontra o Nome Unisys */
x = 525
CALL NmUnisysConfig

/* Linha do fonte onde se encontra o UserCode    */
x = 526
CALL UserConfig

say '*-------------- Parametros Especificos ----------------*'

/* Cria arquivo em branco para ONLINE            */
CALL CriaNovo DSNSC

/* Escreve Replica no arquivo criado             */
Call EscreveFonte DSNSC

/* =========================================================== */
/*                 INICIO REPLICA BATCH                        */
/* =========================================================== */

/* Testa Existencia do modelo fonte (BATCH)  */
Call TestExist MDLST

/* Le Membro a ser copiado - fonte    (BATCH)     */
Call AllocDsn MDLST

Say ' '
say '======================================================='
say 'Linhas com Alteracao (BATCH): ' DSNST
say '======================================================='
x    = 0
Resp = 1
SrotXX    = 'XXXS99'
PrefixoXX = 'XS9901'
SLXX      = 'XXXSL99'
PCXX      = 'XX/X/SC/99'
UserXX    = 'USERCOD'
PortXX    = 08811

Do until x = RegR.0

   Do until Resp = 0
      CALL TestaNomeSrot
   End

   Do until Resp = 0
      CALL TestaPrefixo
   End

   Do until Resp = 0
      CALL TestaLibSubrotina
   End

   x = x + 1

End

say '*-------------- Parametros Especificos ----------------*'

/* Linha do fonte onde se encontra SERVER-PORT   */
x = 155
CALL PortConfig

/* Linha do fonte onde se encontra o IP          */
x = 164
CALL IpConfig

/* Linha do fonte onde se encontra o Nome Unisys */
x = 502
CALL NmUnisysConfig

/* Linha do fonte onde se encontra o UserCode    */
x = 503
CALL UserConfig

say '*-------------- Parametros Especificos ----------------*'

/* Cria arquivo em branco para BATCH             */
CALL CriaNovo DSNST

/* Escreve Replica no arquivo criado             */
Call EscreveFonte DSNST
 Say ' '
 Say '*--------------------------------------------------------*'
 Say '*              EXECUTADO  COM SUCESSO!                   *'
 Say '*--------------------------------------------------------*'

/* ===============  FUNCOES DO PROGRAMA ================ */
/*----------- TestaNomeSrot  -------------------------- */
 TestaNomeSrot:
       Resp = index(regR.x,SrotXX)
       If Resp > 0 Then 
          Do
                 FraseTam = length(regR.x)
                 Ponteiro1 = Resp - 1
                 PalavraTam = length(SrotXX)
   
                 Parte1 = left(regR.x,Ponteiro1)
                 TamP2  = FraseTam - (  Ponteiro1 + PalavraTam )
                 Parte2 = right(regR.x,TamP2)
                 regR.x = Parte1||Srot||Parte2
                 Say x||' - '||regR.x||'.'
          End
 RETURN

/*--------- TestaPrefixo  -------------------------- */
 TestaPrefixo:

       Resp  = index(regR.x,PrefixoXX)
       If Resp > 0 Then 
          Do
             FraseTam    = length(regR.x)
             Ponteiro1   = Resp - 1
             PalavraTam  = length(PrefixoXX)
   
             Parte1    = left(regR.x,ponteiro1)
             TamP2     = FraseTam - (  Ponteiro1 + PalavraTam )
             Parte2    = right(regR.x,TamP2)
             regR.x    = Parte1||Prefixo||Parte2
             Say x||' - '||regR.x
          End

 RETURN

/* --------- TestaLibSubrotina  -------------------------- */
 TestaLibSubrotina:

       Resp  = index(regR.x,SLXX)
       If Resp > 0 Then 
          Do
             FraseTam    = length(regR.x)
             Ponteiro1   = Resp - 1
             PalavraTam  = length(SLXX)
   
             Parte1    = left(regR.x,ponteiro1)
             TamP2     = FraseTam - (  Ponteiro1 + PalavraTam )
             Parte2    = right(regR.x,TamP2)
             regR.x    = Parte1||SL||Parte2
             Say x||' - '||regR.x
          End

 RETURN

/* --------- NmUnisysConfig ------------------------------------ */
 NmUnisysConfig:

       srchObjUni = index(regR.x,PCXX)
       If srchObjUni > 0 Then 
          Do
             FraseTam = length(regR.x)
             Ponteiro1 = srchObjUni - 1
             PalavraTam = length(PCXX)
   
             Parte1 = left(regR.x,ponteiro1)
             TamP2 = FraseTam - (  Ponteiro1 + PalavraTam )
             Parte2 = right(regR.x,TamP2)
             regR.x = Parte1||PC||Parte2
             Say x||' - '||regR.x
          End

 RETURN

/* --------- IpConfig ------------------------------------ */
 IpConfig:

    regR.x = '                                           '||IP
    Say x||' - '||regR.x

 RETURN

/* --------- PortConfig ------------------------------------ */
 PortConfig:

    If Amb \= 'DES' then 
       Do
          Resp = index(regR.x,PortXX)
          If Resp > 0 Then 
             Do
                FraseTam = length(regR.x)
                Ponteiro1 = Resp - 1
                PalavraTam = length(PortXX)
      
                Parte1 = left(regR.x,ponteiro1)
                TamP2 = FraseTam - (  Ponteiro1 + PalavraTam )
                Parte2 = right(regR.x,TamP2)
                regR.x = Parte1||ServPort||Parte2
             End
    End

    Say x||' - Server Port: '||ServPort

 RETURN

/* --------- UserConfig ------------------------------------ */
UserConfig:

      Resp = index(regR.x,UserXX)
      If Resp > 0 Then 
         Do
            FraseTam = length(regR.x)
            Ponteiro1 = Resp - 1
            PalavraTam = length(UserXX)
   
            Parte1 = left(regR.x,ponteiro1)
            TamP2 = FraseTam - (  Ponteiro1 + PalavraTam )
            Parte2 = right(regR.x,TamP2)
            regR.x = Parte1||User||Parte2
            Say x||' - '||regR.x
         End
RETURN

/*--- TESTA EXISTENCIA DO MEMBRO A SER COPIADO ------------*/
TestExist:
arg   CodFonte

Address TSO "PROFILE NOPREFIX"
EXISTS = SYSDSN(CodFonte)
RC = 0

If EXISTS \= 'OK' then
   Do
     say ' ========== EXECUCAO ABORTADA! ============= '
     say ' '
     say ' MEMBRO '||CodFonte||' NAO EXISTE NO DIRETORIO'
     say ' '
     say ' ========== EXECUCAO ABORTADA! ============= '
     RC = 4
     EXIT(RC)
   End
RETURN

/* -------  ALLOCA MEMBRO A SER COPIADO ---------------- */
AllocDsn:
arg DsnFonte

Say ' '
"ALLOCATE DA("DsnFonte") FI(COPIA) SHR REUSE"
If RC == 0 then "EXECIO * DISKR COPIA (STEM RegR. FINIS"
   IF  RC >  0 then 
     Do
       Say 'ARQUIVO NAO ALOCADO! (ERRO) :' DsnFonte
       Exit(12)
     End

RETURN

/* ----- ALLOCA NOVO ARQUIVO PARA SAIDA ---------------  */
CriaNovo:
arg NovoArq

Say ' '
RC = ALLOCNEW(NovoArq,'NOVOARQ',10,50,'TRACKS','PS','F B','080')
   If RC \=0 then 
      do
         say ' ERRO AO ALOCAR ARQUIVO DSN : '||NovoArq
         EXIT(12)
      End
   If RC == 0 then
      do
         Say '- CRIADO ARQUIVO DSN: '||NovoArq
         "FREE FI(NOVOARQ)"
      End

RETURN

/* ----- Escreve a replica do programa origem ------- */
EscreveFonte:
arg DsnFonte

Say ' '
"ALLOCATE DA("DsnFonte") FILE(COPIAW) SHR REUSE"
IF RC == 0 THEN "EXECIO * DISKW COPIAW (STEM REGR. FINIS"
 IF  RC >  0 THEN 
   DO
      SAY 'ARQUIVO NAO ALOCADO! (ERRO) : ' DsnFonte
      EXIT(12)
   END

RETURN
/* =====================    F I M    ========================= */

EXIT(0)
