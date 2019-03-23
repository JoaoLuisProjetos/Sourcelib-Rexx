/* REX */
/* **************************************************************** ** */
/* *Nome           : TestaArqSaida                                  ** */
/* *Uso : Testa existencia de um arquivo. Caso o arquivo jah        ** */
/*        exista, serah deletado                                    ** */
/* *Compatibilidade: WINDOWS REXX                                   ** */
/* *Entradas       : Arq = Arquivo que sera verificada a Existencia ** */
/* *TipoPrograma   : Subrotina                                      ** */
/* **************************************************************** ** */

TestaArqSaida:
Parse arg ., Arq
SAY 'TestaSaida:' Arq

RCode = STREAM(Arq, 'C', 'OPEN READ')
say RCode
IF RCode = 'READY:' THEN DO
   Call Stream Arq, 'C', 'CLOSE' 
   say 'MSG - Arquivosaida existe e sera deletado:' Arq
   Address SYSTEM 'del' Arq
   CALL  STREAM Arq,'C','OPEN WRITE'
END
ELSE Call Stream Arq, 'C', 'OPEN WRITE'

SAY '*-------------------------------------------------------------*'
Return

