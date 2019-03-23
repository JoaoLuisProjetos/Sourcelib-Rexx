/* REXX **/
/* **************************************************************** * */
/* *Nome           : TestaArq                                       * */
/* *Uso: Testa existencia de um arquivo. Caso o arquivo não exista  * */ 
/*       retorna erro (99)                                          * */
/* *Compatibilidade: WINDOWS REXX                                   * */
/* *Entradas       : Arq = Arquivo que sera verificada a Existencia * */
/* *TipoPrograma   : Subrotina                                      * */
/* **************************************************************** * */

TestaArq:
Parse arg Arq

SAY '*------*'
SAY 'TestaArq'
SAY '*------*'

RC_AbreArqMsg  =  STREAM(Arq,'C','OPEN READ')
IF RC_AbreArqMsg <> 'READY:' THEN DO
   Say '**************************************************************'
   SAY ''
   SAY 'CANCELAMENTO : Arquivo nao encontrado:' Arq
   SAY ''   
   Say '**************************************************************'
   EXIT(99)
END
ELSE DO 
   SAY 'MSG - Arquivo de uso :' Arq  ' Existe!' 
end
SAY '*-------------------------------------------------------------*'

Return(0)