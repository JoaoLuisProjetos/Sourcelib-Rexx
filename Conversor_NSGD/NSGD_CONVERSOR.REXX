/**/
OPTIONS SINGLE_INTERPRETER
Call RxFuncAdd 'DW_LoadFuncs', 'rexxdw', 'DW_LoadFuncs'
Call dw_loadfuncs
 SAY '    *=================================================================='
 SAY '    *=           @@    @@   @@@@@@    @@@@@@   @@@@@@@              ==='
 SAY '    *=           @@@   @@  @@    @@  @@    @@  @@    @@             ==='
 SAY '    *=           @@@@  @@  @@        @@        @@    @@             ==='
 SAY '    *=           @@ @@ @@   @@@@@@   @@  @@@@  @@    @@             ==='
 SAY '    *=           @@  @@@@        @@  @@    @@  @@    @@             ==='
 SAY '    *=           @@   @@@  @@    @@  @@    @@  @@    @@             ==='
 SAY '    *=           @@    @@   @@@@@@    @@@@@@   @@@@@@@              ==='
 SAY '    *=================================================================='
Say '     - insira o nome/extensao do arquivo:'
parse pull ArqIn

  /* Trace  a */
/*
 * Depending on platform, set up SliderSpinner constants
 */
Parse Version ver
Parse Source os . prog
lp = Lastpos( !REXXDW.!DIRSEP, prog )
icondir = Substr( prog, 1, lp )

 /* Regina */
Do
   !global.!fixedfont = "20.Terminal"
   !global.!icondir = icondir'img\'
   rexxutil = 'rexxutil'
   !global.!root = 'd:\rexxdw'
   enable_explorer = 0
   enable_rendering = 1
   !global.!widgetheight = 20
End

!global.!foldericon = dw_icon_load_from_file( !global.!icondir'folder' );
!global.!fileicon = dw_icon_load_from_file( !global.!icondir'file'  );

/*
 * Check if rexxutil already loaded...
 */
If RxFuncQuery( 'sysfiletree' ) \= 0 Then
   Do
      If RxFuncAdd( 'sysloadfuncs', rexxutil, 'sysloadfuncs' ) \= 0 Then
         Do
            If RxFuncAdd( 'SysLoadFuncs', rexxutil, 'SysLoadFuncs' ) \= 0 Then
               Do
                  enable_explorer = 0
               End
            Else enable_explorer = 1
         End
      Else enable_explorer = 1
   End
Else
   enable_explorer = 1

If enable_explorer Then Call SysLoadFuncs

!globalv = '!REXXDW. !global. ind. items. !dialog. Fonte. f MarcaArray. Array. ArqLog ArqIn'

Call dw_init

!global.!allowcallbacks = 0

!global.!screen_width = dw_screen_width()
!global.!screen_height = dw_screen_height()

!global.!style = dw_or( !REXXDW.!DW_FCF_SYSMENU, !REXXDW.!DW_FCF_TITLEBAR, !REXXDW.!DW_FCF_SHELLPOSITION, !REXXDW.!DW_FCF_TASKLIST, !REXXDW.!DW_FCF_DLGBORDER )

/* create our toplevel window */
!global.!mainwindow = dw_window_new( !REXXDW.!DW_DESKTOP, 'Analisador  - NSGD                                                                                                          - JoaoLuis' , dw_or( !global.!style, !REXXDW.!DW_FCF_SIZEBORDER, !REXXDW.!DW_FCF_MINMAX ) )

notebookbox = dw_box_new( !REXXDW.!DW_VERT )
Call dw_box_pack_start !global.!mainwindow, notebookbox, 0, 0, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 0

notebook = dw_notebook_new( 0, !REXXDW.!DW_TAB_TO_BOTTOM )
Call dw_box_pack_start notebookbox, notebook, 100, 100, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 5

Call dw_signal_connect !global.!mainwindow, !REXXDW.!DW_EXPOSE_EVENT, 'MainExposeCallback'
Call dw_signal_connect !global.!mainwindow, !REXXDW.!DW_CONFIGURE_EVENT, 'MainConfigureCallback'

tb = dw_taskbar_insert( !global.!mainwindow, !global.!foldericon, 'Rexx/DW Demo', 'TaskbarMenuCallback', 'arg1', 'arg2' )
/*
 * SliderSpinner:
 *   box, groupbox, button, bitmapbutton, slider/percent, radiobutton/checkbutton
 * Container: (explorer)
 *   container, tree, popup menu, splitbar, mle
 * Render: (display file selected in Container)
 *   render, pixmap
 */
pages = 'Seleciona_NSGD'

If \enable_rendering Then pages = Delword( pages, 3, 1 )
If \enable_explorer Then pages = Delword( pages, 2, 1 )


Do i = 1 To Words( pages )
   Call CreatePage notebook, Word( pages, i ), i
End

Call dw_signal_connect notebook, !REXXDW.!DW_SWITCH_PAGE_EVENT, 'SwitchPageCallback'
Call dw_notebook_page_set notebook, !global.!notebookpage.1
Call dw_signal_connect !global.!mainwindow, !REXXDW.!DW_DELETE_EVENT, 'QuitCallback', !global.!mainwindow




/* CONFIGURA TAMANHO  JANELA PRINCIPAL */

Call dw_window_set_pos_size !global.!mainwindow, 0, 600, 1450, 250
Call dw_window_show !global.!mainwindow


/*
 * The following is the main event loop for this program. We check the
 * variable !REXXDW.!HAVE_REXXCALLBACK to determine which mechanism
 * can be used for callbacks. It is far better to use the mechanism
 * offered by an interpreter that offers RexxCallBack() in its API.
 */

!global.!allowcallbacks = 1

If !REXXDW.!HAVE_REXXCALLBACK Then
   Do
      Call dw_main
   End
Else
   Do Forever
      cmd = dw_main_iteration()
      If cmd \= '' Then
         Do
            Interpret 'Call' cmd
         End
   End
Call dw_taskbar_delete tb, !global.!foldericon
Call dw_window_destroy !global.!mainwindow
Call dw_shutdown
Exit 0

Return 0

Call dw_signal_connect splashwindow, !REXXDW.!DW_DELETE_EVENT, 'GenericCloseCallback', splashwindow

CreatePage: Procedure Expose (!globalv)
Parse Arg notebook, page, idx
func = Translate( page, '', '/' )
!global.!notebookbox.idx = dw_box_new( !REXXDW.!DW_VERT )
!global.!notebookpage.idx = dw_notebook_page_new( notebook, 0, !REXXDW.!DW_PAGE_TO_BACK )
Call dw_notebook_pack notebook, !global.!notebookpage.idx, !global.!notebookbox.idx
Call dw_notebook_page_set_text notebook, !global.!notebookpage.idx, page
Interpret 'Call' func idx
Return

/*
 * Display the splash window
 */
Call dw_window_set_pos_size splashwindow, (!global.!screen_width % 2) - (window_x % 2), (!global.!screen_height % 2) - (window_y % 2), window_x, window_y
Call dw_window_show splashwindow
Return 0


GenericCloseCallback: Procedure Expose (!globalv)
If !global.!allowcallbacks = 0 Then Return 1
Parse Arg ., window, dialog
Call dw_window_destroy window
If dialog \= '' Then Call dw_dialog_dismiss dialog, 'close'
Return 0

QuitCallback:
If !global.!allowcallbacks = 0 Then Return 1
Call dw_main_quit
Return 0


Seleciona_NSGD: Procedure Expose (!globalv)
Parse Arg idx
mainbox = dw_box_new( !REXXDW.!DW_HORZ )
Call dw_box_pack_start !global.!notebookbox.idx, mainbox, 0, 0, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 5

selectorbox = dw_groupbox_new( !REXXDW.!DW_VERT, 'Conversor - NSGD' )
Call dw_box_pack_start mainbox, selectorbox, 0, 0, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 2

!global.!ls = dw_listbox_selector_new( 'Linhas Selecionadas', 'Linhas Identificadas  (NUM.LINHA/In-Out/Fonte)', 100, 200 )
Call dw_box_pack_start selectorbox, !global.!ls, 0, 0, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 0

/* *********************************************************************** */
/* *********              B R I E F I N G S                     ********** */
/* *********************************************************************** */
/*                          DATA LEVELS                                    */
/*               * ************************************** *                */
/*               * 1        - Elementar Group Item        *                */
/*               * 02 - 49  - Subelementar Group Item     *                */
/*               * 66       - Renames                     *                */
/*               * 77       - Indivisivel                 *                */
/*               * 88       - Boolean                     *                */
/*               * ************************************** *                */
/*             * ************************************************ *        */
/*             * Array = Variavel que guarda arrays de elementros *        */
/*             * ************************************************ *        */
/*             * Array.(0)     = Guarda QTD IO                    *        */ 
/*             * Array.0.(0)   = Guarda Reg 01                    *        */ 
/*             * ************************************************ *        */
/*             * Array.0.0.(0) = Guarda Subregistros do Reg 01    *        */ 
/*             * ************************************************ *        */
/* *********************************************************************** */

/* ***************************************************************************** */
/* **********    C O N F I G U R A C O E S   I N I C I A I S          ********** */
/* ***************************************************************************** */
TipoPrograma = ''
IF POS('PO',ArqIn) > 0 then  TipoPrograma = 'ONLINE'
IF POS('PB',ArqIn) > 0 then  TipoPrograma = 'BATCH'
IF TipoPrograma = ''   then TipoPrograma = 'BOOK'


Root           =  Directory()
if TipoPrograma = 'ONLINE' then do
    RootEndevor    =  Directory()  || '\Fontes_ENDEVOR_ONLINES\'
    RootConvertido =  Directory()  || '\Fontes_CONVERTIDOS_ONLINES\'
end
if TipoPrograma = 'BATCH' then do
    RootEndevor    =  Directory()  || '\Fontes_ENDEVOR_BATCH\'
    RootConvertido =  Directory()  || '\Fontes_CONVERTIDOS_BATCH\'
end
if TipoPrograma = 'BOOK' then do
    RootEndevor    =  Directory()  || '\Fontes_ENDEVOR_BOOK\'
    RootConvertido =  Directory()  || '\Fontes_CONVERTIDOS_BOOK\'
end


ArqEnt         = RootEndevor || ArqIn || '.cbl'
Call TestaArq  ArqEnt 

ArqLog        =  Directory()  || '\ANALISE\' || ArqIn || '_RESUMO.cbl'
Address system 'del' ArqLog

Call TestaArq  ArqEnt  

Fim_Arq       = 'N'
totNulls      = 0
CP            = 0
INC           = 0
f             = 0
io            = 0
FasePrograma  = ''
LastIO        = ''
TipoIO.       = ''
ArqTipo.      = ''
ArqTam.       = ''
TipoArq.      = ''
Registro.     = 0

/* ***************************************************************************** */
/* **********     C O L O C A   F O N T E   E M   M E M O R I A       ********** */
/* ***************************************************************************** */
SAY '#-----> FONTE EM MEMORIA  '
Do until Fim_Arq = 'S'
   f = f + 1   
   Fonte.f  = LINEIN(ArqEnt)   
   
   IF Fonte.f = '' THEN Do
      totNulls = totNulls + 1
   END 
   
   if POS('*NSGD',Fonte.f) > 0 THEN Convertido = 'SIM'
   /* trata codigo fonte baixados diretos do ENDEVOR */
   IF totNulls = 4 THEN Do
      totNulls = totNulls + 1
      f = f - 1
      Fim_Arq = 'S'
      LEAVE
   END     
   
   VerificaErro = subword(Fonte.f,2,1)
   if datatype(VerificaErro) = 'NUM',
   &  pos(VerificaErro, Fonte.f) > 9,
   &  pos(VerificaErro, Fonte.f) < 16 then do
      Fonte.f = OVERLAY( '      ', Fonte.f,10,6,)
   END
   
   if datatype(VerificaErro) = 'NUM',
   &  pos(VerificaErro, Fonte.f) > 9,
   &  pos(VerificaErro, Fonte.f) < 16 then do
      say '********************************************'
      say '**  FORAM ENCONTRADOS NUMERICOS EM        **'
      say '**       COLUNA DE COMENTARIO             **'
      say '**       VERIFIQUE O FONTE                **'
      say '********************************************'
      exit(99)
   end
End

say 'Linhas Total Arquivo: ' f

/* ***************************************************************************** */
/* **********               C A R R E G A   I - O                     ********** */
/* ***************************************************************************** */
SAY '#-----> CARREGA I-O  '
Do i = 1 to f
iF SUBSTR(Fonte.i,16,1) <> '*' then do /* Desconsidera Linhas comentadas */

   if pos('PROGRAM-ID.',Fonte.i) > 0 then  do 
      NmPrograma = strip(subword(Fonte.i,2,2), 'T', '.' )
      say NmPrograma
     if pos('PO',NmPrograma) > 0 then TipoPrograma = 'ONLINE'
     if pos('PB',NmPrograma) > 0 then TipoPrograma = 'BATCH'
     say TipoPrograma
   end

   Call Marcafase
   
   /* ************************************************ */
   /* ********    C A R R E G A   C O P Y (CP)   ***** */
   /* ************************************************ */
   IF POS('COPY',Fonte.i) > 0  THEN DO
      CP = CP + 1
      Copyes.CP = strip(subword(Fonte.i,3,1), 'T', '.' ) '-' FasePrograma 
   end
   
   /* ************************************************ */
   /* ********    C A R R E G A   INCLUDE (INC)  ***** */
   /* ************************************************ */
   IF POS('INCLUDE',Fonte.i) > 0  THEN DO
      INC = INC + 1
      Includes.INC = strip(subword(Fonte.i,5,1), 'T', '.' ) '-' FasePrograma 
   end
   
   /* ************************************************ */
   /* ********    CLASSIFICA (V-SAM)             ***** */
   /* ************************************************ */
   IF (FasePrograma = 'FILE-CONTROL') THEN DO
      IF POS('SELECT',Fonte.i) > 0 THEN DO
         IO = IO + 1
         Externo.IO = strip(subword(Fonte.i,6,1), 'T', '.' )
         if Externo.IO = '' then Externo.IO = strip(subword(Fonte.i,5,1), 'T', '.' )
         Interno.IO = strip(subword(Fonte.i,3,1), 'T', '.' )
         if  pos('.',Fonte.i) = 0 then do
            m = i 
            do until pos('.',Fonte.m) > 0
               m = m + 1
               IF pos('INDEXED',Fonte.m) > 0 then  TipoArq.IO = 'V-SAM'
            end
         end
      end
   End

   /* ************************************************ */
   /* ********    CLASSIFICA (ARQ IN-OUT)        ***** */
   /* ************************************************ */
   if FasePrograma = 'PROCEDURE-DIVISION' then do 
      IF POS('OPEN',Fonte.i) > 0  & (POS('INPUT',Fonte.i) > 0 | POS('OUTPUT',Fonte.i) > 0  | POS('EXTEND',Fonte.i) > 0 )  then do 

           NWordPos = wordpos('OPEN',LEFT(Fonte.i,80)) +1
           NextWord = Translate(Word(LEFT(Fonte.i,80),NwordPos))
           say NextWord                                                 
                                                                        
           if (NextWord = 'INPUT' ,                                      
           |   NextWord = 'OUTPUT',                                      
           |   NextWord = 'EXTENDED') then LastIO = NextWord    
                                                                        
         /*  sAY ' lINHA io '  Fonte.i */
         
         ProxLinha = i
         FimComando = 'NAO'
         FimLinha   = 'NAO'
         /* Pega IO somente Desta Linha!!! */
         do m = 1 to IO
            if pos(Interno.m,Fonte.i) > 0  then TipoIO.m = LastIO
            if pos('.',Fonte.i) > 0        then FimComando = 'SIM'
        /*    say 'FimComando ' FimComando */
         end

         
         do until FimComando = 'SIM'
            ProxLinha = ProxLinha + 1
            
            if words(left(Fonte.ProxLinha,80)) = 1 then do
               ProxLinha = ProxLinha + 1 
             /*  say '+ linha' */
            end

            do until FimLinha = 'SIM'
            
                if  pos('.',Fonte.ProxLinha) > 0 then do
                    FimComando = 'SIM'
                    /*   say 'FimComando1' */
                    m = IO
                    n = qtd
                end  
                     
                FimLinha = 'NAO'
                qtd = words(left(Fonte.ProxLinha,80)) - 1
                Linha = subword(Fonte.ProxLinha,2,qtd)
                
                Procura = 'NOTFOUND'
                do n=1 to qtd

                    do m = 1 to IO
                        Palavra = strip(strip(subword(Linha,n,1),'B', '.' ),'B', ',' )
                        /*   say Palavra ' - ' Interno.m   'n:' n 'm:' m */
                        
                        if Palavra  = Interno.m then do
                           TipoIO.m = LastIO
                           Procura  = 'ENCONTROU'
                           LEAVE
                        END
                        if n = qtd then do    
                           FimLinha = 'SIM'
                           /*    Say 'fimlinha'   */
                        end
                        IF (Procura = 'NOTFOUND') & (m = IO) THEN Do
                           FimComando = 'SIM'
                           FimLinha = 'SIM'
                           m = IO
                           n = qtd
                           /*  say 'FimComando2'  */
                           leave
                        end
                        /* say 'Procura =' Procura m '-' IO  */
                    end
                end
            end
         end 
      end
   end
   
End
End 
Say '    +----->Total I-O:' IO

/* ***************************************************************************** */
/* **********           C A R R E G A   A R R A Y S  ( I - O )        ********** */
/* ***************************************************************************** */
 SAY '#-----> CARREGA ARRAYS DE I-O '
FasePrograma     = ''
Array.           = 0
WarArray.        = 0
Array.0.         = 0
WarArray.0.      = 0
Array.0.0.       = 0
Array.0.0.0      = 0
WarArray.0.0.    = 0
MarcaArray.0.0.0 = 0
Do i = 1 to f
iF SUBSTR(Fonte.i,16,1) <> '*' then do /* Desconsidera Linhas comentadas */

    Call Marcafase
    
    if FasePrograma = 'FILE-SECTION' then do 
        /* Seleciona Arquivos ou relatórios */
        If (translate(subword(Fonte.i,2,1)) = 'FD' | translate(subword(Fonte.i,2,1)) = 'SD') THEN do
            /* say ' NOVO'  Fonte.i  */
            do n = 1 to IO
                Array.n.  = 0
                
                if Interno.n = translate(subword(Fonte.i,3,1))  then do
                   Description = translate(subword(Fonte.i,2,1))
                   m  = i
                   
                   FimDeclare = 'NAO'
                   do until FimDeclare = 'SIM' 
                       
                      
                       m = m + 1
                       /* say Fonte.m */
                       /* Classifica Sort */
                       if  Description = 'SD' then TipoIO.n = 'SORT'
                         
                       /* Pega tamanho do IO */
                       if pos('RECORD',Fonte.m) > 0 & pos('RECORDS',Fonte.m) =  0  then do
                         
                           RegVariavel = wordpos(' TO ',Fonte.m)
                           /* Pega tamanho variavel */
                           if RegVariavel > 0 then do
                              ArqTam.n = ''
                              ArqTam.n = word(Fonte.m,(RegVariavel - 1)) || ' TO ' word(Fonte.m,(RegVariavel + 1))
                           end
                           /* Pega tamanho fixo */
                           if RegVariavel = 0 then do
                            
                               do teste= 2 to words(left(Fonte.m,80))
                                  Regfixonum = strip(word(Fonte.m,teste),'T','.')
                                  if DATATYPE(Regfixonum) = num then ArqTam.n = Regfixonum
                               end
                           end
                           /*  Say ' ** ' TipoIO.n */
                           if  ArqTam.n = '132' then TipoArq.n = 'REL'
                           else                      TipoArq.n = 'ARQ'
                       end 
                       
                       if subword(Fonte.m,2,1) = '01' then FimDeclare = 'SIM' 
                      
                       /* Say 'm - linha : ' m    */
                       /* Say 'n - inout : ' n    */
                         
                       /* SAY ' entra  Guarda_Elemento: ' Interno.n     */
                       Call Guarda_Elemento ., m, n     /* m = linha fonte / n = io item */
                   end
                end
            end
        end
    END
   
End
End 

/* ***************************************************************************** */
/* **********           C A R R E G A   A R R A Y S    ( W S )        ********** */
/* ***************************************************************************** */
 SAY '#-----> CARREGA ARRAYS DA WS'
/* Trata Elementares de WORKING-STORAGE e FILE-SECTION */
Ln = 0
IO = IO + 1
/* SAY 'IO / N  ' IO '/' n  */
INTERNO.IO   = 'Working_Storage'
 /* IO = IO + 1
INTERNO.IO   = 'Linkage Section' */
Array.0      = IO 
FasePrograma = ''

/* Regula Onliunes */
if TipoPrograma = 'ONLINE' then n = 0

Do i = 1 to f
    iF SUBSTR(Fonte.i,16,1) <> '*' then do /* Desconsidera Linhas comentadas */
        Call Marcafase
            /* say 'Linha' i FasePrograma */
        IF FasePrograma = 'WORKING-STORAGE' then do
           n = n + 1
          
           /* say 'entrou GDA!!!!!!!!!!' n i Fonte.i */
           Call Guarda_Elemento  ., i, n
           Ln = Result
           i  = Ln
           /* SAY 'Linha Retorno  ' i */
           
        end
       
    End
End
Say '    +----->Total Elementos (WS):' Array.IO.0
 SAY '#-----> IMPRIME RELATORIO  '

/* ***************************************************************************** */
/* **********     M O N T A    R E L A T O R I O                      ********** */
/* ***************************************************************************** */

rclog=LINEOUT(ArqLog,'  +-----------------------------------------------------------+' )
rclog=LINEOUT(ArqLog,'  |                                                           |', )
rclog=LINEOUT(ArqLog,'  |     ANALIZE DE DADOS  ' STRIP(SUBWORD(NmPrograma,2,1),'T','.') ||'  'TIME(NORMAL)'                  |', )
rclog=LINEOUT(ArqLog,'  |                                                           |', )
rclog=LINEOUT(ArqLog,'  +-----------------------------------------------------------+' )
rclog=LINEOUT(ArqLog,'  +-----------------------------------------------------------+' )
rclog=LINEOUT(ArqLog,'  | COPIES DO PROGRAMA : 'CP  , )
rclog=LINEOUT(ArqLog,'  +-----------------------------------------------------------+' )

do z = 1 to CP
  rclog=LINEOUT(ArqLog,' - ' Copyes.z, )
  /* Say z ' - ' Copyes.z */
end

rclog=LINEOUT(ArqLog,'  *-----------------------------------------------------------+' )
rclog=LINEOUT(ArqLog,'  | INCLUDES DO PROGRAMA :' INC , )
rclog=LINEOUT(ArqLog,'  *-----------------------------------------------------------+' )

do z = 1 to INC
  rclog=LINEOUT(ArqLog,' - ' Includes.z, )
  /* Say z ' - ' Includes.z */
end

rclog=LINEOUT(ArqLog, '  +-----------------------------------------------------------+',)
rclog=LINEOUT(ArqLog, '  |                    Dados de IN-OUT                        |',)
rclog=LINEOUT(ArqLog, '  +-----------------------------------------------------------+',)
rclog=LINEOUT(ArqLog, '  | DD       | Nome     | Entrada | Tipo    | Tamanho   | Qtd |',)
rclog=LINEOUT(ArqLog, '  | NAME     | Interno  | Saida   | Arquivo | Declarado | Regs|',)
rclog=LINEOUT(ArqLog, '  +----------+----------+---------+---------+-----------------+',)

do z = 1 to IO

  If TipoIO.z  = '' Then TipoIO.z  = '???'
  If ArqTam.z  = '' Then ArqTam.z  = '???'
  If TipoArq.z = '' Then TipoArq.z = 'COPY'       

  if z < io then do 
     SAIDA =               '  |          |          |         |         |           |     |'
     SAIDA = OVERLAY(  z        , SAIDA, 1 , length(z), LEFT(z,1) )
     SAIDA = OVERLAY( Externo.z , SAIDA, 4 , length(Externo.z), LEFT(Externo.z,1) )
     SAIDA = OVERLAY( Interno.z , SAIDA, 15, length(Interno.z), LEFT(Externo.z,1) )
     SAIDA = OVERLAY( TipoIO.z  , SAIDA, 27, length(TipoIO.z) , LEFT(Externo.z,1) )
     SAIDA = OVERLAY( TipoArq.z , SAIDA, 37, length(TipoArq.z), LEFT(Externo.z,1) )
     SAIDA = OVERLAY( ArqTam.z  , SAIDA, 47, length(ArqTam.z) , LEFT(Externo.z,1) )
     SAIDA = OVERLAY( Array.z.0 , SAIDA, 59, length(Array.z.0), LEFT(Array.z.0,1) )
     
     if Array.z.0 = 0 then SAIDA = OVERLAY('OBS: Registros em COPYBOOK!!!',SAIDA, 65, 29,)
     if TipoArq.z = 'REL' then SAIDA = OVERLAY('OBS: Registros do Relatorio podem estar truncados!',SAIDA, 65, 50,)
     
     rclog=LINEOUT(ArqLog, SAIDA,)
  End
end

rclog=LINEOUT(ArqLog, '  +-----------------------------------------------------------+',)
rclog=LINEOUT(ArqLog, '  | Reg ELEMENTAR |        RELACIONADO         | ALERTAS      |',)
rclog=LINEOUT(ArqLog, '  +-----------------------------------------------------------+',)

do z = 1 to IO  
    SAIDA  =          '  |                                                           |'
    SAIDA2 = OVERLAY( Interno.z , SAIDA, 28 , length(Interno.z), LEFT(Externo.z,1) )
    
    rclog = LINEOUT(ArqLog, SAIDA2,)
    
    rclog=LINEOUT(ArqLog, '  +-----------------------------------------------------------+',)
    
    do x = 1 to Array.z.0
       displayVar = subword(Array.z.x,2,1) 
       SAIDA3 = OVERLAY(  x , SAIDA, 1, length(x), LEFT(x,1))
       SAIDA3 = OVERLAY( '|', SAIDA3, 48, 1, '|')
       SAIDA3 = OVERLAY(  displayVar, SAIDA3, 5, length(displayVar), LEFT(displayVar,1))
       if WarArray.z.x <> 0 then SAIDA3 = OVERLAY(  WarArray.z.x, SAIDA3, 50, length(WarArray.z.x), LEFT(WarArray.z.x,1))
       
       rclog = LINEOUT(ArqLog, SAIDA3,)
 /*      if WarArray.z.x <> 0 then say x '-' displayVar 'Subelementos: ' Array.z.x.0 ' WARNING: ' WarArray.z.x
       else                      say x '-' displayVar 'Subelementos: ' Array.z.x.0 */
       /*
       do t=1 to Array.z.x.0 
          say 'SubItem: 'Array.z.x.t ' - Marca: ' MarcaArray.z.x.t
       end  */
    end
rclog=LINEOUT(ArqLog, '  +-----------------------------------------------------------+',)
end

rclog=LINEOUT(ArqLog, '  |                                                           |',)
rclog=LINEOUT(ArqLog, '  +-----------------------------------------------------------+',)

Call Stream ArqLog, 'C', 'CLOSE'
 SAY '#-----> FIM RELATORIO  '
/* ******************************************** */
/* **    ABRIR RESUMO EM BLOCO DE NOTAS      ** */
/* ******************************************** */
ArqEnt
ArqLog
/********************************************** */

iF convertido =  'SIM' then do
   Say '**************************************************'
   Say '**************************************************'
   Say '* FONTE JAH CONVERTIDO MANUALMENTE ' STRIP(SUBWORD(NmPrograma,2,1),'T','.') 
   Say '**************************************************'
   Say '**************************************************'
   EXIT(0)
end


SAY '#-----> INICIA SELECAO - ARRAYS  '
/* ***************************************************************************** */
/* **********     S E L E C I O N A      E L E M E N T O S            ********** */
/* ***************************************************************************** */
y  = 0
FasePrograma = 'WORKING-STORAGE'
do z = 1 to IO
  do x = 1 to Array.z.0

    Call VerificaSelecao ., Array.z.x, 'WS' /*Fixo 'WS'*/
    If Result = 'S' then do
       Call Procura_Linha ., Array.z.x, MarcaArray.z.x
       y = y + 1
       items.y =  '(Linha:'||  Result ||')- |' || Interno.z  || '| ' Array.z.x
    end
    
    do t=1 to Array.z.x.0 
       Call VerificaSelecao ., Array.z.x.t, 'WS' /*Fixo 'WS'*/
       If Result = 'S' then do
          Call Procura_Linha ., Array.z.x.t, MarcaArray.z.x.t
          y = y + 1
          items.y =  '(Linha:'||  Result ||')- |' || Interno.z  || '| ' Array.z.x.t
          
       end
    end
  end
end
 SAY '#-----> INICIA SELECAO - FONTE  '
/* ***************************************************************************** */
/* **********     S E L E C I O N A      F O N T E                      ********** */
/* ***************************************************************************** */
FasePrograma = ''
Do i = 1 to f
   Call Marcafase
   
   If FasePrograma = 'PROCEDURE-DIVISION' then do 
      Call VerificaSelecao ., Fonte.i, 'PD' /*Fixo 'PD'*/
      If Result = 'S' then do
         Call Procura_Linha ., Fonte.i
         y = y + 1
         items.y =  '(Linha:'||  Result ||')- |PROCEDURE| ' Fonte.i
      end
   End

End
items.0 = y
ind. = 0
ind.0 = items.0

/* ***************************************************************************** */
/* **********              F I N A L I Z A             T E L A        ********** */
/* ***************************************************************************** */
 SAY '#-----> MONTA TELA  '
Call dw_listbox_selector_setup !global.!ls, 'items.', 'ind.'

tmpbox = dw_box_new( !REXXDW.!DW_HORZ )
Call dw_box_pack_start mainbox, tmpbox, 0, 0, !REXXDW.!DW_DONT_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 0

Call dw_box_pack_start tmpbox, 0, 0, !global.!widgetheight, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 0
abutton = dw_button_new( 'Converter ' left(subword(NmPrograma,2,1),8), 0 )
Call dw_box_pack_start tmpbox, abutton, 150, 50, !REXXDW.!DW_DONT_EXPAND_HORZ, !REXXDW.!DW_DONT_EXPAND_VERT, 0
Call dw_signal_connect abutton, !REXXDW.!DW_CLICKED_EVENT, 'SaySelectedCallback', RootConvertido, NmPrograma
Call dw_box_pack_start tmpbox, 0, 0, !global.!widgetheight, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_DONT_EXPAND_VERT, 0

FileBox = dw_box_new( !REXXDW.!DW_HORZ )
Call dw_box_pack_start tmpbox, FileBox, 0, 0, !REXXDW.!DW_EXPAND_HORZ, !REXXDW.!DW_EXPAND_VERT, 0

Return


/* ***************************************************************************** */
/* **********     C R I A   I N T E R V E N C A O   N O   F O N T E   ********** */
/* ***************************************************************************** */
SaySelectedCallback: Procedure Expose (!globalv)
 SAY '#-----> CONVERTE FONTE  '
 SAY '    *=================================================================='
 SAY '    *=                    LINHAS DE ALTERACAO                       ==='
 SAY '    *=================================================================='
If !global.!allowcallbacks = 0 Then Return 1
Parse arg ., RootConvertido, NmPrograma

ArqOUT = RootConvertido || left(subword(NmPrograma,2,1),8) || '.cbl'
Address system 'del' ArqOUT

/*  T2 =  length('(LINHA:xxx)-')  */
Intervencao. = ''
FaseF.       = ''
Fonte2.      = ''
qtdConversao = 0
Do i = 1 To ind.0
   If ind.i = 1 Then do 
      LinhaF      = Subword(items.i,1,1)
      TPalavra    = length(LinhaF)
      TPalavraFIM = length(LinhaF) - 2
      Palavra     = LEFT(LinhaF,TPalavraFIM)
      tamfim      = TPalavraFIM - 7
      Linha       = Right(Palavra,tamfim)
     /* say 'Linha' Linha Fonte.Linha */
      Intervencao.Linha = Fonte.Linha
      FaseF.Linha = Subword(items.i,2,1)
      
      qtdConversao = qtdConversao + 1
   end
End

f2=0
ComentAnt= '*         NSGD *'
Erros = 0
Do i = 1 To f
   IntervFim = 'NAO'
   if Intervencao.i <> '' then do
      LComentario = overlay(ComentAnt,Intervencao.i,1,length(ComentAnt))
      f2 = f2 + 1
      Fonte2.f2 = LComentario
      say '1-----' Fonte2.f2   
      f2 = f2 + 1
      
      if pos('|PROCEDURE|',FaseF.i) > 0 then do
         ID1    = '16'
         ID1FIM = '21'
         ID2    = '15'
         ID2FIM = '20'
         ID3    = '19'
         ID3FIM = '28'
         OP1    = '3'
         OP1FIM = '4'
         CC1    = '9'
         CC1FIM = '13'
         CC2    = '8'
         CC2FIM = '12'
         AG_OP_CC   = 'AG-OP-CC'
         WISOTAM    = 'WISO-002-TAM'
         WISOTAMFIM = 'WISO-103-TAM'
         WISOVAR1   = 'WISO-002-NUM-CONTA'
         WISOVARFIM1= 'WISO-103-CTA-CRED '
         WISOVAR2   = 'W016-002-NUM-CONTA'
         WISOVARFIM2= 'W016-103-CTA-CRED '
      end 
      else do
         AG_OP_CC    = 'AG-OP-CC'
         AG_OP_CCFIM = '24)'
         ID1    = '16)'
         ID1FIM = '21)'
         ID2    = '15)'
         ID2FIM = '20)'
         ID3    = '19)'
         ID3FIM = '28)'
         OP1    = '3)'
         OP1FIM = '4)'
         CC1    = '9)'
         CC1FIM = '13)'
         CC2    = '8)'
         CC2FIM = '12)'
      end
      
      /* Verifica AG-OP-CC  PIC 9(19) */
      if pos(AG_OP_CC,Intervencao.i) > 0 & pos(ID3,Intervencao.i) > 0,
      & (IntervFim = 'NAO')  then do
         say 'AG_OP_CC' Intervencao.i
         Call Substitui ., ID3, AG_OP_CCFIM, Intervencao.i
         IntervFim = 'SIM'
      end
      
      /* Verifica WISO-002-TAM */
      if pos(WISOTAM,Intervencao.i) > 0 & (IntervFim = 'NAO')  then do
         say 'TAM' Intervencao.i
         Call Substitui ., WISOTAM, WISOTAMFIM, Intervencao.i
         IntervFim = 'SIM'
      end
      
      /* Verifica W016-002-NUM-CONTA */
      if pos(WISOVAR2,Intervencao.i) > 0  & (IntervFim = 'NAO') then do
         say 'IsoVar' Intervencao.i
         Call Substitui ., WISOVAR2, WISOVARFIM2, Intervencao.i
         IntervFim = 'SIM'
      end
      /* Verifica WISO-002-NUM-CONTA */
      if pos(WISOVAR1,Intervencao.i) > 0  & (IntervFim = 'NAO') then do
         say 'IsoVar' Intervencao.i
         Call Substitui ., WISOVAR1, WISOVARFIM1, Intervencao.i
         IntervFim = 'SIM'
      end
      
      /* Verifica identificacao */
      if pos('ID',Intervencao.i) > 0  & (IntervFim = 'NAO') then do
         say 'ID' Intervencao.i
         select
           when pos(ID1,Intervencao.i) > 0 then Call Substitui ., ID1, ID1FIM, Intervencao.i
           when pos(ID2,Intervencao.i) > 0 then Call Substitui ., ID2, ID2FIM, Intervencao.i
           when pos(ID3,Intervencao.i) > 0 then Call Substitui ., ID3, ID3FIM, Intervencao.i
           otherwise NOP
         end
        IntervFim = 'SIM'
      end
      
      /* Verifica Operacao */
      if pos('OP',Intervencao.i) > 0  & (IntervFim = 'NAO') then do
         say 'OP' Intervencao.i
         select
           when pos(OP1,Intervencao.i) > 0   then Call Substitui ., OP1, OP1FIM, Intervencao.i
           when pos('999',Intervencao.i) > 0 then Call Substitui ., '999', '9999', Intervencao.i
           otherwise NOP
        end
        IntervFim = 'SIM'
      end
      
      /* Verifica Conta */
      if ( pos('CC',Intervencao.i)    > 0,
      |    pos('CT',Intervencao.i)    > 0,
      |    pos('CONTA',Intervencao.i) > 0) & (IntervFim = 'NAO') then do
         say 'CC' Intervencao.i
         select
           when pos(CC1,Intervencao.i) > 0 then Call Substitui ., CC1, CC1FIM, Intervencao.i
           when pos(CC2,Intervencao.i) > 0 then Call Substitui ., CC2, CC2FIM, Intervencao.i
           when pos('99999999',Intervencao.i) > 0  then Call Substitui ., '99999999', '999999999999', Intervencao.i
           when pos('999999999',Intervencao.i) > 0 then Call Substitui ., '999999999', '9999999999999', Intervencao.i
           otherwise NOP
        end
        IntervFim = 'SIM'
      end
      
      if Result = 'RESULT' then Do
         Intervencao.i = '         *---------  ERRO NA INTERVENCAO DESTA LINHA   -----*'
         Erros = Erros + 1
      end
      
      Fonte2.f2 = Result
     say '2-----' Fonte2.f2  
   end
   else do
       f2 = f2 + 1
       Fonte2.f2 = Fonte.i
   end
end

nulls = 0
Do i = 1 To f2
   
   if Fonte2.i = '' then nulls = nulls + 1
   if LENGTH(Fonte2.i) > 9 then do
      TLinha = LENGTH(Fonte2.i) - 9
      Fonte2.i = right(Fonte2.i,TLinha) 
   end
   
   if words(Fonte2.i) = 1 & LENGTH(Fonte2.i) < 9 then Fonte2.i = ''
   
   if nulls = 'OK' then rclog  = LINEOUT(ArqOUT, Fonte2.i, )
   if nulls = 3 then  nulls = 'OK'
   
   if i = f2 then do 
      rclog  = LINEOUT(ArqOUT,'      ', )
      rclog  = LINEOUT(ArqOUT,'      ****************************', )
      rclog  = LINEOUT(ArqOUT,'      * ERROS DE CONVERSAO:' Erros  , )
      rclog  = LINEOUT(ArqOUT,'      * LINHAS CONVERTIDAS:' qtdConversao  , )
      rclog  = LINEOUT(ArqOUT,'      ****************************', )
   end   
end 

Call Stream ArqOUT, 'C', 'CLOSE'
/* Abre programa convertido em bloco de notepad++ */
ArqOUT

 SAY '    *=================================================================='
 SAY '    *=                    FIM DE CONVERSAO                          ==='
 SAY '    *=================================================================='
return

/* ***************************************************************************** */
/* **********    TRANSFORMA NOVO TAMANHO DE VARIAVEL        ******************** */
/* ***************************************************************************** */
Substitui:
Parse Arg ., RepWord1, ToWord1, Frase
/* Say 'RepWord1 ' RepWord1  */
/* Say 'ToWord1  ' ToWord1   */
/* Say 'Frase    ' Frase     */

 IF pos( RepWord1 ,Frase) > 0 
    THEN DO
    FraseLen    = LENGTH(Frase)
    P1Len       = pos( RepWord1,Frase) - 1 + LENGTH(RepWord1)
    P2Len       = FraseLen - P1Len 
    P2          = right(Frase,P2Len)
    char_ajuste = pos( RepWord1 ,Frase) - 1
    Frase   = left(Frase,char_ajuste) || ToWord1 || p2
 END

/* Say '---fim---' Frase  */
return(Frase)

/* ***************************************************************************** */
/* **********     C O N T R O L E   D E   F A S E S         ******************** */
/* ***************************************************************************** */
Marcafase:
If POS('FILE-CONTROL',Fonte.i) > 0 & POS('*',Fonte.i) =0 THEN DO
   FasePrograma = 'FILE-CONTROL'
end

If POS('FILE',Fonte.i) > 0 & POS('SECTION',Fonte.i) > 0 & POS('*',Fonte.i) =0 THEN DO
   FasePrograma = 'FILE-SECTION'
end

If POS('WORKING-STORAGE',Fonte.i) > 0 & POS('*',Fonte.i) =0 THEN DO
   FasePrograma = 'WORKING-STORAGE'
end

If POS('PROCEDURE',Fonte.i) > 0  & POS('DIVISION',Fonte.i) > 0 & POS('*',Fonte.i) =0 THEN DO
   FasePrograma = 'PROCEDURE-DIVISION'
end
Return

/* ***************************************************************************** */
/* **********     G U A R D A   N O V O   E L E M E N T O   ******************** */
/* ***************************************************************************** */
Guarda_Elemento:
Parse arg .,  Linha, Elem
/* SAY 'Guarda_Elemento' */ 
r     = 0
r2    = 0
Status = 'OLD'
FimDescription = 'NAO'
If POS('WORKING-STORAGE',Fonte.Linha) > 0 then Linha = Linha + 1
do until FimDescription = 'SIM'
    /* say 'DENTRO ' Fonte.Linha  */
   iF SUBSTR(Fonte.Linha,16,1) <> '*' then do 
      if subword(Fonte.Linha,2,1) = '01',
      |  subword(Fonte.Linha,2,1) = '66',
      |  subword(Fonte.Linha,2,1) = '77',
      |  (pos('INCLUDE',Fonte.Linha) > 0,
      &  FasePrograma = 'FILE-SECTION' ) then do 
         if  (DATATYPE(subword(Fonte.Linha,2,1)) = 'NUM') then Enum = subword(Fonte.Linha,2,1)
         r = r + 1
         Array.Elem.0 = r  
         Status = 'NEW'

         WarArray.Elem.0 = r  
         if FasePrograma = 'FILE-SECTION' & pos('INCLUDE',Fonte.Linha) > 0  Then Array.Elem.r = '01 '||strip(subword(Fonte.Linha,5,1),'T','.')
         if subword(Fonte.Linha,2,1) = Enum Then Array.Elem.r =  strip(subword(left(Fonte.Linha,81),2,6),'T','.')
         if pos('COMP',Fonte.Linha) > 0  Then WarArray.Elem.r = 'Contem COMP'
         /* SAY '--elem ' Elem '- '  Interno.n ' - ' Array.Elem.r    */
      end
      
      IF Status = 'NEW' then do
         Status = 'OLD'
         r2 = 0
      end
      
      if  (DATATYPE(subword(Fonte.Linha,2,1)) = 'NUM'),
      &   subword(Fonte.Linha,2,1)  > 1 ,
      &   subword(Fonte.Linha,2,1) <> 77 then do
          if pos('COMP',Fonte.Linha) > 0  Then WarArray.Elem.r = 'Contem COMP'
          r2 = r2 + 1
          Array.Elem.r.0 = r2
          Array.Elem.r.r2 = strip(subword(left(Fonte.Linha,81),2,6),'T','.')
          
          MarcaArray.Elem.r.r2 = ''
          if pos('OCCURS',Fonte.Linha)    > 0 then MarcaArray.Elem.r.r2 = 'OCCURS'
          if pos('REDEFINES',Fonte.Linha) > 0 then do
             MarcaArray.Elem.r.r2 = 'REDEFINIDO'
             raux =  r2
             do until subword(Array.Elem.r.raux,1,1) = subword(Array.Elem.r.r2,1,1)
                raux = raux - 1
                if subword(Array.Elem.r.raux,1,1) = subword(Array.Elem.r.r2,1,1) then MarcaArray.Elem.r.raux = 'DEFINICAO'
             end
         /*    if pos('.',Fonte.Linha) = 0 then do
                Linha = Linha + 1
             Array.Elem.r.r2 = Array.Elem.r.r2 ||' '|| subword(Fonte.Linha,2,(words(Fonte.Linha) - 1))
             end
         */    
          end 
          /* say 'SubItem: 'Array.Elem.r.r2 ' - Marca: ' MarcaArray.Elem.r.r2  */
      end
      
      /* say Fonte.Linha */
      LinhaAnt = Linha - 1
      
      if  TipoPrograma = 'BATCH',
      & ( Translate(subword(Fonte.Linha,2,1)) = 'SD'),
      | ( Translate(subword(Fonte.Linha,2,1)) = 'FD') then do
          FimDescription = 'SIM'
          /* say 'FimDescription1' Linha '-' Fonte.Linha  */
          Linha = Linha - 1
      end
      
      if pos('WORKING-STORAGE',Fonte.Linha) > 0 then DO
         FimDescription = 'SIM'
          /* say 'FimDescription2' Linha '-' Fonte.Linha  */
         FasePrograma = 'WORKING-STORAGE'
      END
      
      if pos('PROCEDURE',Fonte.Linha) > 0 & pos('DIVISION',Fonte.Linha) > 0  then do
         FasePrograma = 'PROCEDURE-DIVISION'
         FimDescription = 'SIM'
         /* say 'FimDescription1' Linha '-' Fonte.Linha  */
      end
      
   end
   Linha = Linha + 1
End
/* SAY ' sai Guarda_Elemento' Linha */
Return(Linha)


/* ***************************************************************************** */
/* **********     V E R I F I C A   L I N H A S   E L E I T A S  *************** */
/* ***************************************************************************** */
VerificaSelecao:
Parse arg ., Reg, Fase

If FasePrograma = 'PROCEDURE-DIVISION' then Reg = substr(Reg,16,66)

If Fase = 'WS' then do 
   ID1 = '16)'
   ID2 = '15)'
   ID3 = '19)'
   ID4 = '24)'
   
   OP1 = '3)'
   OP2 = '999'
   
   CC1 = '9)'
   CC2 = '8)'
   CC3 = '99999999'
   CC4 = '999999999'
end
If Fase = 'PD' then do 
   ID1 = '16'
   ID2 = '15'
   ID3 = '19'
   OP1 = '3'
   CC1 = '9'
   CC2 = '8'
end

Seleciona = 'N'

/* Verifica WISO-002-NUM-CONTA */
if pos('AG-OP-CC',Reg) > 0 & pos(ID3,Reg) > 0 then Seleciona = 'S'

/* Verifica W016-002-NUM-CONTA */
if pos('W016-002-NUM-CONTA',Reg) > 0 then Seleciona = 'S'

/* Verifica WISO-002-NUM-CONTA */
if pos('WISO-002-NUM-CONTA',Reg) > 0 then Seleciona = 'S'

/* Verifica WISO-002-NUM-CONTA */
if pos('WISO-002-TAM',Reg) > 0 then  Seleciona = 'S'

/* Verifica identificacao */
if pos('ID',Reg) > 0,
&  (pos(ID1,Reg) > 0 | pos(ID2,Reg) > 0 | pos(ID3,Reg) > 0) then DO
   Seleciona = 'S'
   /* say 'ID: ' reg */
end

/* Verifica Operacao */
if pos('OP',Reg) > 0,
&  (pos(OP1,Reg) > 0 | pos(OP2,Reg) > 0),
&  (pos('OPEN',Reg)=0 & pos('COPY',Reg)=0) then DO
   Seleciona = 'S'
   /* say 'OP: ' reg */
end

/* Verifica Conta */
if ( pos('CC',Reg)    > 0,
|    pos('CT',Reg)    > 0,
|    pos('CONTA',Reg) > 0),
&  ( pos(CC1,Reg) > 0,
   | pos(CC2,Reg) > 0,
   | pos(CC3,Reg) > 0,
   | pos(CC4,Reg) > 0),
   &  (pos('SECTION',Reg)=0,             /* ******************************* */
   &   pos('OBJECT' ,Reg)=0,             /* EXCLUI DA PESQUISA OS SEGUINTES */
   &   pos('OCCURS' ,Reg)=0,             /* ******************************* */
   &   pos('ACCEPT', Reg)=0,
   &   pos('SELECT', Reg)=0) then DO
   Seleciona = 'S'
   /* say 'CC: ' reg */
end
   
Return(Seleciona) 

/* **************************************************************************** */
/* *********     P R O C U R A   L I N H A   S E L E C I  O N A D A *********** */
/* **************************************************************************** */
Procura_Linha:
/* Say 'Procura_Linha' */
Parse arg ., Reg
 /* say Reg */
ix = 0

/* Say 'Reg' Reg */
Encontrou = ''
Do until Encontrou <> ''
  ix = ix + 1
  if pos(Reg,Fonte.ix) > 0 then Do
     Encontrou = 'SIM'
     leave
  end
  if ix > f then do
     say 'notfound' Reg
     EXIT(99)
  end
  
end
/* say 'ProcuraLinha: ' ix */
Return(ix)

/* **************************************************************************** */
/* *********     T E S T A   E X I S T E N C I A     A R Q U I V O   ********** */
/* **************************************************************************** */
TestaArq:
Parse arg Arq

RC_AbreArqMsg  =  STREAM(Arq,'C','OPEN READ')
IF RC_AbreArqMsg <> 'READY:' THEN DO
   Say '**************************************************************'
   SAY ''
   SAY 'CANCELAMENTO : Arquivo nao encontrado:' Arq
   SAY ''   
   Say '**************************************************************'
   EXIT(99)
END
Return(0)