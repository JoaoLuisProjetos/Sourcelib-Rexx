      ******************************************************************
      *     (S T U B / I B M)   REPLICADO A PARTIR DA REXX "REPLSTU"   *
      *-------------------------------------------------------------   *
      * NO STUB DEVERAH SER ALTERADO:                                  *
      *-------------------------------------------------------------   *
      *   - O NOME DO STUB QUE EH EXATAMENTE O NOME DA SUBROTINA       *
      *     ESTE NOME SEGUE O PADRAO  XYZSNN    ONDE:                  *
      *        XXXSNN --> EH O NOME DA SUBROTINA                       *
      *   - O NOME DA SUBROTINA                                        *
      *   - A AREA DE COMUNICACAO DA SUBROTINA                         *
      *     ESTA AREA ESTA DECLARADA NA LINKAGE SECTION LOGO APOS O    *
      *     COMENTARIO  "AREA DE COMUNICACAO DA SUBROTINA"             *
      *   - TODOS OS PREFIXOS DOS CAMPOS DA AREA DE COMUNICACAO QUE    *
      *     ESTAO SENDO USADOS NO STUB                                 *
      *   - A PORTA DE ACESSO AO UNISYS DEVERAH SER INFORMADA NO       *
      *     CAMPO SERVER-PORT. ESTA PORTA ESTA CADASTRADA NO UNISYS    *
      *     NA TABELA SBPM5030                                         *
      *                                                                *
      ******************************************************************

       IDENTIFICATION DIVISION.
      *========================*

       PROGRAM-ID. XXXS99.

      *=====================*
       ENVIRONMENT DIVISION.
      *=====================*

      *==============*
       DATA DIVISION.
      *==============*

       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
      * SOCKET INTERFACE FUNCTION CODES                               *
      *---------------------------------------------------------------*
       01  SOKET-FUNCTIONS.
           02 SOKET-ACCEPT          PIC X(16) VALUE 'ACCEPT          '.
           02 SOKET-BIND            PIC X(16) VALUE 'BIND            '.
           02 SOKET-CLOSE           PIC X(16) VALUE 'CLOSE           '.
           02 SOKET-CONNECT         PIC X(16) VALUE 'CONNECT         '.
           02 SOKET-FCNTL           PIC X(16) VALUE 'FCNTL           '.
           02 SOKET-FREEADDRINFO    PIC X(16) VALUE 'FREEADDRINFO    '.
           02 SOKET-GETADDRINFO     PIC X(16) VALUE 'GETADDRINFO     '.
           02 SOKET-GETCLIENTID     PIC X(16) VALUE 'GETCLIENTID     '.
           02 SOKET-GETHOSTBYADDR   PIC X(16) VALUE 'GETHOSTBYADDR   '.
           02 SOKET-GETHOSTBYNAME   PIC X(16) VALUE 'GETHOSTBYNAME   '.
           02 SOKET-GETHOSTID       PIC X(16) VALUE 'GETHOSTID       '.
           02 SOKET-GETHOSTNAME     PIC X(16) VALUE 'GETHOSTNAME     '.
           02 SOKET-GETNAMEINFO     PIC X(16) VALUE 'GETNAMEINFO     '.
           02 SOKET-GETPEERNAME     PIC X(16) VALUE 'GETPEERNAME     '.
           02 SOKET-GETSOCKNAME     PIC X(16) VALUE 'GETSOCKNAME     '.
           02 SOKET-GETSOCKOPT      PIC X(16) VALUE 'GETSOCKOPT      '.
           02 SOKET-GIVESOCKET      PIC X(16) VALUE 'GIVESOCKET      '.
           02 SOKET-INITAPI         PIC X(16) VALUE 'INITAPI         '.
           02 SOKET-IOCTL           PIC X(16) VALUE 'IOCTL           '.
           02 SOKET-LISTEN          PIC X(16) VALUE 'LISTEN          '.
           02 SOKET-NTOP            PIC X(16) VALUE 'NTOP            '.
           02 SOKET-PTON            PIC X(16) VALUE 'PTON            '.
           02 SOKET-READ            PIC X(16) VALUE 'READ            '.
           02 SOKET-RECV            PIC X(16) VALUE 'RECV            '.
           02 SOKET-RECVFROM        PIC X(16) VALUE 'RECVFROM        '.
           02 SOKET-SELECT          PIC X(16) VALUE 'SELECT          '.
           02 SOKET-SEND            PIC X(16) VALUE 'SEND            '.
           02 SOKET-SENDTO          PIC X(16) VALUE 'SENDTO          '.
           02 SOKET-SETSOCKOPT      PIC X(16) VALUE 'SETSOCKOPT      '.
           02 SOKET-SHUTDOWN        PIC X(16) VALUE 'SHUTDOWN        '.
           02 SOKET-SOCKET          PIC X(16) VALUE 'SOCKET          '.
           02 SOKET-TAKESOCKET      PIC X(16) VALUE 'TAKESOCKET      '.
           02 SOKET-TERMAPI         PIC X(16) VALUE 'TERMAPI         '.
           02 SOKET-WRITE           PIC X(16) VALUE 'WRITE           '.
      *---------------------------------------------------------------*
      * WORK VARIABLES                                                *
      *---------------------------------------------------------------*
       77  INICIALIZADO                   PIC X VALUE 'N'.
       01  ERRNO                          PIC 9(8) BINARY VALUE ZERO.
       01  RETCODE                        PIC S9(8) BINARY VALUE ZERO.
       01  INDEX-COUNTER                  PIC 9(8) BINARY VALUE ZERO.
       01  SERVER-IPADDR-DOTTED           PIC X(15) VALUE SPACE.
       01  CLIENT-IPADDR-DOTTED           PIC X(15) VALUE SPACE.
       01  CLOSE-SERVER                   PIC 9(8) BINARY VALUE ZERO.
           88  CLOSE-SERVER-DOWN          VALUE 1.
       01  CONNECT-FLAG                   PIC X VALUE SPACE.
           88 CONNECTED                         VALUE 'Y'.
       01  CLIENT-SERVER-FLAG             PIC X VALUE SPACE.
           88 CLIENTS                           VALUE 'C'.
           88 SERVERS                           VALUE 'S'.
       01  TERMINATE-OPTIONS              PIC X VALUE SPACE.
           88 OPENED-API                        VALUE 'A'.
           88 OPENED-SOCKET                     VALUE 'S'.
       01  CUR-TIME.
           02  HOUR                       PIC 9(2).
           02  MINUTE                     PIC 9(2).
           02  SECOND                     PIC 9(2).
           02  HUND-SEC                   PIC 9(2).
       77  FAILURE                        PIC S9(8) COMP.
      *---------------------------------------------------------------*
      * VARIABLES USED FOR THE INITAPI CALL                           *
      *---------------------------------------------------------------*
       01  MAXSOC-FWD                     PIC 9(8) BINARY.
       01  MAXSOC-RDF REDEFINES MAXSOC-FWD.
           02 FILLER                      PIC X(2).
           02 MAXSOC                      PIC 9(4) BINARY.
       01  INITAPI-IDENT.
           05  TCPNAME                    PIC X(8) VALUE 'TCPIP   '.
           05  ASNAME                     PIC X(8) VALUE SPACE.
       01  SUBTASK                        PIC X(8) VALUE 'XXXS99'.
       01  MAXSNO                         PIC 9(8) BINARY VALUE 1.
      *---------------------------------------------------------------*
      * VARIABLES USED BY THE SHUTDOWN CALL                           *
      *---------------------------------------------------------------*
       01  HOW                            PIC 9(8) BINARY.
      *---------------------------------------------------------------*
      * VARIABLES RETURNED BY THE GETCLIENTID CALL                    *
      *---------------------------------------------------------------*
       01  CLIENTID.
           05  CLIENTID-DOMAIN            PIC 9(8) BINARY VALUE 19.
           05  CLIENTID-NAME              PIC X(8) VALUE SPACE.
           05  CLIENTID-TASK              PIC X(8) VALUE SPACE.
           05  FILLER                     PIC X(20) VALUE LOW-VALUE.
      *---------------------------------------------------------------*
      * VARIABLES RETURNED BY THE GETNAMEINFO CALL                    *
      *---------------------------------------------------------------*
       01  NAME-LEN                       PIC 9(8) BINARY.
       01  HOST-NAME                      PIC X(255).
       01  HOST-NAME-LEN                  PIC 9(8) BINARY.
       01  SERVICE-NAME                   PIC X(32).
       01  SERVICE-NAME-LEN               PIC 9(8) BINARY.
       01  NAME-INFO-FLAGS                PIC 9(8) BINARY VALUE 0.
       01  NI-NOFQDN                      PIC 9(8) BINARY VALUE 1.
       01  NI-NUMERICHOST                 PIC 9(8) BINARY VALUE 2.
       01  NI-NAMEREQD                    PIC 9(8) BINARY VALUE 4.
       01  NI-NUMERICSERVER               PIC 9(8) BINARY VALUE 8.
       01  NI-DGRAM                       PIC 9(8) BINARY VALUE 16.
      *---------------------------------------------------------------*
      * VARIABLES USED FOR THE SOCKET CALL                            *
      *---------------------------------------------------------------*
       01  AF-INET                        PIC 9(8) BINARY VALUE 2.
       01  AF-INET6                       PIC 9(8) BINARY VALUE 19.
       01  SOCK-STREAM                    PIC 9(8) BINARY VALUE 1.
       01  SOCK-DATAGRAM                  PIC 9(8) BINARY VALUE 2.
       01  SOCK-RAW                       PIC 9(8) BINARY VALUE 3.
       01  IPPROTO-IP                     PIC 9(8) BINARY VALUE ZERO.
       01  IPPROTO-TCP                    PIC 9(8) BINARY VALUE 6.
       01  IPPROTO-UDP                    PIC 9(8) BINARY VALUE 17.
       01  IPPROTO-IPV6                   PIC 9(8) BINARY VALUE 41.
       01  SOCKET-DESCRIPTOR              PIC 9(4) BINARY VALUE ZERO.
      *---------------------------------------------------------------*
      * SERVER SOCKET ADDRESS STRUCTURE                               *
      *---------------------------------------------------------------*
       01  SERVER-SOCKET-ADDRESS.
           05  SERVER-AFINET              PIC 9(4) BINARY VALUE 2.
      * NUMERO DA PORTA NO UNISYS
           05  SERVER-PORT                PIC 9(4) BINARY VALUE 10002.
           05  SERVER-IPADDR              PIC 9(8) BINARY VALUE 0.
           05  FILLER                     PIC X(8).
      *---------------------------------------------------------------*
      * VARIABLES USED FOR THE NTOP/PTON CALL                         *
      *---------------------------------------------------------------*
       01  INADDR-ANY                     PIC X(45) VALUE '.'.
      *ENDERECO IP DO UNISYS
       01  INADDR-UNISYS                  PIC X(45) VALUE
                                                    '99.9.9.999'.
       01  PRESENTABLE-ADDR               PIC X(45) VALUE SPACES.
       01  PRESENTABLE-ADDR-LEN           PIC 9(4) BINARY VALUE 45.
       01  NUMERIC-ADDR.
           05 FILLER                      PIC 9(16) BINARY VALUE 0.
           05 FILLER                      PIC 9(16) BINARY VALUE 0.
      *---------------------------------------------------------------*
      * BUFFER AND LENGTH FIELDS FOR RECV OPERATION                   *
      *---------------------------------------------------------------*
       01  AUX-SKT-BUFFER.
           05  AUX-LEN                    PIC 9(8) BINARY.
       01  SKT-REQUEST-LEN                PIC 9(8) BINARY VALUE 0.
       01  SKT-BUFFER                     PIC X(4096) VALUE SPACE.
       01  SIC-BUFFER.
           05  SIC-LEN                    PIC 9(8) BINARY.
           05  SIC-APOS-LEN.
               10  SIC-HEADER.
                   15 CS0100-FLAG1        PIC X(0004).
      * A SER UTILIZADO
                   15 CS0100-VERSION      PIC 9(0004).
      * VERSAO DA AREA
                   15 CS0100-FUNCAO       PIC 9(0002).
                      88 CS0100-FIMEP          VALUES 2.
                      88 CS0100-FIMPROG        VALUES 3.
      * A SER IMPLEMENTADO
                   15 CS0100-PID          PIC X(0008).
      * PID
                   15 CS0100-JID          PIC 9(0008).
      * JID
                   15 CS0100-SIX          PIC X(0009).
      * AREA A SER PRESERVADA ENTRE AMBIENTES. NO MICRO SESSID
                   15 CS0100-TAM-HDR      PIC 9(0006).
      * TAMANHO DO HEADER
                   15 CS0100-TAM-PAR      PIC 9(0006).
      * TAMANHO DO PARAMETRO
                   15 CS0100-RESULT       PIC 9(0002).
                      88 CS0100-OK             VALUES 0 .
                      88 CS0100-ERRO           VALUES 99.
      * INDICA SE HOUVE ERRO NA INTEGRACAO
                   15 CS0100-EP                PIC X(0017).
      * NOME DO ENTRY-POINT A SER DADO O CALL
                   15 CS0100-SHARING      PIC X(0001).
                      88 SHAREBYALL            VALUE 'S'.
                      88 PRIV                  VALUE 'P'.
      * SE A SUBROTINA E PRIVATE OU SHAREBYALL
                   15 CS0100-USERCODE     PIC X(0018).
      * USERCODE DE EXECUCAO
                   15 CS0100-SUBROTINA    PIC X(0060).
      * NOME DO EXECUTAVEL DA SUBROTINA
                   15 CS0100-TS-STSK      PIC 9(0016).
      * DATA HORA DO STUB/SKEL
                   15 CS0100-MSG          PIC X(0080).
      * MENSAGEM EM CASO ERRO DE INTEGRACAO
                   15 CS0100-PROGID       PIC X(0032).
      * PROGRAMA QUE CHAMOU A SUBROTINA
                   15 CS0100-SEQ          PIC X(0006).
      * SEQUENCIALIZADOR DE MENSAGENS
                   15 CS0100-SF           PIC 9(0003).
                   15 FILLER              PIC X(0014).
                   15 CS0100-DGV          PIC 9(0002).
      * VERIFICA SE A MENSAGEM ESTA INTEGRA
                   15 CS0100-ORIGEM       PIC X(0002).
      * INDICA A ORIGEM DA MENSAGEM
      *   "SS"  STUB --> GERENTE DE CANAL
      *   "SM"  GERENTE CANAL --> SERVIDOR DE SUBROTINA
      *   "MS"  GERENTE CANAL --> STUB
      *   "ZN"  PROGRAMA ONLINE -->
      *   "ZS"  SUBROTINA
               10  SIC-PARAMETROS         PIC X(3600).
      *---------------------------------------------------------------*
      * OTHER FIELDS FOR SEND AND RECCFROM OPERATION                  *
      *---------------------------------------------------------------*
       01  SEND-FLAG                      PIC 9(8) BINARY VALUE ZERO.
       01  RECV-FLAG                      PIC 9(8) BINARY VALUE ZERO.
      *---------------------------------------------------------------*
      * SOCKET OPTIONS                                                *
      *---------------------------------------------------------------*
       01  OPT-VAL.
           05  OPT-VAL1                   PIC S9(15) COMP-3 VALUE ZERO.
           05  OPT-VAL2                   PIC S9(15) COMP-3 VALUE ZERO.
       01  OPT-NAME                       PIC 9(8) BINARY VALUE ZERO.
       01  OPT-LEN                        PIC 9(8) BINARY VALUE ZERO.

       01  IOCTL-COMMAND                  PIC 9(8) BINARY VALUE ZERO.
       01  IOCTL-COMMAND-RED REDEFINES IOCTL-COMMAND PIC X(4).
       01  IOCTL-REQARG                   PIC 9(8) BINARY VALUE ZERO.
       01  IOCTL-REQARG-RED REDEFINES IOCTL-REQARG PIC X(4).
       01  IOCTL-RETARG                   PIC 9(8) BINARY VALUE ZERO.
      *---------------------------------------------------------------*
      * ERROR MESSAGE FOR SOCKET INTERFACE ERRORS                     *
      *---------------------------------------------------------------*
       01  EZAERROR-MSG.
           05  FILLER                     PIC X(9) VALUE 'FUNCTION='.
           05  EZAERROR-FUNCTION          PIC X(16) VALUE SPACE.
           05  FILLER                     PIC X VALUE ' '.
           05  FILLER                     PIC X(8) VALUE 'RETCODE='.
           05  EZAERROR-RETCODE           PIC ---99.
           05  FILLER                     PIC X VALUE ' '.
           05  FILLER                     PIC X(9) VALUE 'ERRORNO='.
           05  EZAERROR-ERRNO             PIC ZZZ99.
           05  FILLER                     PIC X VALUE ' '.
           05  EZAERROR-TEXT              PIC X(50) VALUE ' '.

       LINKAGE SECTION.
      *================
      *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
      *         "AREA DE COMUNICACAO DA SUBROTINA"               *
      *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

           COPY 'XXXSL99'.

      *=============================================*
       PROCEDURE DIVISION USING XS9901-XXXS99.
      *=============================================*

      *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
      *          P R O C E D U R E    C O N T R O L S            *
      *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

            IF INICIALIZADO = 'S'
               GO TO MEIO.
            PERFORM INITIALIZE-API     THRU   INITIALIZE-API-EXIT.
            PERFORM GET-CLIENT-ID      THRU   GET-CLIENT-ID-EXIT.
            PERFORM SOCKETS-DESCRIPTOR THRU   SOCKETS-DESCRIPTOR-EXIT.
            PERFORM PRESENTATION-TO-NUMERIC THRU
                                     PRESENTATION-TO-NUMERIC-EXIT.
            PERFORM CONNECT-SOCKET     THRU   CONNECT-SOCKET-EXIT.
            PERFORM NUMERIC-TO-PRESENTATION THRU
                                     NUMERIC-TO-PRESENTATION-EXIT.
      *     PERFORM GET-NAME-INFORMATION THRU
      *                              GET-NAME-INFORMATION-EXIT.
       MEIO.
            MOVE 'S' TO INICIALIZADO.
            PERFORM WRITE-MESSAGE      THRU   WRITE-MESSAGE-EXIT.
            PERFORM READ-MESSAGE       THRU   READ-MESSAGE-EXIT.
      *     PERFORM SHUTDOWN-RECEIVE   THRU   SHUTDOWN-RECEIVE-EXIT.

            GO TO EXIT-NOW.

      *---------------------------------------------------------------*
      * INITIALIZE SOCKET API                                         *
      *---------------------------------------------------------------*
       INITIALIZE-API.
           MOVE SOKET-INITAPI TO EZAERROR-FUNCTION.
           CALL 'EZASOKET' USING SOKET-INITAPI
                                 MAXSOC
                                 INITAPI-IDENT
                                 SUBTASK
                                 MAXSNO
                                 ERRNO
                                 RETCODE.
           MOVE 'INITAPI FAILED' TO EZAERROR-TEXT.
           IF RETCODE < 0
              MOVE 12 TO FAILURE.
           PERFORM RETURN-CODE-CHECK  THRU RETURN-CODE-EXIT.
           MOVE 'A' TO TERMINATE-OPTIONS.
       INITIALIZE-API-EXIT.
           EXIT.

      *---------------------------------------------------------------*
      * LET US SEE THE CLIENT-ID                                      *
      *---------------------------------------------------------------*
       GET-CLIENT-ID.
            MOVE SOKET-GETCLIENTID TO EZAERROR-FUNCTION.
            CALL 'EZASOKET' USING SOKET-GETCLIENTID
                                  CLIENTID
                                  ERRNO
                                  RETCODE.
      *     DISPLAY SUBTASK
      *            ' OUR CLIENT ID = ' CLIENTID-NAME ' ' CLIENTID-TASK.
            MOVE 'GETCLIENTID FAILED' TO EZAERROR-TEXT.
            IF RETCODE < 0
               MOVE 24 TO FAILURE.
            PERFORM RETURN-CODE-CHECK THRU RETURN-CODE-EXIT.
            MOVE 'C' TO CLIENT-SERVER-FLAG.
       GET-CLIENT-ID-EXIT.
           EXIT.

      *---------------------------------------------------------------*
      * GET US A STREAM SOCKET DESCRIPTOR                             *
      *---------------------------------------------------------------*
       SOCKETS-DESCRIPTOR.
            MOVE SOKET-SOCKET TO EZAERROR-FUNCTION.
            CALL 'EZASOKET' USING SOKET-SOCKET
                                  AF-INET
                                  SOCK-STREAM
                                  IPPROTO-IP
                                  ERRNO
                                  RETCODE.
            MOVE 'SOCKET CALL FAILED' TO EZAERROR-TEXT.
            IF RETCODE < 0
               MOVE 60 TO FAILURE.
            PERFORM RETURN-CODE-CHECK THRU RETURN-CODE-EXIT.
            MOVE 'S' TO TERMINATE-OPTIONS.
            MOVE RETCODE TO SOCKET-DESCRIPTOR.
       SOCKETS-DESCRIPTOR-EXIT.
           EXIT.

      *---------------------------------------------------------------*
      * USE PTON TO CREATE AN IP ADDRESS TO BIND TO.                  *
      *---------------------------------------------------------------*
       PRESENTATION-TO-NUMERIC.
            MOVE SOKET-PTON TO EZAERROR-FUNCTION.
            MOVE INADDR-UNISYS TO PRESENTABLE-ADDR.
            CALL 'EZASOKET' USING SOKET-PTON
                                  AF-INET
                                  PRESENTABLE-ADDR
                                  PRESENTABLE-ADDR-LEN
                                  NUMERIC-ADDR
                                  ERRNO
                                  RETCODE.
            MOVE 'PTON CALL FAILED' TO EZAERROR-TEXT.
            IF RETCODE < 0
               MOVE 24 TO FAILURE.
            PERFORM RETURN-CODE-CHECK THRU RETURN-CODE-EXIT.
            MOVE NUMERIC-ADDR TO SERVER-IPADDR.
       PRESENTATION-TO-NUMERIC-EXIT.
           EXIT.

      *---------------------------------------------------------------*
      * CONNECT                                                       *
      *---------------------------------------------------------------*
       CONNECT-SOCKET.
            MOVE SPACE TO CONNECT-FLAG.
            MOVE ZEROS TO ERRNO RETCODE.
            MOVE SOKET-CONNECT TO EZAERROR-FUNCTION.
            CALL 'EZASOKET' USING SOKET-CONNECT
                                  SOCKET-DESCRIPTOR
                                  SERVER-SOCKET-ADDRESS
                                  ERRNO
                                  RETCODE.
            MOVE 'CONNECTION CALL FAILED' TO EZAERROR-TEXT.
            IF RETCODE < 0
               MOVE 24 TO FAILURE.
            PERFORM RETURN-CODE-CHECK THRU RETURN-CODE-EXIT.
            IF RETCODE = 0
               MOVE 'Y' TO CONNECT-FLAG.
            GO TO CONNECT-SOCKET-EXIT.
       SOCKETS-BLOCKING.
           MOVE X'8004A77E' TO IOCTL-COMMAND-RED.
           MOVE X'00'       TO IOCTL-REQARG-RED.
           MOVE SOKET-IOCTL TO EZAERROR-FUNCTION.
           CALL 'EZASOKET' USING SOKET-IOCTL
                                 SOCKET-DESCRIPTOR
                                 IOCTL-COMMAND
                                 IOCTL-REQARG
                                 IOCTL-RETARG
                                 ERRNO
                                 RETCODE.
           MOVE 'IOCTL CALL FAILED' TO EZAERROR-TEXT.
           IF RETCODE < 0
              MOVE 24 TO FAILURE.
           PERFORM RETURN-CODE-CHECK THRU RETURN-CODE-EXIT.
       SOCKETS-TIMEOUT.
           MOVE 4102 TO OPT-NAME.
           MOVE 3600 TO OPT-VAL1.
           MOVE 3600 TO OPT-VAL2.
           MOVE 16 TO OPT-LEN.
           MOVE SOKET-SETSOCKOPT TO EZAERROR-FUNCTION.
           CALL 'EZASOKET' USING SOKET-SETSOCKOPT
                                 SOCKET-DESCRIPTOR
                                 OPT-NAME
                                 OPT-VAL
                                 OPT-LEN
                                 ERRNO
                                 RETCODE.
           MOVE 'SETSOCKOPT CALL FAILED' TO EZAERROR-TEXT.
           IF RETCODE < 0
              MOVE 24 TO FAILURE.
           PERFORM RETURN-CODE-CHECK THRU RETURN-CODE-EXIT.
       CONNECT-SOCKET-EXIT.
           EXIT.

      *---------------------------------------------------------------*
      * USE NTOP TO DISPLAY THE IP ADDRESS.                           *
      *---------------------------------------------------------------*
       NUMERIC-TO-PRESENTATION.
           MOVE SOKET-NTOP TO EZAERROR-FUNCTION.
           MOVE SERVER-IPADDR TO NUMERIC-ADDR.
           MOVE SOKET-NTOP TO EZAERROR-FUNCTION.
           CALL 'EZASOKET' USING SOKET-NTOP
                                 AF-INET
                                 NUMERIC-ADDR
                                 PRESENTABLE-ADDR
                                 PRESENTABLE-ADDR-LEN
                                 ERRNO
                                 RETCODE.
      *    DISPLAY SUBTASK
      *            ' PRESENTABLE ADDRESS = ' PRESENTABLE-ADDR.
           MOVE 'NTOP CALL FAILED' TO EZAERROR-TEXT.
           IF RETCODE < 0
              MOVE 24 TO FAILURE.
           PERFORM RETURN-CODE-CHECK THRU RETURN-CODE-EXIT.
       NUMERIC-TO-PRESENTATION-EXIT.
           EXIT.

      *---------------------------------------------------------------*
      * USE GETNAMEINFO TO GET THE HOST AND SERVICE NAMES             *
      *---------------------------------------------------------------*
       GET-NAME-INFORMATION.
           MOVE 28 TO NAME-LEN.
           MOVE 255 TO HOST-NAME-LEN.
           MOVE 32 TO SERVICE-NAME-LEN.
           MOVE NI-NAMEREQD TO NAME-INFO-FLAGS.
           MOVE SOKET-GETNAMEINFO TO EZAERROR-FUNCTION.
           CALL 'EZASOKET' USING SOKET-GETNAMEINFO
                                 SERVER-SOCKET-ADDRESS
                                 NAME-LEN
                                 HOST-NAME
                                 HOST-NAME-LEN
                                 SERVICE-NAME
                                 SERVICE-NAME-LEN
                                 NAME-INFO-FLAGS
                                 ERRNO
                                 RETCODE.
           DISPLAY SUBTASK
                   ' HOST NAME = ' HOST-NAME
                   ' SERVICE = ' SERVICE-NAME.
           MOVE 'GETADDRINFO CALL FAILED' TO EZAERROR-TEXT.
           IF RETCODE < 0
              MOVE 24 TO FAILURE.
           PERFORM RETURN-CODE-CHECK THRU RETURN-CODE-EXIT.
       GET-NAME-INFORMATION-EXIT.
           EXIT.

      *---------------------------------------------------------------*
      * WRITE A MESSAGE TO THE SERVER                                 *
      *---------------------------------------------------------------*
       WRITE-MESSAGE.
           MOVE 'SIC1'        TO CS0100-FLAG1.
           MOVE 0001          TO CS0100-VERSION.
           MOVE 00            TO CS0100-FUNCAO.
           MOVE 00            TO CS0100-RESULT.
           MOVE 1234          TO CS0100-JID.
           MOVE 5678          TO CS0100-PID.
           MOVE 1234567812345678
                              TO CS0100-TS-STSK.
           MOVE 'XXXS99'      TO CS0100-EP.
           MOVE 'XX/X/SC/99'  TO CS0100-SUBROTINA.
           MOVE 'USERCOD'     TO CS0100-USERCODE.
           MOVE 300           TO CS0100-TAM-HDR.

      * TAMANHO DA AREA DE COMUNICACAO DA SUBROTINA

           COMPUTE CS0100-TAM-PAR = FUNCTION LENGTH(XS9901-XXXS99).
           MOVE XS9901-XXXS99 TO SIC-PARAMETROS.

           COMPUTE SIC-LEN = CS0100-TAM-HDR + CS0100-TAM-PAR.
           MOVE SIC-BUFFER TO SKT-BUFFER.
           ADD 4 TO SIC-LEN.
           MOVE SIC-LEN TO SKT-REQUEST-LEN.

           MOVE SOKET-WRITE TO EZAERROR-FUNCTION.
           CALL 'EZASOKET' USING SOKET-WRITE
                                 SOCKET-DESCRIPTOR
                                 SKT-REQUEST-LEN
                                 SKT-BUFFER
                                 ERRNO
                                 RETCODE.
           MOVE 'WRITE CALL FAILED' TO EZAERROR-TEXT
           IF RETCODE < 0
              MOVE 24 TO FAILURE.
           PERFORM RETURN-CODE-CHECK THRU RETURN-CODE-EXIT.
       WRITE-MESSAGE-EXIT.
           EXIT.

      *---------------------------------------------------------------*
      * READ A MESSAGE FROM THE SERVER.                               *
      *---------------------------------------------------------------*
       READ-MESSAGE.
           MOVE SPACES TO AUX-SKT-BUFFER.
           MOVE 64 TO RECV-FLAG.
           MOVE 4 TO SKT-REQUEST-LEN.
           MOVE SOKET-RECV TO EZAERROR-FUNCTION.
           CALL 'EZASOKET' USING SOKET-RECV
                                 SOCKET-DESCRIPTOR
                                 RECV-FLAG
                                 SKT-REQUEST-LEN
                                 AUX-SKT-BUFFER
                                 ERRNO
                                 RETCODE.
           IF RETCODE = 0
              GO TO READ-MESSAGE.
           IF RETCODE = 4
              GO TO READ-MESSAGE-2.
           MOVE 'READ CALL1 FAILED' TO EZAERROR-TEXT.
           IF RETCODE < 0
              MOVE 24 TO FAILURE.
           PERFORM RETURN-CODE-CHECK THRU RETURN-CODE-EXIT.
       READ-MESSAGE-2.
           MOVE SPACES TO SKT-BUFFER.
           MOVE 64 TO RECV-FLAG.
           MOVE AUX-LEN TO SKT-REQUEST-LEN.
           MOVE SOKET-RECV TO EZAERROR-FUNCTION.
           CALL 'EZASOKET' USING SOKET-RECV
                                 SOCKET-DESCRIPTOR
                                 RECV-FLAG
                                 SKT-REQUEST-LEN
                                 SKT-BUFFER
                                 ERRNO
                                 RETCODE.
           MOVE 'READ CALL2 FAILED' TO EZAERROR-TEXT.
           IF RETCODE < 0
              MOVE 24 TO FAILURE.
           PERFORM RETURN-CODE-CHECK THRU RETURN-CODE-EXIT.
           MOVE AUX-LEN TO SIC-LEN.
           MOVE SKT-BUFFER TO SIC-APOS-LEN.

           IF CS0100-RESULT = 99
              DISPLAY 'ERRO NO UNISYS: ' CS0100-MSG
              IF CS0100-MSG = 'HANDLER TERMINANDO'
                 MOVE 0 TO AUX-LEN
                 COMPUTE AUX-LEN = 1 / AUX-LEN
              END-IF
           END-IF.

           MOVE SIC-PARAMETROS        TO XS9901-XXXS99.

       READ-MESSAGE-EXIT.
           EXIT.

      *---------------------------------------------------------------*
      * SHUTDOWN RECEIVE PIPE                                         *
      *---------------------------------------------------------------*
       SHUTDOWN-RECEIVE.
            MOVE SOKET-SHUTDOWN TO EZAERROR-FUNCTION.
            MOVE 0 TO HOW.
            CALL 'EZASOKET' USING SOKET-SHUTDOWN
                                  SOCKET-DESCRIPTOR
                                  HOW
                                  ERRNO
                                  RETCODE.
            MOVE 'SHUTDOWN CALL FAILED' TO EZAERROR-TEXT.
            IF RETCODE < 0
               MOVE 99 TO FAILURE.
            PERFORM RETURN-CODE-CHECK THRU RETURN-CODE-EXIT.
       SHUTDOWN-RECEIVE-EXIT.
           EXIT.

      *---------------------------------------------------------------*
      * CLOSE SOCKET                                                  *
      *---------------------------------------------------------------*
       CLOSE-SOCKET.
             MOVE SOKET-CLOSE TO EZAERROR-FUNCTION.
             CALL 'EZASOKET' USING SOKET-CLOSE
                                   SOCKET-DESCRIPTOR
                                   ERRNO
                                   RETCODE.
             MOVE 'CLOSE CALL FAILED' TO EZAERROR-TEXT.
             IF RETCODE < 0
                MOVE 132 TO FAILURE
                PERFORM WRITE-EZAERROR-MSG THRU
                        WRITE-EZAERROR-MSG-EXIT.
             ACCEPT CUR-TIME FROM TIME.
             DISPLAY SUBTASK
                     ' '
                     CUR-TIME
                     ' FUNC= ' EZAERROR-FUNCTION
                     ' RETCODE=' RETCODE
                     ' ERRNO= ' ERRNO.
       CLOSE-SOCKET-EXIT.
           EXIT.

      *---------------------------------------------------------------*
      * TERMINATE SOCKET API                                          *
      *---------------------------------------------------------------*
       EXIT-TERM-API.
           ACCEPT CUR-TIME FROM TIME.
           DISPLAY SUBTASK
                   CUR-TIME
                   ' RETCODE= ' RETCODE
                   ' ERRNO= ' ERRNO.
           CALL 'EZASOKET' USING SOKET-TERMAPI.
       EXIT-TERM-API-EXIT.
           EXIT.

      *---------------------------------------------------------------*
      * TERMINATE PROGRAM                                             *
      *---------------------------------------------------------------*
       EXIT-NOW.
      *    IF OPENED-SOCKET
      *       PERFORM CLOSE-SOCKET THRU CLOSE-SOCKET-EXIT.

      *    IF OPENED-API
      *       PERFORM EXIT-TERM-API THRU EXIT-TERM-API-EXIT.

           MOVE FAILURE TO RETURN-CODE.
           GOBACK.

      *---------------------------------------------------------------*
      * WRITE OUT AN ERROR MESSAGE                                    *
      *---------------------------------------------------------------*
       WRITE-EZAERROR-MSG.
           MOVE ERRNO TO EZAERROR-ERRNO.
           MOVE RETCODE TO EZAERROR-RETCODE.
           DISPLAY EZAERROR-MSG.
       WRITE-EZAERROR-MSG-EXIT.
           EXIT.

      *---------------------------------------------------------------*
      * CHECK RETURN CODE AFTER EACH SOCKET CALL                      *
      *---------------------------------------------------------------*
       RETURN-CODE-CHECK.
            ACCEPT CUR-TIME FROM TIME.
            IF RETCODE < 0
               DISPLAY SUBTASK
                       ' '
                       CUR-TIME
                       ' FUNC=' EZAERROR-FUNCTION
                       ' RETCODE=' RETCODE
                       ' ERRNO= ' ERRNO
               PERFORM WRITE-EZAERROR-MSG THRU WRITE-EZAERROR-MSG-EXIT
               MOVE ZEROS TO ERRNO RETCODE
               GO TO EXIT-NOW.

            MOVE ZEROS TO ERRNO RETCODE.
       RETURN-CODE-EXIT.
           EXIT.
