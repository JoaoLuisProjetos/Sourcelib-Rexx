
This little project was made to Replicate Stub Subroutines to create communication between UNISYS environment and UNIX environment via 
TCP/IP, USER access and other sockets as well.

It was crafted to save time building new Stubs and decreasing human errors, since the inputs will be submitted in a JCL file.

The JCL(REPLSTUB) needs to parse this sequence of 3 parameters:
 1. Subroutine name to be STUB:
     XXXS99
     Where:
     XXX --> System's name abbreviattion
     S   --> Means to be a subroutine in that Schemma
     99  --> Program Number
      
 2. UserCode.
    This is specified on the environment configuration. Therefore it's environment custom made. 
    So let's just use the following:
      XXX999E
      Where:
        XXX --> System's name abbreviattion
        999 --> Number of usercode
        E   --> Target Environment
        
 3. Target environment. 
    Ex: 
      DES --> Development Environment
      HML --> Homologation Environment
      PRD --> Production Environment
      
      
Using the Replicator:
  1. Fill the parameters after the Rexx Call '%REPLSTU' with the parameters shown as above in the JCL(REPLSTUB) file:
  EX:
  %REPLSTU  CDFS01 CDF001P PRD
  
  2. Submit the REPLSTUB JCL
  
  3. Check for this job RC, if it's equal 00 then the Replicator Worked correctly. 
  
  4. Both Datasets created would be:
  'DES.SIS.STUB.CDFS01SC.PRD'   <--- (Online Stub)
  'DES.SIS.STUB.CDFS01ST.PRD'   <--- (Batch Stub)
    
  PS1: It will be generated in Development environment, to be used on production environment.
  
  
  PS2: At the time i was noob on Rexx so it's funcionallity only made changes on pointed lines at 'Parametros Especificos'.
  Anyway it fitted for it's prupose.
  
  PS3: COMMENTS AND ERROR MESSAGES ARE WITTEN IN PORTUGUESE!
  
  
