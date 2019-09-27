

<h1>Why it was built?</h1></br>
Built for <b>Mainframe</b>.</br>
This little project was made to Replicate Stub Subroutines wich connect between UNISYS environment and UNIX environment via 
TCP/IP, USER access and other sockets as well.</br></br>
It was crafted to save time building new Stubs and decreasing human errors, since the inputs will be submitted in a JCL file.</br></br>
<h1>How to use?</h1>
The JCL(REPLSTUB) needs to parse this sequence of 3 parameters:</br>
1. Subroutine name to be STUB:</br>
(XXXS99)</br>
   Where:</br>
   XXX --> System's name abbreviattion</br>
   S   --> Means to be a subroutine in that Schemma</br>
   99  --> Program Number</br>
</br>
2. UserCode:</br>
   This is specified on the environment configuration. Therefore it's environment custom made. </br>
   So let's just use the following:</br>
(XXX999E)</br>
   Where:</br>
     XXX --> System's name abbreviattion</br>
     999 --> Number of usercode</br>
     E   --> Target Environment. (D,H,P)</br>
</br>
3. Target environment. </br>
   Where: </br>
     DES --> Development Environment</br>
     HML --> Homologation Environment</br>
     PRD --> Production Environment</br></br>
<h1>Using the Replicator:</h1></br>
1. Fill the parameters after the Rexx Call '%REPLSTU' with the parameters shown as above in the JCL(REPLSTUB) file:</br>
Parameters Example:</br>
<b>%REPLSTU  CDFS01 CDF001P PRD</b>
</br></br>
2. Submit the REPLSTUB JCL</br>
</br>
3. Check for this job RC, if it's equal 00 then the Replicator Worked correctly. </br>
</br>
4. Both Datasets created would be:</br>
   'DES.SIS.STUB.CDFS01SC.PRD'   <--- (Online Stub)</br>
   'DES.SIS.STUB.CDFS01ST.PRD'   <--- (Batch Stub)</br>
</br>
  PS: This example will be generated in Development environment, to be used on production environment.</br>
</br>
<h1>Another Comments</h1>
  PS1: At the time i was noob on Rexx so it's funcionallity only made changes on pointed lines at 'Parametros Especificos'.
  Anyway it fitted for it's prupose.</br>
</br>
  PS2: COMMENTS AND ERROR MESSAGES ARE WRITTEN IN PORTUGUESE!</br>
  </br></br></br>
  
