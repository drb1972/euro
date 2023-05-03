/*-- rexx - Obtienene componentes de paquetes CHM, su libreria   -----*/
/*--        de STAGE y hace FTP de los elementos                    --*/
/*-- LL02QA                                                      -----*/
/*--------------------------------------------------------------------*/
/* parametros: clave: Nombre de directorio a crear en el UNIX         */
/* entornos  : ANTE - CURS - PRODPR - PRODAL - PRODCI - PRODEX        */
/* DB2       : SI/NO Para obtener las Plantable y Catalog del DB2     */
/* Lista     : Lista de paquetes separados por comas                  */
/*--------------------------------------------------------------------*/
parse upper arg subsis ',' clave ',' entorno ',' db2 ',' lista
parse upper arg parametros
say '<?xml version="1.0"?>'
call init
call alloc
/*--------------------------------------------------------------------*/
drop ftp.
contftp = 0
contftp=contftp+1 ; ftp.contftp = "locsite encoding=sb"
contftp=contftp+1 ; ftp.contftp = "locsite sbd=(IBM-1145,ISO8859-1)"
contftp=contftp+1 ; ftp.contftp = "prompt"
contftp=contftp+1 ; ftp.contftp = "cd "||home
contftp=contftp+1 ; ftp.contftp = "mkdir "||f_salida
contftp=contftp+1 ; ftp.contftp = "cd "||f_salida
contftp=contftp+1 ; ftp.contftp = "mkdir "||cam
contftp=contftp+1 ; ftp.contftp = "cd "||cam
/*--------------------------------------------------------------------*/

if cam = 'XXX' then db2 = 'NO' /* XXX mal configurada en DB2 */
if DB2 = 'SI' then call db2

/*--------------------------------------------------------------------*/
do k = 1 to paq.0
   paquete = paq.k
   x = 0 ; drop sal.
   x=x+1;sal.x= '<?xml version="1.0"?>'
   x=x+1;sal.x= '<service name="CMPONENT">'
   x=x+1;sal.x= ' <scope name="PKG_COMP">'
   x=x+1;sal.x= '  <message name="LIST">'
   x=x+1;sal.x= '   <header>'
   x=x+1;sal.x= '    <subsys>P</subsys>'
   x=x+1;sal.x= '    <product>CMN</product>'
   x=x+1;sal.x= '   </header>'
   x=x+1;sal.x= '  <request>'
   x=x+1;sal.x= '    <package>'||paquete||'</package>'
   x=x+1;sal.x= '   </request>'
   x=x+1;sal.x= '  </message>'
   x=x+1;sal.x= ' </scope>'
   x=x+1;sal.x= '</service>'
   sal.0 = x

   "execio "sal.0" diskw xmlin(finis stem sal."
   drop sal.
   "call '"comcload"(serxmlbc)' "
   retorno = rc
   "execio * diskr xmlout(finis stem sal."
   if retorno > 0 then do
      do z = 1 to sal.0
         parse var sal.z . '<statusMessage>'v_stat'</sta' .
         if v_stat = '' then mens = v_stat
      end
      if substr(mens,1,8) = 'CMN6504I' then do
         retcod = '04'
         mens = 'No existen componentes para analizar'
         end
      else do
         retcod = '08'
      end /* else */
      drop delet.
      x = outtrap(delet.)
      "delete '"libtemp"'"
      if db2 = 'SI' then do
         "delete '"fichdb21"'"
         "delete '"fichdb22"'"
      end
      x = outtrap(off)

      do deb = 1 to delet.0
         debug = debug ' ' delet.deb
      end
      call salir
   end
   component = ''
   compType  = ''
   sourceLib = ''
   drop comp.
   drop type.
   drop slib.
   cont = 0

   do z = 1 to sal.0
      parse var sal.z . '<component>'v_component'</component>' .
      if v_component = '' then do
         component = v_component
         iterate
      end
      parse var sal.z . '<componentType>'v_compType'</componentType>' .
      if v_compType = '' then do
         compType = v_compType
         iterate
      end
      parse var sal.z . '<sourceLib>'v_sourceLib'</sourceLib>' .
      if v_sourceLib = '' then sourceLib = v_sourceLib
      if sourceLib = '' then do
         if index(tipos,compType) > 0 then do
            cont = cont+1
            comp.cont = component
            type.cont = compType
            slib.cont = sourcelib
         end /* if index */
         component = ''
         compType  = ''
         sourceLib = ''
      end /* if sourceLib */
   end /* do z */
   cont_tot = cont_tot + cont
   comp.0 = cont
   type.0 = cont
   slib.0 = cont

/* DO I = 1 TO COMP.0 /* DRB */
      SAY 'COMP'||.I COMP.I
      SAY 'TYPE'||.I TYPE.I
      SAY 'SLIB'||.I SLIB.I
   END  */

   /*-- Copio de origen a destino ------------------------------------*/
   do i = 1 to comp.0
/*    "smcopy fromdataset('"slib.i"("comp.i")')
              todataset('"libtemp"("comp.i")')" */
     call copiar slib.i libtemp comp.i
   end

   do i = 1 to comp.0
      existe = sysdsn("'"libtemp"("comp.i")'")
      call outtrap "LINE.", "*"
      select
         when existe = 'OK' then iterate /*-- No existe miembro  --*/
         when type.i = 'SRC' then type.i = 'CBL'
         when type.i = 'NRC' | type.i = 'NRR' then call comp_basico
         when type.i = 'PRD' | type.i = 'PRG' then call producto
         when index(tipos_JCL,type.i) > 0 then do
            fichero_jcl = libtemp||"("||comp.i||")"
            call LL09QA entorno fichero_jcl
         end
         otherwise nop
      end /* select */
      if type.i = 'NOC' then do
         contftp = contftp+1
         ftp.contftp = "put '"libtemp"("comp.i")' "comp.i"."type.i
      end
   end
end /* do paq.0 */

/*---------- no hay componentes vlidos ------------------------------*/
if cont_tot = 0 & DB2 = 'SI' then do
   retcod = '04'
   mens   = 'Componentes de Tecnologa no analizables por SQA'
   call salir
end

contftp=contftp+1 ; ftp.contftp = "quit"
ftp.0 = contftp

/*-- Datos de usuario vpchm01 ----------------------------------------*/
"alloc fi(NETRC) dsn('chm.pscm.datos("aix")') shr"
if rc=0 then do
   retcod = '08'
   mens   = 'Error en CHM.PSCM.DATOS('AIX') - Falta 'aix
   call salir
end

"alloc fi(INPUT)   lrecl(80)  blksize(0) recfm(F B) ",
      "cylinders space(15,15) unit(sysda) new reu"

"alloc fi(OUTPUT)  lrecl(160) blksize(0) recfm(F B) ",
      "cylinders space(15,15) unit(sysda) new reu"

"execio "ftp.0" diskw input(finis stem ftp."

"FTP "aix" (EXIT"
rcftp = rc

if rcftp > 0 then do
   retcod = '08'
   select
      when rcftp = 44550 then mens= 'Nombre de dir. duplicado en el AIX'
      otherwise mens = 'error 'rcftp' en el ftp'
   end
end


if rcftp=0 then do
   retcod = '00'
   if paq.0 = 1 then mens   = 'paquete 'paquete 'transmitido'
   else mens = 'paquetes 'lista 'transmitidos'
end

drop delet.
x = outtrap(delet.)
   "delete '"libtemp"'"
if db2 = 'SI' then do
   "call *(hepwait) '00001000'" /* hhmmssdc */
   "delete '"fichdb21"'"
   "delete '"fichdb22"'"
end
x = outtrap(off)

do deb = 1 to delet.0
   debug = debug ' ' delet.deb
end

/* salida del ftp */
/* drb */ /*
"execio * diskr output(finis stem output."
do i = 1 to output.0
   say output.i
end */
call salir
/*--------------------------------------------------------------------*/
salir:
   x = outtrap(members.)
   "FREE FI(maix NETRC input output SERPARM xmlin xmlout)"
   x = outtrap(off)
   call salida
   call log
   exit
return
/*--------------------------------------------------------------------*/
log:
/*x=LL00QA('LL02QA',date(),time(),userid(),retcod,mens,debug,parametros) */
  x=LL00QA('LL02QA',date(),time(),userid(),retcod,mens,parametros)
return
/*--------------------------------------------------------------------*/
salida:
   say '<service>'
   say '<name>ll02qa</name>'
   say '<desc>ftp de componentes paquetes ChangeMan</desc>'
   say '<dir>'f_salida'</dir>'
   say '<rc>'retcod'</rc>'
   say '<msg>'mens'</msg>'
   say '</service>'
return
/*--------------------------------------------------------------------*/
init:
   subsis  = strip(subsis)
   DB2     = strip(DB2)
   paquete = strip(paquete)
   retcod  = ''
   mens    = ''
   debug   = ''
   /*-----------------------------------------------------------------*/
   cam   = substr(lista,1,3) /*-- 3 primeros caracteres de la lista --*/
   lista = strip(lista)                      /*--                   --*/
   lista = translate(lista,' ',',')          /*-- quito las comas   --*/
   paquetes = words(lista)                   /*-- n. de paquetes    --*/

   drop paq.                                 /*-- paq. paquetes     --*/
   paq.0 = paquetes
   do i = 1 to paq.0
      paq.i = word(lista,i)
   end
   f_salida = clave /*-- Nombre del directorio --*/
   /*-- Tipos de componentes a mandar por FTP ------------------------*/
   tipos ='CPA,CPY,JCC,JCE,JCL,JCS,JCT,NPA,NPY,NRC,NRR,PRD,PRG,SRC'
   tipos_jcl ='JCC,JCE,JCL,JCS,JCT'
   /*-----------------------------------------------------------------*/
   cont_tot = 0 /*-- Contador de comados del FTP --*/
return
/*--------------------------------------------------------------------*/
alloc:
   comcload = 'CHM.PROD.COMC.LOAD'
   tcpiport = 'CHM.PADM.TCPIPORT'
   "alloc da('"tcpiport"') fi(SERPARM) shr reu"
   "alloc fi(xmlin)  lrecl(255) blksize(0) recfm(v b) ",
         "cylinders space(15,15) unit(sysda) new reu"
   "alloc fi(xmlout)  lrecl(5000) blksize(0) recfm(v b) ",
         "cylinders space(15,15) unit(sysda) new reu"

   /*-- Fichero temporal ---------------------------------------------*/
   CL = ''
   do 6
      CL = CL||RANDOM(1,6)
   end
   libtemp = 'TMPPR.TEST.SQA.CL'||CL
   call outtrap "line.", "*"
   "delete '"libtemp"'"
   "alloc fi(TEMP) lrecl(80) blksize(0) recfm(f,b) " ||,
   "cyl da('"libtemp"') space(15,15) unit(sysda) dir(20) "||,
   "new reuse dsntype(library)"
   x = outtrap(members.)
   "Free fi(temp)"
   x = outtrap(off)

   /*-- Datos de la maquina AIX --------------------------------------*/
   "ALLOC FI(MAIX)  DSN('CHM.PADM.DATOS(AIX)') SHR"
   if rc=0 then do
      retcod = '08'
      mens   = 'Error en CHM.PADM.DATOS(AIX) - Falta 'aix
      call salir
   end
   "execio * diskr maix(finis stem maix."
   do i = 1 to maix.0
      if substr(strip(maix.i),1,1)='*' then iterate
      parse var maix.i aixent aixmaq aixsub .
      if aixent = subsis then do /* R = test SQA */
         aix  = aixmaq           /* Q = prod SQA */
         home = aixsub
         leave
      end
   end
return
/*--------------------------------------------------------------------*/
/*- Los componentes basicos con CBDATOS/CBNEGOCIO tienen dif. ext.  --*/
/*--------------------------------------------------------------------*/
comp_basico:
   type.i = 'CBA'
   var_8_1 = substr(strip(comp.i),8,1)
   if var_8_1 ='P' then type.i = 'CBP'
   if var_8_1 ='Q' then type.i = 'NOC' /* no se copia */
   if var_8_1 ='R' then type.i = 'CBP'
   if var_8_1 ='M' then type.i = 'NOC' /* no se copia */
   if var_8_1 ='D'| var_8_1 ='G' | var_8_1 ='N' then do
      drop reg6.
      "alloc fi(temp) da('"libtemp"("comp.i")') SHR"
      "execio 6 diskr temp (finis stem reg6.)"
      reg6.6 = translate(reg6.6)
   end /* if */
   if var_8_1 ='D' then do
      reg6.6 = translate(reg6.6)
      if index(reg6.6,'*  CBDATOS')   > 0 then type.i = 'CBD'
      else type.i = 'CBA'
   end
   if var_8_1 ='G' | var_8_1 = 'N' then do
      reg6.6 = translate(reg6.6)
      if index(reg6.6,'*  CBNEGOCIO') > 0 then type.i = 'CBU'
      else type.i = 'CBN'
   end
      x = outtrap(members.)
      "Free fi(temp)"
      x = outtrap(off)
return
/*--------------------------------------------------------------------*/
/*- Rutina para copiar las copys simbolicas de los mapas -------------*/
/*--------------------------------------------------------------------*/
producto:
   if type.i = 'PRD' then tipo_copy = 'NPY'
   if type.i = 'PRG' then tipo_copy = 'NPA'
   drop copy_simbolica.
   "alloc fi(temp) da('"libtemp"("comp.i")') SHR"
   "execio 30 diskr temp (finis stem copy_simbolica.)"
   do j = 1 to copy_simbolica.0
      control_mapa = substr(copy_simbolica.j,1,4)
      if control_mapa = 'MAPA' then do
      /* Si el valor es NN en el mapa no hay copy */
         control_mapaSN = substr(copy_simbolica.j,23,1)
         if control_mapaSN = 'N' then iterate
         copy_simbolica.j = translate(copy_simbolica.j)
    /*-- No utilizo la variable c_simb en este servicio --*/
         parse var copy_simbolica.j "MAPA="mapa","c_simb",".
    /*-- hlq = Stagelib sin el tipo de comp             --*/
         hlq = substr(slib.i,1,22)
 /*      "smcopy fromdataset('"hlq||tipo_copy"("mapa")')
                 todataset('"libtemp"("mapa")')" */
         call copiar hlq||typo_copy libtemp mapa
         contftp = contftp+1
         ftp.contftp = "put '"libtemp"("mapa")'" mapa"."tipo_copy
      end /* if */
   end /* do */
   x = outtrap(members.)
   "Free fi(temp)"
   x = outtrap(off)
return
/*--------------------------------------------------------------------*/
DB2:

   TBNAME = cam||'%'
   select
      when entorno = 'TEST' then do
         SSID    = 'DB2T'
         CREATOR = V||'T'||cam
         nodo    = PRUE
      end
      when entorno = 'TESZ' then do
         SSID    = 'DB2Z'
         CREATOR = V||'C'||cam
         nodo    = PRUE
      end
      when entorno = 'PRODPR' then do /*-- Duda: Hay DB2Z - VPcam --*/
         SSID    = 'DB2T'
         CREATOR = V||'P'||cam
         nodo    = PRUE
      end
      when entorno = 'ANTE' then do
         SSID    = 'DB2L'
         CREATOR = V||'A'||cam
         nodo    = ALFA
      end
      when entorno = 'PRODAL' then do
         SSID    = 'DB2L'
         CREATOR = V||'P'||cam
         nodo    = ALFA
      end
      when entorno = 'CURS' then do /*-- Duda: Hay DB2C - VUcam --*/
         SSID    = 'DB2C'
         CREATOR = V||'U'||cam
/*       CREATOR = V||'P'||cam */
         nodo    = CINF
      end
      when entorno = 'PRODCI' then do
         SSID    = 'DB2I'
         CREATOR = V||'P'||cam
         nodo    = CINF
      end
      when entorno = 'PRODEX' then do
         SSID    = 'DB2E'
         CREATOR = V||'P'||cam
         nodo    = EXPL
      end
      otherwise nop
   end
   "delstack"
   fichdb21 = "CHM.PROD.INICIAL.QA21.C1"||CL
   fichdb22 = "CHM.PROD.INICIAL.QA22.C2"||CL
   x=outtrap('novale',0)
   "delete "fichdb21
   "delete "fichdb22
/* "alloc fi(LISTCATA) lrecl(80) blksize(0) recfm(F B) ",
         "da('"fichdb21"')" ,
         "cylinders space(15,15) unit(sysda) new catalog reu"
   "alloc fi(LISTPLAN) lrecl(240) blksize(0) recfm(F B) ",
         "da('"fichdb22"')" ,
         "cylinders space(15,15) unit(sysda) new catalog reu"
   "free fi(LISTCATA LISTPLAN)" */

   job_random = ''
   do 2
      job_random = job_random||RANDOM(1,9)
   end

   queue "//CHMSQ2"||job_random||" JOB (UGASTO,CHM,OSN,VPCHM),"
   queue "//         'HERR.-PROD',MSGCLASS=X,CLASS=P,"
   queue "//         REGION=200M,MSGLEVEL=(1,1)"
   queue "/*ROUTE XEQ "nodo
   queue "//PASO01   EXEC PGM=IKJEFT01,DYNAMNBR=20"
   if subsis = 'Q' then do
      queue "//SYSPROC  DD DSN=CHM.PROD.ZMF.REX.CUST,DISP=SHR"
   end
   if subsis = 'R' then do
      queue "//SYSPROC  DD DSN=CHM.TEST.ZMF.REX.CUST,DISP=SHR"
   end
   queue "//SYSPROC  DD DSN=CHM.PROD.ZMF.REX.CUST,DISP=SHR"
   queue "//SYSPRINT DD  SYSOUT=*"
   queue "//SYSTSPRT DD  SYSOUT=*"
   queue "//*ISTCATA DD DISP=SHR,DSN="||FICHDB21
   queue "//*ISTPLAN DD DISP=SHR,DSN="||FICHDB22
   queue "//LISTCATA DD DISP=(NEW,CATLG,DELETE),"
   queue "//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),UNIT=SYSDA,"
   queue "//         SPACE=(CYL,(15,15)),"
   queue "//         DSN="||fichdb21
   queue "//LISTPLAN DD DISP=(NEW,CATLG,DELETE),"
   queue "//         DCB=(RECFM=FB,LRECL=240,BLKSIZE=240),UNIT=SYSDA,"
   queue "//         SPACE=(CYL,(15,15)),"
   queue "//         DSN="||fichdb22
   queue "//SYSTSIN  DD  *"
   queue "LL06QA" SSID CREATOR TBNAME
   queue "/*"
   queue "//*"
   queue "$$"
   x = outtrap(submit.)
   "submit * end($$)"
   x = outtrap(off)
   "call *(hepwait) '00001500'" /* hhmmssdc */
   do q = 1 to 10
      "call *(hepwait) '00000200'" /* hhmmssdc */
      retcodb2 = "1"
      existe1 = listdsi(fichdb21)
      existe2 = listdsi(fichdb22)
      if existe1 = "0" & existe2 = "0" then do
         retcodb2 = "0"
         leave
      end
   end
   if retcodb2 = 0 then do
      contftp = contftp + 1
      ftp.contftp ="put '"||fichdb21||"' CATA.DB2"
      contftp = contftp + 1
      ftp.contftp ="put '"||fichdb22||"' PLAN.DB2"
   end
return
/*----------------------------------------------------*/
copiar:
arg origen destino miembro
   x=outtrap('novale',0)
   "alloc fi(INDD1)  dsn("||origen||") shr reuse"
   "alloc fi(OUTDD1) dsn("||destino||") shr reuse"
   "alloc fi(sysin) unit(sysda) space(15) blksize(80)
    lrecl(80) recfm(f b) dsorg(ps) new delete reuse"
   "alloc dd(sysprint) new reu unit(sysda) recfm(f b) lrecl(80)"
   drop sysin. ; sysin.0 = 2
   sysin.1 = "COPY1 COPY INDD=INDD1,OUTDD=OUTDD1"
   sysin.2 = "SELM  S M="miembro
   "execio" sysin.0 "diskw sysin (finis stem sysin."
   "call *(iebcopy)"
   x = outtrap(free.)
   "free fi(indd1 outdd1 sysin)"
   x = outtrap(off)
return
