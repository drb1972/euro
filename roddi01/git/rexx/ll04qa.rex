/*-- rexx - FTP de todos los componentes de una aplicacion en un ent -*/
/*-- LL04QA                                                      -----*/
/*--------------------------------------------------------------------*/
/* parametros: cam - entorno - DB2                                    */
/* entornos  : ANTE - CURS - PRODPR - PRODAL - PRODCI - PRODEX        */
/* DB2       : SI/NO Para obtener las Plantable y Catalog del DB2     */
/*--------------------------------------------------------------------*/
/*--------------------------------------------------------------------*/
say '<?xml version="1.0"?>'
parse upper arg subsis ',' cam ',' entorno ',' DB2
parse upper arg parametros
subsis  = strip(subsis)
cam     = strip(cam)
DB2     = strip(DB2)
entorno = strip(entorno)
xmldb2 = ''
xmlnea = 'NO'
call entorno_lib
call cam_naturaleza

CL = ''
do 3
   CL = CL||RANDOM(1,9)
end

libdest = 'TMPPR.TEST.QA.'||cam||CL||'.'||entorno
call copia

drop members.
x = outtrap(members.)
"LISTDS '"libdest"' members"
x = outtrap(off)
drop miembro.
cont = 0
do i = 7 to members.0
   cont = cont + 1
   miembro.cont = strip(members.i)
end
drop members.
miembro.0 = cont
if miembro.0 = 0 then do
   retcod = '04'
   mens   = 'no hay fuentes'
   call salir
end

/*-- Nomenclatura del fichero de salida ------------------------------*/
fecha = substr(date(S),3,6)

hora=space(translate(time(),' ',':'),0)
f_salida = cam||'.'||entorno||'.'||fecha||hora

/*- Se asigna tipo a los miembros -*/
/*- FTP -*/
"ALLOC FI(MAIX)  DSN('CHM.PADM.DATOS(AIX)') SHR"
if rc=0 then do
   retcod = '08'
   mens   = 'Error en CHM.PADM.DATOS(AIX) - Falta 'aix
   call salir
end
"execio * diskr maix(finis stem maix."
x = outtrap(free.)
"FREE FI(maix)"
x = outtrap(off)
do i = 1 to maix.0
   if substr(strip(maix.i),1,1)='*' then iterate
   parse var maix.i aixent aixmaq aixsub .
   if aixent = subsis then do /* Q = prod SQA */
      aix  = aixmaq           /* R = test SQA */
      home = aixsub
      leave
   end
end
drop ftp.
r = 0
r = r + 1 ;   ftp.r = "locsite encoding=sb"
r = r + 1 ;   ftp.r = "locsite sbd=(IBM-1145,ISO8859-1)"
r = r + 1 ;   ftp.r = "prompt"
r = r + 1 ;   ftp.r = "cd    "||home
r = r + 1 ;   ftp.r = "mkdir "||f_salida
r = r + 1 ;   ftp.r = "cd    "||f_salida
r = r + 1 ;   ftp.r = "mkdir "||cam
r = r + 1 ;   ftp.r = "cd    "||cam
rr1 = r+1  /* "mkdir "||zosdb2 - Esta variable valdra 'zosdb2' si --*/
rr2 = r+2  /* "cd "||zosdb2    la aplicacion tiene db2            --*/
contftp = r + 2
/*--------------------------------------------------------------------*/
/*-- Asigno los tipos ------------------------------------------------*/
/*--------------------------------------------------------------------*/
do i = 1 to miembro.0
   type = ''
/* sub_1_3 = substr(miembro.i,1,3)    /* Diferente CAM */
   if sub_1_3 = cam then type = XXX */
   sub_4_2 = substr(miembro.i,4,2) /* JCL Control-M */
   sub_4_1 = substr(miembro.i,4,1)
   sub_7_8 = substr(miembro.i,8,1)
   select
      when index(miembro.i,@) > 0 then type = CPY /* Copy */
      when sub_4_2 = 'JM' then do        /* JCLs de Control-M  */
         type= JCC
         call LL09QA entorno libdest"("miembro.i")"
      end
      when sub_4_1 = 'J' then do         /* JCL */
         type = JCL
         drop leer.
         "alloc da('"libdest"("miembro.i")') fi(leer) shr reu"
         "execio * diskr leer(finis stem leer."
         x = outtrap(free.)
         "free fi(leer)"
         x = outtrap(off)
         do w = 1 to leer.0
            sub_skel1 = substr(leer.w,1,1)
            sub_skel2 = substr(leer.w,2,1)
         /* Si el primer caracter es un ) y el segundo alfanumerico */
            if sub_skel1 = ')' & datatype(sub_skel2,'M') = 1 then do
               type = JCE /* Esqueleto del EBA */
               leave
            end /* if */
         end /* do w */
         call LL09QA entorno libdest"("miembro.i")"
      end /* when sub_4_1 = 'J' */
      when sub_4_1 = 'B' then type = CBL /* Fuente Cobol Batch */
      when sub_4_1 = 'C' then type = CBL /* Fuente Cobol CICS */
      when sub_4_1 = 'E' then iterate    /* Especial */
      when sub_4_1 = 'I' then iterate    /* ISPF */
      when sub_4_1 = 'N' then iterate    /* Natural */
      when sub_4_1 = 'M' then iterate    /* Mapa Natural */
      when sub_4_1 = 'F' then iterate    /* VSAM */
      when sub_4_1 = 'S' then iterate    /* SBC */
      when sub_4_1 = 'A' then iterate    /* Copys Assembler */
      when sub_4_1 = 'P' then iterate    /* Copys PL/I */
      when sub_4_1 = 'Q' then type = CPY /* Copys Cobol */
      when sub_4_1 = 'K' then iterate    /* Programas de utilidad */
      when sub_4_1 = 'X' then iterate    /* Xerox */
      when sub_7_8 = 'C' then type = NPY /* Copy */
      when sub_7_8 = 'O' then type = NPY /* Copy */
      when sub_7_8 = 'I' then type = NPY /* Copy */
      when sub_7_8 = 'Z' then type = NPY /* Copy */
      when sub_7_8 = 'Y' then type = NPY /* Copy */
      when sub_7_8 = 'S' then type = NPY /* Copy */
      when sub_7_8 = 'E' then type = NPY /* Copy */
      when sub_7_8 = 'V' then type = DCL /* DCLGen */
/*    when sub_7_8 = 'G' then type = ''  /* Fuente Cobol */ */
      when sub_7_8 = 'R' then type = CBP /* Fuente Cobol */
      when sub_7_8 = 'P' then type = CBP /* Fuente Cobol */
/*    when sub_7_8 = 'N' then type = ''  /* Fuente Cobol */ */
      when sub_7_8 = 'Q' then iterate    /* Fuente Cobol */
      when sub_7_8 = 'M' then iterate    /* Fuente Cobol */
/*    when sub_7_8 = 'D' then type = ''  /* Fuente Cobol */ */
      otherwise nop
   end
   if type = '' then do
      call add_ftp
      iterate
   end
   drop leer.
   "alloc da('"libdest"("miembro.i")') fi(leer) shr reu"
   "execio * diskr leer(finis stem leer."
   x = outtrap(free.)
   "free fi(leer)"
   x = outtrap(off)
   if index(leer.1,IDEPEPI) > 0 then do /* Productos */
      xmlnea = 'SI'
      type = PRD
      call add_ftp
  /*  call rexx_productos */
      iterate
   end
   if sub_7_8 = 'D' then do
      leer.6 = translate(leer.6)
      if index(leer.6,'*  CBDATOS') > 0 then type = CBD
      else type = CBA
      call add_ftp
      iterate
   end
   if sub_7_8 = 'G' | sub_7_8 = 'N' then do
      leer.6 = translate(leer.6)
      if index(leer.6,'*  CBNEGOCIO') > 0 then type= CBU
      else type = CBN
      call add_ftp
      iterate
   end
   leer.1 = translate(leer.1)
   if index(leer.1,BMSPEP) > 0 then iterate /* Mapas BMS */
   if index(leer.1,'REXX') > 0 then iterate /* Rexx */
   do i2 = 1 to leer.0
      leer.i2 = translate(leer.i2)
      if index(leer.i2,'IDENTIFICATION DIVISION') > 0 |,  /* Cobol */
         index(leer.i2,'ENVIRONMENT DIVISION') > 0 then do
         type = CBL
         call add_ftp
         leave
      end
      if index(leer.i2,' CSECT ') > 0 then leave   /* ASM */
   end
end
/*--------------------------------------------------------------------*/
/*-- Fin de asignacion de tipos --------------------------------------*/
/*--------------------------------------------------------------------*/
zosdb2 = 'ZOS'
ftp.rr1 = "mkdir "||xmldb2
ftp.rr2 = "cd "||xmldb2
/*--------------------------------------------------------------------*/
if db2 = 'SI' then call DB2
/*--------------------------------------------------------------------*/
contftp = contftp + 1
ftp.contftp = "quit"
ftp.0 = contftp
"ALLOC FI(NETRC) DSN('CHM.PSCM.DATOS("||aix||")') SHR"
if rc=0 then do
   retcod = '08'
   mens   = 'Error en CHM.PSCM.DATOS('AIX') - Falta 'aix
   call salir
end

"alloc fi(INPUT)   lrecl(80)  blksize(0) recfm(F B) ",
      "cylinders space(15,15) unit(sysda) new reu"

"alloc fi(OUTPUT)  lrecl(160) blksize(1600) recfm(F B) ",
      "cylinders space(15,15) unit(sysda) new reu"
"execio "ftp.0" diskw input(finis stem ftp."

"FTP "aix" (EXIT"
rcftp = rc

if rcftp=0 then do
   retcod = '08'
   mens   = 'error 'rcftp' en el ftp'
/* call salir */
end

if rcftp=0 then do
   retcod = '00'
/* mens   = contftp-5 'componentes transmitidos' */
/* Lo comento porque ahora puede haber puts      */
/* duplicados al tener que desglosar los .PRD    */
   mens   = 'componentes transmitidos correctamente'
end

x=outtrap('novale',0)
/* "delete '"libdest"'" */
"delete '"fichdb21"'"
"delete '"fichdb22"'"

/* Salida del FTP */
/*
say '<?xml version="1.0"?>'
"execio * diskr output(finis stem output."
do i = 1 to output.0
   say output.i
end
*/
x = outtrap(free.)
"FREE FI(NETRC input output)"
x = outtrap(off)
call salir
/*--------------------------------------------------------------------*/
salir:
   call salida
   call log
   exit
return
/*--------------------------------------------------------------------*/
log:
   x=LL00QA('LL04QA',date(),time(),userid(),retcod,mens,parametros)
return
/*--------------------------------------------------------------------*/
copia:
   x=outtrap('novale',0)
   "delete '"libdest"'"
   "alloc fi(INDD1) dsn('"||lib||"') shr reuse"
   "alloc fi(OUTDD1) dsn('"||libdest||"') new like('"||lib||"')",
   " dsntype(library)"
/* "alloc fi(OUTDD1) dsn('"||libdest||"') new reu ",
        "lrecl(80) blksize(0) recfm(f b) ",
        "cyl space(90,90) unit(sysda) dir(500)" */

   "alloc fi(sysin) unit(sysda) space(15) blksize(80) lrecl(80) recfm(f b)
   dsorg(ps) new delete reuse"
/* "alloc FI(SYSOUT)   DA(*)" si quiero las salidas en spool
   "alloc dd(SYSPRINT) DA(*)" */
   "alloc dd(sysprint) new reu unit(sysda) recfm(f b) lrecl(80)"
   "newstack"
   QUEUE "COPY1 COPY INDD=INDD1,OUTDD=OUTDD1"
   "execio" queued() "diskw sysin (finis"
   "delstack"
/* "tsoexec iebcopy" */
   "call *(iebcopy)"
   x = outtrap(free.)
   "free fi(indd1 outdd1 sysin)"
   x = outtrap(off)
return
/*--------------------------------------------------------------------*/
entorno_lib:
   select
      when entorno = 'ANTE'   then lib = cam||'PR.ANTEAL.FUENTE'
      when entorno = 'CURS'   then lib = cam||'PR.CURSCI.FUENTE'
      when entorno = 'PRODPR' then lib = cam||'PR.PRODPR.FUENTE'
      when entorno = 'PRODAL' then lib = cam||'PR.PRODAL.FUENTE'
      when entorno = 'PRODCI' then lib = cam||'PR.PRODCI.FUENTE'
      when entorno = 'PRODEX' then lib = cam||'PR.PRODEX.FUENTE'
      otherwise do
         retcod = '04'
         mens   = 'Entorno' entorno' no valido'
         call salir
      end
   end
   existe = listdsi(lib)
   if existe = "0" then do
      retcod = '04'
      mens   = 'Entorno' entorno' no valido para el CAM 'cam
      call salir
   end
return
/*--------------------------------------------------------------------*/
/* Lee el fichero del CAM_NATURALEZA y asigna la variable xmldb2 */
cam_naturaleza:
   "ALLOC FI(CAMNAT)  DSN('CHM.PROD.INICIAL.CAMNAT') SHR"
   if rc=0 then do
      retcod = '08'
      mens   = 'Error en CHM.PROD.INICIAL.CAMNAT'
      call salir
   end
   drop camnat.
   "execio * diskr CAMNAT(finis stem camnat."
   x = outtrap(free.)
   "FREE FI(CAMNAT)"
   x = outtrap(off)
   do i = 1 to camnat.0
      parse var camnat.i var1 var2 var3 var4 var5
      if var2 = cam then do
         xmldb2 = strip(var5)
         return
      end
   end
return
/*--------------------------------------------------------------------*/
add_ftp:
   contftp = contftp + 1
   ftp.contftp ="put '"||libdest||"("||miembro.i||")' ",
                miembro.i||"."||type
return
/*--------------------------------------------------------------------*/
DB2:

   if cam = 'XXX' then return /* XXX mal configurada en DB2 */
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
   fichdb21 = "CHM.PROD.INICIAL.QA41.C1"||CL
   fichdb22 = "CHM.PROD.INICIAL.QA42.C2"||CL
   x=outtrap('novale',0)
   "delete "fichdb21
   "delete "fichdb22

   job_random = ''
   do 2
      job_random = job_random||RANDOM(1,9)
   end

   queue "//CHMSQ4"||job_random||" JOB (UGASTO,CHM,OSN,VPCHM),"
   queue "//         'HERR.-PROD',MSGCLASS=X,CLASS=P,"
   queue "//         USER=&SYSUID,MSGLEVEL=(1,1)"
   queue "/*ROUTE XEQ "nodo
   queue "//PASO01   EXEC PGM=IKJEFT01,DYNAMNBR=20"
   if subsis = 'Q' then do
      queue "//SYSPROC  DD DSN=CHM.PROD.ZMF.REX.CUST,DISP=SHR"
   end
   if subsis = 'R' then do
      queue "//SYSPROC  DD DSN=CHM.TEST.ZMF.REX.CUST,DISP=SHR"
   end
   queue "//SYSPRINT DD  SYSOUT=*"
   queue "//SYSTSPRT DD  SYSOUT=*"
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
      retcodb2 = '1'
      "call *(hepwait) '00000200'" /* hhmmssdc */
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
/*--------------------------------------------------------------------*/
salida:
   say '<service>'
   say '<name>ll04qa</name>'
   say '<desc>ftp de aplicacion completa en un entorno</desc>'
   say '<dir>'f_salida'</dir>'
   select
      when xmldb2 = 'ZOSDB2' then xmldb2 = 'SI'
      when xmldb2 = 'ZOS'    then xmldb2 = 'NO'
      otherwise nop
   end
   say '<db2>'xmldb2'</db2>'
   say '<nea>'xmlnea'</nea>'
   say '<rc>'retcod'</rc>'
   say '<msg>'mens'</msg>'
   say '</service>'
return
/*--------------------------------------------------------------------*/
rexx_productos:
  directorio = libdest
  directorio = strip(directorio)
  contador = 0
  tipo = 0
  j = 0
  k = 0
  swcvar = 0
  do until j = leer.0
    call aloca_ficheros
  end
  call escribe_fichero
  x = outtrap(free.)
  "free file(sysut"k")"
  x = outtrap(off)
return
/*--------------------------------------------------------------------*/
aloca_ficheros:
   j=j+1
   control = substr(leer.j,1,7)
   control_copy = substr(leer.j,1,4)
   if control_copy= 'COPY' | control_copy= 'MAPA' then call copia_copy
   if control = 'IDECBLI' then do
          if contador = 0 then do
            if swcvar = 1 then do
               call escribe_fichero
               x = outtrap(free.)
               "free file(sysut"k")"
               x = outtrap(off)
               k = k + 1
            end
            if swcvar = 0 then do
               swcvar = 1
               drop salida.
            end
          end

          contador = 1
          swcvar = 1
          prog = substr(leer.j,8,8)
          program = strip(prog)

        /*dir = destino                               */
          dir = directorio"("program")"
          /* allocamos cuando  existen  */
          x= listdsi("'"dir"'")
          if listdsi("'"dir"'") = 0 then do
            "alloc f(sysut"k") da('"dir"') shr"
          end
          /* allocamos cuando no existen */
          if listdsi("'"dir"'") = 0 then do
           "alloc f(sysut"k") da('"dir"'),
            new space(15,15) tracks lrecl(80)"
          end
   end

   /* escribe la linea en el fichero de salida correspondiente */
   if contador > 1 then do
      contador2 = contador - 1
      izquierda = substr(leer.j,1,72)
      derecha = right(contador2,8,'0')
   /* leer.j = izquierda||derecha */
      centro = substr(leer.j,7,72)
      leer.j = '      'centro
      salida.contador2 = leer.j
   end
   contador = contador + 1

return
/*--------------------------------------------------------------------*/
escribe_fichero:
     "execio * diskw sysut"k" (stem salida. finis"
     miembro.i = program
/*--------------------------------------------------------------------*/
/*-- Asigno los tipos ------------------------------------------------*/
/*--------------------------------------------------------------------*/
   nea_7_8 = substr(miembro.i,8,1)
   select
      when nea_7_8 = 'G' then do
         salida.6 = translate(salida.6)
         if index(salida.6,'*  CBNEGOCIO') > 0 then type= CBU
         else type = CBN
      end
      when nea_7_8 = 'R' then type = CBP /* Fuente Cobol */
      when nea_7_8 = 'P' then type = CBP /* Fuente Cobol */
      when nea_7_8 = 'N' then do
         salida.6 = translate(salida.6)
         if index(salida.6,'*  CBNEGOCIO') > 0 then type= CBU
         else type = CBN
      end
      when nea_7_8 = 'Q' then return     /* Fuente Cobol */
      when nea_7_8 = 'M' then return     /* Fuente Cobol */
      when nea_7_8 = 'D' then do
         salida.6 = translate(salida.6)
         if index(salida.6,'*  CBDATOS') > 0 then type = CBD
         else type = CBA
      end
      otherwise nop
   end
/*--------------------------------------------------------------------*/
/*-- Fin de asignacion de tipos --------------------------------------*/
/*--------------------------------------------------------------------*/
     drop salida.
     call add_ftp
return
/*--------------------------------------------------------------------*/
copia_copy:
   select
      when control_copy = 'COPY' then do
         copy1 = substr(leer.j,6,8)
         copy2 = copy1
      end
      when control_copy = 'MAPA' then do
         parse var leer.j "MAPA="copy1","copy2",".
         copy2 = copy2||"@"
      end
      otherwise nop
   end

   drop libt.
   x = 0
/*- concateno x si no existen en el entorno que coja las del anterior */
/*- En las CGENER la copy simbolica se llama como el Mapa.            */
/*- En las ANTEAL/CURSCI base 24 alfabetica                           */
   select
      when entorno = 'ANTE' then do
         x = x+1 ; libt.x = cam||'PR.TESZ.CGENER('||copy1||')'
         x = x+1 ; libt.x = cam||'PR.TEST.CGENER('||copy1||')'
         x = x+1 ; libt.x = cam||'PR.ANTEAL.FUENTE('||copy2||')'
         libt.0 = x
      end
      when entorno = 'CURS' then do
         x = x+1 ; libt.x = cam||'PR.TESZ.CGENER('||copy1||')'
         x = x+1 ; libt.x = cam||'PR.TEST.CGENER('||copy1||')'
         x = x+1 ; libt.x = cam||'PR.ANTEAL.FUENTE('||copy2||')'
         x = x+1 ; libt.x = cam||'PR.CURSCI.FUENTE('||copy2||')'
         libt.0 = x
      end
      when entorno = 'PRODPR' then do
         x = x+1 ; libt.x = cam||'PR.PRODPR.FUENTE('||copy2||')'
         libt.0 = x
      end
      when entorno = 'PRODAL' then do
         x = x+1 ; libt.x = cam||'PR.PRODAL.FUENTE('||copy2||')'
         libt.0 = x
      end
      when entorno = 'PRODCI' then do
         x = x+1 ; libt.x = cam||'PR.PRODCI.FUENTE('||copy2||')'
         libt.0 = x
      end
      when entorno = 'PRODEX' then do
         x = x+1 ; libt.x = cam||'PR.PRODEX.FUENTE('||copy2||')'
         libt.0 = x
      end
      otherwise nop
   end

   libcopy ='' /* Compruebo que exista el miembro. Me quedo con la  */
               /* ultima */
   do i = 1 to libt.0
      result= sysdsn("'"libt.i"'")
      if result = 'OK' then libcopy = libt.i
   end
   return
   if libcopy = '' then do
      x=outtrap('novale',0)
      "smcopy fromdataset('"libcopy"')
              todataset('"libdest"("copy1")')"
      contftp = contftp + 1
      ftp.contftp ="put '"||libdest||"("||copy1||")' ",
                   copy1||".NPY"
   end /* if libcopy */


return
/*--------------------------------------------------------------------*/
