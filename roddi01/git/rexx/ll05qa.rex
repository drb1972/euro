/*-- rexx - FTP de lista de componentes de un cam en un entorno ------*/
/*-- LL05QA                                                      -----*/
/*--------------------------------------------------------------------*/
/* parametros: cam - clave - entorno - db2 - parametros               */
/* cam       : 3 caracteres                                           */
/* clave     : Proporcionada por SQA                                  */
/* Entornos  : TEST-TESZ-ANTE-CURS-PRODPR-PRODAL-PRODCI-PRODEX        */
/* DB2       : SI/NO. Genera PLAN.DB2 y CATA.DB2                      */
/* Lista     : Lista de componentes                                   */
/*--------------------------------------------------------------------*/
/*--------------------------------------------------------------------*/
say '<?xml version="1.0"?>'
/*--------------------------------------------------------------------*/
parse upper arg subsis','cam','clave','entorno','db2','lista
parse upper arg parametros
CL = ''
do 6
   CL = CL||RANDOM(1,9)
end
fecha = substr(date(S),3,6)                /*-- aammdd            --*/
hora  = space(translate(time(),' ',':'),0) /*-- hhmmss            --*/
subsis     = strip(subsis)                 /*--                   --*/
cam        = strip(cam)                    /*--                   --*/
clave      = strip(clave)                  /*--                   --*/
CL         = strip(CL)                     /*--  quito blancos    --*/
entorno    = strip(entorno)                /*--                   --*/
db2        = strip(db2)                    /*--                   --*/
debug = ''
lista = strip(lista)                       /*--                   --*/
lista = translate(lista,' ',',')           /*-- quito las comas   --*/
componentes = words(lista)                 /*-- n. de componentes --*/
mens   = ''
retcod = '08'
select
   when cam     = '' then do
      mens  = 'Campo CAM vacio'
      call salir
   end
   when clave   = '' then do
      mens  = 'Campo Clave vacio'
      call salir
   end
   when entorno = '' then do
      mens  = 'Campo Entorno vacio'
      call salir
   end
   when lista   = '' then do
      mens  = 'No se han pedido componentes'
      call salir
   end
   otherwise    retcod = '00'
end

drop comp.                                 /*-- comp. componentes --*/
comp.0 = componentes
do i = 1 to comp.0
   comp.i = word(lista,i)
end

call entorno_lib
call crea_libdest
call copia

drop members.
x = outtrap(members.)
"LISTDS '"libdest"' members"
x = outtrap(off)
drop miembro.                              /*-- miembro. tras copia -*/
cont = 0
do i = 7 to members.0
   cont = cont + 1
   miembro.cont = strip(members.i)
end
drop members.
miembro.0 = cont
if miembro.0 = 0 then do
   retcod = '04'
   mens   = 'No hay fuentes'
   call salir
end
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
r = 0
drop ftp.
/*ftp.1 = "ascii"*/
/* ftp.1 = "TYPE b SBD=(ISO8859-1,IBM-1145)" */
/*ftp.1 = "SITE SBD=(IBM-1145,ISO8859-1)"*/
/* ftp.2 = "prompt" */
r = r + 1 ;   ftp.r = "locsite encoding=sb"
r = r + 1 ;   ftp.r = "locsite sbd=(IBM-1145,ISO8859-1)"
r = r + 1 ;   ftp.r = "prompt"
r = r + 1 ;   ftp.r = "cd    "||home
r = r + 1 ;   ftp.r = "mkdir "||clave
r = r + 1 ;   ftp.r = "cd    "||clave
r = r + 1 ;   ftp.r = "mkdir "||cam
r = r + 1 ;   ftp.r = "cd    "||cam
contftp = r
/*--------------------------------------------------------------------*/
if db2 = 'SI' then call DB2
/*--------------------------------------------------------------------*/
/*
if index(clave,'_PEP') > 0 then call trata_PEP
else call asigna_tipos */
call asigna_tipos
contftp = contftp + 1
ftp.contftp = "quit"
ftp.0 = contftp
x=outtrap('novale',0)
"FREE FI(maix)"

/* Para la password                                   */
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
rcftp = strip(rc)
if rcftp=0 then do
   retcod = '00'
   mens   = 'servicio completado'
end
if rcftp > 0 then do
   retcod = '08'
   select
      when rcftp = 44550 then mens= 'Nombre de dir. duplicado en el AIX'
      otherwise mens = 'error 'rcftp' en el ftp'
   end
end
/*
say '<?xml version="1.0"?>'
"execio * diskr output(finis stem output."
   do i = 1 to output.0
   say output.i
end
*/
call salir
salir:
   "call *(hepwait) '00000500'" /* hhmmssdc */
   x = outtrap(delet.)
   "delete '"libdest"'"
   "delete '"fichdb21"'"
   "delete '"fichdb22"'"
   x = outtrap(off)

   do deb = 1 to delet.0
      debug = debug ' ' delet.deb
   end

   x=outtrap('novale',0)
   "FREE FI(NETRC input output)"

   call salida
   call log
   exit
return
/*--------------------------------------------------------------------*/
log:
/*x=LL00QA('LL05QA',date(),time(),userid(),retcod,mens,debug,parametros) */
  x=LL00QA('LL05QA',date(),time(),userid(),retcod,mens,parametros)
return
/*--------------------------------------------------------------------*/
crea_libdest:
   libdest = 'TMPPR.TEST.QA.'||cam||'.CL'CL
   x=outtrap('novale',0)
   "delete '"libdest"'"
   "alloc fi(COPIA) dsn('"||libdest||"') new reu ",
        "lrecl(80) blksize(0) recfm(f b) ",
        "cylinders space(15,15) unit(sysda) dir(500)"
   retorno = rc
   if retorno > 0 then do
      retcod = retorno
      mens   = 'Error en creacion de libreria de destino'
      call salir
   end
   x=outtrap('novale',0)
   "FREE FI(COPIA)"
return
/*-- Copia -----------------------------------------------------------*/
copia:
   "delstack"
/*-- Librerias origen - concatenadas ---------------------------------*/
   do ix1 = 1 to lib.0
      "alloc fi(INDD"||ix1||") da('"||lib.ix1||"') shr reuse"
      queue "COPY1 COPY INDD=INDD"||ix1||",OUTDD=OUTDD1"
      do jx = 1 to comp.0
         queue "     SELECT MEMBER="||comp.jx
      end
   end
/*-- Libreria destino ------------------------------------------------*/
   "alloc fi(OUTDD1) da('"||libdest||"') shr reuse"
        retorno = rc
        if retorno > 0 then do
           retcod = retorno
           mens   = 'Error en alloc de libreria de destino'
           call salir
        end
/*-- Libreria sysin --------------------------------------------------*/
   "alloc fi(SYSIN)  lrecl(80) blksize(0) recfm(f b) ",
          "cylinders space(15,15) unit(sysda) new reu"
   "execio "queued()" diskw sysin(finis"
/*-- Libreria sysprint -----------------------------------------------*/
   "alloc dd(sysprint) new reu unit(sysda) recfm(f b) lrecl(80)"
/*-- Proceso de copia ------------------------------------------------*/
   "call *(iebcopy)"
/*-- Free de ficheros ------------------------------------------------*/
   do ix2 = 1 to lib.0
      x=outtrap('novale',0)
      "free fi(indd"||ix2||")"
   end
   x=outtrap('novale',0)
   "free fi(outdd1 sysin sysprint)"
return
/*--------------------------------------------------------------------*/
entorno_lib:
   drop libt.
   x = 0
   select
      when entorno = 'TEST' then do
         x = x+1 ; libt.x = cam||'PR.TEST.FGENER'
         x = x+1 ; libt.x = cam||'PR.TEST.CGENER'
         x = x+1 ; libt.x = cam||'PR.TEST.JGENER'
         x = x+1 ; libt.x = cam||'PR.TEST.FUENTE'
         libt.0 = x
      end
      when entorno = 'TESZ' then do
         x = x+1 ; libt.x = cam||'PR.TESZ.FGENER'
         x = x+1 ; libt.x = cam||'PR.TESZ.CGENER'
         x = x+1 ; libt.x = cam||'PR.TESZ.JGENER'
         x = x+1 ; libt.x = cam||'PR.TESZ.FUENTE'
         libt.0 = x
      end
/*-- Las copys de ANTE las cogemos de la libreria CGENER de TEST/TESZ */
/*-- si no existen en la ANTEAL                                       */
      when entorno = 'ANTE' then do
         x = x+1 ; libt.x = cam||'PR.ANTEAL.FUENTE'
         x = x+1 ; libt.x = cam||'PR.TEST.CGENER'
         x = x+1 ; libt.x = cam||'PR.TESZ.CGENER'
         libt.0 = x
      end
/*-- Las copys de CURS las cogemos de la libreria CGENER de TEST/TESZ */
/*-- si no existen en la CURSCI                                       */
      when entorno = 'CURS' then do
         x = x+1 ; libt.x = cam||'PR.CURSCI.FUENTE'
         x = x+1 ; libt.x = cam||'PR.TEST.CGENER'
         x = x+1 ; libt.x = cam||'PR.TESZ.CGENER'
         libt.0 = x
      end
      when entorno = 'PRODPR' then do
         x = x+1 ; libt.x = cam||'PR.PRODPR.FUENTE'
         libt.0 = x
      end
      when entorno = 'PRODAL' then do
         x = x+1 ; libt.x = cam||'PR.PRODAL.FUENTE'
         libt.0 = x
      end
      when entorno = 'PRODCI' then do
         x = x+1 ; libt.x = cam||'PR.PRODCI.FUENTE'
         libt.0 = x
      end
      when entorno = 'PRODEX' then do
         x = x+1 ; libt.x = cam||'PR.PRODEX.FUENTE'
         libt.0 = x
      end
      otherwise nop
   end
   x = 0 /* Compruebo que existan estas librerias para este CAM */
   do i = 1 to libt.0
      existe = listdsi("'"libt.i"'")
      if existe = 0 then do
         x = x+1
         lib.x = libt.i
      end
   end
   lib.0 = x
return
/*--------------------------------------------------------------------*/
add_ftp:
   existe = SYSDSN("'"libdest"("miembro.i")'")
   call outtrap "LINE.", "*"
   if existe  = 'OK' then do
      contftp = contftp + 1
      ftp.contftp ="put '"||libdest||"("||miembro.i||")' ",
                   miembro.i||"."||type
   end
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
   if control_copy = 'COPY' then call copia_copy    /* drb */
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
      copy = substr(leer.j,6,8)
      comp.0 = 1
      comp.1 = copy
      call copia
      miembro.i = copy
      type = NPY
      call add_ftp
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
   fichdb21 = "CHM.PROD.INICIAL.QA51.C1"||CL
   fichdb22 = "CHM.PROD.INICIAL.QA52.C2"||CL
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

   queue "//CHMSQ5"||job_random||" JOB (UGASTO,CHM,OSN,VPCHM),"
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
      retcodb2 = '1'
      "call *(hepwait) '00000200'" /* hhmmssdc */
      existe1 = listdsi(fichdb21)
      existe2 = listdsi(fichdb22)
      if existe1 = "0" & existe2 = "0" then do
         retcodb2 = "0"
         leave
      end
   end
   if retcodb2 > 0 then do
      mens = "error db2 - RC "retcodb2
      retcod = '01'
   end
   if existe1  = 0 then do
      contftp  = contftp + 1
      ftp.contftp ="put '"||fichdb21||"' CATA.DB2"
   end
   if existe2  = 0 then do
      contftp  = contftp + 1
      ftp.contftp ="put '"||fichdb22||"' PLAN.DB2"
   end
return
/*--------------------------------------------------------------------*/
salida:
   say '<service>'
   say '<name>ll05qa</name>'
   say '<desc>FTP componentes</desc>'
   say '<dir>'clave'</dir>'
   say '<rc>'retcod'</rc>'
   say '<msg>'mens'</msg>'
   say '</service>'
return
/*--------------------------------------------------------------------*/
/*-- Asigno los tipos ------------------------------------------------*/
/*--------------------------------------------------------------------*/
asigna_tipos:
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
         type= JCL
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
      type = PRD
      call add_ftp
      call rexx_productos
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
   leer.1 = translate(leer.1)
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
return
/*--------------------------------------------------------------------*/
/*-- Fin de asignacion de tipos --------------------------------------*/
/*--------------------------------------------------------------------*/
trata_PEP:
   f_ini = substr(clave,4,8)
   f_fin = '99999999'
   h_ini = '000000'
   h_fin = '999999'
/* altas   - bajas */
   A1 = CBL;              /* MODBATCH.   */
   B1 = CBL; B0 = NO      /* MODCICS.    */
   C1 = CBL; C0 = NO      /* MODGENER.   */
   D1 = CPY; D0 = NO      /* COPYSMAC.   */
   E1 = NO ; E0 = NO      /* MODINTERP.  */
   F1 = NO ; F5 = NO      /* ESQISPF.    */
   F2 = NO ; F6 = NO      /* PANELISPF.  */
   F3 = NO ; F7 = NO      /* CLISTISPF.  */
   F4 = NO ; F8 = NO      /* MSGSISPF.   */
   G1 = JCL; G0 = NO      /* JCLSAT.     */
   H1 = NO ; H4 = NO      /* ESQUELEBA.  */
   H2 = NO ; H5 = NO      /* DLISTEBA.   */
   H3 = NO ; H6 = NO      /* TTRANEBA.   */
   I1 = NO ; I3 = NO      /* MODULNATL.  */
   I2 = NO ; I4 = NO      /* MAPANATL.   */
   J1 = NO ; J0 = NO      /* TESTPROD.   */
   N1 = NO ; NA = NO      /* AREAPARM.   */
   N2 = NO ; NB = NO      /* COPYCODE.   */
   N3 = NO ; NC = NO      /* AREAGLOB.   */
   N4 = NO ; ND = NO      /* HELPRUTINE. */
   N5 = NO ; NE = NO      /* AREALOCAL.  */
   N6 = CBL; NF = NO      /* SUBPROG.    */
   N7 = CBL; NG = NO      /* SUBRUTINA.  */
   R1 = NO ; R0 = NO      /* TABLAISPF.  */
   T1 = JCL; T0 = NO      /* JCLNATL.    */
   U1 = NO ; U0 = NO      /* CLISTEBA.   */
   V1 = CBL; V0 = NO      /* RUTINA.     */
   W1 = NO ; W0 = NO      /* DEFVSAM.    */
   N8 = NO ; NH = NO      /* LIBNATL.    */
   K1 = NO ; K0 = NO      /* USERVIEW.   */
   L1 = NO ; L0 = NO      /* FADABAS.    */
   M1 = NO ; M0 = NO      /* USERVADA.   */
   P1 = NO ; P0 = NO      /* MENSNAT.    */
   Q1 = NO ; Q0 = NO      /* HELPTEXT.   */
   S1 = NO ; S0 = NO      /* PROCEDIM.   */
   X1 = NO ; X0 = NO      /* FUENTENAT.  */
   O1 = NO                /* VERSION.    */
   K2 = PRD               /* PRODUCTO.   */
   M2 = NO                /* MAPABMS.    */
   M4 = NO                /* CARGAMIGA.  */
   M6 = NO                /* ENTMDATOS.  */
   M8 = NO                /* EAPLCONV.   */
   K4 = NO                /* APLAANEA.   */
   K6 = NO                /* ELEMMODIF.  */
   K8 = NO                /* CARGAPRUE.  */

   select
      when entorno = 'ANTE'   then entorno_short = 'AN'
      when entorno = 'CURS'   then entorno_short = 'CU'
      when entorno = 'PRODPR' then entorno_short = 'PR'
      when entorno = 'PRODAL' then entorno_short = 'AL'
      when entorno = 'PRODCI' then entorno_short = 'CI'
      when entorno = 'PRODEX' then entorno_short = 'EX'
      otherwise do
         retcod = '08'
         mens   = 'Entorno 'entorno' no valido'
         call salir
      end
   end /* select */

/* do 6
      CL = CL||RANDOM(1,9)
   end */
   fichpep = "TMPPR.TEST.SQA.PEPLOG.CL"||CL

   job_random = ''
   do 2
      job_random = job_random||RANDOM(1,9)
   end

   "delstack"
   queue "//CHMSQ5"||job_random||" JOB (UGASTO,CHM,OSN,VPCHM),"
   queue "//         'HERR.-PROD',MSGCLASS=X,CLASS=P,"
   queue "//         USER=&SYSUID,MSGLEVEL=(1,1)"
   queue "//*"
   queue "//*  EJECUTA UN PROGRAMA NATURAL"
   queue "//*  QUE EXTRAE LOS PASES DEL PEP EN UNAS"
   queue "//*  FECHAS PROPORCIONADAS POR PARAMETROS"
   queue "//*"
   queue "//*  GENERA UN FICHERO SECUENCIAL:"
   queue "//*     - UN FICHERO CON LOS PASES DEL PEP AFECTADOS"
   queue "//*"
   queue "//****************************************************"
   queue "//*"
   queue "//PASO001  EXEC ADAPG2,BD=PRUE"
   queue "//CMPRINT  DD SYSOUT=X"
   queue "//CMWKF01  DD DSN="||fichpep||","
   queue "//             DISP=(,CATLG,DELETE),"
   queue "//             UNIT=SYSDA,SPACE=(CYL,(15,15),RLSE),"
   queue "//             RECFM=FB,LRECL=298"
   queue "//SYSIN    DD   *"
   queue "PEPN600"
   queue f_ini
   queue f_fin
   queue h_ini
   queue h_fin
   queue "/*"
   queue "//*"
   queue "$$"
   x = outtrap(sdsf.)
   "submit * end($$)"
   x = outtrap(off)
   parse var sdsf.2 . '('vfic')' .
   jobid = vfic
   "call *(hepwait) '00001000'" /* hhmmssdc */
   rc=isfcalls('ON')
   ISFOWNER  = '*'
   ISFPREFIX = CHMSQ5||job_random
   retcodnat = ''
   do j = 1 to 10
      Address SDSF "ISFEXEC H (ALTERNATE DELAYED)"
      ix = isfrows
      "call *(hepwait) '00000300'" /* hhmmssdc */
      do i = 1 to ix
         jobid.i = strip(jobid.i)
         retcode.i = strip(retcode.i)
         if jobid.i = jobid then retcodnat = retcode.i
      end
      select
         when retcodnat = '' then iterate
         when retcodnat = 'CC 0000' then leave
         otherwise do
            retcod = '08'
            mens   = 'timeout del PEPN600'
            call salir
         end
      end  /* select */
   end /* do forever */
   rc=isfcalls('OFF')
   /* SORT */
   /*-- SYSIN: orden por APLI y MIEMBRO y evito duplicados --*/
   drop sysin.
   sysin.1 =" SORT FIELDS=(1,4,CH,A,237,8,CH,A)"
   "alloc fi(SYSIN)  lrecl(80) blksize(0) recfm(f b) ",
          "cylinders space(15,15) unit(sysda) new reu"
   "execio * diskw SYSIN  (stem sysin. finis)"
   "alloc dd(sysout) new reu unit(sysda) recfm(f b) lrecl(80)"
   "alloc fi(SORTIN)  da('"fichpep"') shr"
   "alloc fi(SORTOUT) da('"fichpep"') shr"
   drop sal.
   ADDRESS LINKMVS ICEMAN
   "execio * diskr SORTOUT (finis stem sal.)"
   x = outtrap(free.)
   "FREE FI(SYSOUT SORTIN SORTOUT SYSIN)"
   x = outtrap(off)

   x=outtrap('novale',0)
   "delete '"||fichpep||"'"
   if sal.0 = 0 then do /* si no hay registros */
      retcod = '04'
      mens   = 'No hay registros en el log del PEP'
      call salir
   end

   do i = 1 to sal.0
      call parseo
      if nodo_destino = entorno_short & value(tipo_elem) = 'NO' then do
         x = value(value(miembro),value(tipo_elemento))
      end
   end

   do i = 1 to miembro.0
      type = value(miembro.i)
      call add_ftp
      if value(miembro.i) = 'JCL' then do
         call LL09QA entorno libdest"("miembro.i")"
      end
      if value(miembro.i) = 'PRD' then do
         drop leer.
         "alloc da('"libdest"("miembro.i")') fi(leer) shr reu"
         "execio * diskr leer(finis stem leer."
         x = outtrap(free.)
         "free fi(leer)"
         x = outtrap(off)
         call rexx_productos
      end
   end

return
/*-- Registro del log del PEP ----------------------------------------*/
parseo:
   parse var sal.i    APLI             +4,
                      TIPO_ELEM        +2,
                      FECHA_PETIC      +14,
                      USUARIO          +7,
                      RESPONSABLE      +7,
                      ORDEN_TRABAJO    +8,
                      LITERAL          +80,
                      ESTADO           +1,
                      DIF_FECHA_HORA   +14,
                      RES_FECHA_HORA   +14,
                      NODO_DESTINO     +2,
                      LIB_ORIGEN       +35,
                      LIB_DESTINO      +35,
                      BD_DESTINO       +8,
                      CICS_DESTINO     +5,
                      MIEMBRO          +8,
                      LENGUAJE         +1,
                      IND_DBUG         +1,
                      TRABAJO          +8,
                      LISTADO          +8,
                      VERSION_PEP      +8,
                      TRANSACCION      +4,
                      PETICIONARIO     +7,
                      ENTORNO_PEP      +1,
                      UNIDAD_GASTO     +5,
                      PEP_VERSION      +1,
                      PETIC_FECHA_HORA +8,
                      TIPO_ELEMENTO    /* 2 */

   APLI            = strip(APLI)
   TIPO_ELEM       = strip(TIPO_ELEM)
   FECHA_PETIC     = strip(FECHA_PETIC)
   USUARIO         = strip(USUARIO)
   RESPONSABLE     = strip(RESPONSABLE)
   ORDEN_TRABAJO   = strip(ORDEN_TRABAJO)
   LITERAL         = strip(LITERAL)
   ESTADO          = strip(ESTADO)
   DIF_FECHA_HORA  = strip(DIF_FECHA_HORA)
   RES_FECHA_HORA  = strip(RES_FECHA_HORA)
   NODO_DESTINO    = strip(NODO_DESTINO)
   LIB_ORIGEN      = strip(LIB_ORIGEN)
   LIB_DESTINO     = strip(LIB_DESTINO)
   BD_DESTINO      = strip(BD_DESTINO)
   CICS_DESTINO    = strip(CICS_DESTINO)
   MIEMBRO         = strip(MIEMBRO)
   LENGUAJE        = strip(LENGUAJE)
   IND_DBUG        = strip(IND_DBUG)
   TRABAJO         = strip(TRABAJO)
   LISTADO         = strip(LISTADO)
   VERSION_PEP     = strip(VERSION_PEP)
   TRANSACCION     = strip(TRANSACCION)
   PETICIONARIO    = strip(PETICIONARIO)
   ENTORNO_PEP     = strip(ENTORNO_PEP)
   UNIDAD_GASTO    = strip(UNIDAD_GASTO)
   PEP_VERSION     = strip(PEP_VERSION)
   PETIC_FECHA_HORA= strip(PETIC_FECHA_HORA)
   TIPO_ELEMENTO   = strip(TIPO_ELEMENTO)
return
