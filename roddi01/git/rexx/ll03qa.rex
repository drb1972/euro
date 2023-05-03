/*-- rexx - Lista de pases del PEP                                  --*/
/*-- LL03QA                                                      -----*/
/*--------------------------------------------------------------------*/
/*-- Ejemplo con un result por aplicacion ----------------------------*/
/*--------------------------------------------------------------------*/
/*-- Argumentos:                                                    --*/
/*-- entornos  : ANTE-CURS-PRODPR-PRODAL-PRODCI-PRODEX              --*/
/*-- f_ini     : aaaammdd                                           --*/
/*-- f_fin     : aaaammdd                                           --*/
/*-- h_ini     : hhmmss                                             --*/
/*-- h_fin     : hhmmss                                             --*/
/*-- aplperm   : * para todas                                       --*/
/*--------------------------------------------------------------------*/
/* ccid 1234 */
/*--------------------------------------------------------------------*/
parse upper arg subsis','ent','f_ini','f_fin','h_ini','h_fin','aplperm
parse upper arg parametros
entorno  = ent

/*--------------------------------------------------------------------*/
cont = 0
retcod = ''
mens   = ''
if f_ini > f_fin then do
   retcod = '08'
   mens   = 'Fecha inicio posterior a fecha fin'
   call salir
end
if f_ini = f_fin & h_ini = h_fin then do
   retcod = '08'
   mens   = 'Fecha inicio similar a fecha fin'
   call salir
end
call init
call peplog /*-- obtengo el fichero del PEP --*/
call sort   /*-- ordeno el fichero por CAM  --*/
x=outtrap('novale',0)
"delete '"||fichpep||"'"
if sal.0 = 0 then do /* si no hay registros */
   retcod = '04'
   mens   = 'No hay registros en el log del PEP'
   call salir
end

existe = NO          /* SI = registros validos */
cam. = ''
elem. = ''
zosnea. = 'NO'
do i = 1 to sal.0
   call parseo
   cam = substr(apli,1,3)
   if index(aplperm,cam) > 0 then aplperm = 'SI'
   if aplperm = '*' then aplperm = 'SI'
   if nodo_destino = entorno_short & value(tipo_elem) = SI ,
      & aplperm = SI then do
      existe = SI
      select
         when PEP_VERSION <> '' then nop
         when apli = apliant then do
            cont = cont + 1
            cam.cont  = apli
            elem.cont = miembro
            apliant = apli
            elemant = miembro
            end
         when apli = apliant & miembro = elemant then do
            elem.cont = elem.cont ||','||miembro
            elemant = miembro
            end
         otherwise nop
      end
      if tipo_elem = 'K2' then zosnea.cont = 'SI'
   end /* if 1 */
end /* do */

if cont = 0 then do
   retcod = '04'
   mens   = 'No hay registros validos'
   call salir
end
retcod = '00'
mens   = cont||' registros validos'
call salir
/*--------------------------------------------------------------------*/
salir:
   call salida
   call log
   exit
return
/*== RUTINAS =========================================================*/
/*--------------------------------------------------------------------*/
log:
   x=LL00QA('LL03QA',date(),time(),userid(),retcod,mens,parametros)
return
/*-- PEPLOG ----------------------------------------------------------*/
peplog:
   do 6
      CL = CL||RANDOM(1,9)
   end
   fichpep = "TMPPR.TEST.SQA.PEPLOG."||CL
   "delstack"

   job_random = ''
   do 2
      job_random = job_random||RANDOM(1,9)
   end

   queue "//CHMSQ3"||job_random||" JOB (UGASTO,CHM,OSN,VPCHM),"
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
   "call *(hepwait) '00000500'" /* hhmmssdc */
   rc=isfcalls('ON')
   ISFOWNER  = '*'
   ISFPREFIX = CHMSQ3||job_random
   retcodnat = ''
   do j = 1 to 10
      Address SDSF "ISFEXEC H (ALTERNATE DELAYED)"
      ix = isfrows
      "call *(hepwait) '00000100'" /* hhmmssdc */
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
return
/*-- Sort ------------------------------------------------------------*/
sort:
   /*-- SYSIN: orden por APLI y MIEMBRO y evito duplicados --*/
   drop sysin.
   sysin.1 =" SORT FIELDS=(1,4,CH,A,237,8,CH,A)"
   "alloc fi(SYSIN)  lrecl(80) blksize(0) recfm(f b) ",
          "cylinders space(1,1) unit(sysda) new reu"
   "execio * diskw SYSIN  (stem sysin. finis)"
   "alloc dd(sysout) new reu unit(sysda) recfm(f b) lrecl(80)"
   "alloc fi(SORTIN)  da('"fichpep"') shr"
   "alloc fi(SORTOUT) da('"fichpep"') shr"
   drop sal.
   ADDRESS LINKMVS ICEMAN
   "execio * diskr SORTOUT (finis stem sal.)"
   x = outtrap(members.)
   "FREE FI(SYSOUT SORTIN SORTOUT SYSIN)"
   x = outtrap(off)
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
/*--------------------------------------------------------------------*/
init:
/* altas   - bajas */
   A1 = SI ; A0 = NO      /* MODBATCH.   */
   B1 = SI ; B0 = NO      /* MODCICS.    */
   C1 = SI ; C0 = NO      /* MODGENER.   */
   D1 = SI ; D0 = NO      /* COPYSMAC.   */
   E1 = NO ; E0 = NO      /* MODINTERP.  */
   F1 = NO ; F5 = NO      /* ESQISPF.    */
   F2 = NO ; F6 = NO      /* PANELISPF.  */
   F3 = NO ; F7 = NO      /* CLISTISPF.  */
   F4 = NO ; F8 = NO      /* MSGSISPF.   */
   G1 = SI ; G0 = NO      /* JCLSAT.     */
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
   N6 = NO ; NF = NO      /* SUBPROG.    */
   N7 = NO ; NG = NO      /* SUBRUTINA.  */
   R1 = NO ; R0 = NO      /* TABLAISPF.  */
   T1 = SI ; T0 = NO      /* JCLNATL.    */
   U1 = NO ; U0 = NO      /* CLISTEBA.   */
   V1 = SI ; V0 = NO      /* RUTINA.     */
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
   K2 = SI                /* PRODUCTO.   */
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
         say '<rc =08>'
         say '<msg=Entorno 'entorno' no valido>'
         call salir
      end
   end /* select */
return
/*--------------------------------------------------------------------*/
salida:
   say '<?xml version="1.0"?>'
   say '<service>'
   say '<name>ll03qa</name>'
   say '<desc>lista de pases del PEP</desc>'
   say '<entregasPEP>'
   say '<entorno>'||entorno||'</entorno>'
   do i = 1 to cont
      say '<entrega>'
      say '<cam>'||cam.i||'</cam>'
      say '<nea>'||zosnea.i||'</nea>'
      say '<elemento>'||elem.i||'</elemento>'
      say '</entrega>'
   end
   if cont = 0 then do
      say '<entrega>'
      say '</entrega>'
   end
   say '<rc>'retcod'</rc>'
   say '<msg>'mens'</msg>'
   say '</entregasPEP>'
   say '</service>'
return
