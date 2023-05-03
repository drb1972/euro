/* REXX - Log de los uss                                      */
/*  x=usslog('ussxmlxx',date(),time(),userid(),parametros)    */
/* parse arg xml , fecha , hora , usuario , retcod, mens , debug , param */

/* CHANGE 99 */

parse arg xml , fecha , hora , usuario , retcod, mens , param
/* comment 1 */
/* comment 2 */
/* comment 3 */
/* comment 4 */
say 'Hola'

/* VAUGHN    */
/* ccid 1234 */
sal.1='Ejecutado:  'xml' - Fecha:'fecha' - Hora:'hora' - User:'usuario
sal.2='Parametros: 'param
sal.3='RC 'retcod
sal.4= mens
sal.5= '=============================================================='
sal.6= ''
sal.0= 6
sufijo = 'A'substr(date('S'),1,4)
fichero='CHM.PROD.INICIAL.SQALOG.'||sufijo
miembro = 'D'substr(date('S'),2)
fich_miembro = fichero||"("||miembro||")"
existe=sysdsn("'"fichero"'")
/* Primera vez - Se crea el PDS-E */
if existe=translate('DATASET NOT FOUND') then do
   "alloc da('"fichero"') lrecl(1250) blksize(0) ",
   "recfm(f b) dsorg(po) dsntype(library) " ,
   "cylinders space(25,25) fi(usslog) unit(sysda) new reu "
   x=outtrap('novale',0)
   "free fi(usslog)"
end

/* Fichero existe */
existe=sysdsn("'"fich_miembro"'")
"alloc da('"fich_miembro"') shr fi(usslog)"
if existe='OK' then do
   "EXECIO * DISKR usslog (FINIS stem datos.)"
   cont = datos.0
   do rr  = 1 to sal.0
      cont = cont + 1
      datos.cont = sal.rr
   end
   datos.0 = cont
   "EXECIO * DISKW usslog (FINIS stem datos.)"
end
   else do
"EXECIO * DISKW usslog (FINIS stem sal.)"
end
x=outtrap('novale',0)
"free fi(usslog)"
exit 0
