/*-- rexx - Obtienene lista de productos NEA disponibles en un    ----*/
/*          entorno determinado para un filtro                      --*/
/*-- LL07QA                                                      -----*/
/*--------------------------------------------------------------------*/
/* parametros: filtro - entorno - listapl                             */
/* filtro    : Minimo el CAM                                          */
/* entornos  : TEST-TESZ-ANTE-CURS-PRODPR-PRODAL-PRODCI-PRODEX        */
/* listapl   : aplicaciones permitidas (* - todas)                    */
/*           : si hay una lista, tiene que ser CAM + T o Z            */
/*--------------------------------------------------------------------*/
/*--------------------------------------------------------------------*/
parse upper arg subsis ',' filtro ',' entorno ',' listapl
parse upper arg parametros
/*--------------------------------------------------------------------*/

subsis   = strip(subsis)
filtro   = strip(filtro)
entorno  = strip(entorno)
listapl  = strip(listapl)
filtro   = space(translate(filtro,' ','*'))
cam = substr(filtro,1,3)
cam = strip(cam)
call check_aplicaciones

call entornos

x = outtrap(members.)
"LISTDS '"lib"' members"
x = outtrap(off)
drop miembro.
cont = 0
do i = 7 to members.0
   members.i = strip(members.i)
   Long = length(members.i)
/* cara = datatype(substr(members.i,1,3),m) puede ser T2C */
   cola = datatype(substr(members.i,4,4),n)
   if Long=7 & index(members.i,filtro) > 0 & cola=1 then do
/* if Long=7 & index(members.i,filtro) > 0 then do */
      "alloc fi(prod) da('"lib"("members.i")') SHR"
      "execio 1 diskr prod (finis stem idepepi.)"
      x = outtrap(free.)
      "Free fi(prod)"
      x = outtrap(off)
      if index(idepepi.1,IDEPEPI) > 0 then do
         cont = cont + 1
         miembro.cont = members.i
      end
   end
end
drop members.
miembro.0 = cont
if cont = 0 then do
   retcod = '04'
   mens   = 'no hay productos con el criterio 'filtro
   end
   else do
   retcod = '00'
   mens   = miembro.0||' productos'
   end
/*--------------------------------------------------------------------*/
salir:
   call salida
   call log
   exit
return
/*--------------------------------------------------------------------*/
check_aplicaciones:
   x = length(cam)
   if x = 3 then do
      retcod = '08'
      mens   = 'El filtro tiene que tener al menos el CAM'
      call salir
   end
   if listapl = '*' then return
   cam_entorno = ''
   select
      when cam     = 'NEA'  then cam_entorno = cam
      when entorno = 'TEST' then cam_entorno = cam||'T'
      when entorno = 'TESZ' then cam_entorno = cam||'Z'
      otherwise cam_entorno = cam
   end

   if index(listapl,cam_entorno) = 0 then do
      retcod = '08'
      mens   = cam_entorno' no autorizado'
      call salir
   end
return
/*--------------------------------------------------------------------*/
/*-- Entornos                                                       --*/
/*-- TEST - TESZ - ANTE - CURS - PRODPR - PRODAL - PRODCI - PRODEX  --*/
entornos:
   select
      when entorno = 'TEST'   then lib = cam||'PR.TEST.FGENER'
      when entorno = 'TESZ'   then lib = cam||'PR.TESZ.FGENER'
      when entorno = 'ANTE'   then lib = cam||'PR.ANTEAL.FUENTE'
      when entorno = 'CURS'   then lib = cam||'PR.CURSCI.FUENTE'
      when entorno = 'PRODPR' then lib = cam||'PR.PRODPR.FUENTE'
      when entorno = 'PRODAL' then lib = cam||'PR.PRODAL.FUENTE'
      when entorno = 'PRODCI' then lib = cam||'PR.PRODCI.FUENTE'
      when entorno = 'PRODEX' then lib = cam||'PR.PRODEX.FUENTE'
      otherwise do
         retcod = '08'
         mens   = 'Entorno' entorno' no valido'
         call salir
         exit
      end
   end
return
/*--------------------------------------------------------------------*/
salida:
   say '<?xml version="1.0"?>'
   say '<service>'
   say '<name>ll07qa</name>'
   say '<desc>lista de productos NEA en un entorno</desc>'
   say '<prodlist>'
   if miembro.0 > 0 then do
      do i = 1 to miembro.0
         say '<prod>'miembro.i'</prod>'
      end
   end
   say '</prodlist>'
   say '<rc>'retcod'</rc>'
   say '<msg>'mens'</msg>'
   say '</service>'
return
/*--------------------------------------------------------------------*/
log:
   x=LL00QA('LL07QA',date(),time(),userid(),retcod,mens,parametros)
return
/*--------------------------------------------------------------------*/
/*--------------------------------------------------------------------*/
/*-- Rutina que guardo por si acaso ----------------------------------*/
/*--------------------------------------------------------------------*/
/*-- Si queremos la lista de aplicaciones en listap. -----------------*/
/*-- Obtenemos el valor de listapl. -----------------------------------*/
/*--------------------------------------------------------------------*/
/*
if listapl = '' then do
   listapl  = translate(listapl,' ',',')
   n_listapl = words(listapl)
   drop listapl_t. /* Cargo listapl_t. con las cam que me vienen      */
   do i = 1 to n_listapl
      listapl_t.i = word(listapl,i)
      listapl_t.i = strip(listapl_t.i)
      listapl_t.i = substr(listapl_t.i,1,3)
   end
   listapl_t.0 = n_listapl
   drop listapl.
   cont = 0
   /*-- Hago esto porque no se si me llegan cams de 3 o 4 caracteres -*/
   do i = 1 to listapl_t.0
      j = i - 1
      if listapl_t.i = listapl_t.j then do
         cont = cont + 1
         listapl.cont = listapl_t.i
      end
   end
   listapl.0 = cont
   end /* then */
   else do
      drop listapl.
      listapl.0 = 1
      listapl.1 = '*'
end /* if */
*/
/*-- las aplicaciones autorizadas estan en listapl. ------------------*/
