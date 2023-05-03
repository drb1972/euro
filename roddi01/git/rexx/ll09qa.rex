/*-- rexx - Cambia las variables corchete de los JCLs               --*/
/*-- LL09QA                                                         --*/
/*--------------------------------------------------------------------*/
arg entorno fichero

select
   when entorno = PRODPR then entorno = PRUE
   when entorno = PRODAL then entorno = ALFA
   when entorno = PRODCI then entorno = CINF
   when entorno = PRODEX then entorno = EXPL
   otherwise nop
end

call cargar_tabla
call tratar_jcl

x = outtrap(members.)
"free fi(jcl)"
x = outtrap(off)
exit
/*--------------------------------------------------------------------*/
cargar_tabla:
   tabla = 'CHM.PADM.DATOS(VARS)'
   drop tabla.

   "alloc fi(tabla) da('"tabla"') shr"
   "execio * diskr tabla(finis stem tabla."
   x = outtrap(members.)
   "free fi(tabla)"
   x = outtrap(off)

   do ivc = 1 to tabla.0
      tabla.ivc = translate(tabla.ivc,' ',';')
   end
   columnas_tabla = words(tabla.1)

   do ivc= 1 to columnas_tabla/* 1 pila por cada columna de la tabla */
      vari= word(tabla.1,ivc) /*  pila.TEST. pila.TESZ. ...          */
      drop pila.vari.         /*  La clave es pila.ORIG.             */
      pila.vari.0 = tabla.0
   end

   do ivc = 1 to columnas_tabla /* cargo los datos de las tablas */
      do jvc = 1 to tabla.0
         vari= word(tabla.1,ivc)
         pila.vari.jvc = word(tabla.jvc,ivc)
         pila.vari.jvc = strip(pila.vari.jvc) /* quito blancos */
      end
   end
/*
   do ivc = 1 to pila.entorno.0
      say pila.entorno.ivc
   end */
return
/*--------------------------------------------------------------------*/
tratar_jcl:

   "alloc fi(jcl) da('"fichero"') shr"
   drop jcl.
   "execio * diskr jcl(finis stem jcl."
   cambia_jcl = 'NO'
   do iv1 = 1 to jcl.0
      do jv1 = 1 to pila.ORIG.0
         posicion = 0
         posicion = pos(pila.ORIG.jv1,jcl.iv1)
         if posicion = 0 then do
            cambia_jcl= 'SI'
            long_jcl  = length(jcl.iv1)
            longitud  = length(pila.ORIG.jv1)
            cabecera  = substr(jcl.iv1,1,posicion - 1)
            cola      = substr(jcl.iv1,posicion + longitud,80)
            jcl.iv1   = cabecera || pila.entorno.jv1 ||cola
            iv1 = iv1 - 1 /* por si hay mas de una variable en 1 reg */
         end
      end
   end
   if cambia_jcl= 'SI' then do
      "execio * diskw jcl(finis stem jcl."
   end
return
/*--------------------------------------------------------------------*/
