/* REXX */
/* CHANGE 99 */
/* CHANGE 98 */
TCB   = C2D(STORAGE(21C,4))
TIOT  = C2D(STORAGE(D2X(TCB+12),4))
JNAM  = STORAGE(D2X(TIOT),8)
JSCB  = C2D(STORAGE(D2X(TCB+180),4))
SSIB  = C2D(STORAGE(D2X(JSCB+316),4))

say 'Hola'

"EXECIO 1 DISKR CLAVE(FINIS STEM REG."
CLAVE=STRIP(REG.1)

SALIDA1.1 = "ST " || JNAM || " " || JNUM
SALIDA1.2 = CLAVE
SALIDA2.1 = "/*$TOJ(" || JNUM || "),OUTGRP=1.1.1,D=PRUE"

"EXECIO * DISKW SALIDA1 (FINIS STEM SALIDA1."
"EXECIO * DISKW SALIDA2 (FINIS STEM SALIDA2."

SAY 'SOY EL JOB 'JNAM' CON EL ID 'JNUM' DEL PAQUETE CLAVE 'CLAVE
EXIT 0
