/*-- rexx - 1.6    Obtencin de catlogo y plan_table DB2        -----*/
/*-- LL06QA                                                      -----*/
/*--------------------------------------------------------------------*/
/* parametros: SSID - TBCREATOR - TBNAME                              */
/*--------------------------------------------------------------------*/
/*--------------------------------------------------------------------*/
/* TRACE I  */
STATUS = MSG("OFF")
ARG SSID TBCREATOR TBNAME
ARG parametros
RETCODE = 0

CALL CONECTAR_DB2_1
CALL LEER_CATALOGO
CALL ESCRIBIR_SALIDA_1
CALL CONECTAR_DB2_2
CALL LEER_PLANTABLE
CALL ESCRIBIR_SALIDA_2
/* call log */

/*drop sal.
"execio * diskr listcata(finis stem sal."
do i = 1 to sal.0
   say sal.i
end
"execio * diskr listplan(finis stem sal."
do i = 1 to sal.0
   say sal.i
end */

FIN:
/* "free fi(listcata sysprint systsprt)" */
/* "delete "libtemp */
   exit retcode

/* ------------------------------------------------------------- */
/* Conectar con el subsistema DB2                                */
/* ------------------------------------------------------------- */

CONECTAR_DB2_1:

ADDRESS TSO "SUBCOM DSNREXX"
IF RC THEN
  S_RC = RXSUBCOM('ADD','DSNREXX','DSNREXX')

/* Conexion con el subsistema DB2 */
ADDRESS DSNREXX "CONNECT" SSID
IF SQLCODE <> 0 THEN DO
  SQLERRORPOSITION = 'Conectar con subsistema' SSID
  mens = 'Error conectar_db2_1'
  retcod = 08
  CALL SQLERRORROUTINE
END
RETURN

/* ------------------------------------------------------------- */
/* Leer CATALOGO                                                 */
/* ------------------------------------------------------------- */

LEER_CATALOGO:

LTNAME. = ''
LTCARD. = ''
LTPAGE. = ''
LINAME. = ''
LIUQRL. = ''
LI1KCD. = ''
LIFKCD. = ''
LCNAME. = ''
LCCLNO. = ''

/* Leer el catalogo de DB2 para obtener los campos del fichero   */
/* de salida                                                     */
/*"   LPAD(DEC(I.FIRSTKEYCARDF, 9), 9, ' ') AS I1KCD," ,
  "   LPAD(DEC(I.FULLKEYCARDF, 9), 9, ' ')  AS IFKCD," , */
SQLSTMT = ,
  "SELECT                             ",
  "   RPAD(T.NAME, 31, '0') AS TNAME," ,
  "   LPAD(T.CARD, 10, '0')  AS TCARD," ,
  "   LPAD(T.NPAGES, 9, '0') AS TPAGE," ,
  "   RPAD(I.NAME, 31, '0') AS INAME," ,
  "   RPAD(I.UNIQUERULE, 1)  AS IUQRL," ,
  "   LPAD(DEC(I.FIRSTKEYCARDF, 10), 10, '0') AS I1KCD," ,
  "   LPAD(DEC(I.FULLKEYCARDF, 10), 10, '0')  AS IFKCD," ,
  "   RPAD(K.COLNAME , 31, '0') AS CNAME," ,
  "   LPAD(K.COLNO, 5, '0')  AS CCLNO " ,
  "  FROM SYSIBM.SYSTABLES  T,        " ,
  "       SYSIBM.SYSINDEXES I,        " ,
  "       SYSIBM.SYSKEYS    K         " ,
  "  WHERE  T.NAME LIKE '"TBNAME"'    " ,
  "    AND  T.CREATOR = '"TBCREATOR"' " ,
  "    AND I.TBNAME = T.NAME          " ,
  "    AND I.TBCREATOR = T.CREATOR    " ,
  "    AND K.IXNAME = I.NAME          " ,
  "    AND K.IXCREATOR = I.TBCREATOR  " ,
  "  ORDER BY T.NAME, I.NAME, K.COLSEQ" ,
  "  WITH UR                          "

ADDRESS DSNREXX
"EXECSQL DECLARE  C1 CURSOR FOR S1"
IF SQLCODE <> 0 THEN DO
  mens = 'DECLARE C1 CURSOR FOR S1'
  retcod = 08
  SQLERRORPOSITION = 'DECLARE C1 CURSOR FOR S1'
  CALL SQLERRORROUTINE
END
"EXECSQL PREPARE  S1 INTO :OUTSQLDA FROM :SQLSTMT"
IF SQLCODE <> 0 THEN DO
  mens = 'prepare s1'
  retcod = 08
  SQLERRORPOSITION = 'PREPARE S1'
  CALL SQLERRORROUTINE
END
"EXECSQL OPEN     C1"
IF SQLCODE <> 0 THEN DO
  mens = 'open c1'
  retcod = 08
  SQLERRORPOSITION = 'OPEN C1'
  CALL SQLERRORROUTINE
END

K = 0
DO UNTIL (SQLCODE <> 0)
  ADDRESS DSNREXX
  "EXECSQL FETCH  C1 USING DESCRIPTOR :OUTSQLDA"
  IF SQLCODE = 0 THEN DO
    K = K + 1
    DO I = 1 TO OUTSQLDA.SQLD  /* numero de columnas */
      SELECT
        WHEN OUTSQLDA.I.SQLNAME = 'TNAME'        THEN
          LTNAME.K   = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'TCARD'        THEN
          LTCARD.K   = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'TPAGE'        THEN
          LTPAGE.K   = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'INAME'        THEN
          LINAME.K   = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'IUQRL'        THEN
          LIUQRL.K   = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'I1KCD'        THEN
          LI1KCD.K   = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'IFKCD'        THEN
          LIFKCD.K   = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'CNAME'        THEN
          LCNAME.K   = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'CCLNO'        THEN
          LCCLNO.K   = OUTSQLDA.I.SQLDATA
        OTHERWISE
          NOP
      END /* SELECT */
    END I /* DO I de columnas */
  END  /* IF SQLCODE */
END /* DO UNTIL de filas */
NU_FILAS = K
ADDRESS DSNREXX
"EXECSQL COMMIT"
"DISCONNECT"
RETURN

/* ------------------------------------------------------------- */
/* Rutina ESCRIBIR_SALIDA                                        */
/* ------------------------------------------------------------- */
ESCRIBIR_SALIDA_1:

LINEA. = ''
LINEA.0 = 0
NLIN = 0

TNAME_ANT = ''
INAME_ANT = ''

DO J = 1 TO NU_FILAS
  IF LTNAME.J <> TNAME_ANT THEN DO
    CALL LINEA_TABLA
    TNAME_ANT = LTNAME.J
  END
  IF LINAME.J <> INAME_ANT THEN DO
    CALL LINEA_INDICE
    INAME_ANT = LINAME.J
  END
  CALL LINEA_COLUMNA
END
LINEA.0 = NLIN

/* Escribir fichero de salida en DD LISTCATA */
/*do i = 1 to linea.0
   say linea.i
end*/
"EXECIO * DISKW LISTCATA (STEM LINEA. FINIS)"
IF RC = 0 THEN DO
  mens = 'SCTRE001 - Error' RC 'en escritura'
  RETCOD  = RC
  RETCODE = RC
/*  SIGNAL FIN */
END
RETURN

/* ------------------------------------------------------------- */
/* Rutinas lineas de detalle                                     */
/* ------------------------------------------------------------- */
LINEA_TABLA:
  NLIN = NLIN + 1
  LINEA.NLIN = 'T' || LTNAME.J || LTCARD.J || LTPAGE.J
RETURN

LINEA_INDICE:
  NLIN = NLIN + 1
  LINEA.NLIN = 'I' || LINAME.J || LIUQRL.J || LI1KCD.J || LIFKCD.J
RETURN

LINEA_COLUMNA:
  NLIN = NLIN + 1
  LINEA.NLIN = 'C' || LCNAME.J || LCCLNO.J
RETURN


/* ------------------------------------------------------------- */
/* Conectar con el subsistema DB2                                */
/* ------------------------------------------------------------- */

CONECTAR_DB2_2:

ADDRESS TSO "SUBCOM DSNREXX"
IF RC THEN
  S_RC = RXSUBCOM('ADD','DSNREXX','DSNREXX')

/* Conexion con el subsistema DB2 */
ADDRESS DSNREXX "CONNECT" SSID
IF SQLCODE <> 0 THEN DO
  mens = 'Conectar con subsistema' SSID
  retcod = 08
  SQLERRORPOSITION = 'Conectar con subsistema' SSID
  CALL SQLERRORROUTINE
END
RETURN

/* ------------------------------------------------------------- */
/* Leer CATALOGO                                                 */
/* ------------------------------------------------------------- */

LEER_PLANTABLE:

Lqueryno. = ''
Lqblockno. = ''
Lapplname. = ''
Lprogname. = ''
Lplanno. = ''
Lmethod. = ''
Lcreator. = ''
Ltname. = ''
Ltabno. = ''
Laccesstype. = ''
Lmatchcols. = ''
Laccesscreator. = ''
Laccessname. = ''
Lindexonly. = ''
Lsortn_uniq. = ''
Lsortn_join. = ''
Lsortn_orderby. = ''
Lsortn_groupby. = ''
Lsortc_uniq. = ''
Lsortc_join. = ''
Lsortc_orderby. = ''
Lsortc_groupby. = ''
Ltslockmode. = ''
Lprefetch. = ''
Lcolumn_fn_eval. = ''
Lmixopseq. = ''
Ljoin_type. = ''
Lqblock_type. = ''
Lbind_time. = ''
Lcollid. = ''

/* Leer el catalogo de DB2 para obtener los campos del fichero
   de salida  */
SQLSTMT = ,
  "SELECT",
  "   QUERYNO, QBLOCKNO, APPLNAME, PROGNAME, PLANNO,",
  "   METHOD, CREATOR, TNAME, TABNO, ACCESSTYPE, MATCHCOLS,",
  "   ACCESSCREATOR, ACCESSNAME, INDEXONLY, SORTN_UNIQ,",
  "   SORTN_JOIN, SORTN_ORDERBY, SORTN_GROUPBY, SORTC_UNIQ,",
  "   SORTC_JOIN, SORTC_ORDERBY, SORTC_GROUPBY, TSLOCKMODE,",
  "   PREFETCH, COLUMN_FN_EVAL, MIXOPSEQ, JOIN_TYPE,",
  "   QBLOCK_TYPE, BIND_TIME, COLLID",
  "   FROM "TBCREATOR||".PLAN_TABLE",
  "   WHERE PROGNAME LIKE '"||TBNAME||"'   ",
  "  ORDER BY PROGNAME, BIND_TIME DESC,",
  "   QUERYNO ASC,QBLOCKNO ASC,PLANNO ASC,MIXOPSEQ ASC",
  "  WITH UR"
     /*
SQLSTMT = ,
  "  SELECT",
  "         LPAD(QUERYNO, 9, '0'),         ",
  "         LPAD(QBLOCKNO, 4, '0'),        ",
  "         RPAD(APPLNAME, 8),             ",
  "         RPAD(PROGNAME, 8),             ",
  "         LPAD(PLANNO, 4, '0'),          ",
  "         LPAD(METHOD, 4, '0'),          ",
  "         RPAD(CREATOR,  8),             ",
  "         RPAD(TNAME,  18),              ",
  "         LPAD(TABNO,  4, '0'),          ",
  "         RPAD(ACCESSTYPE, 2),           ",
  "         LPAD(MATCHCOLS, 4, '0'),       ",
  "         RPAD(ACCESSCREATOR, 8),        ",
  "         RPAD(ACCESSNAME, 18),          ",
  "         INDEXONLY,                     ",
  "         SORTN_UNIQ,                    ",
  "         SORTN_JOIN,                    ",
  "         SORTN_ORDERBY,                 ",
  "         SORTN_GROUPBY,                 ",
  "         SORTC_UNIQ,                    ",
  "         SORTC_JOIN,                    ",
  "         SORTC_ORDERBY,                 ",
  "         SORTC_GROUPBY,                 ",
  "         RPAD(TSLOCKMODE, 3),           ",
  "         PREFETCH,                      ",
  "         COLUMN_FN_EVAL,                ",
  "         LPAD(MIXOPSEQ, 4, '0'),        ",
  "         JOIN_TYPE,                     ",
  "         RPAD(QBLOCK_TYPE, 6),          ",
  "         CHAR(BIND_TIME),               ",
  "         RPAD(COLLID, 18)               ",
  "  FROM                                  ",
  "   "||TBCREATOR||".PLAN_TABLE           ",
  "   WHERE PROGNAME LIKE '"||TBNAME||"'   ",
  "   ORDER BY PROGNAME, BIND_TIME DESC,   ",
  "   QUERYNO ASC,QBLOCKNO ASC,PLANNO ASC,MIXOPSEQ ASC ",
  "  WITH UR                               "
      */
ADDRESS DSNREXX
"EXECSQL DECLARE  C1 CURSOR FOR S1"
IF SQLCODE <> 0 THEN DO
  mens = 'DECLARE C2 CURSOR FOR S2'
  retcod = 08
  SQLERRORPOSITION = 'DECLARE C1 CURSOR FOR S1'
  CALL SQLERRORROUTINE
END
"EXECSQL PREPARE  S1 INTO :OUTSQLDA FROM :SQLSTMT"
IF SQLCODE <> 0 THEN DO
  mens = 'PREPARE S2'
  retcod = 08
  SQLERRORPOSITION = 'PREPARE S1'
  CALL SQLERRORROUTINE
END
"EXECSQL OPEN     C1"
IF SQLCODE <> 0 THEN DO
  mens = 'OPEN C2'
  retcod = 08
  SQLERRORPOSITION = 'OPEN C1'
  CALL SQLERRORROUTINE
END

K = 0
DO UNTIL (SQLCODE <> 0)
  ADDRESS DSNREXX
  "EXECSQL FETCH  C1 USING DESCRIPTOR :OUTSQLDA"
  IF SQLCODE = 0 THEN DO
    K = K + 1
    DO I = 1 TO OUTSQLDA.SQLD  /* numero de columnas */
      SELECT
        WHEN OUTSQLDA.I.SQLNAME = 'QUERYNO'        THEN
          LQUERYNO.K        = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'QBLOCKNO'       THEN
          LQBLOCKNO.K       = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'APPLNAME'       THEN
          LAPPLNAME.K       = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'PROGNAME'       THEN
          LPROGNAME.K       = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'PLANNO'         THEN
          LPLANNO.K         = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'METHOD'         THEN
          LMETHOD.K         = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'CREATOR'        THEN
          LCREATOR.K        = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'TNAME'          THEN
          LTNAME.K          = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'TABNO'          THEN
          LTABNO.K          = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'ACCESSTYPE'     THEN
          LACCESSTYPE.K     = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'MATCHCOLS'      THEN
          LMATCHCOLS.K      = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'ACCESSCREATOR'  THEN
          LACCESSCREATOR.K  = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'ACCESSNAME'     THEN
          LACCESSNAME.K     = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'INDEXONLY'      THEN
          LINDEXONLY.K      = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'SORTN_UNIQ'     THEN
          LSORTN_UNIQ.K     = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'SORTN_JOIN'     THEN
          LSORTN_JOIN.K     = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'SORTN_ORDERBY'  THEN
          LSORTN_ORDERBY.K  = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'SORTN_GROUPBY'  THEN
          LSORTN_GROUPBY.K  = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'SORTC_UNIQ'     THEN
          LSORTC_UNIQ.K     = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'SORTC_JOIN'     THEN
          LSORTC_JOIN.K     = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'SORTC_ORDERBY'  THEN
          LSORTC_ORDERBY.K  = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'SORTC_GROUPBY'  THEN
          LSORTC_GROUPBY.K  = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'TSLOCKMODE'     THEN
          LTSLOCKMODE.K     = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'PREFETCH'       THEN
          LPREFETCH.K       = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'COLUMN_FN_EVAL' THEN
          LCOLUMN_FN_EVAL.K = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'MIXOPSEQ'       THEN
          LMIXOPSEQ.K       = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'JOIN_TYPE'      THEN
          LJOIN_TYPE.K      = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'QBLOCK_TYPE'    THEN
          LQBLOCK_TYPE.K    = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'BIND_TIME'      THEN
          LBIND_TIME.K      = OUTSQLDA.I.SQLDATA
        WHEN OUTSQLDA.I.SQLNAME = 'COLLID'         THEN
          LCOLLID.K         = OUTSQLDA.I.SQLDATA
        OTHERWISE
          NOP
      END /* SELECT */
    END I /* DO I de columnas */
  END  /* IF SQLCODE */
END /* DO UNTIL de filas */
NU_FILAS = K
ADDRESS DSNREXX
"EXECSQL COMMIT"
"DISCONNECT"
RETURN

/* ------------------------------------------------------------- */
/* Rutina ESCRIBIR_SALIDA                                        */
/* ------------------------------------------------------------- */
ESCRIBIR_SALIDA_2:

LINEA. = ''
DO J = 1 TO NU_FILAS
/*
say       " Lqueryno.j           "right(Lqueryno.j,9,0)
say       " Lqblockno.j          "right(Lqblockno.j,4,0)
say       " Lapplname.j          "left(Lapplname.j,8)
say       " Lprogname.j          "left(Lprogname.j,8)
say       " Lplanno.j            "right(Lplanno.j,4,0)
say       " Lmethod.j            "right(Lmethod.j,4,0)
say       " Lcreator.j           "left(Lcreator.j,8)
say       " Ltname.j             "left(Ltname.j,18)
say       " Ltabno.j             "right(Ltabno.j,4,0)
say       " Laccesstype.j        "left(Laccesstype.j,2)
say       " Lmatchcols.j         "right(Lmatchcols.j,4,0)
say       " Laccesscreator.j     "left(Laccesscreator.j,8)
say       " Laccessname.j        "left(Laccessname.j,18)
say       " Lindexonly.j         "Lindexonly.j
say       " Lsortn_uniq.j        "Lsortn_uniq.j
say       " Lsortn_join.j        "Lsortn_join.j
say       " Lsortn_orderby.j     "Lsortn_orderby.j
say       " Lsortn_groupby.j     "Lsortn_groupby.j
say       " Lsortc_uniq.j        "Lsortc_uniq.j
say       " Lsortc_join.j        "Lsortc_join.j
say       " Lsortc_orderby.j     "Lsortc_orderby.j
say       " Lsortc_groupby.j     "Lsortc_groupby.j
say       " Ltslockmode.j        "Ltslockmode.j
say       " Lprefetch.j          "Lprefetch.j
say       " Lcolumn_fn_eval.j    "Lcolumn_fn_eval.j
say       " Lmixopseq.j          "Lmixopseq.j
say       " Ljoin_type.j         "Ljoin_type.j
say       " Lqblock_type.j       "Lqblock_type.j
say       " Lbind_time.j         "Lbind_time.j
say       " Lcollid.j            "Lcollid.j */
  LINEA.J = right(Lqueryno.j,9,0)      ||,
            right(Lqblockno.j,4,0)     ||,
            left(Lapplname.j,8)        ||,
            left(Lprogname.j,8)        ||,
            right(Lplanno.j,4,0)       ||,
            right(Lmethod.j,4,0)       ||,
            left(Lcreator.j,8)         ||,
            left(Ltname.j,31,0)        ||,
            right(Ltabno.j,4,0)        ||,
            left(Laccesstype.j,2)      ||,
            right(Lmatchcols.j,4,0)    ||,
            left(Laccesscreator.j,8)   ||,
            left(Laccessname.j,31,0)   ||,
            Lindexonly.j               ||,
            Lsortn_uniq.j              ||,
            Lsortn_join.j              ||,
            Lsortn_orderby.j           ||,
            Lsortn_groupby.j           ||,
            Lsortc_uniq.j              ||,
            Lsortc_join.j              ||,
            Lsortc_orderby.j           ||,
            Lsortc_groupby.j           ||,
            left(Ltsockmode.j,3)       ||,
            Lprefetch.j                ||,
            Lcolumn_fn_eval.j          ||,
            right(Lmixopseq.j,4,0)     ||,
            Ljoin_type.j               ||,
            left(Lqblock_type.j,6)     ||,
            Lbind_time.j               ||,
            left(Lcollid.j,31,0)
END
LINEA.0 = J

/* Escribir fichero de salida en DD LISTCATA */
/*do i = 1 to linea.0
   say linea.i
end*/
"EXECIO * DISKW LISTPLAN (STEM LINEA. FINIS)"
IF RC = 0 THEN DO
  mens = 'SCTRE001 - Error' RC 'en escritura'
  RETCOD  = RC
  RETCODE = RC
/*SIGNAL FIN*/
END
RETURN

/* ------------------------------------------------------------- */
/* Rutina SQLERRORROUTINE para formatear el error                */
/* ------------------------------------------------------------- */
SQLERRORROUTINE:
  SAY 'POSITION   = ' SQLERRORPOSITION
  SAY 'SQLCODE    = ' SQLCODE
  SAY 'SQLSTATE   = ' SQLSTATE
  SAY 'SQLERRP    = ' SQLERRP
  SAY 'TOKENS     = ' TRANSLATE(SQLERRMC,',','FF'X)
  SAY 'SQLERRD.1  = ' SQLERRD.1
  SAY 'SQLERRD.2  = ' SQLERRD.2
  SAY 'SQLERRD.3  = ' SQLERRD.3
  SAY 'SQLERRD.4  = ' SQLERRD.4
  SAY 'SQLERRD.5  = ' SQLERRD.5
  SAY 'SQLERRD.6  = ' SQLERRD.6
  SAY 'SQLWARN.0  = ' SQLWARN.0
  SAY 'SQLWARN.1  = ' SQLWARN.1
  SAY 'SQLWARN.2  = ' SQLWARN.2
  SAY 'SQLWARN.3  = ' SQLWARN.3
  SAY 'SQLWARN.4  = ' SQLWARN.4
  SAY 'SQLWARN.5  = ' SQLWARN.5
  SAY 'SQLWARN.6  = ' SQLWARN.6
  SAY 'SQLWARN.7  = ' SQLWARN.7
  SAY 'SQLWARN.8  = ' SQLWARN.8
  SAY 'SQLWARN.9  = ' SQLWARN.9
  SAY 'SQLWARN.10 = ' SQLWARN.10

  ADDRESS DSNREXX 'EXECSQL ROLLBACK'

  IF SQLCODE <> 0 THEN
    DO
      retcod = 08
      mens = 'ROLLBACK SQLCODE : ' SQLCODE
      SAY 'ROLLBACK SQLCODE : ' SQLCODE
    END

RETCODE=1
SIGNAL FIN
RETURN

/* END  */
/*--------------------------------------------------------------------*/
log:
   x=LL00QA('LL06QA',date(),time(),userid(),retcod,mens,parametros)
return
/*--------------------------------------------------------------------*/
