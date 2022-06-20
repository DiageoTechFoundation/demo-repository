*----------------------------------------------------------------------*
* Program   : ZMMFGI033                                                *
* Title     : BOM SAP TO 3PM                                           *
* Type      : IDOC Outbound Interface                                  *
* Created by: Erliza Banez (BANEZERL)                                  *
* Created on: March 13, 2006                                           *
* Change/Transport Number: DA2K920589                                  *
* Report Description     : The RICEF 33 Bill of Material (BOM)         *
* interface will cover the requirement of sending the Diageo Bill of   *
* Material information to the subcontractor. The subcontractor is then *
* required to automatically or manually upload the information received*
* into its system.                                                     *
*----------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Date   Modified by   Username     Reference number  Change Request # *
* ====   ===========   ===========  ================  ================ *
* 13/03/2006 Erliza    BANEZERL                       DA2K920589       *
*            Banez                                                     *
* 26/07/2006 SANCHEMA               dEFECT 3143
*----------------------------------------------------------------------*
* 03/10/2007 SANCHEMA  R11 . Replace plant LG1 by LW1 and LA by LPA
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Description: Initial Creation                                        *
*----------------------------------------------------------------------*

REPORT  zmmfgi033 NO STANDARD PAGE HEADING
                  LINE-SIZE 142
                  LINE-COUNT 65(2).



*----------------------------------------------------------------------*
*        TABLE DECLARATION                                             *
*----------------------------------------------------------------------*
TABLES: t415s, t001w, cdhdr, mast, stko, stzu, stas, stpo, mara.

*----------------------------------------------------------------------*
*         CONSTANTS DECLARATION                                        *
*----------------------------------------------------------------------*
*--BEGIN OF MODIFICATION: PRILANO 07/26/2006 DA2K926190
*--Replace constants with text-elements.
CONSTANTS: c_e(1) VALUE 'E',
           c_x(1) VALUE 'X',
           c_0(1) VALUE '0',
           c_1(1) VALUE '1',
           c_i(1) VALUE 'I',
           c_eq(2) VALUE 'EQ',
           c_l(1) VALUE 'L',
           c_s(1) VALUE 'S'.



CONSTANTS:
          c_53(2)     VALUE '53',
          c_03(2)     VALUE '03',
          c_30(2)     VALUE '30',
          c_to(4)     VALUE 'TO  ',
          c_pagen(4)  VALUE 'Page',
          c_plant(5)  VALUE 'Plant',
          c_date(5)      TYPE c VALUE 'Date:',       "date
          c_time(5)      TYPE c VALUE 'Time:',       "time
          c_page(5)      TYPE c VALUE 'Page:',       "page
          c_bom(3)  VALUE 'BOM',
          c_logxt(4)  VALUE 'LOGS',
          c_not(3)    VALUE 'NOT',
          c_poitem(4) VALUE 'Item',
          c_mf(2)     VALUE 'MF',
          c_ls(2)     VALUE 'LS'.

*{ begin 03/10/2007 sanchema  r11
  constants: c_3p(2) value '3P',
             c_werks(8) value 'WERKS_NA',
             C_meins(8) value 'MEINS_NA'.
*} end 03/10/2007 sanchema  r11

*----------------------------------------------------------------------*
*       TYPES OR TYPE-POOLS					                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF t_bom,
        stlnr LIKE mast-stlnr,
        stlal LIKE mast-stlal,
        stldt LIKE stzu-stldt,
        tcode like sy-tcode,
       END OF t_bom.

TYPES: BEGIN OF t_stzu,
         stlnr LIKE stzu-stlnr,
         stldt LIKE stzu-stldt,
       END OF t_stzu.

TYPES: BEGIN OF t_mast,
         matnr LIKE mast-matnr,
         werks LIKE mast-werks,
         stlan LIKE mast-stlan,
         stlnr LIKE mast-stlnr,
         stlal LIKE mast-stlal,
       END OF t_mast.

TYPES: BEGIN OF t_mara,
         matnr LIKE mara-matnr,
         mtart LIKE mara-mtart,
       END OF t_mara.

TYPES: BEGIN OF t_stko,
         stlnr LIKE stko-stlnr,
         stlal LIKE stko-stlal,
         stktx LIKE stko-stktx,
         stlst LIKE stko-stlst,
         bmeng LIKE stko-bmeng,
         datuv LIKE stko-datuv,
         loekz LIKE stko-loekz,
         bmein LIKE stko-bmein,
       END OF t_stko.

TYPES: BEGIN OF t_stas,
         stlnr LIKE stas-stlnr,
         stlal LIKE stas-stlal,
         stlkn LIKE stas-stlkn,
         stvkn LIKE stas-stvkn,
       END OF t_stas.

TYPES: BEGIN OF t_stpo,
         stlnr LIKE stpo-stlnr,
         stlkn LIKE stpo-stlkn,
         stvkn LIKE stpo-stvkn,
         idnrk LIKE stpo-idnrk,
         datuv LIKE stpo-datuv,
         lkenz LIKE stpo-lkenz,
         meins LIKE stpo-meins,
         menge LIKE stpo-menge,
         ausch LIKE stpo-ausch,
         itsob LIKE stpo-itsob,
         nfgrp LIKE stpo-nfgrp,
         nfeag LIKE stpo-nfeag,
       END OF t_stpo.

TYPES: BEGIN OF t_output,
         stlnr LIKE stzu-stlnr,
         stldt LIKE stzu-stldt,
         matnr LIKE mast-matnr,
         werks LIKE mast-werks,
         stlal LIKE mast-stlal,
         stktx LIKE stko-stktx,
         stlst LIKE stko-stlst,
         bmeng LIKE stko-bmeng,
         datuv1 LIKE stko-datuv,
         loekz LIKE stko-loekz,
         bmein LIKE stko-bmein,
         stlkn LIKE stas-stlkn,
         stvkn LIKE stas-stvkn,
         itsob LIKE stpo-itsob,
         datuv2 LIKE stpo-datuv,
         lkenz LIKE stpo-lkenz,
         idnrk LIKE stpo-idnrk,
         meins LIKE stpo-meins,
         menge LIKE stpo-menge,
         ausch LIKE stpo-ausch,
         nfgrp LIKE stpo-nfgrp,
         nfeag LIKE stpo-nfeag,
       END OF t_output.

TYPES: BEGIN OF t_status,
        stlnr LIKE stzu-stlnr,
        status LIKE edidc-status,
        docnum LIKE edidc-docnum,
        txt LIKE teds2-descrp,
       END OF t_status.

TYPES: BEGIN OF t_statxt,
        value LIKE teds1-status,
        descrp LIKE teds2-descrp,
        direct LIKE teds1-direct,
        layer LIKE teds1-layer,
        lay_descrp LIKE dd07v-ddtext,
       END OF t_statxt.

TYPES: BEGIN OF t_idoc_data.
        INCLUDE STRUCTURE edidd.
TYPES: END OF t_idoc_data.

TYPES: BEGIN OF t_edidc.
        INCLUDE STRUCTURE edidc.
TYPES: END OF t_edidc.

TYPES: BEGIN OF t_werks,
        werks LIKE t001w-werks,                  "Plant
      END OF t_werks.

TYPES: BEGIN OF t_stlst,
        stlst LIKE t415s-stlst,                  "BOM
      END OF t_stlst.

TYPES: BEGIN OF t_objectid,                      "OBJECT VALUES
        objectid LIKE cdhdr-objectid,
        tcode like cdhdr-tcode,
      END OF t_objectid.

*----------------------------------------------------------------------*
*        DATA/VARIABLE DECLARATION                                     *
*----------------------------------------------------------------------*
*{ begin 03/10/2007 sanchema  r11
* structure with plant and unit of measure for interface 3PM
 DATA: begin of i_werks_aux occurs 0,
         werks like mseg-werks,
         werks_new like mseg-werks,
       end of i_werks_aux.

 DATA: begin of i_meins occurs 0,
         meins like mseg-meins,
         meins_new like mseg-meins,
       end of i_meins.
*{ END 03/10/2007 sanchema  r11
DATA: x_werks TYPE t_werks,
      i_werks TYPE STANDARD TABLE OF t_werks.

DATA: x_stlst TYPE t_stlst,
      i_stlst TYPE STANDARD TABLE OF t_stlst.

DATA: x_objectid TYPE t_objectid,
      i_objectid TYPE STANDARD TABLE OF t_objectid.

DATA: x_bom TYPE t_bom,
      i_bom TYPE STANDARD TABLE OF t_bom.

DATA: x_stzu TYPE t_stzu,
      i_stzu TYPE STANDARD TABLE OF t_stzu.

DATA: x_mast TYPE t_mast,
      i_mast TYPE STANDARD TABLE OF t_mast.

DATA: x_mara TYPE t_mara,
      i_mara TYPE STANDARD TABLE OF t_mara.

DATA: x_stko TYPE t_stko,
      i_stko TYPE STANDARD TABLE OF t_stko.

DATA: x_stas TYPE t_stas,
      i_stas TYPE STANDARD TABLE OF t_stas.

DATA: x_stpo TYPE t_stpo,
      i_stpo TYPE STANDARD TABLE OF t_stpo.

DATA: x_output TYPE t_output,
      x_output2 TYPE t_output,
      i_output TYPE STANDARD TABLE OF t_output.

DATA: x_status TYPE t_status,
      i_status TYPE STANDARD TABLE OF t_status.

DATA: i_statxt TYPE STANDARD TABLE OF t_statxt,
      x_statxt  TYPE t_statxt.

DATA: x_status2 TYPE t_status,
      i_status2 TYPE STANDARD TABLE OF t_status.

DATA: i_idoc_data TYPE STANDARD TABLE OF t_idoc_data,
      x_idoc_data TYPE t_idoc_data.

DATA: i_edidc TYPE STANDARD TABLE OF t_edidc ,
      x_edidc  TYPE t_edidc.

DATA: x_control_rec TYPE t_edidc.

DATA: x_e1stzum TYPE e1stzum,
      x_e1mastm TYPE e1mastm,
      x_e1stkom TYPE e1stkom,
      x_e1stasm TYPE e1stasm,
      x_e1stpom TYPE e1stpom.



DATA: v_tline TYPE i,
      v_lines TYPE i,
      v_txt LIKE teds2-descrp,
      v_count_item TYPE i,
      v_temp(100) TYPE c.


RANGES: r_tcode FOR cdhdr-tcode.

*=========================== SELECTION SCREEN ==========================
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME.

*----------------------------------------------------------------------*
*        SELECT-OPTIONS                                                *
*----------------------------------------------------------------------*
*  You can use this section to declare selection tables and create     *
*  corresponding input fields on the associated selection screen.      *
*----------------------------------------------------------------------*
SELECT-OPTIONS: s_werks FOR t001w-werks OBLIGATORY,
                s_matnr for mara-matnr,   "material number
                s_mtart for mara-mtart,  "material number
                s_date FOR sy-datum obligatory,
                s_stlst FOR t415s-stlst.

SELECTION-SCREEN END OF BLOCK blk1.

*----------------------------------------------------------------------*
*        INITIALIZATION                                                *
*----------------------------------------------------------------------*
INITIALIZATION.

  CLEAR: v_temp,
         v_lines,
         v_count_item.
  PERFORM f_get_tcode USING: text-t23,
                           text-t22.


  REFRESH: i_werks, i_stzu, i_stpo, i_stlst,
           i_stko, i_status2, i_status, i_stas,
           i_output, i_objectid, i_mast, i_mara.

*----------------------------------------------------------------------*
*        AT SELECTION-SCREEN                                           *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

* Check the Plants are SAP valid plants (T001W table)
  SELECT werks INTO TABLE i_werks
  FROM t001w
  WHERE werks IN s_werks.
  IF sy-subrc <> 0.
    MESSAGE text-002 TYPE c_e. "No PLANT exist.
  ENDIF.

* Check the BOM Status are valid (T415S Table)
  SELECT stlst INTO TABLE i_stlst
  FROM t415s
  WHERE stlst IN s_stlst.
  IF sy-subrc <> 0.
    MESSAGE text-003 TYPE c_e. "No BOM Status exist.
  ENDIF.



*----------------------------------------------------------------------*
*       TOP-OF-PAGE                                                    *
*----------------------------------------------------------------------*
TOP-OF-PAGE.
  ULINE.
  WRITE:  5 sy-repid, 130 sy-uname.
  WRITE:/5 sy-datum, 20 sy-uzeit, 132 sy-pagno, 130 c_pagen.
  ULINE.

*----------------------------------------------------------------------*
*       START OF SELECTION                                             *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*{ begin 03/10/2007 sanchema  r11
* retrieve data from t9con
  perform f_get_values_changes.
*} end 03/10/2007 sanchema  r11

* check if the date field has values when RB_BILL2 is chosen
  PERFORM f_get_data.
*--create idoc.
  if not i_mast[] is initial.

    PERFORM f_create_idoc.
    PERFORM f_show_log.
  else.
    MESSAGE text-017 TYPE 'I'.
    leave list-processing.

  endif.

END-OF-SELECTION.

*----------------------------------------------------------------------*
*        SUBROUTINES                                                   *
*----------------------------------------------------------------------*


*&--------------------------------------------------------------------*
*&      Form  f_get_tcode
*&--------------------------------------------------------------------*
* this will get the transaction code names that will be used
*---------------------------------------------------------------------*

FORM f_get_tcode USING p_tcode.

  r_tcode-sign = c_i.
  r_tcode-option = c_eq.
  r_tcode-low = p_tcode.
  APPEND r_tcode.

ENDFORM.                    "F_GET_TCODE


*&--------------------------------------------------------------------*
*&      Form  f_get_data
*&--------------------------------------------------------------------*
* this will data from series of tables that will be passed
*---------------------------------------------------------------------*
FORM f_get_data.

* The first step is to select the BOM that have been created or modified
* since the last execution of the program.

  REFRESH: i_objectid.
  SELECT objectid
         tcode
    INTO TABLE i_objectid
  FROM cdhdr
  WHERE objectclas = text-t01
    AND tcode IN r_tcode
    AND udate IN s_date.
  IF sy-subrc = 0.
  ENDIF.

* No data for this selection
  DESCRIBE TABLE i_objectid LINES v_lines.



* Get the BOM code and BOM Alternative code from OBJECTID
  REFRESH i_bom.
  CLEAR: x_bom,
         x_objectid.
  LOOP AT i_objectid INTO x_objectid.
    x_bom-stlnr = x_objectid-objectid+4(8).
    x_bom-stlal = x_objectid-objectid+22(2).
    x_bom-tcode = x_objectid-tcode.
    APPEND x_bom TO i_bom.
    CLEAR: x_bom,
           x_objectid.
  ENDLOOP.

  SORT I_BOM BY stlnr stlal.
  DELETE ADJACENT DUPLICATES FROM I_BOM.

** Get BOM date modified
  REFRESH: i_stzu.
  SELECT stlnr stldt INTO TABLE i_stzu
      FROM stzu
      FOR ALL ENTRIES IN i_bom
      WHERE stlnr = i_bom-stlnr.

      CLEAR: x_bom,
             x_stzu.
      LOOP AT i_bom INTO x_bom.
*        get corresponding dates of BOM
         CLEAR: x_stzu.
         READ TABLE i_stzu INTO x_stzu WITH KEY stlnr = x_bom-stlnr.
         IF sy-subrc = 0.
            MOVE x_stzu-stldt TO x_bom-stldt.
         MODIFY i_bom FROM x_bom TRANSPORTING stldt.
         endif.
*
     ENDLOOP.

* Select BOM Items, in table MAST. the BOM that correspond to the
* correct Plant and are relevant for Production
  IF NOT i_bom[] IS INITIAL.

    REFRESH: i_mast.
    SELECT matnr werks stlan stlnr stlal
    INTO TABLE i_mast
    FROM mast
    FOR ALL ENTRIES IN i_bom
    WHERE stlnr = i_bom-stlnr
      AND stlal = i_bom-stlal
      AND werks IN s_werks
      AND matnr in s_matnr
      AND stlan = c_1.

    IF sy-subrc = 0.
    ENDIF.

  ENDIF.
*--BEGIN OF MODIFICATION: PRILANO 07/26/2006 DA2K926190
*--Replace constants with text-elements.

  if i_mast[] is initial.
*    MESSAGE text-017 TYPE 'I'.
    MESSAGE text-017 TYPE c_i.
    leave list-processing.
  else.
** Check material type
    if not s_mtart[] is initial.
      SELECT matnr mtart
        INTO TABLE i_mara
        FROM mara
        FOR ALL ENTRIES IN i_mast
        WHERE matnr = i_mast-matnr and
              mtart in s_mtart.

      loop at i_mast into x_mast.
        read table i_mara into x_mara with key matnr = x_mast-matnr.
        if sy-subrc <> 0.
            delete i_mast where matnr = x_mast-matnr.
        endif.
      endloop.
    endif.
  endif.



* Select BOMs with the correct status
  IF NOT i_mast[] IS INITIAL.

    REFRESH: i_stko.
    SELECT stlnr stlal stktx stlst bmeng datuv loekz bmein
    INTO TABLE i_stko
    FROM stko
    FOR ALL ENTRIES IN i_mast
    WHERE stlnr = i_mast-stlnr
      AND stlal = i_mast-stlal
      AND stlst IN s_stlst.

    IF sy-subrc = 0.
    ENDIF.

  ENDIF.


* determine the Items associated to the BOM that we want to send
  IF NOT i_mast[] IS INITIAL.

    REFRESH: i_stas.
    SELECT stlnr stlal stlkn stvkn
    INTO TABLE i_stas
    FROM stas
    FOR ALL ENTRIES IN i_mast
    WHERE stlnr = i_mast-stlnr
      AND stlal = i_mast-stlal.

    IF sy-subrc = 0.
    ENDIF.

  ENDIF.


* determine which of the BOM Items should be sent
  IF NOT i_stas[] IS INITIAL.

    REFRESH: i_stpo.
    SELECT stlnr stlkn stvkn idnrk datuv lkenz meins
           menge ausch itsob nfgrp nfeag
    INTO TABLE i_stpo
    FROM stpo
    FOR ALL ENTRIES IN i_stas
    WHERE stlnr = i_stas-stlnr
      AND stlkn = i_stas-stlkn
      AND postp = c_l.
    IF sy-subrc = 0.
    ENDIF.

  ENDIF.


ENDFORM.                    "f_get_data


*&--------------------------------------------------------------------*
*&      Form  f_create_idoc
*&--------------------------------------------------------------------*
* this will transmit the data to its recipient, thus creating IDOC
*---------------------------------------------------------------------*
FORM f_create_idoc.
**********************************************************************
* Create a data segment for each line
**********************************************************************

  REFRESH: i_statxt,
           i_status.
  CALL FUNCTION 'IDOC_STATUS_VALUES_READ'
    TABLES
      values               = i_statxt
* EXCEPTIONS
*   INTERNAL_ERROR       = 1
*   OTHERS               = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  SORT i_MAST BY stlnr.

  IF sy-subrc EQ 0.
  ENDIF.

  CLEAR: x_output,
         x_output2.

  CLEAR v_tline.

  LOOP AT i_MAST INTO x_mast.

    clear x_output.
    MOVE-corresponding x_mast TO x_output.

    AT new stlnr.

**********************************************************************
* Save the message type and the basic IDoc type in the control segment
**********************************************************************
      CLEAR: x_control_rec.

* retrieve partner form T9CON usin plant
      SELECT SINGLE result
          FROM t9con
          INTO  x_control_rec-rcvprn
          WHERE key1 = c_mf AND
                key2 = c_bom AND
                key3 = x_output-werks.

        IF sy-subrc EQ 0.
        ENDIF.

      x_control_rec-mandt  = sy-mandt.
      x_control_rec-direct = 1.
      x_control_rec-idoctp = text-t03.
      x_control_rec-mestyp = text-t02.
      x_control_rec-rcvprt = c_ls.

      CLEAR: x_idoc_data,
             x_e1stzum.
* Create IDoc data segments -E1STZUM
     read table i_bom into x_bom with key stlnr = x_output-stlnr.

      MOVE: text-t16       TO x_idoc_data-segnam.
     CASE x_bom-tcode.
     WHEN text-t22.
         x_e1stzum-msgfn = '009'.
     WHEN text-t23.
         x_e1stzum-msgfn = '004'.
     ENDCASE.

      MOVE: x_output-stlnr  TO x_e1stzum-stlnr,
            x_bom-stldt  TO x_e1stzum-stldt.
      MOVE: x_e1stzum       TO x_idoc_data-sdata.
      APPEND x_idoc_data TO i_idoc_data.

      CLEAR: x_idoc_data,
             x_e1mastm.
* Create IDoc data segments -E1MASTM
      MOVE: text-t17       TO x_idoc_data-segnam.
      MOVE: x_output-matnr  TO x_e1mastm-matnr .
*{begin 03/10/2008 sanchema
* read table and replace value of plant and unit of measure
      read table i_werks_aux with key werks_new = x_e1mastm-werks.
      if sy-subrc = 0.
        move: i_werks_aux-werks  TO x_e1mastm-werks.

      else.
*}end 03/10/2008 sanchema
        move: x_output-werks  TO x_e1mastm-werks.
      endif.
      move: x_output-stlal  TO x_e1mastm-stlal.
      MOVE: x_e1mastm       TO x_idoc_data-sdata.
      APPEND x_idoc_data TO i_idoc_data.

      CLEAR: x_idoc_data,
             x_e1stkom.

* Create IDoc data segments -E1STKON
    READ TABLE i_stko INTO x_stko WITH KEY stlnr = x_output-stlnr
                                           stlal = x_output-stlal.
      MOVE: text-t18       TO x_idoc_data-segnam.
      MOVE: x_stko-stktx  TO x_e1stkom-stktx,
            x_stko-stlst  TO x_e1stkom-stlst,
            x_stko-bmeng  TO x_e1stkom-bmeng,
            x_stko-datuv TO x_e1stkom-datuv,
            x_stko-loekz  TO x_e1stkom-loekz.
*{begin 03/10/2008 sanchema
* read table and replace value of plant and unit of measure
      read table i_meins with key meins_new = x_stko-bmein.
      if sy-subrc = 0.
        move: i_meins-meins  TO x_e1stkom-bmein.
      else.
        move: x_stko-bmein  TO x_e1stkom-bmein.
      endif.

*}end 03/10/2008 sanchema

      MOVE: x_e1stkom       TO x_idoc_data-sdata.
      APPEND x_idoc_data TO i_idoc_data.

    endat.


    AT END OF stlnr.
* Create IDoc data segments -E1STPOM

      loop at i_stpo into x_stpo where stlnr = x_output-stlnr.
        CLEAR: x_idoc_data,
               x_e1stpoM.
        MOVE: text-t20       TO x_idoc_data-segnam.
        MOVE: x_stpo-STLKN  TO x_e1stpom-STLKN,
              x_stpo-itsob  TO x_e1stpom-itsob,
              x_stpo-datuv TO x_e1stpom-datuv,
              x_stpo-lkenz  TO x_e1stpom-lkenz,
              x_stpo-idnrk  TO x_e1stpom-idnrk,
              x_stpo-meins  TO x_e1stpom-meins,
              x_stpo-menge  TO x_e1stpom-menge,
              x_stpo-ausch  TO x_e1stpom-ausch,
              x_stpo-nfgrp  TO x_e1stpom-nfgrp,
              x_stpo-nfeag  TO x_e1stpom-nfeag.
        MOVE: x_e1stpom       TO x_idoc_data-sdata.
        APPEND x_idoc_data TO i_idoc_data.
        clear x_stpo.
      endloop.

**********************************************************************
* Call the IDoc creation function
**********************************************************************
      REFRESH: i_edidc.
      CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
        EXPORTING
          master_idoc_control            = x_control_rec
        TABLES
          communication_idoc_control     = i_edidc
          master_idoc_data               = i_idoc_data
*      EXCEPTIONS
*        error_in_idoc_control          = 1
*        error_writing_idoc_status      = 2
*        error_in_idoc_data             = 3
*        sending_logical_system_unknown = 4
*        OTHERS                         = 5.
                .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
** start 27-07-2006 sanchema  defect 3143
      commit work.
** end 27-07-2006 sanchema  defect 3143

      CLEAR: x_edidc,
             x_status.
      READ TABLE i_edidc INTO x_edidc INDEX 1.

      IF sy-subrc EQ 0.

        CLEAR: x_statxt.
        READ TABLE i_statxt INTO x_statxt
        WITH KEY value = x_edidc-status.

        IF sy-subrc EQ 0.

          MOVE: x_statxt-descrp TO x_status-txt.

        ENDIF.

        MOVE: x_output-stlnr TO x_status-stlnr,
              x_edidc-status TO x_status-status,
              x_edidc-docnum TO x_status-docnum.

      ENDIF.

      REFRESH: i_idoc_data.
      APPEND x_status TO i_status.

      CLEAR: x_output,
             x_output2,
             x_status.
      v_tline = v_tline + 1.

    ENDAT.

  ENDLOOP.

ENDFORM.                    "f_create_idoc


*&--------------------------------------------------------------------*
*&      Form  f_show_log
*&--------------------------------------------------------------------*
* this will show log on IDOC transfers
*---------------------------------------------------------------------*
FORM f_show_log.
  data: l_error.
  WRITE:/.
  WRITE:/5  text-t05.
  WRITE:/10(60) sy-uline.

  WRITE:/10  c_plant.
  WRITE: 55  c_to.
  FORMAT COLOR 3 ON.
  WRITE: 40 s_werks-low.
  WRITE: 59 s_werks-high.
  FORMAT COLOR OFF.

  WRITE:/10  text-000.
  WRITE: 55  c_to.
  FORMAT COLOR 3 ON.
  WRITE: 40 s_matnr-low.
  WRITE: 59 s_matnr-high.
  FORMAT COLOR OFF.

  WRITE:/10  text-019.
  WRITE: 55  c_to.
  FORMAT COLOR 3 ON.
  WRITE: 40 s_mtart-low.
  WRITE: 59 s_mtart-high.
  FORMAT COLOR OFF.

  WRITE:/10  text-t06.
  WRITE: 55  c_to.
  FORMAT COLOR 3 ON.
  WRITE: 40 s_date-low.
  WRITE: 59 s_date-high.
  FORMAT COLOR OFF.

  WRITE:/10  text-t07.
  WRITE: 55  c_to.
  FORMAT COLOR 3 ON.
  WRITE: 40 s_stlst-low.
  WRITE: 59 s_stlst-high.
  FORMAT COLOR OFF.

  WRITE:/10(60) sy-uline.
  WRITE:/.

* total number of lines/records for internal table I_PO
*  CLEAR v_tline.
*  DESCRIBE TABLE i_output LINES v_tline.

*  WRITE:/40 v_tline COLOR 4,5  c_bom_tot.
  WRITE:/40 v_tline COLOR 4,5  text-t08.

  WRITE:/.
  ULINE.
  FORMAT COLOR 1 ON.
  WRITE:/60 c_logxt,
         142 space.
  FORMAT COLOR OFF.
  ULINE.
  WRITE:/.

  FORMAT COLOR 1 ON.
  WRITE:/10 c_bom, 25 text-t14, 50 text-t15,

         142 space.
  FORMAT COLOR OFF.
  SKIP.
  SKIP.

  FORMAT COLOR 5 ON.
  WRITE:/10  text-t09,
         142 space.
  FORMAT COLOR OFF.
  WRITE:/.

* Looping through I_status to output the records with Success status

  DELETE i_status WHERE stlnr IS INITIAL.
  IF sy-subrc EQ 0.
  ENDIF.

  CLEAR x_status.
  LOOP AT i_status INTO x_status.
    v_txt = x_status-txt.
    TRANSLATE v_txt TO UPPER CASE.                       "#EC TRANSLANG

    IF x_status-status = c_03.

      WRITE:/10 x_status-stlnr, 25 x_status-docnum , 50 x_status-txt.

    ENDIF.
    IF x_status-status = c_30.

      WRITE:/10 x_status-stlnr, 25 x_status-docnum , 50 x_status-txt.

    ENDIF.

    CLEAR x_status.
    CLEAR v_txt.
  ENDLOOP.
  .

* Looping through I_status to output the records with ERROR status


  CLEAR: x_status, l_error.
  LOOP AT i_status INTO x_status.
    v_txt = x_status-txt.
    TRANSLATE v_txt TO UPPER CASE.                       "#EC TRANSLANG

*   stat other than 03 for outbound is Error
    IF x_status-status <> c_03 AND
       x_status-status <> c_30.

      if l_error is initial.
        WRITE:/.
        FORMAT COLOR 6 ON.
        WRITE:/10  text-t10,
             142 space.
        FORMAT COLOR OFF.
        WRITE:/.
        l_error = c_x.
      endif.
      WRITE:/10 x_status-stlnr, 25 x_status-docnum , 50 x_status-txt.

    ENDIF.
    CLEAR x_status.
    CLEAR v_txt.
  ENDLOOP.




ENDFORM.                    "f_show_log
*{ begin 03/10/2007 sanchema  r11
*&---------------------------------------------------------------------*
*&      Form  f_get_values_changes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_get_values_changes .
  refresh: I_werks_aux, i_meins.

  select key3 result
    into table i_werks_aux
    from t9con
    where key1 = c_3p and
          key2 = c_werks.

  select key3 result
    into table i_meins
    from t9con
    where key1 = c_3p and
          key2 = c_meins.

endform.                    " f_get_values_changes
*} end 03/10/2007 sanchema  r11
