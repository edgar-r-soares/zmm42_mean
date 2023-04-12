*----------------------------------------------------------------------*
***INCLUDE ZMM_MM42_LOGISTIC_DATA_C01 .
*----------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS: on_double_click FOR EVENT double_click OF cl_gui_alv_grid
                             IMPORTING e_row e_column es_row_no,
             on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
                             IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm sender,
             on_data_changed_end FOR EVENT data_changed_finished OF cl_gui_alv_grid
                             IMPORTING e_modified et_good_cells,
             handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive,
             handle_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD handle_command.
    DATA: lt_rows TYPE lvc_t_row,
          lt_cols TYPE lvc_t_col.
    FIELD-SYMBOLS: <ls_row> TYPE LINE OF lvc_t_row,
                   <ls_col> TYPE LINE OF lvc_t_col.
    DATA: l_lines TYPE i.
    FIELD-SYMBOLS: <ls_data> TYPE t_data.
    DATA: l_stable TYPE lvc_s_stbl.

    CASE e_ucomm.
      WHEN 'SAVE_DEL'.
        PERFORM f_save USING 'D'.
      WHEN 'MASS'.
*        clear: gs_2010.
*        gr_alv->get_selected_columns( importing et_index_columns = lt_cols ).
*        read table lt_cols with key fieldname = 'DISMM' transporting no fields.
*        if sy-subrc eq 0.
*          gs_2010-columns-dismm = 'X'.
*        endif.
*        read table lt_cols with key fieldname = 'DISPO' transporting no fields.
*        if sy-subrc eq 0.
*          gs_2010-columns-dispo = 'X'.
*        endif.
*        read table lt_cols with key fieldname = 'EISBE' transporting no fields.
*        if sy-subrc eq 0.
*          gs_2010-columns-eisbe = 'X'.
*        endif.
*        read table lt_cols with key fieldname = 'MINBE' transporting no fields.
*        if sy-subrc eq 0.
*          gs_2010-columns-minbe = 'X'.
*        endif.
*        read table lt_cols with key fieldname = 'SOBST' transporting no fields.
*        if sy-subrc eq 0.
*          gs_2010-columns-sobst = 'X'.
*        endif.
*        if gs_2010-columns is initial.
*          gs_2010-columns = 'XXXXX'.
*        endif.
*        gr_alv->get_selected_rows( importing et_index_rows = lt_rows ).
*        if lt_rows is initial.
*          l_lines = lines( gt_data ).
*          do l_lines times.
*            append initial line to lt_rows assigning <ls_row>.
*            <ls_row>-index = sy-index.
*          enddo.
*          gr_alv->set_selected_rows( exporting it_index_rows = lt_rows is_keep_other_selections = 'X' ).
*        endif.
*
*        call screen 2010 starting at 10 2 ending at 60 10.
*        if ok_code eq 'ADD'.
*          if ok_code eq 'FIX'.
*          clear ok_code.
*          loop at lt_rows assigning <ls_row>.
*            read table gt_data assigning <ls_data> index <ls_row>-index.
*            if gs_2010-dismm is not initial.
*              <ls_data>-dismm = gs_2010-dismm.
*            endif.
*            if gs_2010-dispo is not initial.
*              <ls_data>-dispo = gs_2010-dispo.
*            endif.
*            if gs_2010-eisbe is not initial.
*              <ls_data>-eisbe = gs_2010-eisbe.
*            endif.
*            if gs_2010-minbe is not initial.
*              <ls_data>-minbe = gs_2010-minbe.
*            endif.
*            if gs_2010-sobst is not initial.
*              <ls_data>-sobst = gs_2010-sobst.
*            endif.
*          endloop.
*          l_stable-row = 'X'.
*          l_stable-col = 'X'.
*          if gr_alv is not initial.
*            gr_alv->refresh_table_display( is_stable = l_stable )." i_soft_refresh = 'X' ).
*          endif.
*        else.
*          clear: gs_2010.
*          clear ok_code.
*        endif.
      WHEN 'DOWNLOAD'.
        PERFORM f_download.
      WHEN 'UPLOAD'.
        PERFORM f_upload.
    ENDCASE.
  ENDMETHOD.                    "handle_command
  METHOD handle_toolbar.
    FIELD-SYMBOLS <ls_toolbar> TYPE stb_button.
    DATA ls_toolbar TYPE stb_button.

    DELETE e_object->mt_toolbar WHERE function CP '&LOCAL&*'.

    INSERT INITIAL LINE INTO e_object->mt_toolbar ASSIGNING <ls_toolbar> INDEX 3.
    <ls_toolbar>-function = '&ALL'.
    <ls_toolbar>-butn_type = 0.
    <ls_toolbar>-icon = icon_select_all.
    <ls_toolbar>-quickinfo = 'Seleccionar tudo'(201).
    INSERT INITIAL LINE INTO e_object->mt_toolbar ASSIGNING <ls_toolbar> INDEX 4.
    <ls_toolbar>-function = '&SALL'.
    <ls_toolbar>-butn_type = 0.
    <ls_toolbar>-icon = icon_deselect_all.
    <ls_toolbar>-quickinfo = 'Desseleccionar tudo'(202).

*    READ TABLE e_object->mt_toolbar WITH KEY function = '&REFRESH' INTO ls_toolbar.
*    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <ls_toolbar>.
*    <ls_toolbar> = ls_toolbar.
*    <ls_toolbar>-function = 'REFRESH'.
    DELETE e_object->mt_toolbar WHERE function EQ '&REFRESH'.

    append initial line to e_object->mt_toolbar assigning <ls_toolbar>.
    <ls_toolbar>-butn_type = 3.
    append initial line to e_object->mt_toolbar assigning <ls_toolbar>.
    <ls_toolbar>-function = 'SAVE_DEL'.
    <ls_toolbar>-butn_type = 0.
    <ls_toolbar>-icon = icon_system_save.
    <ls_toolbar>-quickinfo = 'Gravação c/eliminação actuais'(q00).
    <ls_toolbar>-text = <ls_toolbar>-quickinfo.

    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <ls_toolbar>.
    <ls_toolbar>-butn_type = 3.
    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <ls_toolbar>.
    <ls_toolbar>-function = 'DOWNLOAD'.
    <ls_toolbar>-butn_type = 0.
    <ls_toolbar>-icon = icon_export.
    <ls_toolbar>-quickinfo = 'Download Template'(q01).
    <ls_toolbar>-text = <ls_toolbar>-quickinfo.
    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <ls_toolbar>.
    <ls_toolbar>-function = 'UPLOAD'.
    <ls_toolbar>-butn_type = 0.
    <ls_toolbar>-icon = icon_import.
    <ls_toolbar>-quickinfo = 'Upload Calibração'(q02).
    <ls_toolbar>-text = <ls_toolbar>-quickinfo.


  ENDMETHOD.                    "handle_toolbar
  METHOD on_double_click.
  ENDMETHOD.                    "on_double_click
  METHOD on_data_changed.
*    FIELD-SYMBOLS: <ls_cell> LIKE LINE OF er_data_changed->mt_good_cells.
*    DATA: ls_good TYPE lvc_s_modi.
*    DATA: l_plkpt TYPE mlgn-plkpt.
*    DATA: l_matnr TYPE mara-matnr.
*    DATA: l_lgnum TYPE mlgn-lgnum.
*    FIELD-SYMBOLS: <ls_data> TYPE t_data.
*    DATA: ls_mlgt TYPE mlgt.
*
*    CLEAR g_flg_changed.
*    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
*      CASE ls_good-fieldname.
*        WHEN 'PLKPT'.
*          CALL METHOD er_data_changed->get_cell_value
*            EXPORTING
*              i_row_id    = ls_good-row_id
*              i_fieldname = 'PLKPT'
*            IMPORTING
*              e_value     = l_plkpt.
*          CALL METHOD er_data_changed->get_cell_value
*            EXPORTING
*              i_row_id    = ls_good-row_id
*              i_fieldname = 'LGNUM'
*            IMPORTING
*              e_value     = l_lgnum.
*          CALL METHOD er_data_changed->get_cell_value
*            EXPORTING
*              i_row_id    = ls_good-row_id
*              i_fieldname = 'MATNR'
*            IMPORTING
*              e_value     = l_matnr.
*          READ TABLE gt_data ASSIGNING <ls_data> WITH KEY matnr = l_matnr lgnum = l_lgnum lgtyp = l_plkpt.
*          IF sy-subrc NE 0.
*            SELECT SINGLE * INTO ls_mlgt FROM mlgt WHERE matnr = l_matnr AND lgnum = l_lgnum AND lgtyp = l_plkpt.
*            IF sy-subrc EQ 0.
*              READ TABLE gt_data ASSIGNING <ls_data> INDEX ls_good-row_id.
*              IF sy-subrc EQ 0.
*                MOVE-CORRESPONDING ls_mlgt TO <ls_data>.
*                g_flg_changed = 'X'.
*              ENDIF.
*            ELSE.
**nova linha
*              READ TABLE gt_data ASSIGNING <ls_data> INDEX ls_good-row_id.
*              IF sy-subrc EQ 0.
*                CLEAR: <ls_data>-lgpla,
*                       <ls_data>-lpmax,
*                       <ls_data>-lpmin,
*                       <ls_data>-mamng,
*                       <ls_data>-nsmng.
*                <ls_data>-lgtyp = l_plkpt.
*                g_flg_changed = 'X'.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*
*      ENDCASE.
*    ENDLOOP.

  ENDMETHOD.                    "on_data_changed
  METHOD on_data_changed_end.
*    DATA: l_stable TYPE lvc_s_stbl.
*
*    IF g_flg_changed IS NOT INITIAL.
*      CLEAR g_flg_changed.
*      l_stable-row = 'X'.
*      l_stable-col = 'X'.
*      gr_alv->refresh_table_display( is_stable = l_stable )." i_soft_refresh = 'X' ).
*    ENDIF.


*    LOOP AT et_good_cells ASSIGNING <ls_cells>.
*      READ TABLE gt_data ASSIGNING <ls_data> INDEX <ls_cells>-row_id.
*      IF sy-subrc EQ 0.
**        READ TABLE gt_changed_lines ASSIGNING <ls_data_good> INDEX <ls_cells>-row_id.
**        IF sy-subrc EQ 0.
**          <ls_data> = <ls_data_good>.
**        ENDIF.
***        IF <ls_data>-menge NE <ls_data>-banfb.
**    FIELD-SYMBOLS <ls_color> TYPE lvc_s_scol.
***          READ TABLE <ls_data>-color_tab ASSIGNING <ls_color> WITH KEY fname = 'BANFB'.
***          IF sy-subrc NE 0.
***            APPEND INITIAL LINE TO <ls_data>-color_tab ASSIGNING <ls_color>.
***            <ls_color>-fname = 'BANFB'.
***          ENDIF.
***          <ls_color>-color-col = 6.  "vermelho
***        ELSE.
***          DELETE <ls_data>-color_tab WHERE fname EQ 'BANFB'.
***        ENDIF.
*      ENDIF.
*    ENDLOOP.

*    DATA: l_stable TYPE lvc_s_stbl.
*    l_stable-row = 'X'.
*    l_stable-col = 'X'.
*    gr_alv->refresh_table_display( is_stable = l_stable )." i_soft_refresh = 'X' ).


  ENDMETHOD.                    "on_data_changed_end
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
