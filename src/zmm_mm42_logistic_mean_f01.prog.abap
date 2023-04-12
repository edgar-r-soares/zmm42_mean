*----------------------------------------------------------------------*
***INCLUDE ZMM_MM42_LOGISTIC_DATA_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM f_display_data .
  STATICS: ls_layout TYPE lvc_s_layo.
  STATICS: ls_variant TYPE disvariant.
  STATICS: lt_fieldcat TYPE lvc_t_fcat.
  FIELD-SYMBOLS: <ls_fieldcat> LIKE LINE OF lt_fieldcat.
  DATA: lt_comp TYPE cl_abap_structdescr=>component_table,
        ls_dfies TYPE dfies,
        lr_elemdescr TYPE REF TO cl_abap_elemdescr,
        lr_typedescr TYPE REF TO cl_abap_typedescr.
  FIELD-SYMBOLS <lr_comp> LIKE LINE OF lt_comp.
  DATA: lr_struct_typ TYPE REF TO cl_abap_structdescr.
  DATA: ls_data TYPE t_data.
  FIELD-SYMBOLS: <l_field>.
  DATA: l_help_id TYPE string.

  IF gr_alv IS INITIAL.
    lr_struct_typ ?= cl_abap_structdescr=>describe_by_data( ls_data ).
    lt_comp = lr_struct_typ->get_components( ).
    LOOP AT lt_comp ASSIGNING <lr_comp> WHERE name NE 'COLOR_TAB'.
      TRY.
          lr_typedescr ?= <lr_comp>-type.

          APPEND INITIAL LINE TO lt_fieldcat ASSIGNING <ls_fieldcat>.
          IF lr_typedescr->is_ddic_type( ) = 'X'.
            lr_elemdescr ?= <lr_comp>-type.
            ls_dfies = lr_elemdescr->get_ddic_field( sy-langu ).
            <ls_fieldcat>-tabname     = ls_dfies-tabname.
            <ls_fieldcat>-fieldname   = <lr_comp>-name.
            <ls_fieldcat>-scrtext_l   = ls_dfies-scrtext_l.
            <ls_fieldcat>-scrtext_m   = ls_dfies-scrtext_m.
            <ls_fieldcat>-scrtext_s   = ls_dfies-scrtext_s.

*            if <ls_fieldcat>-fieldname eq 'SALES_3D'.
*              <ls_fieldcat>-scrtext_l   = '3dVendas'.
*              <ls_fieldcat>-coltext     = <ls_fieldcat>-scrtext_l.
*              <ls_fieldcat>-scrtext_m   = <ls_fieldcat>-scrtext_m.
*              <ls_fieldcat>-scrtext_s   = <ls_fieldcat>-scrtext_s.
*              <ls_fieldcat>-qfieldname = 'MEINS'.
**              <ls_fieldcat>-edit = 'X'.
*              <ls_fieldcat>-tech = 'X'.
*editables
            IF <ls_fieldcat>-fieldname EQ 'MEINH'.
*              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'EAN11'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'EANTP'.
              <ls_fieldcat>-edit = 'X'.
            ELSEIF <ls_fieldcat>-fieldname EQ 'HPEAN'.
              <ls_fieldcat>-checkbox = 'X'.
            ENDIF.

            <ls_fieldcat>-reptext     = ls_dfies-reptext.
            <ls_fieldcat>-domname     = ls_dfies-domname.
            <ls_fieldcat>-rollname    = ls_dfies-rollname.
            <ls_fieldcat>-intlen      = ls_dfies-intlen.
            <ls_fieldcat>-outputlen   = ls_dfies-outputlen.
            <ls_fieldcat>-decimals    = ls_dfies-decimals.
            <ls_fieldcat>-datatype    = ls_dfies-datatype.
            <ls_fieldcat>-inttype     = ls_dfies-inttype.
            <ls_fieldcat>-rollname    = ls_dfies-rollname.
            <ls_fieldcat>-convexit    = ls_dfies-convexit.
            <ls_fieldcat>-f4availabl  = ls_dfies-f4availabl.
          ELSE.
            <ls_fieldcat>-intlen      = lr_typedescr->length.
            <ls_fieldcat>-inttype     = lr_typedescr->type_kind.
            <ls_fieldcat>-fieldname   = <lr_comp>-name.

          ENDIF.

*         --- describe the HELP-ID to take over information about ref_table and ref_field
          ASSIGN COMPONENT <ls_fieldcat>-fieldname OF STRUCTURE ls_data"<ls_line>
                                                                TO <l_field>.
          IF sy-subrc EQ 0.
            DESCRIBE FIELD <l_field> HELP-ID l_help_id.
            IF l_help_id CA '-'.
              SPLIT l_help_id AT '-' INTO <ls_fieldcat>-ref_table <ls_fieldcat>-ref_field.
            ELSE.
              <ls_fieldcat>-ref_field = l_help_id.
            ENDIF.
          ENDIF.
        CATCH cx_root.

      ENDTRY.
    ENDLOOP.



    CREATE OBJECT gr_alv
      EXPORTING
        i_parent          = cl_gui_custom_container=>screen0
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    ls_layout-cwidth_opt = 'X'.
    ls_layout-zebra = 'X'.
*    ls_layout-SEL_MODE = 'D'.
    ls_layout-ctab_fname = 'COLOR_TAB'.
    ls_variant-report = sy-repid.
    ls_variant-variant = p_var.

    DATA: lr_event_handler TYPE REF TO lcl_handle_events.

    CREATE OBJECT lr_event_handler.
    SET HANDLER lr_event_handler->on_double_click FOR gr_alv.
    SET HANDLER lr_event_handler->on_data_changed FOR gr_alv. "PB+
    SET HANDLER lr_event_handler->on_data_changed_end FOR gr_alv.
**  lr_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    SET HANDLER lr_event_handler->handle_toolbar FOR gr_alv .
    SET HANDLER lr_event_handler->handle_command FOR gr_alv .
    gr_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    CALL METHOD gr_alv->set_table_for_first_display
      EXPORTING
        is_variant                    = ls_variant
        i_save                        = 'X'
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = gt_data
        it_fieldcatalog               = lt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ENDIF.

ENDFORM.                    " F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  F_SAVE
*&---------------------------------------------------------------------*
FORM f_save USING value(p_delete).
  DATA: lt_mara TYPE TABLE OF t_mara WITH KEY matnr,
        ls_mara TYPE t_mara.
  FIELD-SYMBOLS: <ls_mara> TYPE t_mara,
                 <ls_mean> TYPE t_mean,
                 <ls_data> TYPE t_data.
  DATA: ls_return TYPE bapireturn1.
  DATA: lt_return TYPE TABLE OF bapireturn1.

  LOOP AT gt_data ASSIGNING <ls_data>.
    READ TABLE lt_mara ASSIGNING <ls_mara> WITH TABLE KEY matnr = <ls_data>-matnr.
    IF sy-subrc NE 0.
      CLEAR ls_mara.
      ls_mara-matnr = <ls_data>-matnr.
      INSERT ls_mara INTO TABLE lt_mara ASSIGNING <ls_mara>.
    ENDIF.
    APPEND INITIAL LINE TO <ls_mara>-mean ASSIGNING <ls_mean>.
    <ls_mean>-meinh = <ls_data>-meinh.
    <ls_mean>-ean11 = <ls_data>-ean11.
    <ls_mean>-eantp = <ls_data>-eantp.
    <ls_mean>-hpean = <ls_data>-hpean.
  ENDLOOP.
  LOOP AT lt_mara ASSIGNING <ls_mara>.
    PERFORM f_update_material USING <ls_mara> ls_return p_delete.
    IF ls_return IS NOT INITIAL.
      APPEND ls_return TO lt_return.
    ENDIF.
  ENDLOOP.
  IF lt_return[] IS NOT INITIAL.
    zbatch_input=>display_return_messages( lt_return ).
  ENDIF.
ENDFORM.                    " F_SAVE
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD
*&---------------------------------------------------------------------*
FORM f_download .
  DATA: l_file TYPE string.

  CHECK gt_data IS NOT INITIAL.
  PERFORM f_file_save_dialog USING l_file.
  IF l_file IS NOT INITIAL.
    PERFORM f_create_excel USING l_file.
  ENDIF.
ENDFORM.                    " F_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  f_file_save_dialog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE     text
*----------------------------------------------------------------------*
FORM f_file_save_dialog USING p_file.
  DATA: l_file TYPE string,
        l_path TYPE string,
        l_fullpath TYPE string.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = 'Download template de carregamento'
      default_extension    = 'XLS'
*      default_file_name    =
*      file_filter          =
    CHANGING
      filename             = l_file
      path                 = l_path
      fullpath             = l_fullpath
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4
          .
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    TRANSLATE l_file TO LOWER CASE.
    IF l_file CP '*.xls'.
      CONCATENATE l_fullpath 'X' INTO p_file.
    ELSE.
      p_file = l_fullpath.
    ENDIF.
  ENDIF.

ENDFORM.                    "f_file_save_dialog
*&---------------------------------------------------------------------*
*&      Form  f_create_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE     text
*----------------------------------------------------------------------*
FORM f_create_excel USING p_file.
  DATA: lr_excel TYPE REF TO zcl_excel.

  CREATE OBJECT lr_excel.

  PERFORM f_init_excel_styles USING lr_excel.
  PERFORM f_add_table_to_excel USING lr_excel.

  DATA: cl_writer TYPE REF TO zif_excel_writer.
  DATA: xdata       TYPE xstring,             " Will be used for sending as email
        t_rawdata   TYPE solix_tab,           " Will be used for downloading or open directly
        bytecount   TYPE i.                   " Will be used for downloading or open directly

  CREATE OBJECT cl_writer TYPE zcl_excel_writer_2007.
  xdata = cl_writer->write_file( lr_excel ).
  t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring  = xdata ).
  bytecount = XSTRLEN( xdata ).

  cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = bytecount
                                                    filename     = p_file
                                                    filetype     = 'BIN'
                                           CHANGING data_tab     = t_rawdata ).
ENDFORM.                    "f_create_excel
*&---------------------------------------------------------------------*
*&      Form  f_init_excel_styles
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_EXCEL   text
*----------------------------------------------------------------------*
FORM f_init_excel_styles USING pr_excel TYPE REF TO zcl_excel.
  DATA: lo_worksheet            TYPE REF TO zcl_excel_worksheet,
        lo_style_header_up      TYPE REF TO zcl_excel_style,
        lo_style_header_edit    TYPE REF TO zcl_excel_style,
        lo_style_header_down    TYPE REF TO zcl_excel_style,
        lo_style_normal         TYPE REF TO zcl_excel_style,
        lo_style_edit           TYPE REF TO zcl_excel_style,
        lo_style_red            TYPE REF TO zcl_excel_style,
        lo_style_light_red      TYPE REF TO zcl_excel_style,
        lo_style_green          TYPE REF TO zcl_excel_style,
        lo_style_yellow         TYPE REF TO zcl_excel_style,
        lo_style_light_blue     TYPE REF TO zcl_excel_style,
        lo_border_dark          TYPE REF TO zcl_excel_style_border.

  " Create border object
  CREATE OBJECT lo_border_dark.
  lo_border_dark->border_color-rgb = zcl_excel_style_color=>c_black.
  lo_border_dark->border_style = zcl_excel_style_border=>c_border_thin.

  "Create style top header
  lo_style_header_up                         = pr_excel->add_new_style( ).
  lo_style_header_up->borders->allborders    = lo_border_dark.
  lo_style_header_up->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_header_up->fill->fgcolor-rgb  = '0F243E'. "top zahara blue
  lo_style_header_up->font->bold   = abap_true.
  lo_style_header_up->font->color-rgb  = zcl_excel_style_color=>c_white.
  lo_style_header_up->font->name         = zcl_excel_style_font=>c_name_calibri.
  lo_style_header_up->font->size = 10.
*  lo_style_header_up->font->scheme       = zcl_excel_style_font=>c_scheme_major.
*  lo_style_header_up->protection->locked = zcl_excel_style_protection=>c_protection_locked.
  s_styles-header_up                    = lo_style_header_up->get_guid( ).


  "Create style top header edit column
  lo_style_header_edit                         = pr_excel->add_new_style( ).
  lo_style_header_edit->borders->allborders    = lo_border_dark.
  lo_style_header_edit->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_header_edit->fill->fgcolor-rgb  = '0F243E'. "top zahara blue
  lo_style_header_edit->font->bold        = abap_true.
  lo_style_header_edit->font->color-rgb  = zcl_excel_style_color=>c_red.
  lo_style_header_edit->font->name         = zcl_excel_style_font=>c_name_calibri.
  lo_style_header_edit->font->size = 10.
*  lo_style_header_edit->font->scheme       = zcl_excel_style_font=>c_scheme_major.
*  lo_style_header_edit->protection->locked = zcl_excel_style_protection=>c_protection_locked.
  s_styles-header_edit                    = lo_style_header_edit->get_guid( ).

  "Create style second line header
  lo_style_header_down                         = pr_excel->add_new_style( ).
  lo_style_header_down->borders->allborders    = lo_border_dark.
  lo_style_header_down->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_header_down->fill->fgcolor-rgb  = 'ADD1A5'. "small header zahara green
  lo_style_header_down->font->name         = zcl_excel_style_font=>c_name_calibri.
  lo_style_header_down->font->size = 10.
*  lo_style_header_down->protection->locked = zcl_excel_style_protection=>c_protection_locked.
  s_styles-header_down                    = lo_style_header_down->get_guid( ).

  "Create style table cells
  lo_style_normal                         = pr_excel->add_new_style( ).
  lo_style_normal->borders->allborders    = lo_border_dark.
  lo_style_normal->font->name             = zcl_excel_style_font=>c_name_calibri.
  lo_style_normal->font->size             = 8.
*  lo_style_normal->protection->locked = zcl_excel_style_protection=>c_protection_locked.
  s_styles-normal                         = lo_style_normal->get_guid( ).

  lo_style_red                         = pr_excel->add_new_style( ).
  lo_style_red->borders->allborders    = lo_border_dark.
  lo_style_red->font->name             = zcl_excel_style_font=>c_name_calibri.
  lo_style_red->font->size             = 8.
  lo_style_red->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_red->fill->fgcolor-rgb  = 'FF3333'. "red
  s_styles-red                         = lo_style_red->get_guid( ).

  lo_style_light_red                         = pr_excel->add_new_style( ).
  lo_style_light_red->borders->allborders    = lo_border_dark.
  lo_style_light_red->font->name             = zcl_excel_style_font=>c_name_calibri.
  lo_style_light_red->font->size             = 8.
  lo_style_light_red->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_light_red->fill->fgcolor-rgb  = 'FFCCCC'. "light_red
  s_styles-light_red                         = lo_style_light_red->get_guid( ).

  lo_style_green                         = pr_excel->add_new_style( ).
  lo_style_green->borders->allborders    = lo_border_dark.
  lo_style_green->font->name             = zcl_excel_style_font=>c_name_calibri.
  lo_style_green->font->size             = 8.
  lo_style_green->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_green->fill->fgcolor-rgb  = '00CC44'. "green
  s_styles-green                         = lo_style_green->get_guid( ).

  lo_style_yellow                         = pr_excel->add_new_style( ).
  lo_style_yellow->borders->allborders    = lo_border_dark.
  lo_style_yellow->font->name             = zcl_excel_style_font=>c_name_calibri.
  lo_style_yellow->font->size             = 8.
  lo_style_yellow->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_yellow->fill->fgcolor-rgb  = 'FFFF00'. "yellow
  s_styles-yellow                         = lo_style_yellow->get_guid( ).

  lo_style_light_blue                         = pr_excel->add_new_style( ).
  lo_style_light_blue->borders->allborders    = lo_border_dark.
  lo_style_light_blue->font->name             = zcl_excel_style_font=>c_name_calibri.
  lo_style_light_blue->font->size             = 8.
  lo_style_light_blue->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_light_blue->fill->fgcolor-rgb  = 'DCE6F1'. "light blue
  s_styles-light_blue                         = lo_style_light_blue->get_guid( ).

  "Create style edit cells
  lo_style_edit                         = pr_excel->add_new_style( ).
  lo_style_edit->borders->allborders    = lo_border_dark.
  lo_style_edit->font->name             = zcl_excel_style_font=>c_name_calibri.
  lo_style_edit->font->size             = 8.
  lo_style_edit->fill->filltype     = zcl_excel_style_fill=>c_fill_solid.
  lo_style_edit->fill->fgcolor-rgb  = 'FFFF66'. "top zahara blue
  lo_style_edit->number_format->format_code = zcl_excel_style_number_format=>c_format_text."c_format_number.
  lo_style_edit->protection->locked = zcl_excel_style_protection=>c_protection_unlocked.
  s_styles-edit                        = lo_style_edit->get_guid( ).

ENDFORM.                    "f_init_excel_styles
*&---------------------------------------------------------------------*
*&      Form  f_add_table_to_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_EXCEL   text
*----------------------------------------------------------------------*
FORM f_add_table_to_excel USING pr_excel TYPE REF TO zcl_excel.
  DATA: lo_column      TYPE REF TO cl_salv_column.
  DATA: lo_columns     TYPE REF TO cl_salv_columns_table.
  DATA: lt_col TYPE salv_t_column_ref.
  FIELD-SYMBOLS: <ls_col> TYPE LINE OF salv_t_column_ref.
  DATA: l_s_text TYPE scrtext_s.
  DATA: l_col TYPE i.
  FIELD-SYMBOLS: "<lt_table> TYPE STANDARD TABLE,
*                 <ls_line> TYPE ANY,
                 <l_field> TYPE ANY.
  DATA: lo_type  TYPE REF TO cl_abap_typedescr,
        lo_element  TYPE REF TO cl_abap_elemdescr,
        l_name TYPE string.
  DATA: l_td TYPE string.
  DATA: l_field(255).
  DATA: lr_line TYPE REF TO data.

  DATA: lo_worksheet TYPE REF TO zcl_excel_worksheet.
  DATA: l_row TYPE i.
  DATA: l_style TYPE zexcel_cell_style.
  DATA: l_color_column TYPE lvc_fname.
  DATA: lr_descr TYPE REF TO cl_abap_structdescr.
  FIELD-SYMBOLS: <ls_comp> TYPE abap_compdescr.
  DATA: lt_struct_fields  TYPE cl_abap_structdescr=>component_table.
  FIELD-SYMBOLS: <ls_struct_fields> TYPE LINE OF cl_abap_structdescr=>component_table.
  FIELD-SYMBOLS: <ls_data> TYPE t_data.
  DATA: lo_elem_descr TYPE REF TO cl_abap_elemdescr.
  DATA: l_dfies TYPE dfies.

  l_color_column = 'COLOR_TAB'.

  lo_worksheet = pr_excel->get_active_worksheet( ).
  lo_worksheet->set_title( ip_title = 'Logistica' ).

  READ TABLE gt_data ASSIGNING <ls_data> INDEX 1.
  lr_descr ?= cl_abap_structdescr=>describe_by_data( <ls_data> ).
  lt_struct_fields = lr_descr->get_components( ).

  l_col = 1.
  LOOP AT lr_descr->components ASSIGNING <ls_comp> WHERE name NE l_color_column.
    ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE <ls_data> TO <l_field>.
    IF sy-subrc EQ 0.
      READ TABLE lt_struct_fields ASSIGNING <ls_struct_fields> WITH KEY name = <ls_comp>-name.
      lo_elem_descr ?= <ls_struct_fields>-type.
      l_dfies = lo_elem_descr->get_ddic_field( ).
      l_s_text = l_dfies-scrtext_l.
      lo_worksheet->set_cell( ip_column = l_col ip_row = 1 ip_value = l_s_text ip_style = s_styles-header_up ).
      lo_worksheet->set_cell( ip_column = l_col ip_row = 2 ip_value = <ls_comp>-name ip_style = s_styles-header_down ).
      ADD 1 TO l_col.
    ENDIF.
  ENDLOOP.

  l_row = 2.
  LOOP AT gt_data ASSIGNING <ls_data>.
    ADD 1 TO l_row.
    l_col = 1.

    LOOP AT lr_descr->components ASSIGNING <ls_comp> WHERE name NE l_color_column.
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE <ls_data> TO <l_field>.
      IF sy-subrc EQ 0.
**        GET REFERENCE OF <ls_data> INTO lr_line.
**        l_name = <ls_comp>-name.
**        l_name = <ddic_info>-fieldname.
**          l_style = me->get_cell_style( i_name = l_name ir_line = lr_line ).
        CASE <ls_comp>-name.
          WHEN 'EAN11' OR 'EANTP'." OR 'HPEAN'.
            l_style = s_styles-edit.
          WHEN OTHERS.
            l_style = s_styles-normal.
        ENDCASE.
        lo_worksheet->set_cell( ip_column = l_col ip_row = l_row ip_value = <l_field> ip_style = l_style ).
        ADD 1 TO l_col.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    "f_add_table_to_excel
*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD
*&---------------------------------------------------------------------*
FORM f_upload .
  DATA: l_file TYPE string.
  DATA: l_stable TYPE lvc_s_stbl.

  PERFORM f_file_open_dialog USING l_file.
  IF l_file IS NOT INITIAL.
    PERFORM f_read_excel USING l_file.

    l_stable-row = 'X'.
    l_stable-col = 'X'.
    IF gr_alv IS NOT INITIAL.
      gr_alv->refresh_table_display( is_stable = l_stable )." i_soft_refresh = 'X' ).
    ENDIF.

  ENDIF.
ENDFORM.                    " F_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  f_file_open_dialog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE     text
*----------------------------------------------------------------------*
FORM f_file_open_dialog USING p_file.
  DATA: lt_file TYPE filetable,
        l_rc TYPE i.
  FIELD-SYMBOLS: <ls_file> TYPE LINE OF filetable.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Upload de template preenchido'
      default_extension       = 'XLS'
*      default_filename        =
*      file_filter             =
*      with_encoding           =
*      initial_directory       =
*      multiselection          =
    CHANGING
      file_table              = lt_file
      rc                      = l_rc
*      user_action             =
*      file_encoding           =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5
          .
  IF sy-subrc <> 0 OR l_rc NE 1.

  ELSE.
    LOOP AT lt_file ASSIGNING <ls_file>.
      p_file = <ls_file>-filename.
      EXIT.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "f_file_open_dialog
*&---------------------------------------------------------------------*
*&      Form  f_read_excel
*&---------------------------------------------------------------------*
FORM f_read_excel USING p_file.
  DATA: cl_reader TYPE REF TO zif_excel_reader.
  DATA: lo_excel TYPE REF TO zcl_excel.

  CREATE OBJECT cl_reader TYPE zcl_excel_reader_2007.
  TRY.
      CALL METHOD cl_reader->load_file
        EXPORTING
          i_filename = p_file
        RECEIVING
          r_excel    = lo_excel.
    CATCH zcx_excel .
      MESSAGE e899(mm) WITH 'Erro a abrir ficheiro'.
  ENDTRY.

  PERFORM f_populate_from_excel USING lo_excel.

ENDFORM.                    "f_read_excel
*&---------------------------------------------------------------------*
*&      Form  f_populate_from_excel
*&---------------------------------------------------------------------*
FORM f_populate_from_excel USING po_excel TYPE REF TO zcl_excel.
  FIELD-SYMBOLS: <ls_data> LIKE LINE OF gt_data.
  DATA: lo_worksheet            TYPE REF TO zcl_excel_worksheet.
  DATA: l_title TYPE zexcel_sheet_title.
  DATA: l_row TYPE i,
        l_col TYPE i.
  DATA: l_value TYPE zexcel_cell_value,
        l_rc TYPE sysubrc,
        l_lenum_temp TYPE lenum VALUE IS INITIAL .
  DATA: BEGIN OF ls_col,
         matnr TYPE i,
         meinh TYPE i,"marm-meinh,

         ean11 TYPE i,"mean-ean11,
         eantp TYPE i,"mean-eantp,
*         hpean TYPE i,"mean-hpean,
        END OF ls_col.
  FIELD-SYMBOLS: <l_comp>.
  DATA: l_empty_col.
  DATA: ls_data TYPE t_data.

  lo_worksheet = po_excel->get_active_worksheet( ).

  l_row = 2. "2nd header
  l_col = 1.
*map colunms
  DO.
    TRY.
        CALL METHOD lo_worksheet->get_cell
          EXPORTING
            ip_column = l_col
            ip_row    = l_row
          IMPORTING
            ep_value  = l_value
            ep_rc     = l_rc.
      CATCH zcx_excel .
        MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
    ENDTRY.
    IF l_rc NE 0.
      EXIT.
    ENDIF.
    IF l_value IS INITIAL.
*      exit.
    ELSE.
      ASSIGN COMPONENT l_value OF STRUCTURE ls_col TO <l_comp>.
      IF sy-subrc EQ 0.
        <l_comp> = l_col.
      ENDIF.
    ENDIF.

    CLEAR l_empty_col.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE ls_col TO <l_comp>.
      IF sy-subrc NE 0.
        EXIT.
      ELSE.
        IF <l_comp> IS INITIAL.
          l_empty_col = 'X'.
        ENDIF.
      ENDIF.
    ENDDO.
    IF l_empty_col IS INITIAL.
      EXIT.
    ENDIF.
    IF l_col GT 9999. "max search
      EXIT.
    ENDIF.
    ADD 1 TO l_col.
  ENDDO.
  IF l_empty_col IS NOT INITIAL.
    MESSAGE e899(mm) WITH 'Formato de ficheiro' 'inválido'.
  ELSE.


*go
    DO.
      ADD 1 TO l_row.
      CLEAR ls_data.

      l_col = ls_col-matnr.
      TRY.
          CALL METHOD lo_worksheet->get_cell
            EXPORTING
              ip_column = l_col
              ip_row    = l_row
            IMPORTING
              ep_value  = l_value
              ep_rc     = l_rc.
        CATCH zcx_excel .
          MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
      ENDTRY.
      IF l_rc NE 0.
        EXIT.
      ENDIF.
      IF l_value IS INITIAL.
        EXIT.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = l_value
          IMPORTING
            output       = ls_data-matnr
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.
      ENDIF.

      l_col = ls_col-meinh.
      TRY.
          CALL METHOD lo_worksheet->get_cell
            EXPORTING
              ip_column = l_col
              ip_row    = l_row
            IMPORTING
              ep_value  = l_value
              ep_rc     = l_rc.
        CATCH zcx_excel .
          MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
      ENDTRY.
      IF l_rc NE 0.
        EXIT.
      ENDIF.
      IF l_value IS INITIAL.
        EXIT.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input          = l_value
          IMPORTING
            output         = ls_data-meinh
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
      ENDIF.

      l_col = ls_col-ean11.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
          TRANSLATE l_value TO UPPER CASE.
          ls_data-ean11 = l_value.
        ENDIF.
      ENDIF.

      l_col = ls_col-eantp.
      IF l_col IS NOT INITIAL.
        TRY.
            CALL METHOD lo_worksheet->get_cell
              EXPORTING
                ip_column = l_col
                ip_row    = l_row
              IMPORTING
                ep_value  = l_value
                ep_rc     = l_rc.
          CATCH zcx_excel .
            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
        ENDTRY.
        IF l_rc NE 0.
          EXIT.
        ENDIF.
        IF l_value IS INITIAL.
*        EXIT.
        ELSE.
*          ls_data-eantp = l_value.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_value
            IMPORTING
              output = ls_data-eantp.
        ENDIF.
      ENDIF.

*      l_col = ls_col-hpean.
*      IF l_col IS NOT INITIAL.
*        TRY.
*            CALL METHOD lo_worksheet->get_cell
*              EXPORTING
*                ip_column = l_col
*                ip_row    = l_row
*              IMPORTING
*                ep_value  = l_value
*                ep_rc     = l_rc.
*          CATCH zcx_excel .
*            MESSAGE e899(mm) WITH 'Erro a ler célula' l_row l_col.
*        ENDTRY.
*        IF l_rc NE 0.
*          EXIT.
*        ENDIF.
*        IF l_value IS INITIAL.
**        EXIT.
*        ELSE.
*          ls_data-hpean = 'X'."l_value.
*        ENDIF.
*      ENDIF.

*here we go!
      READ TABLE gt_data ASSIGNING <ls_data> WITH KEY matnr = ls_data-matnr meinh = ls_data-meinh.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO gt_data ASSIGNING <ls_data>.
        <ls_data>-matnr = ls_data-matnr.
        <ls_data>-meinh = ls_data-meinh.
        SELECT SINGLE * FROM mean INTO CORRESPONDING FIELDS OF <ls_data> WHERE matnr = ls_data-matnr AND meinh = ls_data-meinh.
        SELECT SINGLE maktx INTO <ls_data>-maktx FROM makt WHERE matnr EQ ls_data-matnr AND spras EQ sy-langu.
      ENDIF.
      <ls_data>-ean11 = ls_data-ean11.
      <ls_data>-eantp = ls_data-eantp.
*      <ls_data>-hpean = ls_data-hpean.
    ENDDO.

  ENDIF.
ENDFORM.                    "f_populate_from_excel
*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_MATERIAL
*&---------------------------------------------------------------------*
FORM f_update_material  USING    ps_mara TYPE t_mara
                                 ps_return TYPE bapireturn1
                                 p_delete.
  FIELD-SYMBOLS: <ls_mean> TYPE t_mean,
                 <ls_data> TYPE t_data.
  DATA: l_destination(30).
  DATA: ls_head TYPE bapie1mathead,
        lt_client TYPE TABLE OF bapie1marart,
        lt_clientx TYPE TABLE OF bapie1marartx,
        lt_ean TYPE TABLE OF bapie1meanrt,
        lt_uom TYPE TABLE OF bapie1marmrt,
        lt_uomx TYPE TABLE OF bapie1marmrtx.

  FIELD-SYMBOLS: <ls_client> TYPE bapie1marart.
  FIELD-SYMBOLS: <ls_clientx> TYPE bapie1marartx.
  FIELD-SYMBOLS: <ls_ean> TYPE bapie1meanrt.
  FIELD-SYMBOLS: <ls_uom> TYPE bapie1marmrt.
  FIELD-SYMBOLS: <ls_uomx> TYPE bapie1marmrtx.

  DATA: ls_mara TYPE mara,
        ls_mean TYPE mean.

  CLEAR ps_return.
  CONCATENATE 'ERPMRP' sy-mandt INTO l_destination.
  ls_head-material = ps_mara-matnr.
  ls_head-logst_view = 'X'.
  ls_head-logdc_view = 'X'.
  ls_head-function = '004'.
  ls_head-no_appl_log = 'X'."p_no_log.


  SELECT SINGLE * INTO ls_mara FROM mara WHERE matnr EQ ps_mara-matnr.
  IF sy-subrc EQ 0.
    APPEND INITIAL LINE TO lt_client ASSIGNING <ls_client>.
    <ls_client>-function = '004'.
    <ls_client>-material = ps_mara-matnr.
    <ls_client>-changed_by = sy-uname.
    APPEND INITIAL LINE TO lt_clientx ASSIGNING <ls_clientx>.
    <ls_clientx>-function = '004'.
    <ls_clientx>-material = ps_mara-matnr.
    <ls_clientx>-changed_by = 'X'.

    LOOP AT ps_mara-mean ASSIGNING <ls_mean>.
      CLEAR ls_mean.
      SELECT SINGLE * INTO ls_mean FROM mean WHERE matnr EQ ps_mara-matnr AND ean11 EQ <ls_mean>-ean11.
      IF ls_mean-ean11 EQ <ls_mean>-ean11 AND ls_mean-meinh EQ <ls_mean>-meinh AND ( ls_mean-eantp EQ <ls_mean>-eantp AND ls_mean-eantp IS NOT INITIAL ).
      ELSE.
        APPEND INITIAL LINE TO lt_ean ASSIGNING <ls_ean>.
        <ls_ean>-function = '004'.
        <ls_ean>-material = ps_mara-matnr.
        <ls_ean>-ean_upc = <ls_mean>-ean11.

        <ls_ean>-unit = <ls_mean>-meinh.
        <ls_ean>-ean_cat = <ls_mean>-eantp.
      ENDIF.
    ENDLOOP.

    IF p_delete IS NOT INITIAL.
      DATA: lt_mean TYPE TABLE OF mean.
      SELECT * INTO TABLE lt_mean FROM mean WHERE matnr EQ ps_mara-matnr.
      LOOP AT lt_mean INTO ls_mean.
        READ TABLE ps_mara-mean WITH KEY ean11 = ls_mean-ean11 TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0. "delete!!
        APPEND INITIAL LINE TO lt_ean ASSIGNING <ls_ean>.
        <ls_ean>-function = '003'. "delete
        <ls_ean>-material = ps_mara-matnr.
        <ls_ean>-ean_upc = ls_mean-ean11.
        <ls_ean>-unit = ls_mean-meinh.
        <ls_ean>-ean_cat = ls_mean-eantp.
        ENDIF.
      ENDLOOP.
    ENDIF.


    IF lt_ean IS INITIAL.
      DELETE lt_client WHERE material EQ ps_mara-matnr.
      DELETE lt_clientx WHERE material EQ ps_mara-matnr.
    ENDIF.

    IF lt_client IS NOT INITIAL.
      LOOP AT lt_ean ASSIGNING <ls_ean>.
        READ TABLE lt_uom WITH KEY alt_unit = <ls_ean>-unit TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0. "marcar EAN principal
          APPEND INITIAL LINE TO lt_uom ASSIGNING <ls_uom>.
          <ls_uom>-function = '004'.
          <ls_uom>-material = ps_mara-matnr.
          <ls_uom>-alt_unit = <ls_ean>-unit.
          APPEND INITIAL LINE TO lt_uomx ASSIGNING <ls_uomx>.
          <ls_uomx>-function = '004'.
          <ls_uomx>-material = ps_mara-matnr.
          <ls_uomx>-alt_unit = <ls_ean>-unit.

          <ls_uom>-ean_upc = <ls_ean>-ean_upc.
          <ls_uomx>-ean_upc = 'X'.
          IF <ls_ean>-ean_cat IS NOT INITIAL.
            <ls_uom>-ean_cat = <ls_ean>-ean_cat.
            <ls_uomx>-ean_cat = 'X'.
          ENDIF.

        ENDIF.
      ENDLOOP.

      CALL FUNCTION 'BAPI_MATERIAL_MAINTAINDATA_RT'
        DESTINATION l_destination
        EXPORTING
          headdata            = ls_head
        IMPORTING
          return              = ps_return
        TABLES
          clientdata          = lt_client
          clientdatax         = lt_clientx
          unitsofmeasure      = lt_uom
          unitsofmeasurex     = lt_uomx
          internationalartnos = lt_ean.
      IF ps_return-type EQ 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
          DESTINATION l_destination.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          DESTINATION l_destination.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_UPDATE_MATERIAL
