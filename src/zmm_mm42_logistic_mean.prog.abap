*&---------------------------------------------------------------------*
*& Report  ZMM_MM42_LOGISTIC_DATA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zmm_mm42_logistic_mean.
TYPE-POOLS: icon.
TABLES: mara, marc, marm, mean.
TYPES: BEGIN OF t_data,
         matkl TYPE mara-matkl,
         matnr TYPE mara-matnr,
         maktx TYPE makt-maktx,
         meinh TYPE marm-meinh,

         ean11 TYPE mean-ean11,
         eantp TYPE mean-eantp,
         hpean TYPE mean-hpean,

         meins TYPE mara-meins,
         color_tab TYPE lvc_t_scol,

       END OF t_data.
DATA: gt_data TYPE TABLE OF t_data.
DATA: ok_code TYPE sy-ucomm.
DATA: gr_alv TYPE REF TO cl_gui_alv_grid.
DATA: g_flg_changed.

TYPES: BEGIN OF t_styles,
         header_up      TYPE zexcel_cell_style,
         header_edit    TYPE zexcel_cell_style,
         header_down    TYPE zexcel_cell_style,
         normal         TYPE zexcel_cell_style,
         edit           TYPE zexcel_cell_style,
         red            TYPE zexcel_cell_style,
         light_red      TYPE zexcel_cell_style,
         green          TYPE zexcel_cell_style,
         yellow         TYPE zexcel_cell_style,
         light_blue     TYPE zexcel_cell_style,
       END OF t_styles.
DATA: s_styles TYPE t_styles.

TYPES: BEGIN OF t_mean,
         meinh TYPE marm-meinh,
         ean11 TYPE mean-ean11,
         eantp TYPE mean-eantp,
         hpean TYPE mean-hpean,
       END OF t_mean,
       BEGIN OF t_mara,
         matnr TYPE mara-matnr,
         mean TYPE t_mean OCCURS 0,
       END OF t_mara.

SELECT-OPTIONS: s_matnr FOR mara-matnr,
                s_matkl FOR mara-matkl,
                s_werks FOR marc-werks.
SELECT-OPTIONS: s_meinh FOR marm-meinh.

SELECTION-SCREEN BEGIN OF BLOCK v0 WITH FRAME TITLE text-v01.
PARAMETERS p_var TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK v0.

INCLUDE zmm_mm42_logistic_mean_o01.
*INCLUDE zmm_mm42_logistic_marm_o01.
*INCLUDE zmm_mm42_logistic_data_o01.
INCLUDE zmm_mm42_logistic_mean_c01.
*INCLUDE zmm_mm42_logistic_marm_c01.
*INCLUDE zmm_mm42_logistic_data_c01.
INCLUDE zmm_mm42_logistic_mean_f01.
*INCLUDE zmm_mm42_logistic_marm_f01.
*INCLUDE zmm_mm42_logistic_data_f01.

START-OF-SELECTION.
  PERFORM f_select.

END-OF-SELECTION.
  IF gt_data IS NOT INITIAL.
    IF sy-batch IS INITIAL. "repid EQ 'ZMM_MRP_REPORT'.
      CALL SCREEN 2000.
    ELSE.
      PERFORM f_list_only.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  f_select
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_select.
  IF s_werks[] IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_data FROM mara JOIN makt ON makt~matnr EQ mara~matnr
           JOIN mean ON mean~matnr EQ mara~matnr
           WHERE mara~matnr IN s_matnr AND mara~matkl IN s_matkl AND mean~meinh IN s_meinh.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_data FROM mara JOIN makt ON makt~matnr EQ mara~matnr
           JOIN mean ON mean~matnr EQ mara~matnr
           WHERE mara~matnr IN s_matnr AND mara~matkl IN s_matkl AND mean~meinh IN s_meinh
            AND EXISTS ( SELECT * FROM marc WHERE matnr EQ mara~matnr AND werks IN s_werks ).
  ENDIF.
ENDFORM.                    "f_select
*&---------------------------------------------------------------------*
*&      Form  f_list_only
*&---------------------------------------------------------------------*
FORM f_list_only .
  DATA: gr_salv TYPE REF TO cl_salv_table.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = gr_salv
    CHANGING
      t_table      = gt_data ).

  DATA: lr_functions TYPE REF TO cl_salv_functions_list.
  lr_functions = gr_salv->get_functions( ).
  lr_functions->set_all( abap_true ).
  lr_functions->set_default( abap_true ).
  DATA: lo_funcs TYPE salv_t_ui_func.
  DATA: lo_func TYPE salv_s_ui_func.
  lo_funcs = lr_functions->get_functions( ).
  LOOP AT lo_funcs INTO lo_func.
    lo_func-r_function->set_enable( value = 'X' ).
    lo_func-r_function->set_visible( value = 'X' ).
  ENDLOOP.

  DATA: lo_layout  TYPE REF TO cl_salv_layout,
        lf_variant TYPE slis_vari,
        ls_key    TYPE salv_s_layout_key.
  lo_layout = gr_salv->get_layout( ).
  ls_key-report = sy-repid.

  lo_layout->set_key( ls_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  lo_layout->set_default( abap_true ).

*  lf_variant = p_var.
  lo_layout->set_initial_layout( lf_variant ).

  gr_salv->display( ).

ENDFORM.                    "f_list_only
*

INCLUDE zmm_mm42_logistic_mean_i01.
*INCLUDE zmm_mm42_logistic_marm_i01.
*INCLUDE zmm_mm42_logistic_data_i01.
