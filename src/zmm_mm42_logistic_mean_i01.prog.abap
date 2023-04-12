*----------------------------------------------------------------------*
***INCLUDE ZMM_MM42_LOGISTIC_DATA_I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_2000 INPUT.
  case ok_code.
    when 'EXIT' or 'BACK' or 'CANCEL'.
      clear ok_code.
      leave to screen 0.
    when 'SAVE'.
      clear ok_code.
*      perform f_check.
      perform f_save USING space.
    when others.
      clear ok_code.

      data: l_stable type lvc_s_stbl.
      l_stable-row = 'X'.
      l_stable-col = 'X'.
      if gr_alv is not initial.
        gr_alv->refresh_table_display( is_stable = l_stable )." i_soft_refresh = 'X' ).
      endif.
  endcase.
ENDMODULE.                 " USER_COMMAND_2000  INPUT
