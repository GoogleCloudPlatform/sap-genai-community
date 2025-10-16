FUNCTION Z_PNG_TO_BASE64.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FILENAME) TYPE  STRING
*"  EXPORTING
*"     VALUE(EV_BASE64) TYPE  STRING
*"  EXCEPTIONS
*"      FILE_NOT_FOUND
*"      CONVERSION_ERROR
*"----------------------------------------------------------------------
  "----------------------------------------------------------------------

  DATA:
    lv_xstring TYPE xstring,
    lv_string  TYPE string,
    lv_filelen TYPE i,
    lv_return  TYPE i.

  FIELD-SYMBOLS:
    <ls_file> TYPE LINE OF x031l_tab.

  TRY.
      " Read the file into an XSTRING.
      CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          filename                = iv_filename
          filetype                = 'BIN'
        IMPORTING
          filelength              = lv_filelen
        CHANGING
          data_tab                = lv_xstring
        EXCEPTIONS
          file_open_error         = 1
          file_read_error         = 2
          invalid_type            = 3
          no_batch                = 4
          unknown_error           = 5
          invalid_table_width     = 6
          gui_refuse_filetransfer = 7
          customer_error          = 8
          OTHERS                  = 9.

      IF sy-subrc <> 0.
        RAISE file_not_found.
      ENDIF.

      " Convert XSTRING to BASE64.
      CALL FUNCTION 'SCMS_XSTRING_TO_BASE64'
        EXPORTING
          input_length = lv_filelen
          input        = lv_xstring
        IMPORTING
          output       = lv_string
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.

      IF sy-subrc <> 0.
        RAISE conversion_error.
      ENDIF.

      ev_base64 = lv_string.

    "CATCH file_not_found.
    " MESSAGE 'File not found' TYPE 'E'.
    "CATCH conversion_error.
    "  MESSAGE 'Base64 conversion error' TYPE 'E'.
    CATCH cx_sy_conversion_error.
      MESSAGE 'Xstring conversion error' TYPE 'E'.
  ENDTRY.






ENDFUNCTION.
