CLASS zcl_llm_json_converter_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS : convert_to_json           IMPORTING input          TYPE any
                                        RETURNING VALUE(rv_json) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_llm_json_converter_util IMPLEMENTATION.

  METHOD convert_to_json.
*** Method implementation to take input of any type and produce JSON format

    IF input IS NOT INITIAL.

      /ui2/cl_json=>serialize(
        EXPORTING
          data             = input
          pretty_name      = /ui2/cl_json=>pretty_mode-low_case
          format_output    = abap_true
        RECEIVING
          r_json           = rv_json
      ).


    ENDIF.

  ENDMETHOD.

ENDCLASS.
