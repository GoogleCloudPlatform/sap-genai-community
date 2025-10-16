INTERFACE zif_llm_mcp
  PUBLIC .


  METHODS process IMPORTING method_name TYPE string
                            data        TYPE zllm_fc_parameters_tt
                            skip_json  type boolean DEFAULT abap_false
                  EXPORTING response    TYPE any
                  RAISING   zcx_llm.

ENDINTERFACE.
