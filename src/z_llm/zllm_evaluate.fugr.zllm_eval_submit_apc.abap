FUNCTION zllm_eval_submit_apc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_GUID) TYPE  SYSUUID_C32
*"     VALUE(IV_RESPONSE) TYPE  STRING
*"----------------------------------------------------------------------
  DATA(lref_apc_handler) = NEW zcl_fiori_generic_apc_handler( ).
  DATA(lref_apc_object) = NEW zcl_fiori_generic_apc_object( ).

  DATA(lt_ai_response) = VALUE zllm_eval_response_tt( ( response = iv_response
                                               uuid = iv_guid
                                               user_id = sy-uname ) ).

* Setting the results in a generic apc object. So that results will be fetched in the process method.
  lref_apc_object->set_eval_response( lt_ai_response ).

  CONSTANTS:
    lc_controller_id TYPE zapc_controller_id VALUE 'AI_EVALUATE_REPORT',
    lc_class_name    TYPE zfiori_class_name VALUE 'ZCL_LLM_EVALUATE_UTIL',
    lc_result_tt_name type zapc_result_table_type_name value 'ZLLM_EVAL_RESPONSE_TT'.

  TRY.

*     Registering APC request in a generic APC handler
      DATA(registered) = lref_apc_handler->register_apc_request(
        EXPORTING
          iv_controller_id   = lc_controller_id
          iv_class_name      = lc_class_name
          io_class_object    = lref_apc_object
          iv_publish_message = abap_true
          iv_result_tt_name  = lc_result_tt_name ).

    CATCH zcx_apc_handler INTO DATA(lcx_apc_handler).

*       Throw error if APC fails
      MESSAGE 'APC failed' TYPE 'X'.

  ENDTRY.

ENDFUNCTION.
