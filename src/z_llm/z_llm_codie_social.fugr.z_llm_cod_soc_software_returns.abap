FUNCTION z_llm_cod_soc_software_returns.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_FUNCTION_PARAMETERS) TYPE
*"        /GOOG/T_FUNCTION_PARAMETERS
*"  EXPORTING
*"     REFERENCE(EV_FUNCTION_RESPONSE) TYPE  STRING
*"  CHANGING
*"     REFERENCE(CV_PROMPT) TYPE  STRING
*"  RAISING
*"      /GOOG/CX_SDK
*"----------------------------------------------------------------------
  DATA : function_call_uuid TYPE guid16,
         app_id             TYPE zllm_app_id VALUE 'CSHUB',
         business_function  TYPE zllm_buss_function_name VALUE 'SOCIAL'.

  DATA : lv_memory_id       TYPE char50.

  lv_memory_id = |ZLLM_{ app_id }_{ business_function }|.

* Import unique UUID linking call_model to agent.
  IMPORT p1 = function_call_uuid FROM MEMORY ID lv_memory_id.

* Output log to show called agent.
  DATA(lcl_llm_log) = NEW zcl_llm_log( app_id            = app_id
                                       business_function = business_function ).


* Get the function module name. Due to the nature of ABAP call stack using system functions doesnt work.
  DATA: lt_callstack TYPE abap_callstack,
        ls_callstack TYPE abap_callstack_line,
        lv_fname     TYPE rs38l_fnam.

* Get the ABAP stack.
  CALL FUNCTION 'SYSTEM_CALLSTACK'
    EXPORTING
      max_level = 0
    IMPORTING
      callstack = lt_callstack.


  TRY.

      DATA(lcl_llm_util) = NEW zcl_llm_util( app_id            = app_id
                                             business_function = business_function ).

* Capture the original request
      DATA(lv_original_request) = cv_prompt.

*** Set the global context of this FM. ***
* Set the global intro of this FM.
      DATA(ls_prompt) = lcl_llm_util->fetch_prompt( iv_prompt_key = 'INTROCONTEXT' ).
      cv_prompt = |{ cv_prompt } \n\n { ls_prompt-prompt_text }|.


      ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'SYSTEM' ).
      cv_prompt = |{ cv_prompt } \n\n { ls_prompt-prompt_text }|.

* Set the FM specific prompt.
      TRY.

          ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = CONV zllm_prompt_key( lt_callstack[ 1 ]-blockname ) ).
          cv_prompt = |{ cv_prompt }\n\n{ ls_prompt-prompt_text } |.

        CATCH cx_sy_itab_line_not_found.
      ENDTRY.


* Set the global ending of this FM.
      ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'SIGNOFFCONTEXT' ).
      cv_prompt = |{ cv_prompt }\n\n{ ls_prompt-prompt_text } |.


      TRY.
* LLM should have identified the article number
          DATA(lv_item_descryption) = it_function_parameters[ parameter_name ='RETURNITEMDESCRYPTION' ]-parameter_value.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.


      TRY.
* LLM should have identified the return reason
          DATA(lv_return_reason) = it_function_parameters[ parameter_name ='RETURNREASON' ]-parameter_value.
        CATCH cx_sy_itab_line_not_found.
          lv_return_reason = |UNKNOWN|.
      ENDTRY.


* LLM should have identified the order number
      TRY.
          DATA(lv_hybris_order) = it_function_parameters[ parameter_name ='HYBRISORDER' ]-parameter_value.
          DATA(email) = it_function_parameters[ parameter_name ='EMAIL' ]-parameter_value.
          lv_hybris_order = to_upper( lv_hybris_order ).
* The LLM may have identified the order but it might be formatted with spaces. Remove.
          CONDENSE lv_hybris_order NO-GAPS.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.


* Order number found.

      DATA(lcl_llm_rag_util) = NEW zcl_cs_hub_llm_util( hybris_order   = CONV bstnk( lv_hybris_order )
                                                        customer_email = email ).
      ev_function_response = lcl_llm_rag_util->llm_get_data( fd_time       = abap_true
                                                             order         = abap_true
                                                             returns       = abap_false
                                                             giftcard      = abap_true
                                                             stripe_email  = abap_true
                                                             stripe_hybris = abap_true
                                                             tracking      = abap_true
                                                             weekdays      = abap_true ).



      "Add the return reason to
      ev_function_response = |{ ev_function_response }\n\n<RETURN REASON> { lv_return_reason }|.






    CATCH /goog/cx_sdk.
    CATCH zcx_llm.
    CATCH zcx_cs_hub.
    CATCH cx_sy_itab_line_not_found.

* Update the global context to error context.
      ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'RETURNERRORCONTEXT' ).
      cv_prompt = ls_prompt-prompt_text.

      CLEAR: ls_prompt.

      ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'RETURNNOTFOUND' ).
      ev_function_response =  ls_prompt-prompt_text.


  ENDTRY.

*** LOGGING
  TRY.
      lcl_llm_log->create_log_from_llm_fm( it_function_parameters = it_function_parameters
                                           iv_function_name       = CONV zllm_function_name( lt_callstack[ 1 ]-blockname )
                                           iv_uuid                = function_call_uuid
                                           iv_prompt              = cv_prompt
                                           iv_function_response   = ev_function_response ).

    CATCH cx_sy_itab_line_not_found.
* If nothing found, don't dump.
  ENDTRY.




ENDFUNCTION.
