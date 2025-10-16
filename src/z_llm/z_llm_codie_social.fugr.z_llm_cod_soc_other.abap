FUNCTION z_llm_cod_soc_other.
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

* Set the global context of this FM.
      DATA(ls_prompt) = lcl_llm_util->fetch_prompt( iv_prompt_key = 'INTROCONTEXT' ).
      cv_prompt = |{ cv_prompt } \n\n { ls_prompt-prompt_text }|.


      ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'SYSTEM' ).
      cv_prompt = |{ cv_prompt } \n\n { ls_prompt-prompt_text }|.

* Set the context of this fm.
      TRY.

          ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = CONV zllm_prompt_key( lt_callstack[ 1 ]-blockname ) ).
          cv_prompt = |\n{ cv_prompt }','{ ls_prompt-prompt_text } |.

          ev_function_response = '<No data>'.

        CATCH cx_sy_itab_line_not_found.
      ENDTRY.


      CLEAR: ls_prompt.


* Set the global context of this FM.
      ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'SIGNOFFCONTEXT' ).
      cv_prompt = |\n{ cv_prompt }','{ ls_prompt-prompt_text } |.

    CATCH zcx_llm.
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
