FUNCTION z_llm_cod_soc_sub_del_price.
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
  CONSTANTS: no_data_error TYPE string VALUE 'No data - an error occured',
             no_data       TYPE string VALUE 'No data'.

**********************************************************************
* Pre-amble
**********************************************************************

* Instantiate global classes.
  DATA(lcl_template_util) = NEW zcl_llm_template_util(  ).


* Call templated methods.
  DATA(lv_uuid) = lcl_template_util->get_memory_id( memory_id = 'ZLLM_CODIE_UUID' ).
  DATA(app_id) = lcl_template_util->get_memory_id( memory_id = 'ZLLM_CODIE_APPID' ).
  DATA(business_function) = lcl_template_util->get_memory_id( memory_id = 'ZLLM_CODIE_BUSINESS_FUNCTION' ).
  DATA(lv_stackline) = lcl_template_util->get_app_stack( ).


* Prepare logging.
  DATA(lcl_llm_log) = NEW zcl_llm_log( app_id            = CONV zllm_app_id( app_id )
                                       business_function = CONV zllm_buss_function_name( business_function ) ).

* Setup performance trace.
  DATA(lcl_performance_trace) = NEW zcl_llm_performance_util( app_id            = CONV zllm_app_id( app_id )
                                                              business_function = CONV zllm_buss_function_name( business_function )
                                                              uuid_key          = CONV guid16( lv_uuid ) ).

* Begin performance trace.
  lcl_performance_trace->start_perforamnce_trace( type = lv_stackline ).

* Copy the original prompt so we can manipulate it later.
  DATA(lv_request) = cv_prompt.


**********************************************************************
* Begin main work
**********************************************************************
  TRY.

      DATA: lv_cv_prompt TYPE string.

      DATA(lcl_llm_util) = NEW zcl_llm_util( app_id            = CONV zllm_app_id( app_id )
                                             business_function = CONV zllm_buss_function_name( business_function ) ).

*     Trigger prompt chaining
      ev_function_response = lcl_llm_util->execute_prompt_chain( EXPORTING agent    = CONV zllm_function_name( lv_stackline )
                                                                           fc       = abap_true
                                                                           prompt   = cv_prompt
                                                                 IMPORTING cv_prompt = lv_cv_prompt )-response.

      cv_prompt = |{ cv_prompt } \n\n{ lv_cv_prompt }|.


    CATCH /goog/cx_sdk INTO DATA(lcx_sdk).
      lcl_llm_log->write_general_log( uuid_key    = CONV guid16( lv_uuid )
                                      request     = cv_prompt
                                      object_type = 'SDK ERROR'
                                      new_value   = lcx_sdk->get_text( ) ).

      ev_function_response = no_data_error.

    CATCH zcx_llm INTO DATA(lcx_llm).
      lcl_llm_log->write_general_log( uuid_key    = CONV guid16( lv_uuid )
                                      request     = cv_prompt
                                      object_type = 'CODIE ERROR'
                                      new_value   = lcx_llm->get_text( ) ).

      ev_function_response = no_data_error.

  ENDTRY.


**********************************************************************
*** Logging
**********************************************************************
  TRY.
      lcl_llm_log->create_log_from_llm_fm( it_function_parameters = it_function_parameters
                                           iv_function_name       = CONV zllm_function_name( lv_stackline )
                                           iv_uuid                = CONV guid16( lv_uuid )
                                           iv_prompt              = cv_prompt
                                           iv_function_response   = ev_function_response ).

    CATCH cx_sy_itab_line_not_found.
  ENDTRY.


* If no data selected in RAG process return no data.
  IF ev_function_response IS INITIAL.
    ev_function_response = no_data.
  ENDIF.

* End performance trace.
  lcl_performance_trace->end_performance_trace( ).

ENDFUNCTION.
