FUNCTION z_llm_cod_soc_sup_returns.
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
* FUTURE CHANGES:
* Include Evaluation bot
* Include security
**********************************************************************


**********************************************************************
* Pre-amble
**********************************************************************

* Instantiate global classes.
  DATA(lcl_template_util) = NEW zcl_llm_template_util(   ).


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
* System prompt chain
**********************************************************************
  TRY.

      DATA(lcl_llm_system_util) = NEW zcl_llm_util( app_id            = conv zllm_app_id( app_id )
                                                    business_function = conv zllm_buss_function_name( business_function ) ).

* In this instance there is no real prompt. The system prompt is enough.
      DATA(system_response) = lcl_llm_system_util->execute_prompt_chain( agent  = CONV zllm_function_name( lv_stackline )
                                                                         fc     = abap_true
                                                                         prompt = '-' ).

      IF system_response-response IS NOT INITIAL.
        lv_request = |<SYSTEM INFORMATION>\n\n { system_response-response }\n\n</SYSTEM INFORMATION>|.
      ENDIF.

    CATCH zcx_llm INTO DATA(lcx_system_llm).
* Its possible for their to be no system prompt chain. This is ok.
  ENDTRY.


**********************************************************************
* User prompt chain
**********************************************************************
* Max of 3 links
  TRY.

      DATA(lcl_llm_user_util) = NEW zcl_llm_util( app_id            = conv zllm_app_id( app_id )
                                                  business_function = conv zllm_buss_function_name( business_function ) ).

* In this instance there is no real prompt. The system prompt is enough.
      DATA(user_response) = lcl_llm_system_util->execute_prompt_chain( agent  = CONV zllm_function_name( lv_stackline )
                                                                       fc     = abap_true
                                                                       prompt = lv_request ).

      IF user_response-response IS NOT INITIAL.
        lv_request = |{ lv_request }| &
                     |\n\n<GENERAL INFORMATION>\n\n{ user_response-response }\n\n</GENERAL INFORMATION>|.
      ENDIF.
    CATCH zcx_llm INTO DATA(lcx_user_llm).
* Its possible for their to be no user prompt chain. This is ok.
  ENDTRY.


**********************************************************************
* Main body of work
**********************************************************************

* Build new lv_request before continuing.
  lv_request = |\n\n<INPUT QUERY>\n{ lv_request }\n</INPUT QUERY>|.

  TRY.

      DATA(lcl_llm_util) = NEW zcl_llm_util( app_id            = CONV zllm_app_id( app_id )
                                             business_function = CONV zllm_buss_function_name( business_function ) ).


* Set prompt context.
      cv_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'SYSTEM' )-prompt_text.
      cv_prompt = |{ cv_prompt }\n\n{ lcl_llm_util->fetch_prompt( iv_prompt_key = CONV zllm_prompt_key( lv_stackline ) )-prompt_text } \n\n { lv_request }|.

**********************************************************************
* Set GCS URI
**********************************************************************
      DATA(lcl_llm_gcs_util) = NEW zcl_llm_gcs_util( app_id            = CONV zllm_app_id( app_id )
                                                     business_function = CONV zllm_buss_function_name( business_function ) ).


      DATA(lt_gcs) = lcl_llm_gcs_util->get_gcs_map( agent = lv_stackline
                                                    user  = sy-uname ).

      LOOP AT lt_gcs ASSIGNING FIELD-SYMBOL(<fs_gcs>).

        lcl_llm_util->set_bucket( bucket = <fs_gcs>-gcs_uri ).

      ENDLOOP.

**********************************************************************
*     Sub agent function calling
**********************************************************************

      lcl_llm_util->add_function_call_properties( supv_for_sub_agents = CONV zllm_function_name( lv_stackline ) ).
      DATA(response) = lcl_llm_util->call_model( prompt   = lv_request
                                                 sub_call = abap_true ).

* Get subfunction response
      ev_function_response = |<SUB>\n\n{ response-response } \n\n</SUB>|.



**********************************************************************
* Action agent function calling
**********************************************************************
* Check if a decision has been made.
      DATA(lcl_decision_util) = NEW zcl_llm_decision_util( app_id            = CONV zllm_app_id( app_id )
                                                           business_function = CONV zllm_buss_function_name( business_function ) ).


      ev_function_response = lcl_decision_util->action_decision( agent_response = |<REQUEST>\n{ lv_request }\n</REQUEST>\n\n{ ev_function_response }|
                                                                 agent          = CONV zllm_function_name( lv_stackline )
                                                                 uuid_key       = CONV guid16( lv_uuid ) ).

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
* Logging
**********************************************************************
  TRY.
      lcl_llm_log->create_log_from_llm_fm( it_function_parameters = it_function_parameters
                                           iv_function_name       = CONV zllm_function_name( lv_stackline )
                                           iv_uuid                = CONV guid16( lv_uuid )
                                           iv_prompt              = cv_prompt
                                           iv_function_response   = ev_function_response ).

    CATCH cx_sy_itab_line_not_found.
* If nothing found, don't dump.
  ENDTRY.


* If no data selected in RAG process return no data.
  IF ev_function_response IS INITIAL.
    ev_function_response = no_data.
  ENDIF.

* End performance trace.
  lcl_performance_trace->end_performance_trace( ).

ENDFUNCTION.
