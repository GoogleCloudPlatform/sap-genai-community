FUNCTION z_llm_cod_buyer_sup_analyst.
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

* Copy the original prompt so we can manipulate it later.
  DATA(lv_request) = cv_prompt.

* Build new lv_request before continuing.
  lv_request = |\n\n<INITIAL REQUEST>\n{ lv_request }\n</INITIAL REQUEST>|.



**********************************************************************
* User prompt chain
**********************************************************************
* Max of 3 links

  DATA : final_link_prompt TYPE string.

  TRY.

      DATA(lcl_llm_user_util) = NEW zcl_llm_util( app_id            = CONV zllm_app_id( app_id )
                                                  business_function = CONV zllm_buss_function_name( business_function ) ).

    CATCH zcx_llm INTO DATA(lcx_user_llm).
* Its possible for their to be no user prompt chain. This is ok.
  ENDTRY.


**********************************************************************
* Main body of work
**********************************************************************

  TRY.

      DATA(lcl_llm_util) = NEW zcl_llm_util( app_id            = CONV zllm_app_id( app_id )
                                             business_function = CONV zllm_buss_function_name( business_function ) ).


*     Set prompt context
*     ---SYSTEM CONTEXT---
      cv_prompt = | \n{ lv_request }|.

*     ---INTRO CONTEXT---
      cv_prompt = |{ cv_prompt }\n\n{ lcl_llm_util->fetch_prompt( iv_prompt_key = 'INTROCONTEXT' )-prompt_text }\n|.

*     ---SYSTEM CONTEXT---
      cv_prompt = |{ cv_prompt }\n\n<SYSTEMCONTEXT>\n{ lcl_llm_util->fetch_prompt( iv_prompt_key = 'SYSTEM' )-prompt_text }\n</SYSTEMCONTEXT>\n|.

*     ---AGENT'S RESPONSE PROMPT
      IF final_link_prompt IS NOT INITIAL.
        cv_prompt = |{ cv_prompt }\n\n<PROMPT FOR RESPONSE>\n{ final_link_prompt }\n</PROMPT FOR RESPONSE>\n|.
      ELSE.
        cv_prompt = |{ cv_prompt }\n\n<PROMPT FOR RESPONSE>\n{ lcl_llm_util->fetch_prompt( iv_prompt_key = CONV zllm_prompt_key( lv_stackline ) )-prompt_text }\n</PROMPT FOR RESPONSE>\n|.
      ENDIF.

*     ---SIGN-OFF CONTEXT
      cv_prompt = |\n{ cv_prompt }\n\n{ lcl_llm_util->fetch_prompt( iv_prompt_key = 'SIGNOFFCONTEXT' )-prompt_text }\n |.

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
      TRY.
          lcl_llm_util->add_function_call_properties( supv_for_sub_agents = CONV zllm_function_name( lv_stackline ) ).
          DATA(response) = lcl_llm_util->call_model( prompt   = lv_request
                                                     sub_call = abap_true ).


* Get subfunction response
          ev_function_response = |<SUB>\n\n{ response-response } \n\n</SUB>|.

          lcl_llm_util->close_model(  ).

        CATCH zcx_llm INTO DATA(lcx_llm).
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
  ENDTRY.


ENDFUNCTION.
