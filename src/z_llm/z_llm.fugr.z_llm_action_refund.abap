FUNCTION z_llm_action_refund.
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


**********************************************************************
* Fetch parameters provided from function call process
**********************************************************************
  TRY.
* With the app id and business function now set, we should re-instantiate the template class.
      FREE lcl_template_util.
      lcl_template_util = NEW zcl_llm_template_util( app_id            = CONV zllm_app_id( app_id )
                                                     business_function = CONV zllm_buss_function_name( business_function ) ).
* Hybris order
      IF it_function_parameters IS NOT INITIAL.

        lcl_template_util->get_parameters( EXPORTING parameters     = it_function_parameters
                                                     specific_param = 'HYBRISORDER'
                                           IMPORTING param          = DATA(ls_hybris_order) ).

        ls_hybris_order-param_value = to_upper( ls_hybris_order-param_value ).

      ENDIF.

* Article
      IF it_function_parameters IS NOT INITIAL.

        lcl_template_util->get_parameters( EXPORTING parameters     = it_function_parameters
                                                     specific_param = 'ARTICLE'
                                           IMPORTING param          = DATA(ls_article) ).

        ls_article-param_value = to_upper( ls_article-param_value ).

      ENDIF.


* Quantity to be returned
      IF it_function_parameters IS NOT INITIAL.

        lcl_template_util->get_parameters( EXPORTING parameters     = it_function_parameters
                                                     specific_param = 'QUANTITY'
                                           IMPORTING param          = DATA(ls_quantity) ).

        ls_quantity-param_value = to_upper( ls_quantity-param_value ).

      ENDIF.

    CATCH zcx_llm INTO DATA(lcx_llm).

      lcl_llm_log->write_general_log( uuid_key    = CONV guid16( lv_uuid )
                                  request     = cv_prompt
                                  object_type = 'CODIE ERROR'
                                  new_value   = lcx_llm->get_text( ) ).

      ev_function_response = no_data_error.

      RETURN.

    CATCH cx_sy_itab_line_not_found.

* This agent expects parameters. If none were passed the rest will fail. Return now.

      lcl_llm_log->write_general_log( uuid_key    = CONV guid16( lv_uuid )
                                  request     = cv_prompt
                                  object_type = 'CODIE ERROR'
                                  new_value   = 'Agent parameters expected. None provided' ).

      ev_function_response = no_data_error.

      RETURN.
  ENDTRY.


**********************************************************************
* System prompt chain
**********************************************************************
  TRY.

      DATA(lcl_llm_system_util) = NEW zcl_llm_util( app_id            = 'LLM'
                                                    business_function = 'RAG' ).

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

      DATA(lcl_llm_user_util) = NEW zcl_llm_util( app_id            = 'LLM'
                                                  business_function = 'MCP' ).

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

  DATA(lcl_llm_util) = NEW zcl_llm_util( app_id            = CONV zllm_app_id( app_id )
                                         business_function = CONV zllm_buss_function_name( business_function ) ).


* Set prompt context.
  cv_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'SYSTEM' )-prompt_text.
  cv_prompt = |{ cv_prompt }\n\n{ lcl_llm_util->fetch_prompt( iv_prompt_key = CONV zllm_prompt_key( lv_stackline ) )-prompt_text } \n\n { lv_request }|.



  TRY.
      DATA(lref_llm_rag) = NEW zcl_llm_mcp_config_util( ).
      DATA: response_json   TYPE string,
            response        TYPE zllm_mcp_input_for_json_con_tt,
            lt_class_params TYPE abap_parmbind_tab,
            ls_agent_params TYPE zllm_fc_parameters,
            lt_agent_params TYPE zllm_fc_parameters_tt.



* Get the class configurations for RAG
      DATA(lt_class_config) = lref_llm_rag->get_rag_class_details( app_id            = CONV zllm_app_id( app_id )
                                                                   business_function = CONV zllm_buss_function_name( business_function )
                                                                   agent_name        = CONV zllm_function_name( lv_stackline ) ).


* Populate dynamic class structure.
      ls_agent_params-param_name = 'ORDER'.
      ls_agent_params-param_value = ls_hybris_order-param_value.
      APPEND ls_agent_params TO lt_agent_params.

      ls_agent_params-param_name = 'ARTICLE'.
      ls_agent_params-param_value = ls_article-param_value.
      APPEND ls_agent_params TO lt_agent_params.

      ls_agent_params-param_name = 'QUANTITY'.
      ls_agent_params-param_value = ls_quantity-param_value.
      APPEND ls_agent_params TO lt_agent_params.


      TRY.
          DATA: lo_object TYPE REF TO object.

* Loop at the configuration.
          LOOP AT lt_class_config ASSIGNING FIELD-SYMBOL(<fs_class_config>).

            CREATE OBJECT lo_object TYPE (<fs_class_config>-class_name).

* Pass agent parameters to dynamic class.
            lt_class_params = VALUE #( ( name = 'CHILD_METHOD_NAME'
                                         kind = cl_abap_objectdescr=>exporting
                                         value = REF #( <fs_class_config>-child_method_name ) )
                                       ( name = 'DATA'
                                         kind = cl_abap_objectdescr=>exporting
                                         value = REF #( lt_agent_params ) )
                                       ( name = 'RESPONSE_JSON'
                                         kind = cl_abap_objectdescr=>importing
                                         value = REF #( response_json ) )
                                       ( name = 'RESPONSE'
                                         kind = cl_abap_objectdescr=>importing
                                         value = REF #( response ) ) ).

* Call class / method combo.
            CALL METHOD lo_object->(<fs_class_config>-parent_method_name)
              PARAMETER-TABLE lt_class_params.

            ev_function_response = |{ ev_function_response } \n\n { response_json }|.

            FREE lo_object.
            CLEAR: response_json.

          ENDLOOP.
        CATCH zcx_llm INTO lcx_llm.

* Dynamic call failure.
          lcl_llm_log->write_general_log( uuid_key    = CONV guid16( lv_uuid )
                                      request     = cv_prompt
                                      object_type = 'CODIE ERROR'
                                      new_value   = lcx_llm->get_text( ) ).
      ENDTRY.




    CATCH /goog/cx_sdk INTO DATA(lcx_sdk).
      lcl_llm_log->write_general_log( uuid_key    = CONV guid16( lv_uuid )
                                      request     = cv_prompt
                                      object_type = 'SDK ERROR'
                                      new_value   = lcx_sdk->get_text( ) ).

      ev_function_response = no_data_error.

    CATCH zcx_llm INTO lcx_llm.
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


ENDFUNCTION.
