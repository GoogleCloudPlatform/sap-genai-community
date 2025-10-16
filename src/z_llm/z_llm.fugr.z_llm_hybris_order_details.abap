FUNCTION z_llm_hybris_order_details.
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


  IF app_id IS INITIAL.
    app_id = 'CSHUB'.    "For Testing - D
  ENDIF.

  IF business_function IS INITIAL.
    business_function = 'EMAIL'.    "For Testing - D
  ENDIF.

* Output log to show called agent.
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

      IF it_function_parameters IS NOT INITIAL.

        lcl_template_util->get_parameters( EXPORTING parameters     = it_function_parameters
                                                     specific_param = 'HYBRISORDER'
                                           IMPORTING param          = DATA(ls_hybris_order) ).

        ls_hybris_order-param_value = to_upper( ls_hybris_order-param_value ).

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
* Perform RAG
**********************************************************************
  TRY.

      DATA(lref_llm_rag) = NEW zcl_llm_mcp_config_util( ).
      DATA: response_json   TYPE string,
            response        TYPE zllm_mcp_input_for_json_con_tt,
            lt_class_params TYPE abap_parmbind_tab,
            ls_agent_params TYPE zllm_fc_parameters,
            lt_agent_params TYPE zllm_fc_parameters_tt.


* Get the class configurations for RAG
      DATA(lt_class_config) = lref_llm_rag->get_rag_class_details( app_id            = 'LLM'                      "CONV zllm_app_id( app_id )
                                                                   business_function = 'MCP'                      "CONV zllm_buss_function_name( business_function )
                                                                   agent_name        = CONV zllm_function_name( lv_stackline ) ).

* Populate dynamic class structure.
      ls_agent_params-param_name = 'HYBRISORDER'.
      ls_agent_params-param_value = ls_hybris_order-param_value.
      APPEND ls_agent_params TO lt_agent_params.
      CLEAR : ls_agent_params.


      TRY.
          DATA: lo_object TYPE REF TO object.

*         Loop at the configuration
          LOOP AT lt_class_config ASSIGNING FIELD-SYMBOL(<fs_class_config>).

            CREATE OBJECT lo_object TYPE (<fs_class_config>-class_name).

*           Pass agent parameters to dynamic class.
            lt_class_params = VALUE #( ( name = 'DATA'
                                         kind = cl_abap_objectdescr=>exporting
                                         value = REF #( lt_agent_params ) )
                                       ( name = 'CHILD_METHOD_NAME'
                                         kind = cl_abap_objectdescr=>exporting
                                         value = REF #( <fs_class_config>-child_method_name ) )
                                       ( name = 'SKIP_JSON'
                                         kind = cl_abap_objectdescr=>exporting
                                         value = REF #( abap_false ) )
                                       ( name = 'RESPONSE_JSON'
                                         kind = cl_abap_objectdescr=>importing
                                         value = REF #( response_json ) )
                                       ( name = 'RESPONSE'
                                         kind = cl_abap_objectdescr=>importing
                                         value = REF #( response ) ) ).

*           Call class / method combo.
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

    CATCH zcx_llm INTO lcx_llm.

      lcl_llm_log->write_general_log( uuid_key    = CONV guid16( lv_uuid )
                                  request     = cv_prompt
                                  object_type = 'CODIE ERROR'
                                  new_value   = lcx_llm->get_text( ) ).

  ENDTRY.

*  Pass response prompt
  cv_prompt = |{ cv_prompt }\n| &
              |Return the response in a formatted JSON structure as provided|.

**********************************************************************
*** LOGGING
**********************************************************************
  TRY.
      lcl_llm_log->create_log_from_llm_fm( it_function_parameters = it_function_parameters
                                           iv_function_name       = CONV zllm_function_name( lv_stackline )
                                           iv_uuid                = CONV guid( lv_uuid )
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
