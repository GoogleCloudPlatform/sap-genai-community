CLASS zcl_llm_decision_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: constructor     IMPORTING app_id            TYPE zllm_app_id
                                       business_function TYPE zllm_buss_function_name,

      action_decision IMPORTING agent_response  TYPE string
                                agent           TYPE zllm_function_name
                                uuid_key        TYPE guid16
                      RETURNING VALUE(response) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.


    DATA: gv_app_id             TYPE zllm_app_id,
          gv_business_function  TYPE zllm_buss_function_name,
          gcl_log               TYPE REF TO zcl_llm_log,
          gv_function_call_uuid TYPE guid16.


    METHODS :            check_eligibility_for_action   IMPORTING agent           TYPE zllm_function_name
                                                        RETURNING VALUE(eligible) TYPE boolean.

ENDCLASS.



CLASS zcl_llm_decision_util IMPLEMENTATION.

  METHOD constructor.

    me->gv_app_id = app_id.
    me->gv_business_function = business_function.

* Set logging up
    me->gcl_log = NEW zcl_llm_log( app_id            = app_id
                                   business_function = business_function ).


* Instantiate global classes.
    DATA(lcl_template_util) = NEW zcl_llm_template_util(   ).


* Call templated methods.
    gv_function_call_uuid = lcl_template_util->get_memory_id( memory_id = 'ZLLM_CODIE_UUID' ).


  ENDMETHOD.


  METHOD action_decision.

* Check the agent's eligibility for taking actions
    DATA(eligible) = me->check_eligibility_for_action( agent = agent ).

    IF eligible = abap_true.

* Check if a decision has been made.
      TRY.
          DATA(lcl_llm_decision) = NEW zcl_llm_util( app_id            = me->gv_app_id
                                                     business_function = me->gv_business_function ).

          DATA(lv_decision_extract_prompt) = |{ lcl_llm_decision->fetch_prompt( iv_prompt_key = 'ACTION_DECISION' )-prompt_text } \n\n { agent_response }|.

          DATA(lv_decision_response) = lcl_llm_decision->call_simple_model( prompt = lv_decision_extract_prompt )-response.

          REPLACE ALL OCCURRENCES OF '```json' IN lv_decision_response WITH ''.
          REPLACE ALL OCCURRENCES OF '```' IN lv_decision_response WITH ''.

* Log decision(s) extracted
          gcl_log->write_general_log( uuid_key    = gv_function_call_uuid
                                      request     = lv_decision_extract_prompt
                                      object_type = 'ACTION_DECISION'
                                      new_value   = lv_decision_response ).


***       ---START OF ACTION FLOW---
          DATA: lt_decision TYPE zllm_decision_tt.

* Convert the decision into an abap structure.
          TRY.

              /goog/cl_json_util=>deserialize_json( EXPORTING iv_json        = lv_decision_response
                                                              iv_pretty_name = /ui2/cl_json=>pretty_mode-extended " Optional: for pretty-printing in logs
                                                    IMPORTING es_data        = lt_decision ).

            CATCH cx_root INTO DATA(lcx_error_with_deserialisaion).
* Log the error that occured during deserialisation of the decision JSON
              gcl_log->write_general_log( uuid_key    = gv_function_call_uuid
                                          request     = lv_decision_extract_prompt
                                          object_type = 'ACTION_DECISION_ERR'
                                          new_value   = |JSON deserialisation error : { lcx_error_with_deserialisaion->get_text( ) }| ).
          ENDTRY.


* Use this to perform the action
          DATA(lcl_llm_mcp_action) = NEW zcl_llm_util( app_id            = 'LLM'
                                                       business_function = 'MCP' ).


          LOOP AT lt_decision ASSIGNING FIELD-SYMBOL(<fs_decision>).

            IF <fs_decision>-decision = 'TRUE'.

* Log action
              gcl_log->write_general_log( uuid_key    = gv_function_call_uuid
                                          request     = <fs_decision>-decision
                                          object_type = 'ACTION_TRIGGER'
                                          new_value   = <fs_decision>-decision_reason ).


* True means a decision has been made and we must now execute the decision.
              lcl_llm_mcp_action->add_function_call_properties( type     = 'ACTION'
                                                                uuid_key = uuid_key ).

* We must pass what we said we would do vs the decision response.
              DATA(lv_decision_prompt) = |<DECISION>\n\n{ <fs_decision>-decision } : { <fs_decision>-decision_reason } \n\n</DECISION>\n\n<QUERY>{ agent_response } \n\n</QUERY>|.

              DATA(action_response) = lcl_llm_mcp_action->call_model( prompt   = lv_decision_prompt
                                                                      sub_call = abap_true )-response.

* We need to ensure that the action was successful.
              IF action_response(5) = 'FALSE'.

* Log action
                gcl_log->write_general_log( uuid_key    = gv_function_call_uuid
                                            request     = 'FAILED'
                                            object_type = 'ACTION_EVENT'
                                            new_value   = |Action event failed. { action_response }| ).

* Remove any reference to an action been taken in our response.
                DATA(lv_action_prompt) = lcl_llm_decision->call_simple_model( prompt = | { lcl_llm_decision->fetch_prompt( iv_prompt_key = 'ACTION_FAILED' )-prompt_text } \n\n { action_response } \n\n { lv_decision_prompt } | )-response.
              ELSE.

* Log action
                gcl_log->write_general_log( uuid_key    = gv_function_call_uuid
                                            request     = 'SUCCESS'
                                            object_type = 'ACTION_EVENT'
                                            new_value   = |Action event succeeded. { action_response }| ).

* Ensure we include reference to an action being taken.
                lv_action_prompt = lcl_llm_decision->call_simple_model( prompt = | { lcl_llm_decision->fetch_prompt( iv_prompt_key = 'ACTION_SUCCESS' )-prompt_text } \n\n { action_response } \n\n { lv_decision_prompt } | )-response.
              ENDIF.

              IF lv_action_prompt IS NOT INITIAL.

                response = lcl_llm_decision->call_simple_model( prompt = |{ lv_action_prompt } \n\n { response }| )-response.

              ENDIF.

            ENDIF.

            CLEAR : lv_decision_prompt.

          ENDLOOP.

        CATCH zcx_llm INTO DATA(lcx_llm).
* At this point in development, do nothing about the exception.
      ENDTRY.

    ELSE.

      response = |No action taken, as agent { agent } is not eligible for actions.|.


* Log action
      gcl_log->write_general_log( uuid_key    = gv_function_call_uuid
                                  request     = 'N.A'
                                  object_type = 'ACTION_EVENT'
                                  new_value   = response ).

    ENDIF.

  ENDMETHOD.


  METHOD check_eligibility_for_action.

*   Check if agent is configured for action
*   Get the active version of the bot
    TRY.

        DATA(ls_version) = NEW zcl_llm_version_control_util( )->get_active_app_version( app_id = me->gv_app_id
                                                                                        business_function = me->gv_business_function ).

      CATCH zcx_llm INTO DATA(lcx_llm).

    ENDTRY.

*   Check for user version & return 'not eligible' if user version is available
    IF ls_version IS NOT INITIAL.

      IF ls_version-user_version IS NOT INITIAL.

        eligible = abap_false.
        RETURN.

      ENDIF.

*     Check for the automation flag for the agent(Automate to be renamed as Action later)
      SELECT SINGLE automate
      FROM zllm_func_declar
      INTO @DATA(lv_automate)
      WHERE app_id = @me->gv_app_id
      AND business_function = @me->gv_business_function
      AND version = @ls_version-active_version
      AND function_name = @agent.

      IF sy-subrc <> 0.

        eligible = abap_false.

      ELSE.

        eligible = lv_automate.

      ENDIF.

*   If no version is available, something is wrong, hence return 'not eligible'
    ELSE.

      eligible = abap_false.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
