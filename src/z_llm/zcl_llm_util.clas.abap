CLASS zcl_llm_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      constructor       IMPORTING model             TYPE string DEFAULT 'gemini-2.0-flash-001'
                                  app_id            TYPE zllm_app_id
                                  business_function TYPE zllm_buss_function_name
                                  temperature       TYPE zllm_temperature OPTIONAL
                                  enable_simple     TYPE boolean DEFAULT abap_true
                        RAISING   zcx_llm,

**********************************************************************
* Add function calling properties to the model instance
**********************************************************************
      add_function_call_properties  IMPORTING supv_for_sub_agents   TYPE zllm_function_name OPTIONAL
                                              parent_for_rag_agents TYPE zllm_function_name OPTIONAL
                                              type                  TYPE zllm_agent_type OPTIONAL
                                              uuid_key              TYPE guid16 OPTIONAL
                                    RAISING   zcx_llm,

**********************************************************************
* Call the LLM model
**********************************************************************
      call_model IMPORTING VALUE(prompt)   TYPE string
                           system_key      TYPE string OPTIONAL
                           sub_call        TYPE boolean DEFAULT abap_false
                 RETURNING VALUE(response) TYPE zllm_model_response
                 RAISING   zcx_llm,

**********************************************************************
* Fetch prompts for a given agent based on the prompt key.
**********************************************************************
      fetch_prompt IMPORTING iv_prompt_key    TYPE zllm_prompt_key
                             prompt_type      TYPE zllm_prompt_type OPTIONAL
                   RETURNING VALUE(rs_prompt) TYPE zllm_prompts
                   RAISING   zcx_llm,

**********************************************************************
* Check if the current user is authorised to be using the current agent.
**********************************************************************
      check_auths  RETURNING VALUE(authorised) TYPE boolean,


**********************************************************************
* To be used to set business parameters of an agent.
*For example, when customer services are to consider a lost in transit.
**********************************************************************
      fetch_agent_param IMPORTING parameter    TYPE char50
                                  type         TYPE zllm_config_type
                        RETURNING VALUE(value) TYPE char255,

**********************************************************************
* Default, global GCS params to be included for every app / business function
* For example, to include files in a GCS bucket
**********************************************************************
      set_gcs_params IMPORTING app_id            TYPE zllm_app_id
                               business_function TYPE zllm_buss_function_name
                               function_name     TYPE zllm_function_name
                     RETURNING VALUE(gcs_config) TYPE zllm_gcs_bucket_config_tt
                     RAISING   zcx_llm,

**********************************************************************
* Set a specific GCS file to be included in the context of a prompt.
* To be used by specific scenarios. This is not included in the global context.
**********************************************************************
      set_gcs_file      IMPORTING mime      TYPE string DEFAULT 'application/PDF'
                                  file_name TYPE zllm_gcs_file_name
                                  simple    TYPE boolean DEFAULT abap_false
                        RAISING   zcx_llm,

**********************************************************************
* Set raw file format base64 as inline data to the prompt.
**********************************************************************
      set_inline_data      IMPORTING mime         TYPE string DEFAULT 'application/PDF'
                                     data         TYPE string
                                     simple_model TYPE char1 OPTIONAL
                           RAISING   zcx_llm,

**********************************************************************
* Set temperature for model
**********************************************************************
      set_temperature IMPORTING temp TYPE  /goog/temperature
                      RAISING   zcx_llm,

**********************************************************************
* Sets a GCS bucket to be used for the entire agent. This is its KB.
**********************************************************************
      set_bucket      IMPORTING bucket TYPE zllm_bucket
                      RAISING   zcx_llm,


**********************************************************************
* Fetches the prompt sequence for the prompt key passed
**********************************************************************
      fetch_prompt_sequence IMPORTING iv_prompt_key             TYPE zllm_prompt_key
                                      iv_version                TYPE zllm_version OPTIONAL
                            RETURNING VALUE(rt_prompt_sequence) TYPE zllm_prompts_seq_tt
                            RAISING   zcx_llm,


**********************************************************************
* Fetch models
**********************************************************************
      get_config RETURNING VALUE(rt_config) TYPE zllm_ai_config_tt
                 RAISING   zcx_llm,


**********************************************************************
* Build a small RAG to assist with function calling
**********************************************************************
      pre_rag IMPORTING prompt          TYPE string
              RETURNING VALUE(response) TYPE string,

**********************************************************************
* Call simple model without function call
**********************************************************************
      call_simple_model  IMPORTING VALUE(prompt)   TYPE string
                                   model           TYPE string DEFAULT 'Gemini-Flash'
                         RETURNING VALUE(response) TYPE zllm_model_response
                         RAISING   zcx_llm,

      rotate_log RAISING zcx_llm,

**********************************************************************
* Trigger function calling for sub agents of a supervisor agent
**********************************************************************
      trigger_sub_agents IMPORTING VALUE(supervisor_agent) TYPE zllm_function_name
                                   VALUE(query)            TYPE string
                         RETURNING VALUE(response)         TYPE zllm_model_response
                         RAISING   zcx_llm,

**********************************************************************
* Perform prompt chaining
* fc = true, with function calling
* Agent, agent name.
**********************************************************************
      execute_prompt_chain IMPORTING agent           TYPE zllm_function_name
                                     fc              TYPE boolean
                                     prompt          TYPE string
                           EXPORTING cv_prompt       TYPE string
                           RETURNING VALUE(response) TYPE zllm_model_response
                           RAISING   zcx_llm,

      close_model,

      assemble_prompts_for_response RETURNING VALUE(prompt) TYPE string
                                    RAISING zcx_llm.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
**********************************************************************
* Used to check if the agent configuration is correct.
**********************************************************************
      check_app_id_and_buss_function IMPORTING app_id            TYPE zllm_app_id
                                               business_function TYPE zllm_buss_function_name
                                     RETURNING VALUE(valid)      TYPE boolean,

**********************************************************************
* Fetch function declaration details
**********************************************************************
      fetch_func_declaration         IMPORTING function_type       TYPE zllm_function_type OPTIONAL
                                               parent_function     TYPE zllm_function_name OPTIONAL
                                               type                TYPE zllm_agent_type OPTIONAL
                                     RETURNING VALUE(function_dec) TYPE zllm_func_dec_details_tt
                                     RAISING   zcx_llm,

**********************************************************************
* call the helper bot to perform actions if possible
**********************************************************************
      call_helper_bot IMPORTING app_id            TYPE zllm_app_id
                                business_function TYPE zllm_buss_function_name
                                prompt            TYPE string
                                request           TYPE string OPTIONAL
                                system_key        TYPE string
                      RETURNING VALUE(response)   TYPE string
                      RAISING   zcx_llm,

**********************************************************************
* Evaluate the bots answer. We may call the original task again if not good
**********************************************************************
      call_evaluate_bot IMPORTING app_id            TYPE zllm_app_id
                                  business_function TYPE zllm_buss_function_name
                                  prompt            TYPE string
                        RETURNING VALUE(response)   TYPE string,

**********************************************************************
* Get the function call function module UUID_KEY
**********************************************************************
      fetch_fc_fm IMPORTING uuid_key               TYPE guid
                  RETURNING VALUE(function_module) TYPE string,

**********************************************************************
* Get sub agents for a given supervisor agent
**********************************************************************
      fetch_sub_agents IMPORTING supv_function   TYPE zllm_function_name
                       RETURNING VALUE(sub_func) TYPE zllm_func_dec_details_tt,

**********************************************************************
* Evaluation counter counts how many times we should apply the evaluation
**********************************************************************
      set_bot_configuration,

**********************************************************************
* Evaluation the LLM response
**********************************************************************
      evaluate_response IMPORTING llm_response    TYPE string
                                  original_query  TYPE string
                        RETURNING VALUE(accepted) TYPE boolean,

**********************************************************************
* Rebuild the function call excluding an agent depending on evaluation bot
**********************************************************************
      prepare_function_call,

**********************************************************************
* Remove HTML formatting from prompt.
**********************************************************************
      strip_html IMPORTING prompt          TYPE string
                 RETURNING VALUE(response) TYPE string,

**********************************************************************
* Handle a blank response from the LLM.
**********************************************************************
      handle_blank_llm_response IMPORTING prompt          TYPE string OPTIONAL
                                RETURNING VALUE(response) TYPE string,

      prompt_clean_up IMPORTING prompt          TYPE string
                      RETURNING VALUE(response) TYPE string,

      safe_guarding IMPORTING prompt          TYPE string
                    RETURNING VALUE(response) TYPE string,

**********************************************************************
* Log all function declarations and parameters.
**********************************************************************
      create_fm_desc_log IMPORTING VALUE(it_functions) TYPE zllm_func_dec_details_tt
                                   VALUE(iv_uuid_key)  TYPE guid16,

**********************************************************************
* Escape the special chars in the json log.
**********************************************************************
      escape_special_chars CHANGING VALUE(cv_string) TYPE string,

**********************************************************************
* Escape the special chars in the json log.
**********************************************************************
      escape_quotes IMPORTING VALUE(iv_string) TYPE string
                    RETURNING VALUE(rv_string) TYPE string,

**********************************************************************
* Do basic checks to ensure no user has submitted a naughty prompt
**********************************************************************
      naughty_prompt IMPORTING VALUE(prompt)   TYPE string
                     RETURNING VALUE(response) TYPE string,


      get_child_mappings IMPORTING parent_agent                   TYPE zllm_function_name
                                   version                        TYPE zllm_version
                                   mapping_type                   TYPE zllm_function_type OPTIONAL
                         RETURNING VALUE(rt_agent_mappings_range) TYPE zllm_function_name_range_tt
                         RAISING   zcx_llm,

      determine_keys_for_func_type  IMPORTING function_type        TYPE zllm_function_type
                                    EXPORTING ev_app_id            TYPE zllm_app_id
                                              ev_business_function TYPE zllm_buss_function_name
                                              ev_version           TYPE zllm_version.



    DATA: go_model              TYPE REF TO /goog/cl_generative_model,
          go_simple_model       TYPE REF TO /goog/cl_generative_model,
          app_id                TYPE zllm_app_id,
          version               TYPE zllm_version,
          user_version          TYPE boolean,
          business_function     TYPE zllm_buss_function_name,
          gcl_log               TYPE REF TO zcl_llm_log,
          gcl_performance_trace TYPE REF TO zcl_llm_performance_util,
          function_call_uuid    TYPE guid16,
          gs_exclude_fc_range   TYPE zllm_exclude_fc,
          gt_exclude_fc_range   TYPE zllm_exclude_fc_tt,
          gs_bot_configuration  TYPE zllm_bots,
          gt_log_config         TYPE TABLE OF zllm_config.

ENDCLASS.



CLASS zcl_llm_util IMPLEMENTATION.


  METHOD constructor.

*** Note : The model passed as parameter is applied only for the main model instantiation, NOT SIMPLE MODEL
***        Simple model will use the default value set to the parameter for now(Might need to enhance later)

    DATA: lv_model            TYPE string,
          lv_model_key        TYPE string,
          lv_default_model_id TYPE string VALUE 'gemini-2.0-flash-001'.


* Ensure correct app ID
    IF me->check_app_id_and_buss_function( app_id = app_id
                                           business_function = business_function ) = abap_true.

* Set application id
      me->app_id = app_id.

* Set business function id
      me->business_function = business_function.

* Set logging up
      me->gcl_log = NEW zcl_llm_log( app_id            = app_id
                                     business_function = business_function ).

* Set bot configuration
      me->set_bot_configuration(  ).


* Get active version.
      DATA(lcl_llm_version_controller) = NEW zcl_llm_version_control_util(  ).
      DATA(versions) = lcl_llm_version_controller->get_active_app_version( app_id            = me->app_id
                                                                           business_function = me->business_function ).

* User version will always override current version.
      IF versions-user_version IS NOT INITIAL.
        version = versions-user_version.
        user_version = abap_true.
      ELSE.
        version = versions-active_version.
      ENDIF.

*     Check if model ID has been passed as input during instantiation. The way we can understand that is
*     by comparing the the input with the default model maintained as fallback
      IF model <> lv_default_model_id.

        lv_model = model.


*     If the input and default models are the same, we understand that no input has been passed, hence we go ahead
*     with looking up the model maintained for the bot (app ID and business function) globally
      ELSEIF model = lv_default_model_id.

* Set the user model if user overwrite active
        IF versions-user_model IS NOT INITIAL.
          lv_model = versions-user_model.
        ELSE.
          lv_model = versions-active_model. "If no user model set the active model for this version
        ENDIF.

      ENDIF.

*     Final check to see if the model ID is populated. If not, pass the default model ID and carry on to fetch the
*     respective model key
      IF lv_model IS INITIAL.

        lv_model = lv_default_model_id.

      ENDIF.

* Get the models configured from Google's config table
      DATA(lt_models) = me->get_config( ).

      TRY.
          lv_model_key = lt_models[ model_id = lv_model ]-model_key.
        CATCH cx_sy_itab_line_not_found.
* If not found do nothing. Default models is already set up.
      ENDTRY.


      TRY.
* Initialise the class
          go_model = NEW /goog/cl_generative_model( iv_model_key = CONV /goog/model_key( lv_model_key ) ).

* If the calling program wishes to utilise the simple model call in addition to normal model, create a second instance.
          IF enable_simple = abap_true.
            go_simple_model = NEW /goog/cl_generative_model( iv_model_key = CONV /goog/model_key( 'Gemini-Flash2.0' ) ).
          ENDIF.


* Set the configured temp for this agent. It can be overriden as the temperature method is public.
          IF temperature IS INITIAL.
            me->set_temperature( temp = CONV /goog/temperature( me->fetch_agent_param( type = 'AGENT'
                                                                                       parameter = 'TEMPERATURE' ) ) ).
          ELSE.
            me->set_temperature( temp =  CONV /goog/temperature( temperature ) ).
          ENDIF.

        CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).

          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>sdk_exception
              msg1   = lo_cx_sdk->get_text( ).

      ENDTRY.

    ELSE.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>invalid_app_id
          msg1   = CONV string( app_id ).
    ENDIF.


  ENDMETHOD.


  METHOD close_model.
    go_model->close(  ).
  ENDMETHOD.

  METHOD execute_prompt_chain.

*** Global rules:
*   -- 1. Function calling can be enabled as part of prompt chaining, however, only RAG agents
*         can be part of function calling at this stage

    IF agent IS NOT INITIAL AND
       prompt IS NOT INITIAL.

      DATA(lcl_llm_prompt_util) = NEW zcl_llm_prompt_util( app_id            = me->app_id
                                                           business_function = me->business_function ).

*     Get the prompt chain for the agent.
      DATA(lt_prompt_chain) = lcl_llm_prompt_util->get_prompt_chain( prompt_key = CONV zllm_prompt_key( agent )
                                                                     version    = me->version ).


      IF fc = abap_true.

        TRY.
            DATA(lcl_llm_rag_util) = NEW zcl_llm_agents_util( app_id            = me->app_id
                                                              business_function = me->business_function ).

            DATA(lt_rag_agents) = lcl_llm_rag_util->get_rag_agents( agent   = agent
                                                                    type    = 'MCP'
                                                                    version = me->version ).


          CATCH zcx_llm INTO DATA(lcx_llm).
* Its ok if no RAG agents are assigned.
        ENDTRY.


*       Set up Gemini for function calling & loop through the prompts of the prompt chain to
*       gather responses
        TRY.

            DATA: lv_counter TYPE i.

* Setup performance trace.
            DATA(lcl_performance_trace) = NEW zcl_llm_performance_util( app_id            = me->app_id
                                                                        business_function = me->business_function
                                                                        uuid_key          = me->function_call_uuid ).

* Allow skipping of a function call true / false.
            DATA: lv_skip TYPE boolean.

            LOOP AT lt_prompt_chain ASSIGNING FIELD-SYMBOL(<fs_prompt>) .


              DATA(lref_prompt_chain_model_call) = NEW zcl_llm_util( app_id            = me->app_id
                                                                     business_function = me->business_function
                                                                     model             = CONV string( <fs_prompt>-model_id ) ).

              IF lt_rag_agents IS NOT INITIAL.
                lref_prompt_chain_model_call->add_function_call_properties( parent_for_rag_agents = agent ).
              ENDIF.

* Begin performance trace.
              lcl_performance_trace->start_perforamnce_trace( type = | { <fs_prompt>-prompt_key } - { <fs_prompt>-prompt_chain }| ).

              IF <fs_prompt>-final_link = abap_true.
                cv_prompt = <fs_prompt>-prompt_text.
                lref_prompt_chain_model_call->close_model(  ).
              ELSE.

                lv_counter = lv_counter + 1.

                DATA(query) = |<STATEMENT>\n\n{ <fs_prompt>-prompt_text } \n\n</STATEMENT>| &
                              |\n\n<QUERY>\n{ prompt }\n\n</QUERY>|.

* If this is a think link, include previous responses.
                IF <fs_prompt>-think_link = abap_true.
                  query = |{ query } \n\n<CONVERSATION HISTORY>\n\n { response-response } \n\n</CONVERSATION HISTORY>|.
                ENDIF.


                IF lv_skip = abap_false.
*                 Get the Knowledge base files for the agent
                  DATA(gcs_util) = NEW zcl_llm_gcs_util( app_id            = me->app_id
                                                         business_function = me->business_function ).
                  TRY.
                      DATA(lt_gcs) = gcs_util->get_gcs_map( EXPORTING agent = CONV #( agent )  ).

                      LOOP AT lt_gcs ASSIGNING FIELD-SYMBOL(<fs_gcs>) WHERE gcs_uri is NOT INITIAL AND
                                                                            mime    is NOT INITIAL.
                        TRY.
*                           Set the Knowledge base files to the model
                            lref_prompt_chain_model_call->set_gcs_file( mime      = <fs_gcs>-mime
                                                                        file_name = <fs_gcs>-gcs_uri
                                                                        simple    = abap_false ).
                          CATCH zcx_llm.
                        ENDTRY.
                      ENDLOOP.

                    CATCH zcx_llm.
                  ENDTRY.
*
                  DATA(partial_response) = lref_prompt_chain_model_call->call_model( prompt = query
                                                                                     sub_call = abap_true )-response.
                ENDIF.

                lv_skip = abap_false.

                IF partial_response IS NOT INITIAL.
* Build response
                  IF partial_response CP 'SKIP'.

                    lv_skip = abap_true.

                  ENDIF.


                  IF partial_response CP 'TERMINATE'.

                    RETURN.

                  ENDIF.



                  IF <fs_prompt>-think_link = abap_true.
* A think link will replace any previous history
                    response-response = |{ response-response }\n\n<AI RESPONSE { lv_counter }>\n\n{ partial_response } \n\n</AI RESPONSE { lv_counter }>|.

                  ELSE.
                    response-response = |\n<AI RESPONSE { lv_counter }>\n\n{ partial_response } \n\n</AI RESPONSE { lv_counter }>| &
                                        |\n\n\n{ response-response }|.
                  ENDIF.


* Wrap the response around a decision indictor.
                  IF <fs_prompt>-decision_link = abap_true.
                    response-response = |<DECISION>\n{ response-response }\n\n<AI RESPONSE { lv_counter }>\n\n{ partial_response } \n\n</AI RESPONSE { lv_counter }> \n</DECISION>|.
                  ENDIF.

                ENDIF.
              ENDIF.
              CLEAR: query,
                     partial_response.


* End performance trace.
              lcl_performance_trace->end_performance_trace( ).
            ENDLOOP.


          CATCH zcx_llm INTO lcx_llm.
            RAISE EXCEPTION TYPE zcx_llm
              EXPORTING
                textid = lcx_llm->if_t100_message~t100key
                msg1   = lcx_llm->msg1
                msg2   = lcx_llm->msg2.

        ENDTRY.

      ENDIF.

    ENDIF.




  ENDMETHOD.


  METHOD rotate_log.

* Load log configuration.
    SELECT param, type, value
    FROM zllm_config
    INTO CORRESPONDING FIELDS OF TABLE @me->gt_log_config
    WHERE type = 'LOG'
    AND app_id = @me->app_id
    AND buss_function = @me->business_function.

* If subrc 4 then this app / business function does not have a log configuration. Do nothing.
    IF sy-subrc = 4.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>invalid_config_type.

    ENDIF.


    DATA: ls_log_config TYPE zllm_log_types,
          lt_log_config TYPE zllm_log_types_tt.


* Build range of log types to rotate
    LOOP AT me->gt_log_config ASSIGNING FIELD-SYMBOL(<fs_log>).

      ls_log_config-sign = 'I'.
      ls_log_config-option = 'EQ'.
      ls_log_config-low = <fs_log>-type.

      APPEND ls_log_config TO lt_log_config.

    ENDLOOP.


* Get the start and end date for the log rotation
    TRY.
        DATA(lv_start_date) = VALUE datum( gt_log_config[ param = |start_date| ] ).
        DATA(lv_end_date) = VALUE datum( gt_log_config[ param = |start_date| ] ).

      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>invalid_config_type.

    ENDTRY.


* Rotate the log.
    DATA(lcl_llm_crud) = NEW zcl_llm_crud_control_util( app_id            = me->app_id
                                                        business_function = me->business_function ).


    lcl_llm_crud->roate_log( start_date = lv_start_date
                             end_date   = lv_end_date
                             type       = lt_log_config ).

  ENDMETHOD.

  METHOD set_bot_configuration.

    SELECT SINGLE limited_rag, evaluation_count, eval_bot_enabled, helper_bot_enabled,
                  eval_limit, strip_html, enable_kb, gcs_kb_uri, clean_prompt, safe_gaurd_enabled
    FROM zllm_bots
    INTO CORRESPONDING FIELDS OF @me->gs_bot_configuration
    WHERE app_id = @me->app_id
    AND business_function = @me->business_function.

* If this has not been configured, set default as 1.
    IF me->gs_bot_configuration-evaluation_count = 0.
      me->gs_bot_configuration-evaluation_count = 1.
    ENDIF.

  ENDMETHOD.


  METHOD pre_rag.
* At present we can only handle limited rag for scenarios where we have hybris order.
    TRY.
        DATA(lcl_llm_util) = NEW zcl_llm_util( app_id            = 'HELPER'
                                               business_function = 'HELPER' ).


* Set the global context of this FM.
        DATA(ls_prompt) = lcl_llm_util->fetch_prompt( iv_prompt_key = 'HYBRISORDER' ).


        DATA(simple_response) = lcl_llm_util->call_simple_model( prompt = |{ ls_prompt-prompt_text } \n\n { prompt }| )-response.

        REPLACE ALL OCCURRENCES OF '\n' IN simple_response WITH ''.

* If the response is more than 13 then this is not a hybris order.
        TRY.
            IF strlen( simple_response ) <= 13.
              IF simple_response(2) = 'PR'.
                simple_response = simple_response(13).
              ELSEIF simple_response(2) = 'UK'.
                simple_response = simple_response(11).
              ELSEIF simple_response(2) = 'IE'.
                simple_response = simple_response(11).
              ELSE.
* We must have halluncinated
                CLEAR: simple_response.
                RETURN.
              ENDIF.
            ENDIF.

          CATCH cx_sy_range_out_of_bounds INTO DATA(lcx_out_of_bounds).
* Bad data in our response. Clear and return.
            CLEAR: simple_response.
            RETURN.
        ENDTRY.


        DATA(lcl_llm) = NEW zcl_cs_hub_llm_util( hybris_order = CONV bstnk( simple_response ) ).
        response = lcl_llm->llm_limited_rag(  ).

      CATCH zcx_llm.
* Do not fall over because limited rag had an error.
    ENDTRY.

  ENDMETHOD.


  METHOD fetch_fc_fm.

* Get the function that was called.
    SELECT SINGLE object_key
    FROM zllm_log
    INTO @function_module
    WHERE uuid_key = @me->function_call_uuid
    AND object_type = 'FUNCTION_CALL'.

  ENDMETHOD.


  METHOD set_temperature.
**********************************************************************
* Use method to increase / decrease the model temperature / creativity.
**********************************************************************

    go_model->set_generation_config( iv_temperature = temp ).

  ENDMETHOD.


  METHOD set_gcs_params.
**********************************************************************
* Method to be used to set global GCS information. Such as buckets or files to be used for an agent.
* This can be anything, such as a bucket, or file.
**********************************************************************

    SELECT app_id, buss_function, function_name, bucket, file_name, mime
    FROM zllm_gcs_param
    INTO CORRESPONDING FIELDS OF TABLE @gcs_config
    WHERE app_id = @me->app_id
    AND buss_function = @me->business_function
    AND function_name = @function_name.


* Set required GCS config
    LOOP AT gcs_config ASSIGNING FIELD-SYMBOL(<fs_gcs_config>).

* Set GCS file if specified.
      IF <fs_gcs_config>-file_name IS NOT INITIAL.

        IF <fs_gcs_config>-mime IS INITIAL.
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>invalid_gcs_config
              msg1   = |{ <fs_gcs_config>-file_name } - MIME missimg|.
        ENDIF.


* Set the GCS file.
        TRY.
            me->set_gcs_file( file_name = <fs_gcs_config>-file_name
                              mime      = <fs_gcs_config>-mime ).

          CATCH zcx_llm INTO DATA(lcx_llm).

            RAISE EXCEPTION TYPE zcx_llm
              EXPORTING
                textid = lcx_llm->if_t100_message~t100key
                msg1   = lcx_llm->msg1.

        ENDTRY.
      ENDIF.


* Set bucket if specified.
      IF <fs_gcs_config>-bucket IS NOT INITIAL.

        TRY.

            me->set_bucket( bucket = <fs_gcs_config>-bucket ).

          CATCH zcx_llm INTO lcx_llm.

            RAISE EXCEPTION TYPE zcx_llm
              EXPORTING
                textid = lcx_llm->if_t100_message~t100key
                msg1   = lcx_llm->msg1.

        ENDTRY.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD set_bucket.
**********************************************************************
* Method to be used to set only a GCS bucket to be used for a given agent
**********************************************************************
    TRY.
        go_model->set_files_from_gcs( iv_storage_bucket_name = bucket ).

      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>invalid_gcs_bucket
            msg1   = bucket.
    ENDTRY.



  ENDMETHOD.


  METHOD set_inline_data.
**********************************************************************
* Method to be used to set raw file data, such as base64 as inline content to the context window.
**********************************************************************

    TRY.

        IF simple_model = abap_true.

          go_simple_model->set_inline_data( iv_mime_type = mime
                                            iv_data      = data ).
        ELSE.

          go_model->set_inline_data( iv_mime_type = mime
                                     iv_data      = data ).

        ENDIF.

      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).

* Inline data maybe base64. Do not report this back up the stack.
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>invalid_inline_data.
    ENDTRY.

  ENDMETHOD.


  METHOD set_gcs_file.
**********************************************************************
* Method to be used to set a specific file for a given agent
**********************************************************************

    TRY.
        IF simple = abap_true.
          go_simple_model->set_file_data( iv_mime_type = mime
                                          iv_file_uri  = file_name ).

        ELSE.
          go_model->set_file_data( iv_mime_type = mime
                                   iv_file_uri  = file_name ).

        ENDIF.
      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>invalid_gcs_file
            msg1   = file_name.

    ENDTRY.


  ENDMETHOD.


  METHOD fetch_agent_param.
**********************************************************************
* Use this method to load default agent parameters
**********************************************************************
    TRY.
        DATA(lcl_config) = NEW zcl_llm_config_util( app_id            = me->app_id
                                                    business_function = me->business_function
                                                    type              = type ).

        DATA(ls_config) = lcl_config->get_config( parameter = 'TEMPERATURE' ).

        value = ls_config-value.
      CATCH zcx_llm.
    ENDTRY.

  ENDMETHOD.


  METHOD check_auths.

    SELECT SINGLE *
        FROM zllm_auths
        INTO @DATA(auths)
        WHERE user_name = @sy-uname.

    IF sy-subrc = 0.
      authorised = abap_true.
    ELSE.
      authorised = abap_false.
    ENDIF.


  ENDMETHOD.



  METHOD check_app_id_and_buss_function.

    valid =  abap_true.

  ENDMETHOD.


  METHOD fetch_sub_agents.

*    DATA: ls_param_details TYPE zllm_func_dec_param_details,
*          ls_func_details  TYPE zllm_func_dec_details.
*
*
** Get the function declarations defined for the app id.
*    SELECT *
*    FROM zllm_sub_func
*    INNER JOIN zllm_func_declar ON zllm_func_declar~sub_function_id = zllm_sub_func~sub_function_id
*    INNER JOIN zllm_sub_func_p ON zllm_sub_func~app_id = zllm_sub_func_p~app_id AND
*                                   zllm_sub_func~business_function = zllm_sub_func_p~business_function AND
*                                   zllm_sub_func~function_parameters_id = zllm_sub_func_p~parameters_id AND
*                                   zllm_sub_func~version = zllm_sub_func_p~version AND
*                                   zllm_sub_func~deletion_indicator <> @abap_true
*
*    INTO TABLE @DATA(lt_function_dec)
*    WHERE  zllm_sub_func~app_id = @me->app_id
*    AND zllm_sub_func~business_function = @me->business_function
*    AND zllm_sub_func~version = @me->version
*    AND zllm_sub_func~deletion_indicator <> @abap_true
*    AND zllm_func_declar~function_name = @supv_function
*    AND zllm_func_declar~type = 'SUB'.
*
*
** Build function declaration output
*    LOOP AT lt_function_dec ASSIGNING FIELD-SYMBOL(<fs_function_dec>).
*
** For first pass, populate the function module details. Do this once
*      IF <fs_function_dec>-zllm_sub_func-function_name <> ls_func_details-fm.
*
** Dont do this if this is the first pass.
*        IF sy-tabix <> 1.
*          APPEND ls_func_details TO sub_func.
*          CLEAR: ls_func_details.
*        ENDIF.
*
*        ls_func_details-fm = <fs_function_dec>-zllm_sub_func-function_name.
*        ls_func_details-fm_description = <fs_function_dec>-zllm_sub_func-function_description.
*      ENDIF.
*
** A function module may not have parameters.
*      IF <fs_function_dec>-zllm_sub_func_p IS NOT INITIAL.
*
*        ls_param_details-param_name = <fs_function_dec>-zllm_sub_func_p-parameter_name.
*        ls_param_details-param_description = <fs_function_dec>-zllm_sub_func_p-parameter_description.
*        ls_param_details-param_type = <fs_function_dec>-zllm_sub_func_p-parameter_type.
*        ls_param_details-is_required = <fs_function_dec>-zllm_sub_func_p-is_required.
*
*
*      ELSE.
** Currently the ABAP SDK does not allow us to specify a FM without a parameter. Raise an exception if the config is wrong here.
*        RAISE EXCEPTION TYPE zcx_llm
*          EXPORTING
*            textid = zcx_llm=>invalid_func_call_param
*            msg1   = CONV string( <fs_function_dec>-zllm_sub_func-function_name ).
*      ENDIF.
*
*
*      APPEND ls_param_details TO ls_func_details-param_details.
*
*      CLEAR: ls_param_details,
*             <fs_function_dec>-zllm_sub_func_p.
*
*    ENDLOOP.
*
*
** Catch the last entry.
*    APPEND ls_func_details TO sub_func.


  ENDMETHOD.


  METHOD fetch_func_declaration.

*   --RULES:
*   1. This method operates based on the function-type passed, and the list of values it can hold are
*      - SUPERVISOR/''
*      - SUB
*      - RAG
*   2. This will bypass the global attributes App ID, Business function and version, if need be!


    DATA: ls_param_details TYPE zllm_func_dec_param_details,
          ls_func_details  TYPE zllm_func_dec_details.


*   Determine App ID, business function and version based on the function type
    me->determine_keys_for_func_type( EXPORTING function_type        = function_type
                                      IMPORTING ev_app_id            = DATA(lv_app_id)
                                                ev_business_function = DATA(lv_business_function)
                                                ev_version           = DATA(lv_version) ).

    TRY.

        IF function_type IS NOT INITIAL.
          DATA(lt_agent_mappings_range) = me->get_child_mappings( parent_agent = parent_function
                                                                  version      = me->version "lv_version
                                                                  mapping_type = function_type ).

        ENDIF.

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2.

    ENDTRY.


    IF lt_agent_mappings_range IS NOT INITIAL.

*     Get the function declarations for the child agents passed
      SELECT *
      FROM zllm_func_declar
      INNER JOIN zllm_func_parame ON zllm_func_declar~app_id = zllm_func_parame~app_id AND
                                     zllm_func_declar~business_function = zllm_func_parame~business_function AND
                                     zllm_func_declar~function_parameters_id = zllm_func_parame~parameters_id AND
                                     zllm_func_declar~version = zllm_func_parame~version AND
                                     zllm_func_parame~deletion_indicator <> @abap_true
      INNER JOIN zllm_func_index ON zllm_func_index~app_id = zllm_func_declar~app_id AND
                                    zllm_func_index~business_function = zllm_func_declar~business_function AND
                                    zllm_func_index~function_name = zllm_func_declar~function_name

      WHERE  zllm_func_declar~app_id = @lv_app_id
      AND zllm_func_declar~business_function = @lv_business_function
      AND zllm_func_declar~version = @lv_version
      AND zllm_func_declar~deletion_indicator <> @abap_true
      AND zllm_func_declar~function_name IN @gt_exclude_fc_range
      AND zllm_func_declar~type = @function_type                    "Could be SUPERVISOR/SUB/RAG
      AND zllm_func_declar~function_name IN @lt_agent_mappings_range
      ORDER BY function_index ASCENDING            "Fetch results in ascending order of function/agent index
      INTO TABLE @DATA(lt_function_dec).

    ELSE.

*   Get the function declarations defined for the app id and business function
      SELECT *
      FROM zllm_func_declar
      INNER JOIN zllm_func_parame ON zllm_func_declar~app_id = zllm_func_parame~app_id AND
                                     zllm_func_declar~business_function = zllm_func_parame~business_function AND
                                     zllm_func_declar~function_parameters_id = zllm_func_parame~parameters_id AND
                                     zllm_func_declar~version = zllm_func_parame~version AND
                                     zllm_func_parame~deletion_indicator <> @abap_true
      INNER JOIN zllm_func_index ON zllm_func_index~app_id = zllm_func_declar~app_id AND
                                    zllm_func_index~business_function = zllm_func_declar~business_function AND
                                    zllm_func_index~function_name = zllm_func_declar~function_name

      INTO TABLE @lt_function_dec
      WHERE  zllm_func_declar~app_id = @lv_app_id
      AND zllm_func_declar~business_function = @lv_business_function
      AND zllm_func_declar~version = @lv_version
      AND zllm_func_declar~deletion_indicator <> @abap_true
      AND zllm_func_declar~function_name IN @gt_exclude_fc_range
      AND zllm_func_declar~type IN ( '','SUPERVISOR' )                    "Could be SUPERVISOR/SUB/RAG
      ORDER BY zllm_func_index~function_index ASCENDING.            "Fetch results in ascending order of function/agent index

    ENDIF.

* Build function declaration output
    LOOP AT lt_function_dec ASSIGNING FIELD-SYMBOL(<fs_function_dec>).

* For first pass, populate the function module details. Do this once
      IF <fs_function_dec>-zllm_func_declar-function_name <> ls_func_details-fm.

* Dont do this if this is the first pass.
        IF sy-tabix <> 1.
          APPEND ls_func_details TO function_dec.
          CLEAR: ls_func_details.
        ENDIF.

        ls_func_details-fm = <fs_function_dec>-zllm_func_declar-function_name.
        ls_func_details-fm_description = <fs_function_dec>-zllm_func_declar-function_description.
      ENDIF.

* A function module may not have parameters.
      IF <fs_function_dec>-zllm_func_parame IS NOT INITIAL.

        ls_param_details-param_name = <fs_function_dec>-zllm_func_parame-parameter_name.
        ls_param_details-param_description = <fs_function_dec>-zllm_func_parame-parameter_description.
        ls_param_details-param_type = <fs_function_dec>-zllm_func_parame-parameter_type.
        ls_param_details-is_required = <fs_function_dec>-zllm_func_parame-is_required.


      ELSE.
* Currently the ABAP SDK does not allow us to specify a FM without a parameter. Raise an exception if the config is wrong here.
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>invalid_func_call_param
            msg1   = CONV string( <fs_function_dec>-zllm_func_declar-function_name ).
      ENDIF.


      APPEND ls_param_details TO ls_func_details-param_details.

      CLEAR: ls_param_details,
             <fs_function_dec>-zllm_func_parame.

    ENDLOOP.


* Catch the last entry.
    APPEND ls_func_details TO function_dec.

  ENDMETHOD.


  METHOD call_helper_bot.

*   If a user version is enabled, do no let the helper bot help
    IF me->user_version = abap_true.
      RETURN.
    ENDIF.


    IF system_key IS INITIAL OR system_key = ' '.
* If the system key is a blank value return as we cannot do anything
      RETURN.
    ENDIF.

*   Check to avoid proceeding further if the agent that handled the ticket is a supervisor agent, as it
*   has its own action logic
    SELECT SINGLE object_key
    FROM zllm_log
    INTO @DATA(lv_agent)
    WHERE uuid_key = @me->function_call_uuid
    AND object_type = 'FUNCTION_CALL'.

    IF lv_agent IS NOT INITIAL.

      SELECT SINGLE type, legacy
        FROM zllm_func_declar
        INTO @DATA(ls_function_type)
        WHERE app_id = @me->app_id
          AND business_function = @me->business_function
          AND version = @me->version
          AND function_name = @lv_agent.

*     If the agent is NOT a legacy agent, then do no proceed with this helper logic
      IF ls_function_type-legacy = abap_false.
        RETURN.
      ENDIF.

    ENDIF.

*   This check is for action agents in the new framework
    SELECT SINGLE uuid_key
      FROM zllm_log
      INTO @DATA(actioned_through_new_fw)
      WHERE object_key = @system_key
        AND object_type IN ( 'ACTIONED', 'ACTIONED-RETURN' ).

    IF actioned_through_new_fw IS NOT INITIAL.
      response = 'X'.
      RETURN.
    ENDIF.


    SELECT SINGLE helper_app_id, helper_business_function
    FROM zllm_bots
    INTO @DATA(helper_bot)
    WHERE app_id = @app_id
    AND business_function = @business_function.

    IF sy-subrc = 4.
* No helper bot maintained.
      RETURN.
    ENDIF.

* If its the helper class that has called this, do nothing, otherwise this becomes recursive.
    IF app_id = helper_bot-helper_app_id.
      RETURN.
    ENDIF.

* If helper bot is configured.
    IF helper_bot-helper_app_id IS NOT INITIAL.

* We must undestand what has just occured and determine if an action is configured.

      TRY.

          DATA: lv_actioned TYPE xfeld.

          CALL FUNCTION 'Z_LLM_EMAIL_ACTION'
            EXPORTING
              prompt    = prompt
              request   = request
              fd_ticket = CONV zcs_freshdesk_ticket_id( system_key )
              uuid_key  = me->function_call_uuid
            IMPORTING
              actioned  = lv_actioned.

          response = lv_actioned.

        CATCH zcx_llm INTO DATA(lcx_llm).
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD call_evaluate_bot.


* For now, only enable helper bot for developers.
    IF sy-uname = 'ASEXTON' OR
       sy-uname = 'DRAMACHANDRA' OR
       sy-uname = 'MBONDAROWICZ' OR
       sy-uname = 'OPUGLISI' OR
       sy-uname = 'NFARRELL'.


      SELECT SINGLE eval_app_id, eval_business_function
      FROM zllm_bots
      INTO @DATA(helper_bot)
      WHERE app_id = @app_id
      AND business_function = @business_function.

* If its the helper class that has called this, do nothing, otherwise this becomes recursive.
      IF app_id = helper_bot-eval_app_id.
        RETURN.
      ENDIF.

      DATA(function_call) = me->fetch_fc_fm( uuid_key = me->function_call_uuid ).

* If helper bot is configured.
      IF helper_bot-eval_app_id IS NOT INITIAL.

        SELECT SINGLE new_value
        FROM zllm_log
        INTO @DATA(rag_data)
        WHERE object_type = 'RAG_DATA'
        AND object_key = @function_call
        AND uuid_key = @me->function_call_uuid
        AND app_id = @app_id
        AND buss_function = @business_function.


        TRY.
            DATA(zcl_llm_model) = NEW zcl_llm_util( app_id            = helper_bot-eval_app_id
                                                    business_function = helper_bot-eval_business_function ).


            zcl_llm_model->add_function_call_properties(  ).
            DATA(eval_response) = zcl_llm_model->call_model( prompt = |<SYSTEM DATA>\n { rag_data }\n\n\n { prompt }| ).
            eval_response-response = response.

          CATCH zcx_llm INTO DATA(lcx_llm).
            RAISE EXCEPTION TYPE zcx_llm
              EXPORTING
                textid = lcx_llm->if_t100_message~t100key
                msg1   = lcx_llm->msg1
                msg2   = lcx_llm->msg2
                msg3   = lcx_llm->msg3.
        ENDTRY.
      ENDIF.
    ELSE.

      response = abap_false.

    ENDIF.



  ENDMETHOD.


  METHOD call_simple_model.

* Cannot pass a blank prompt, especially when model armour is enabled.
    IF prompt IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        DATA(lo_response) = go_simple_model->generate_content( iv_prompt_text = prompt ).

        response-response = lo_response->get_text( ).



      CATCH  /goog/cx_sdk INTO DATA(lo_cx_sdk).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>simple_model_fail
            msg1   = lo_cx_sdk->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD evaluate_response.

* If the evaluation bot is disabled, return true to continue.
    IF me->gs_bot_configuration-eval_bot_enabled = abap_false.
      accepted = abap_true.
      RETURN.
    ENDIF.

* First, if the response is blank, its an instant rejection.
    IF llm_response IS INITIAL.
      accepted = abap_false.
      RETURN.
    ENDIF.

* Perform evaluation of answer
    DATA(eval_response) = me->call_evaluate_bot( app_id            = app_id
                                                 business_function = business_function
                                                 prompt            = |INPUTQUERY:\n\n { original_query } \n\n RESPONSE:\n\n{ llm_response  }\n\n| ).

* Get the evaluation result.
    SPLIT eval_response AT '</SCORE>' INTO DATA(lv_score) DATA(lv_rest).
    SPLIT lv_score AT '<SCORE>' INTO DATA(lv_tag) DATA(lv_num_score).


* If we have dropped below the evaluation limit, return unaccepted
    IF lv_score < me->gs_bot_configuration-eval_limit.
      accepted = abap_false.
    ELSE.
      accepted = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD prepare_function_call.


* Add the currently selected agent to the exclusion list.
    me->gs_exclude_fc_range-sign = 'E'.
    me->gs_exclude_fc_range-option = 'EQ'.

* Get the function that was called.
    DATA(function_call) = me->fetch_fc_fm( uuid_key = me->function_call_uuid ).

* If nothing found, exit loop as we cannot change anything.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    me->gs_exclude_fc_range-low = function_call.

  ENDMETHOD.


  METHOD strip_html.

    TRY.
        DATA(ls_removehtml) = me->fetch_prompt( iv_prompt_key = 'REMOVEHTML' ).
        ls_removehtml-prompt_text = |{ ls_removehtml-prompt_text } \n\n { prompt }|.

        response = me->call_simple_model( prompt = ls_removehtml-prompt_text )-response.

      CATCH zcx_llm INTO DATA(lcx_llm).
* If we failed to remove HTML do not error
    ENDTRY.
  ENDMETHOD.


  METHOD handle_blank_llm_response.

    TRY.
        DATA(ls_prompt) = me->fetch_prompt( iv_prompt_key = 'NORESPONSE' ).


* if a prompt is provided, maybe we could try to explain better why we have no answer.
        IF prompt IS NOT INITIAL.
          ls_prompt-prompt_text = |{ ls_prompt-prompt_text } \n\n { prompt }|.
        ENDIF.

        response = me->call_simple_model( prompt = ls_prompt-prompt_text )-response.

      CATCH zcx_llm INTO DATA(lcx_llm).
    ENDTRY.
  ENDMETHOD.



  METHOD prompt_clean_up.

    TRY.
        DATA(ls_prompt) = me->fetch_prompt( iv_prompt_key = 'CLEANUP' ).


* if a prompt is provided, maybe we could try to explain better why we have no answer.
        IF prompt IS NOT INITIAL.
          ls_prompt-prompt_text = |{ ls_prompt-prompt_text } \n\n { prompt }|.
        ENDIF.

        response = me->call_simple_model( prompt = ls_prompt-prompt_text )-response.

      CATCH zcx_llm INTO DATA(lcx_llm).
    ENDTRY.


  ENDMETHOD.


  METHOD safe_guarding.

    TRY.
        DATA(ls_prompt) = me->fetch_prompt( iv_prompt_key = 'SAFEGAURD' ).


* if a prompt is provided, maybe we could try to explain better why we have no answer.
        IF prompt IS NOT INITIAL.
          ls_prompt-prompt_text = |{ ls_prompt-prompt_text } \n\n { prompt }|.
        ENDIF.

        response = me->call_simple_model( prompt = ls_prompt-prompt_text )-response.

      CATCH zcx_llm INTO DATA(lcx_llm).
    ENDTRY.

  ENDMETHOD.


  METHOD naughty_prompt.

    TRY.
        DATA(ls_prompt) = me->fetch_prompt( iv_prompt_key = 'NAUGHTYPROMPT' ).


* if a prompt is provided, maybe we could try to explain better why we have no answer.
        IF prompt IS NOT INITIAL.
          ls_prompt-prompt_text = |{ ls_prompt-prompt_text } \n\n { prompt }|.
        ENDIF.

        response = me->call_simple_model( prompt = ls_prompt-prompt_text )-response.

      CATCH zcx_llm INTO DATA(lcx_llm).
    ENDTRY.

  ENDMETHOD.

  METHOD call_model.


* If a UUID has already been exported to memory, use the exported UUID. If no exported UUID exists, generate a new one.
    DATA(lcl_template_util) = NEW zcl_llm_template_util( ).


**********************************************************************
* Instantiate uuid_key for logging.
**********************************************************************
    IF me->function_call_uuid IS INITIAL.

* Call templated methods.
      DATA(lv_uuid) = lcl_template_util->get_memory_id( memory_id = 'ZLLM_CODIE_UUID' ).

      IF lv_uuid IS NOT INITIAL.
        me->function_call_uuid = lv_uuid.
      ELSE.
        me->function_call_uuid = me->gcl_log->generate_uuid( ).
      ENDIF.
    ENDIF.

* Setup performance trace.
    me->gcl_performance_trace = NEW zcl_llm_performance_util( app_id            = app_id
                                                              business_function = business_function
                                                              uuid_key          = me->function_call_uuid ).

    DATA: lv_memory_id TYPE char50,
          memory_value TYPE char50.


* For the new framework
    lv_memory_id = 'ZLLM_CODIE_UUID'.
    memory_value = function_call_uuid.
    EXPORT p1 = memory_value TO MEMORY ID lv_memory_id.


* For the new framework
    lv_memory_id = 'ZLLM_CODIE_APPID'.
    memory_value = app_id.
    EXPORT p1 = memory_value TO MEMORY ID lv_memory_id.

* For the new framework
    lv_memory_id = 'ZLLM_CODIE_BUSINESS_FUNCTION'.
    memory_value = business_function.
    EXPORT p1 = memory_value TO MEMORY ID lv_memory_id.

*   To access Freshdesk ticket ID
    IF system_key IS NOT INITIAL.
      lv_memory_id = 'ZLLM_CODIE_SYSTEM_KEY'.
      memory_value = system_key.
      EXPORT p1 = memory_value TO MEMORY ID lv_memory_id.
    ENDIF.

* For legacy.
    lv_memory_id = |ZLLM_{ app_id }_{ business_function }|.
    EXPORT p1 = function_call_uuid TO MEMORY ID lv_memory_id.

**********************************************************************
* Check if this is a safe gaurding issue.
**********************************************************************
* Begin performance trace.
    gcl_performance_trace->start_perforamnce_trace( type = 'SAFE GAURD check' ).

    IF me->gs_bot_configuration-safe_gaurd_enabled = abap_true AND
       sub_call = abap_false.
      DATA(safe_gaurd) = me->safe_guarding( prompt = prompt ).

* If this is a safe gaurding issue do not process any further.
      IF safe_gaurd IS NOT INITIAL.
        IF safe_gaurd(4) = 'TRUE'.

          gcl_log->write_general_log( uuid_key    = me->function_call_uuid
                                      request     = prompt
                                      object_type = 'SAFEGUARD'
                                      new_value   = safe_gaurd ).

          response-response = 'SAFEGUARD'.
          RETURN.
        ENDIF.
      ENDIF.

    ENDIF.

    gcl_performance_trace->end_performance_trace( ).

**********************************************************************
* GCS knowledge base
**********************************************************************
* Begin performance trace.
    gcl_performance_trace->start_perforamnce_trace( type = 'KB set' ).

    IF me->gs_bot_configuration-enable_kb = abap_true AND
       sub_call = abap_false.
      me->set_gcs_file( file_name = me->gs_bot_configuration-gcs_kb_uri
                        mime      = 'application/pdf' ).
    ENDIF.


    gcl_performance_trace->end_performance_trace(  ).

**********************************************************************
* Pre model call tasks
**********************************************************************

* Begin performance trace.
    gcl_performance_trace->start_perforamnce_trace( type = 'BLANK LLM check' ).

* We cannot allow a blank prompt.
    IF prompt IS INITIAL.
      response-response = me->handle_blank_llm_response( ).
    ENDIF.

    gcl_performance_trace->end_performance_trace(  ).


* Begin performance trace.
    gcl_performance_trace->start_perforamnce_trace( type = 'STRIP HTML check' ).

* If we are to remove any HTML formatting before calling the model, do so now.
    IF me->gs_bot_configuration-strip_html = abap_true AND
       sub_call = abap_false.

      DATA(stripped_html_prompt) = me->strip_html( prompt = prompt ).

      IF stripped_html_prompt IS NOT INITIAL.
        prompt = stripped_html_prompt.
      ENDIF.

      CLEAR : stripped_html_prompt.

    ENDIF.

    gcl_performance_trace->end_performance_trace(  ).


* Begin performance trace.
    gcl_performance_trace->start_perforamnce_trace( type = 'CLEAN PROMPT check' ).

* Remove unwanted parts of the prompt.
    IF me->gs_bot_configuration-clean_prompt = abap_true AND
       sub_call = abap_false.

      DATA(clean_prompt) = me->prompt_clean_up( prompt = prompt ).

*     Check if everything has been striped off after the clean up, as we do not want that
      IF clean_prompt IS NOT INITIAL.
        prompt = clean_prompt.
      ENDIF.

      CLEAR : clean_prompt.

    ENDIF.

    gcl_performance_trace->end_performance_trace(  ).

**********************************************************************
* Check malicious prompts
**********************************************************************

* Begin performance trace.
    gcl_performance_trace->start_perforamnce_trace( type = 'NAUGHTY BOT check' ).


    IF sub_call = abap_false.
      DATA(naughty_prompt) = me->naughty_prompt( prompt = prompt ).
    ENDIF.

* If this prompt contains attempts to break our logic. Stop.
    IF naughty_prompt IS NOT INITIAL.
      IF naughty_prompt(4) = 'TRUE'.

        gcl_log->write_general_log( uuid_key    = me->function_call_uuid
                                    request     = prompt
                                    object_type = 'UNSAFE_PROMPT'
                                    new_value   = naughty_prompt ).

        response-response = 'Unsafe prompt detected.'.
        RETURN.
      ENDIF.
    ENDIF.


    gcl_performance_trace->end_performance_trace(  ).


**********************************************************************
* MAIN MODEL CALL
**********************************************************************


    IF gs_bot_configuration-expansion_prompt = abap_true.
* Check if this bot has expansion prompts enabled.
      TRY.
          DATA(ls_expansion_prompt) = me->fetch_prompt( iv_prompt_key = 'EXPANSION_PROMPT' ).
          prompt = |{ ls_expansion_prompt-prompt_text }\n\n{ prompt }|.

          prompt = me->call_simple_model( prompt = prompt )-response.


        CATCH zcx_llm.
* its fine if this prompt is not maintained.
      ENDTRY.

    ENDIF.


    TRY.
* For now, allow a possible loop 5 times to find a better answer. This process will need changing.
        DO me->gs_bot_configuration-evaluation_count TIMES.

* Update the prompt to indicate the query.


* Get the limited / pre-rag
* Pre rag here is for legacy agents.
          IF me->gs_bot_configuration-limited_rag = abap_true AND
             sub_call = abap_false.

*           Wrapping the customer query around this tag for easy identification
*           Adding this here, as we want it to be applied only for the first call(supervisor level)
            prompt = |<CUSTOMER REQUEST>\n{ prompt }\n</CUSTOMER_REQUEST>\n|.

            prompt = |{ me->pre_rag( prompt = prompt  ) }\n\n{ prompt } |.
          ENDIF.



* Set system prompt.
          TRY.
              go_model->set_system_instructions( iv_text = lcl_template_util->get_system_instructions( ) ).

            CATCH zcx_llm.
* If system instruction is not maintained, don't error out
          ENDTRY.

* Begin performance trace.
          gcl_performance_trace->start_perforamnce_trace( type = 'GENERATE CONTENT' ).

** Generate the response content
          DATA(lo_response) = go_model->generate_content( iv_prompt_text = prompt ).

          gcl_performance_trace->end_performance_trace(  ).

* Pass back response with uuid_key
          response-response = lo_response->get_text( ).

          REPLACE ALL OCCURRENCES OF '```html' IN response-response WITH ''.
          REPLACE ALL OCCURRENCES OF '```' IN response-response WITH ''.

          response-uuid_key = me->function_call_uuid.

* Handle blank response (if eval bot is enabled, it should be picked up).
          IF response-response IS INITIAL.
            response-response = me->handle_blank_llm_response(  ).
          ENDIF.

**********************************************************************
* EVALUATION BOT
* This bot will evaluate the response, attempting to ensure quality
**********************************************************************

* Determine if we are to accept the response.
          IF me->evaluate_response( original_query = prompt
                                    llm_response   = response-response ) = abap_false.

* Prepare new function call parameters, removing the bad agent
            me->prepare_function_call( ).

* Clear the current function declarations and rebuild.
            go_model->clear_function_declaration(  ).
            me->add_function_call_properties(  ).

          ENDIF.

        ENDDO.



**********************************************************************
* ADVISOR BOT / HELPER BOT
* This bot will be able to take a specific action
**********************************************************************
* Don't allow the email to be sent if this is a sub agent.
        IF me->gs_bot_configuration-helper_bot_enabled = abap_true AND
           sub_call = abap_false.
* Provide additional assistance if possible.
          response-actioned = me->call_helper_bot( app_id            = app_id
                                                   business_function = business_function
                                                   prompt            = response-response
                                                   request           = prompt
                                                   system_key        = system_key ).

        ENDIF.
* Generate log of model call.
        gcl_log->write_general_log( uuid_key             = me->function_call_uuid
                                    request              = prompt
                                    request_no_html      = go_model->generate_content( iv_prompt_text = prompt )->get_text(  )
                                    object_type          = 'GENERATE_CONTENT'
                                    new_value            = response-response
                                    request_token_count  = lo_response->get_prompt_token_count( )
                                    response_token_count = lo_response->get_candidates_token_count( ) ).


      CATCH  /goog/cx_sdk INTO DATA(lo_cx_sdk).

* Generate log of model call even if it errors
        gcl_log->write_general_log( uuid_key    = me->function_call_uuid
                                    request     = prompt
                                    object_type = 'GENERATE_CONTENT-ERR'
                                    new_value   = lo_cx_sdk->get_text( ) ).

        response-uuid_key = me->function_call_uuid.
        response-response = lo_cx_sdk->get_text( ).

    ENDTRY.

  ENDMETHOD.



  METHOD add_function_call_properties.

    DATA function_call_properties TYPE /goog/cl_generative_model=>tt_parameter_properties.

    IF me->function_call_uuid IS INITIAL.

*     DR: Have added UUID_KEY as a parameter to this method. This is for the new framework to retain the UUID key for the full flow
*         instead of generating new ones at every level of function call
      IF uuid_key IS NOT INITIAL.

        me->function_call_uuid = uuid_key.

      ELSE.
**********************************************************************
* Instantiate uuid_key for logging.
**********************************************************************
        me->function_call_uuid = me->gcl_log->generate_uuid( ).

      ENDIF.

    ENDIF.


*   Select the required function call properties based on the type
    IF supv_for_sub_agents IS NOT INITIAL.

      TRY.

          DATA(lt_function_call) = me->fetch_func_declaration( function_type   = 'SUB'
                                                               parent_function = supv_for_sub_agents ).
        CATCH zcx_llm INTO DATA(lcx_llm).

          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2.

      ENDTRY.

    ELSEIF parent_for_rag_agents IS NOT INITIAL.

      TRY.

          lt_function_call = me->fetch_func_declaration( function_type   = 'RAG'
                                                         parent_function = parent_for_rag_agents ).
        CATCH zcx_llm INTO lcx_llm.

          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2.

      ENDTRY.
    ELSE.

      TRY.
          lt_function_call = me->fetch_func_declaration( function_type = CONV zllm_function_type( type ) ).

        CATCH zcx_llm INTO lcx_llm.

          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2.

      ENDTRY.

    ENDIF.


    LOOP AT lt_function_call ASSIGNING FIELD-SYMBOL(<fs_function_call>).

      LOOP AT <fs_function_call>-param_details ASSIGNING FIELD-SYMBOL(<fs_param_details>).

        IF <fs_param_details> IS NOT INITIAL.
          APPEND VALUE #( parameter_name = <fs_param_details>-param_name
                          type           = <fs_param_details>-param_type
                          description    = <fs_param_details>-param_description
                          is_required    = <fs_param_details>-is_required ) TO function_call_properties.
        ENDIF.

      ENDLOOP.


* Disable function chaining
      go_model->set_function_calling_config( iv_disable_function_chaining = abap_true ).


* Add function call declaration.
      go_model->add_function_declaration( iv_name        = <fs_function_call>-fm
                                          iv_description = <fs_function_call>-fm_description
                                          it_parameters  = function_call_properties )->set_auto_invoke_sap_function( abap_true ).


      CLEAR function_call_properties.
    ENDLOOP.


* Save the Function Descriptions to the log.
    me->create_fm_desc_log( EXPORTING it_functions = lt_function_call
                                      iv_uuid_key  = me->function_call_uuid ).

  ENDMETHOD.


  METHOD fetch_prompt.

    IF iv_prompt_key IS NOT INITIAL.

      IF prompt_type IS NOT INITIAL.

        SELECT SINGLE *
        FROM zllm_prompts
        INTO rs_prompt
        WHERE app_id = me->app_id
        AND business_function = me->business_function
        AND prompt_key = iv_prompt_key
        AND prompt_type = prompt_type
        AND version = me->version
        AND deleted = abap_false.

      ELSE.

        SELECT SINGLE *
        FROM zllm_prompts
        INTO rs_prompt
        WHERE app_id = me->app_id
        AND business_function = me->business_function
        AND prompt_key = iv_prompt_key
        AND version = me->version
        AND deleted = abap_false.

      ENDIF.

      IF sy-subrc <> 0.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>no_prompt
            msg1   = |{ me->app_id }|
            msg2   = |{ iv_prompt_key }|.

      ENDIF.


    ENDIF.

  ENDMETHOD.

  METHOD fetch_prompt_sequence.
*** Method implementation to fetch the sequence of prompts for the prompt document

    IF iv_prompt_key IS NOT INITIAL.

      SELECT zllm_prompts_seq~mandt,
             zllm_prompts_seq~version,
             zllm_prompts_seq~app_id,
             zllm_prompts_seq~business_function,
             zllm_prompts_seq~function_name,
             zllm_prompts_seq~prompt_key,
             zllm_prompts_seq~sequence
      FROM zllm_prompts_seq
      INTO TABLE @rt_prompt_sequence
      WHERE app_id = @me->app_id
      AND business_function = @me->business_function
      AND prompt_key = @iv_prompt_key
      AND version = @iv_version.

      IF sy-subrc <> 0.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>no_prompt
            msg1   = |{ me->app_id }|
            msg2   = |{ iv_prompt_key }|.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_config.

    SELECT *
    FROM /goog/ai_config
    INTO TABLE @rt_config.

  ENDMETHOD.


  METHOD create_fm_desc_log.

    DATA: lv_log_string TYPE string,
          lv_len        TYPE i.


    TRY.

* Add starting tag
        lv_log_string = '['.

        LOOP AT it_functions ASSIGNING FIELD-SYMBOL(<fs_functions>).

* Add function descriptions
          lv_log_string = |{ lv_log_string }\{|.

          lv_log_string = |{ lv_log_string }"function_name":"{ <fs_functions>-fm }","function_desc":"{ me->escape_quotes( <fs_functions>-fm_description ) }","parameters":[|.

          LOOP AT <fs_functions>-param_details ASSIGNING FIELD-SYMBOL(<fs_param>).

* Add parameters
            lv_log_string = |{ lv_log_string }\{|.

            lv_log_string = |{ lv_log_string }"type":"{ <fs_param>-param_type }","name":"{ <fs_param>-param_name }","desc":"{ me->escape_quotes( <fs_param>-param_description ) }"|.

            lv_log_string = |{ lv_log_string }\},|.

          ENDLOOP.
* Remove the , after the last parameter
          lv_len = strlen( lv_log_string ) - 1.

          lv_log_string = |{ lv_log_string(lv_len) }]\},|.

          CLEAR lv_len.

        ENDLOOP.

* Remove the , after the last function declaration
        lv_len = strlen( lv_log_string ) - 1.

        lv_log_string = lv_log_string(lv_len).

* Add ending tag
        lv_log_string = |{ lv_log_string }]|.

        CLEAR lv_len.

* Escape all special chars in the json string
        me->escape_special_chars( CHANGING cv_string = lv_log_string ).

* Write log
        gcl_log->write_general_log(
          request     = lv_log_string
*         request_no_html      =
*         sentiment   =
          object_type = |FUNCTION_PROPERTIES|
*         object_key  =
*         old_value   =
*         new_value   =
*         request_token_count  =
*         response_token_count =
          uuid_key    = me->function_call_uuid
        ).

      CATCH cx_root.
* Just continue without the log if any errors.
    ENDTRY.


  ENDMETHOD.


  METHOD escape_special_chars.

    REPLACE ALL OCCURRENCES OF '\d' IN cv_string WITH '\\d' .
    REPLACE ALL OCCURRENCES OF '\s' IN cv_string WITH '\\s' .
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN cv_string WITH '\r\n' .
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN cv_string WITH '\n' .
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN cv_string WITH '\t' .
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace IN cv_string WITH '\b' .
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN cv_string WITH '\f' .

  ENDMETHOD.

  METHOD escape_quotes.

    rv_string = iv_string.
    REPLACE ALL OCCURRENCES OF '"' IN rv_string WITH '\"' .

  ENDMETHOD.

  METHOD trigger_sub_agents.
*** Method implementation to trigger function calling for sub agents of the supervisor agent provided

*   Ensure all the keys are populated
    IF me->app_id IS NOT INITIAL AND
       me->business_function IS NOT INITIAL AND
       supervisor_agent IS NOT INITIAL AND
       query IS NOT INITIAL.

      DATA(lt_sub_agents) = me->fetch_sub_agents( supv_function = supervisor_agent ).

      IF lt_sub_agents IS NOT INITIAL.

        TRY.

            DATA(lcl_sub_llm_util) = NEW zcl_llm_util( app_id            = app_id
                                                       business_function = business_function ).

            lcl_sub_llm_util->add_function_call_properties( EXPORTING supv_for_sub_agents = supervisor_agent ).

            response = lcl_sub_llm_util->call_model( prompt = query ).

          CATCH zcx_llm INTO DATA(lcx_llm).

        ENDTRY.

      ENDIF.


    ELSE.

      RAISE EXCEPTION TYPE zcx_llm
*      EXPORTING
*        textid   =
*        previous =
*        msg1     =
*        msg2     =
*        msg3     =
        .


    ENDIF.

  ENDMETHOD.

  METHOD get_child_mappings.
*** Method implementation to fetch child mappings for the parent agent/name passed

    IF parent_agent IS NOT INITIAL.

*     Fetch the list of mappings available for the parent agent/function
      SELECT parent,
             child,
             type
         FROM zllm_agent_map
         INTO TABLE @DATA(lt_agent_mappings)
        WHERE parent = @parent_agent
          AND version = @version
          AND deletion_indicator = @abap_false
          AND type = @mapping_type.

* If nothing found then there is no sub agent confgiured. This is ok.
      IF sy-subrc = 4.
*       If parent function is not provided, raise an exception, as we cannot proceed
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>no_sub_agent.
      ENDIF.

*     Build the range of children mapped to the parent agent passed
      rt_agent_mappings_range = VALUE zllm_function_name_range_tt( FOR ls_agent_map IN lt_agent_mappings
                                                                 ( sign   = 'I'
                                                                   option = 'EQ'
                                                                   low    = ls_agent_map-child ) ).

    ELSE.

      IF mapping_type IS NOT INITIAL.

        SELECT *
          FROM zllm_agent_cat
          INTO TABLE @DATA(lt_catalogue_agents)
          WHERE type = @mapping_type
            AND deleted = @abap_false.


* If nothing found then there is no sub agent confgiured. This is ok.
        IF sy-subrc = 4.
*       If parent function is not provided, raise an exception, as we cannot proceed
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>no_sub_agent.
        ENDIF.

*       Build the range of agents available for the function type passed
        rt_agent_mappings_range = VALUE zllm_function_name_range_tt( FOR ls_cat_agent IN lt_catalogue_agents
                                                                   ( sign   = 'I'
                                                                     option = 'EQ'
                                                                     low    = ls_cat_agent-agent_name ) ).

      ELSE.


*       If parent function is not provided, raise an exception, as we cannot proceed
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>no_parent_function_provided.



      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD determine_keys_for_func_type.
*** Method implementation to determine keys such as App ID, business function and version for
*   the function type passed

    CASE function_type.

      WHEN 'RAG' OR 'ACTION'.

        ev_app_id = 'LLM'.
        ev_business_function = 'MCP'.
        ev_version = 1.

      WHEN OTHERS.

        ev_app_id = me->app_id.
        ev_business_function = me->business_function.
        ev_version = me->version.

    ENDCASE.

  ENDMETHOD.

  METHOD assemble_prompts_for_response.
*** Method implementation to gather all the global prompts needed for the final response generation
*** These incluse INTRO / SYSTEM and SIGN OFF prompts

    TRY.

        DATA(ls_prompt) = me->fetch_prompt( iv_prompt_key = 'INTROCONTEXT' ).
        prompt = |{ ls_prompt-prompt_text }\n\n|.


        ls_prompt = me->fetch_prompt( iv_prompt_key = 'SYSTEM' ).
        prompt = |{ prompt }\n\n{ ls_prompt-prompt_text }|.


        ls_prompt = me->fetch_prompt( iv_prompt_key = 'SIGNOFFCONTEXT' ).
        prompt = |{ prompt }\n\n{ ls_prompt-prompt_text } |.

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2.

    ENDTRY.


  ENDMETHOD.

ENDCLASS.
