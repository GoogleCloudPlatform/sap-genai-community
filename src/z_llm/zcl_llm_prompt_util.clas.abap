CLASS zcl_llm_prompt_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_fiori_llm_util
                 zcl_llm_fi_util
                 zcl_llm_util.

  PUBLIC SECTION.

    METHODS:
      constructor IMPORTING app_id            TYPE zllm_app_id
                            business_function TYPE zllm_buss_function_name
                  RAISING   zcx_llm.


  PROTECTED SECTION.
  PRIVATE SECTION.
**********************************************************************
* Method to get prompt based on key and version
**********************************************************************
    METHODS: get_prompt  IMPORTING prompt_key    TYPE  zllm_prompt_key OPTIONAL
                                   version       TYPE zllm_version OPTIONAL
                         RETURNING VALUE(prompt) TYPE zllm_prompt_odata_tt
                         RAISING   zcx_llm,

**********************************************************************
* Method to get the prompt name / label
**********************************************************************
      get_prompt_name IMPORTING prompt_key         TYPE zllm_prompt_key
                                prompt_chain       TYPE zllm_prompt_chain
                      RETURNING VALUE(prompt_name) TYPE string,

**********************************************************************
* Method to save prompt.
* Saving a prompt will automatically generate a change history entry
**********************************************************************
      save_prompt IMPORTING prompt_key    TYPE zllm_prompt_key
                            prompt        TYPE zllm_prompt_text
                            is_signed_off TYPE zllm_prompt_sign_off
                            prompt_type   TYPE zllm_prompt_type
                            is_deletable  TYPE zllm_prompt_deletable
                            version       TYPE zllm_version OPTIONAL
                  RAISING   zcx_llm,

**********************************************************************
* Method to save prompt connector.
* Saving a prompt connector will automatically generate a change history entry
**********************************************************************
      save_prompt_connector IMPORTING prompt_connector TYPE zllm_prompt_connector_tt
                            RAISING   zcx_llm,

      get_prompt_connector IMPORTING prompt_chain            TYPE zllm_prompt_chain
                                     prompt_key              TYPE zllm_prompt_key
                                     source                  TYPE char10
                           RETURNING VALUE(prompt_connector) TYPE zllm_prompt_connector_tt
                           RAISING   zcx_llm,

**********************************************************************
* Method to get the prompt history
**********************************************************************
      get_prompt_history IMPORTING prompt_key    TYPE zllm_prompt_key
                                   version       TYPE zllm_version OPTIONAL
                         RETURNING VALUE(prompt) TYPE zllm_prompt_history_odata_tt
                         RAISING   zcx_llm,

**********************************************************************
* Method to publish a prompt template from a function declaration
**********************************************************************
      publish_prompt IMPORTING func_name TYPE zllm_function_name
                     RAISING   zcx_llm,

**********************************************************************
* Method to get the prompt usage to display in the Fiori Response prompt tab
**********************************************************************
      get_prompt_usage IMPORTING prompt_key          TYPE zllm_prompt_key
                       RETURNING VALUE(prompt_usage) TYPE zllm_prompt_usage_tt,

**********************************************************************
* Method to get average rating of a prompt
**********************************************************************
      get_prompt_avg_rating IMPORTING prompt_key        TYPE zllm_prompt_key
                                      start_date        TYPE datum OPTIONAL
                                      end_date          TYPE datum OPTIONAL
                            RETURNING VALUE(avg_rating) TYPE zllm_prompt_avg_rating,

**********************************************************************
* Method to save prompt examples against a prompt key
**********************************************************************
      save_prompt_example IMPORTING prompt_key TYPE zllm_prompt_key
                                    example    TYPE string
                                    uuid       TYPE guid OPTIONAL
                                    delete     TYPE zllm_deletion_flag
                          RAISING   zcx_llm,

**********************************************************************
* Method to get prompt example
**********************************************************************
      get_prompt_example IMPORTING prompt_key            TYPE zllm_prompt_key
                                   app_id                TYPE zllm_app_id
                                   business_function     TYPE zllm_buss_function_name
                         RETURNING VALUE(prompt_example) TYPE zllm_prompt_eg_tt,

**********************************************************************
* Method to trigger a prompt FM to see the RAG response
**********************************************************************
      trigger_prompt IMPORTING function        TYPE zllm_function_name
                               parameters      TYPE /goog/t_function_parameters
                     RETURNING VALUE(response) TYPE string
                     RAISING   zcx_llm,

**********************************************************************
* Method to get the prompt FM details.
**********************************************************************
      get_prompt_fm_config IMPORTING prompt_key    TYPE zllm_prompt_key
                           RETURNING VALUE(config) TYPE /goog/t_function_parameters
                           RAISING   zcx_llm,

**********************************************************************
* Method to get system prompt chain
**********************************************************************
      get_prompt_chain IMPORTING prompt_key          TYPE zllm_prompt_key
                                 version             TYPE zllm_version
                       RETURNING VALUE(prompt_chain) TYPE zllm_prompt_chain_tt,

      get_prompt_key_ext_value RETURNING VALUE(prompt_key_ext_value) TYPE zllm_ext_value_tt,

      get_prompt_chain_name    RETURNING VALUE(prompt_chain_name) TYPE zllm_prompt_name_tt,

      get_prompt_prop         IMPORTING prompt_key         TYPE zllm_prompt_key
                              RETURNING VALUE(prompt_prop) TYPE zllm_prompt_properties_tt,

      check_parent_connector  IMPORTING prompt_key         TYPE zllm_prompt_key
                              RETURNING VALUE(prompt_prop) TYPE zllm_prompt_properties_tt,

      check_child_connector  IMPORTING prompt_key         TYPE zllm_prompt_key
                             RETURNING VALUE(prompt_prop) TYPE zllm_prompt_properties_tt,

      update_prompt_connector_chain IMPORTING prompt_connector_chain TYPE zllm_prompt_connector_chain_tt
                                    RAISING   zcx_llm,

      save_prompt_through_connector IMPORTING prompt TYPE zllm_prompts_tt.

    DATA: gt_app_id            TYPE zllm_app_id,
          gt_business_function TYPE zllm_buss_function_name.

ENDCLASS.



CLASS zcl_llm_prompt_util IMPLEMENTATION.


  METHOD constructor.

    IF app_id IS INITIAL OR
       business_function IS INITIAL.

* Not enough info to select prompt.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>prompt_select_failed
          msg1   = 'app_id or business_function is missing'.

    ELSE.

      gt_app_id = app_id.
      gt_business_function = business_function.

    ENDIF.

  ENDMETHOD.

  METHOD save_prompt_connector.

    DATA(lcl_llm_crud) = NEW zcl_llm_crud_control_util( app_id            = gt_app_id
                                                        business_function = gt_business_function ).

    TRY.
        lcl_llm_crud->save_prompt_connector( prompt_connector = prompt_connector ).

      CATCH zcx_llm INTO DATA(lcx_llm).
* Raise exception.
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.


    ENDTRY.

  ENDMETHOD.

  METHOD get_prompt_name.

    SELECT SINGLE prompt_name
    FROM zllm_prompt_name
    INTO prompt_name
    WHERE prompt_key = prompt_key
    AND prompt_chain = prompt_chain.

  ENDMETHOD.


  METHOD get_prompt_chain.

*   Get the prompt chain for the agent. Fetch only those links that are enabled for usage.
    SELECT *
    FROM zllm_prompts
    INTO CORRESPONDING FIELDS OF TABLE @prompt_chain
    WHERE app_id = @me->gt_app_id
    AND business_function = @me->gt_business_function
    AND version = @version
    AND deleted = @abap_false
    AND prompt_key = @prompt_key
    AND prompt_chain_enabled = @abap_true
    ORDER BY prompt_chain ASCENDING.


  ENDMETHOD.


  METHOD get_prompt_fm_config.

* Get the current active version
* Get the current version of the app.
    DATA(zcl_llm_version_util) = NEW zcl_llm_version_control_util( ).


    DATA(versions) = zcl_llm_version_util->get_active_app_version( app_id            = gt_app_id
                                                                   business_function = gt_business_function ).


    SELECT parameter_name, parameter_type
    FROM zllm_func_declar
    INNER JOIN zllm_func_parame ON zllm_func_declar~function_parameters_id = zllm_func_parame~parameters_id
    INTO TABLE @DATA(lt_config)
    WHERE zllm_func_declar~app_id = @gt_app_id
    AND zllm_func_declar~business_function = @gt_business_function
    AND zllm_func_declar~version = @versions-active_version
    AND zllm_func_declar~function_name = @prompt_key.


    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>no_function_call_properties
          msg1   = CONV string( prompt_key ).

    ENDIF.

    DATA: ls_parameters TYPE /goog/function_parameters.

    LOOP AT lt_config ASSIGNING FIELD-SYMBOL(<fs_config>).

      ls_parameters-parameter_name = <fs_config>-parameter_name.
      ls_parameters-parameter_value = <fs_config>-parameter_type.

      APPEND ls_parameters TO config.


    ENDLOOP.

  ENDMETHOD.


  METHOD trigger_prompt.

    DATA: cv_prompt TYPE string.


* Make sure that the FM exists.
    SELECT SINGLE funcname
    FROM tfdir
    INTO @DATA(lv_funcname)
    WHERE funcname = @function.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>no_fm_found
          msg1   = CONV string( function ).
    ENDIF.


* Call the function module.
    CALL FUNCTION function
      EXPORTING
        it_function_parameters = parameters
      IMPORTING
        ev_function_response   = response
      CHANGING
        cv_prompt              = cv_prompt.



  ENDMETHOD.


  METHOD save_prompt_example.


    DATA(lcl_llm_crud) = NEW zcl_llm_crud_control_util( app_id            = gt_app_id
                                                        business_function = gt_business_function ).

    DATA: ls_prompt_example TYPE zllm_prompt_eg.

    ls_prompt_example-uuid = uuid.
    ls_prompt_example-app_id = gt_app_id.
    ls_prompt_example-business_function = gt_business_function.
    ls_prompt_example-prompt_key = prompt_key.
    ls_prompt_example-object = example.
    ls_prompt_example-deleted = delete.


    TRY.
        lcl_llm_crud->save_prompt_example( prompt_example = ls_prompt_example ).

      CATCH zcx_llm INTO DATA(lcx_llm).
* Raise exception.
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.


    ENDTRY.

  ENDMETHOD.


  METHOD get_prompt_example.

    SELECT *
    FROM zllm_prompt_eg
    INTO TABLE @prompt_example
    WHERE app_id = @gt_app_id
    AND business_function = @gt_business_function
    AND prompt_key = @prompt_key
    AND deleted = @abap_false.


  ENDMETHOD.


  METHOD get_prompt_avg_rating.

* Get the UUID_KEY for the needed prompt.
* First, get all the logs that relate to a prompt
    SELECT uuid_key
    FROM zllm_log
    INTO TABLE @DATA(lt_uuid_key)
    WHERE zllm_log~object_type = 'FUNCTION_CALL'
    AND zllm_log~object_key = @prompt_key
    AND dat BETWEEN @start_date AND @end_date.


    DATA: lv_positive TYPE i,
          lv_negative TYPE i.

    lv_positive = 0.
    lv_negative = 0.

    LOOP AT lt_uuid_key ASSIGNING FIELD-SYMBOL(<fs_uuid_key>).


      SELECT SINGLE sentiment
      FROM zllm_log
      INTO @DATA(lv_sentiment)
      WHERE zllm_log~uuid_key = @<fs_uuid_key>-uuid_key
      AND zllm_log~object_type = 'Freshdesk'
      AND zllm_log~sentiment IN ( 'POSITIVE', 'NEGATIVE' ).


      IF lv_sentiment = 'POSITIVE'.

        lv_positive = lv_positive + 1.

      ELSEIF lv_sentiment = 'NEGATIVE'.

        lv_negative = lv_negative + 1.

      ENDIF.

      CLEAR: lv_sentiment.
    ENDLOOP.

    DATA: lv_rating TYPE p DECIMALS 2.
    lv_rating = ( lv_positive / ( lv_positive + lv_negative ) ) * 100.
    avg_rating = lv_rating.

  ENDMETHOD.


  METHOD get_prompt_usage.

    DATA: ls_prompt_usage TYPE zllm_prompt_usage,
          lv_start_date   TYPE datum.

* We only need to see prompt usage for past 7 days. More than that things change too much.
    lv_start_date = sy-datum - 7.


* First, get all the logs that relate to a prompt
    SELECT DISTINCT uuid_key, object_key
    FROM zllm_log
    INTO TABLE @DATA(lt_uuid_key)
    WHERE object_key = @prompt_key
    AND object_type = 'FUNCTION_CALL'
    AND app_id = @me->gt_app_id
    AND buss_function = @me->gt_business_function
    AND dat BETWEEN @lv_start_date AND @sy-datum.


* Loop at all the UUID keys fetching the request (source) and AI response
    LOOP AT lt_uuid_key ASSIGNING FIELD-SYMBOL(<fs_uuid_key>).

      SELECT SINGLE request, new_value, rating, sentiment, zllm_log~object_key, zllm_log~dat, zllm_log~time, zllm_log~user_name, edit_category, zllm_log~uuid, zllm_log~resolved, zllm_log~resolved_note
      FROM zllm_log
      LEFT OUTER JOIN zllm_star_rating ON zllm_log~uuid_key = zllm_star_rating~uuid_key AND
                                          zllm_star_rating~object_type = 'Freshdesk'
      LEFT OUTER JOIN zllm_res_compare ON zllm_log~uuid_key = zllm_res_compare~uuid_key
      WHERE zllm_log~app_id = @gt_app_id
      AND zllm_log~buss_function = @gt_business_function
      AND zllm_log~uuid_key = @<fs_uuid_key>-uuid_key
      AND zllm_log~object_type = 'Freshdesk'
      AND ( zllm_log~sentiment <> 'NOFEEDBACK' AND
            zllm_log~sentiment IS NOT INITIAL )      "Adding this to not include Freshdesk entry for Actioned, but to
                                                     "force it to pick up actioned log entry
      AND zllm_log~dat BETWEEN @lv_start_date AND @sy-datum
      INTO @DATA(ls_prompt_log).

*     Logic addition to pull Actioned(auto replied) tickets
      SELECT SINGLE request, new_value, rating, sentiment, zllm_log~object_key, zllm_log~dat, zllm_log~time, zllm_log~user_name, edit_category, zllm_log~uuid, zllm_log~resolved, zllm_log~resolved_note
      FROM zllm_log
      LEFT OUTER JOIN zllm_star_rating ON zllm_log~uuid_key = zllm_star_rating~uuid_key AND
                                          zllm_star_rating~object_type = 'Freshdesk'
      LEFT OUTER JOIN zllm_res_compare ON zllm_log~uuid_key = zllm_res_compare~uuid_key
      INTO @DATA(ls_actioned_log)
      WHERE zllm_log~app_id IN ( @gt_app_id , 'LLM'  )
        AND zllm_log~buss_function IN ( @gt_business_function, 'MCP' )
        AND zllm_log~uuid_key = @<fs_uuid_key>-uuid_key
        AND zllm_log~object_type = 'ACTIONED'
        AND zllm_log~dat BETWEEN @lv_start_date AND @sy-datum.

*     For normal cases, where Freshdesk log exists with sentiment as positive/negative
*     Also covers cases where an actioned ticket is edited before sending a corrected response, in addition to auto response
      IF ls_prompt_log IS NOT INITIAL.

        ls_prompt_usage-app_id = gt_app_id.
        ls_prompt_usage-business_function = gt_business_function.
        ls_prompt_usage-prompt_key = <fs_uuid_key>-object_key.
        ls_prompt_usage-request = ls_prompt_log-request.
        ls_prompt_usage-response = ls_prompt_log-new_value.
        ls_prompt_usage-rating = ls_prompt_log-rating.
        ls_prompt_usage-sentiment = ls_prompt_log-sentiment.
        ls_prompt_usage-object_key = ls_prompt_log-object_key.
        ls_prompt_usage-date = ls_prompt_log-dat.
        ls_prompt_usage-time = ls_prompt_log-time.
        ls_prompt_usage-user = ls_prompt_log-user_name.
        ls_prompt_usage-edit_category = ls_prompt_log-edit_category.
        ls_prompt_usage-guid = ls_prompt_log-uuid.
        ls_prompt_usage-resolved = ls_prompt_log-resolved.
        ls_prompt_usage-resolved_note = ls_prompt_log-resolved_note.

        IF ls_actioned_log IS NOT INITIAL.

          ls_prompt_usage-actioned = abap_true.

        ENDIF.

*     Cases when Freshdesk log might not exist/might exist but with no sentiment, but the ticket was actioned
      ELSEIF ls_actioned_log IS NOT INITIAL.

        ls_prompt_usage-app_id = gt_app_id.
        ls_prompt_usage-business_function = gt_business_function.
        ls_prompt_usage-prompt_key = <fs_uuid_key>-object_key.
        ls_prompt_usage-request = ls_actioned_log-request.
        ls_prompt_usage-response = ls_actioned_log-new_value.
        ls_prompt_usage-rating = ls_actioned_log-rating.
        ls_prompt_usage-sentiment = ls_actioned_log-sentiment.
        ls_prompt_usage-object_key = ls_actioned_log-object_key.
        ls_prompt_usage-date = ls_actioned_log-dat.
        ls_prompt_usage-time = ls_actioned_log-time.
        ls_prompt_usage-user = ls_actioned_log-user_name.
        ls_prompt_usage-edit_category = ls_actioned_log-edit_category.
        ls_prompt_usage-guid = ls_actioned_log-uuid.
        ls_prompt_usage-resolved = ls_actioned_log-resolved.
        ls_prompt_usage-resolved_note = ls_actioned_log-resolved_note.
        ls_prompt_usage-actioned = abap_true.

      ELSE.

        CONTINUE.

      ENDIF.

* Get the original response from the bot.
      SELECT new_value, time
      FROM zllm_log
      INTO TABLE @DATA(lt_original_response)
      WHERE uuid_key = @<fs_uuid_key>-uuid_key
      AND object_type = 'GENERATE_CONTENT'.
      IF sy-subrc = 0.
        SORT lt_original_response DESCENDING BY time.
* Pass the most recent response
        ls_prompt_usage-original_response = VALUE #( lt_original_response[ 1 ]-new_value ).
      ENDIF.


* Get the rag data.
      SELECT SINGLE new_value
      FROM zllm_log
      INTO @ls_prompt_usage-rag_data
      WHERE uuid_key = @<fs_uuid_key>-uuid_key
      AND object_type = 'RAG_DATA'.


      APPEND ls_prompt_usage TO prompt_usage.
      CLEAR: ls_prompt_usage,
             ls_prompt_log,
             ls_actioned_log.

    ENDLOOP.

    SORT prompt_usage BY date DESCENDING.

  ENDMETHOD.


  METHOD get_prompt_history.

    DATA ls_prompt TYPE zllm_prompt_history_odata.

* If a version is passed in, then only select the specific version history
    IF version IS NOT INITIAL.


    ELSE.

      SELECT app_id, business_function,
             prompt_key, prompt_text,
             is_signed_off, prompt_type,
             deletable, deleted,
             dat, time, concat( dat, time ) AS timestamp,
             usr,
             prompt_chain, prompt_manual_chain,
             prompt_chain_enabled,
             think_link, final_link
      FROM zllm_prompt_hist
      WHERE app_id = @gt_app_id
      AND business_function = @gt_business_function
      AND prompt_key = @prompt_key
      ORDER BY timestamp DESCENDING
      INTO TABLE @DATA(lt_prompt_history).

    ENDIF.
* If there is no prompt history, that is ok.


    LOOP AT lt_prompt_history ASSIGNING FIELD-SYMBOL(<fs_prompt_history>).

      ls_prompt-app_id = <fs_prompt_history>-app_id.
      ls_prompt-business_function = <fs_prompt_history>-business_function.
      ls_prompt-dat = <fs_prompt_history>-dat.
      ls_prompt-deletable = <fs_prompt_history>-deletable.
      ls_prompt-deleted = <fs_prompt_history>-deleted.
      ls_prompt-is_signed_off = <fs_prompt_history>-is_signed_off.
      ls_prompt-prompt = <fs_prompt_history>-prompt_text.
      ls_prompt-prompt_key = <fs_prompt_history>-prompt_key.
      ls_prompt-prompt_type = <fs_prompt_history>-prompt_type.
      ls_prompt-time = <fs_prompt_history>-time.
      ls_prompt-usr = <fs_prompt_history>-usr.
      ls_prompt-prompt_chain = <fs_prompt_history>-prompt_chain.

      APPEND ls_prompt TO prompt.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_prompt.


* Get the current version of the app.
    DATA(zcl_llm_version_util) = NEW zcl_llm_version_control_util( ).

* If no version is provided, default to active version.
    IF version IS INITIAL.

      DATA(versions) = zcl_llm_version_util->get_active_app_version( app_id            = gt_app_id
                                                                     business_function = gt_business_function ).

      DATA(lv_version) = versions-active_version.

    ELSE.
      lv_version = version.
    ENDIF.


* Get the prompt.
    IF prompt_key IS INITIAL.

      SELECT app_id, business_function, prompt_key, prompt_chain,prompt_chain_enabled, prompt_text, is_signed_off, prompt_type, deletable, deleted, version, external_value,think_link,final_link, model_id, decision_link
      FROM zllm_prompts
      LEFT OUTER JOIN zllm_ext_value ON zllm_prompts~prompt_key = zllm_ext_value~internal_value
      INTO TABLE @DATA(lt_prompts)
      WHERE app_id = @gt_app_id
      AND business_function = @gt_business_function
      AND prompt_type IN ( 'GLOBAL', 'HELPER' )
      AND deleted = @abap_false
      AND version = @lv_version
      ORDER BY prompt_chain ASCENDING.


    ELSE.

      SELECT app_id, business_function, prompt_key, prompt_chain,prompt_chain_enabled, prompt_text, is_signed_off, prompt_type, deletable, deleted, version, external_value,think_link,final_link, model_id, decision_link
      FROM zllm_prompts
      LEFT OUTER JOIN zllm_ext_value ON zllm_prompts~prompt_key = zllm_ext_value~internal_value
      INTO TABLE @lt_prompts
      WHERE app_id = @gt_app_id
      AND business_function = @gt_business_function
      AND prompt_key = @prompt_key
      AND deleted = @abap_false
      AND version = @lv_version
      ORDER BY prompt_chain ASCENDING.

    ENDIF.


    DATA ls_prompt TYPE zllm_prompt_odata.

    LOOP AT lt_prompts ASSIGNING FIELD-SYMBOL(<fs_prompts>).

      ls_prompt-app_id = <fs_prompts>-app_id.
      ls_prompt-business_function = <fs_prompts>-business_function.
      ls_prompt-prompt_key = <fs_prompts>-prompt_key.
      ls_prompt-prompt_chain = <fs_prompts>-prompt_chain.
      ls_prompt-prompt = <fs_prompts>-prompt_text.
      ls_prompt-sign_off = <fs_prompts>-is_signed_off.
      ls_prompt-deletable = <fs_prompts>-deletable.
      ls_prompt-prompt_type = <fs_prompts>-prompt_type.
      ls_prompt-external_name = <fs_prompts>-external_value.
      ls_prompt-prompt_chain_enabled = <fs_prompts>-prompt_chain_enabled.
      ls_prompt-think_link = <fs_prompts>-think_link.
      ls_prompt-final_link = <fs_prompts>-final_link.
      ls_prompt-model_id = <fs_prompts>-model_id.
      ls_prompt-decision_link = <fs_prompts>-decision_link.


* Get the prompt name.
      IF me->get_prompt_name( prompt_key = <fs_prompts>-prompt_key
                              prompt_chain = <fs_prompts>-prompt_chain ) IS NOT INITIAL.

        ls_prompt-prompt_name = me->get_prompt_name( prompt_key   = <fs_prompts>-prompt_key
                                                     prompt_chain = <fs_prompts>-prompt_chain ).

      ENDIF.

* Append to return structure.
      APPEND ls_prompt TO prompt.
      CLEAR ls_prompt.
    ENDLOOP.


  ENDMETHOD.


  METHOD save_prompt.

    DATA: ls_prompt TYPE zllm_prompts,
          lt_prompt TYPE zllm_prompts_tt.


    IF prompt_key IS INITIAL.

* Raise exception. Cannot save as mandatory information is missing.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>prompt_save_failed
          msg1   = 'Missing prompt key.'.

    ELSE.

* Check if this is a child prompt connector. If true, check that bidirectional is enabled.
* If this is a child prompt connector without bidirectional, pass back exception to UI to disable prompt connector

      DATA(lcl_llm_crud_util) = NEW zcl_llm_crud_control_util( app_id            = gt_app_id
                                                               business_function = gt_business_function ).



      ls_prompt-mandt = sy-mandt.
      ls_prompt-app_id = gt_app_id.
      ls_prompt-business_function = gt_business_function.
      ls_prompt-prompt_key = prompt_key.
      ls_prompt-prompt_text = prompt.
      ls_prompt-is_signed_off = is_signed_off.
      ls_prompt-deletable = is_deletable.
      ls_prompt-prompt_type = prompt_type.


* If no version passed, then assume default is active version
      IF version IS INITIAL.

        DATA(zcl_llm_version_util) = NEW zcl_llm_version_control_util( ).
        DATA(versions) = zcl_llm_version_util->get_active_app_version( app_id            = gt_app_id
                                                                       business_function = gt_business_function ).

        ls_prompt-version = versions-active_version.
      ELSE.
        ls_prompt-version = version.
      ENDIF.



      APPEND ls_prompt TO lt_prompt.

      TRY.
          lcl_llm_crud_util->save_prompt( prompt = lt_prompt
                                          commit = abap_true ).

        CATCH zcx_llm INTO DATA(lcx_llm).
* Raise exception.
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.

      ENDTRY.
    ENDIF.


  ENDMETHOD.


  METHOD publish_prompt.

* Check if prompt already exists with this key.
    DATA(prompt) = me->get_prompt( prompt_key = CONV zllm_prompt_key( func_name ) ).

    IF prompt IS INITIAL.
      TRY.
          me->save_prompt( prompt_key = CONV zllm_prompt_key( func_name )
                           prompt_type = 'AGENT'
                           prompt = 'Create your prompt here'
                           is_signed_off = abap_false
                           is_deletable = abap_true ).

        CATCH zcx_llm INTO DATA(lcx_llm).
* Raise exception.
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.
      ENDTRY.

    ELSE.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>prompt_exists
          msg1   = CONV string( func_name ).
    ENDIF.
  ENDMETHOD.

  METHOD get_prompt_connector.

    DATA lt_where_clause TYPE TABLE OF string.

* Get all the entries so we don't hit the DB for all the records
* Get the external value of prompt key
    DATA(ext_value) = me->get_prompt_key_ext_value(  ).

* Get the prompt chain name
    DATA(name) = me->get_prompt_chain_name(  ).

    IF source = 'Parent'.
* If the UI call is from the parent
      IF gt_app_id IS NOT INITIAL.
        APPEND |parent_app_id = { cl_abap_dyn_prg=>quote( gt_app_id ) } AND| TO lt_where_clause.
      ENDIF.

      IF gt_business_function IS NOT INITIAL.
        APPEND |parent_business_function = { cl_abap_dyn_prg=>quote( gt_business_function ) } AND| TO lt_where_clause.
      ENDIF.

      IF prompt_chain IS NOT INITIAL.
        APPEND |parent_prompt_chain = { cl_abap_dyn_prg=>quote( prompt_chain ) } AND| TO lt_where_clause.
      ENDIF.

      IF prompt_key IS NOT INITIAL.
        APPEND |parent_prompt_key = { cl_abap_dyn_prg=>quote( prompt_key ) } AND| TO lt_where_clause.
      ENDIF.

    ELSEIF source = 'Child'.
* If the UI call is from the child
      IF gt_app_id IS NOT INITIAL.
        APPEND |child_app_id = { cl_abap_dyn_prg=>quote( gt_app_id ) } AND| TO lt_where_clause.
      ENDIF.

      IF gt_business_function IS NOT INITIAL.
        APPEND |child_business_function = { cl_abap_dyn_prg=>quote( gt_business_function ) } AND| TO lt_where_clause.
      ENDIF.

      IF prompt_chain IS NOT INITIAL.
        APPEND |child_prompt_chain = { cl_abap_dyn_prg=>quote( prompt_chain ) } AND| TO lt_where_clause.
      ENDIF.

      IF prompt_key IS NOT INITIAL.
        APPEND |child_prompt_key = { cl_abap_dyn_prg=>quote( prompt_key ) } AND| TO lt_where_clause.
      ENDIF.
    ENDIF.

    IF lt_where_clause IS NOT INITIAL.
* Always ignore deleted prompt connectors
      APPEND |deleted = { cl_abap_dyn_prg=>quote( abap_false ) } | TO lt_where_clause.

      SELECT FROM zllm_prompt_conn AS a
       INNER JOIN zllm_prompt_prop AS b
               ON a~uuid = b~uuid
       FIELDS a~uuid,
              a~parent_app_id,
              a~parent_business_function,
              a~parent_prompt_chain,
              a~parent_prompt_key,
              a~child_app_id,
              a~child_business_function,
              a~child_prompt_chain,
              a~child_prompt_key,
              b~bidirectional
        WHERE (lt_where_clause)
        INTO TABLE @DATA(lt_connector).
    ENDIF.

    LOOP AT lt_connector INTO DATA(ls_connector).
      APPEND INITIAL LINE TO prompt_connector ASSIGNING FIELD-SYMBOL(<fs_connector>).
      <fs_connector> = CORRESPONDING #( ls_connector ).

      <fs_connector>-parent_prompt_chain_name    = VALUE #( name[ prompt_key   = ls_connector-parent_prompt_key
                                                               prompt_chain = ls_connector-parent_prompt_chain ]-prompt_name OPTIONAL ).

      <fs_connector>-parent_prompt_key_ext_value = VALUE #( ext_value[ internal_value = ls_connector-parent_prompt_key ]-external_value OPTIONAL ).

      <fs_connector>-child_prompt_chain_name     = VALUE #( name[ prompt_key   = ls_connector-child_prompt_key
                                                              prompt_chain = ls_connector-child_prompt_chain ]-prompt_name OPTIONAL ).

      <fs_connector>-child_prompt_key_ext_value  = VALUE #( ext_value[ internal_value = ls_connector-child_prompt_key ]-external_value OPTIONAL ).

    ENDLOOP.
  ENDMETHOD.


  METHOD get_prompt_key_ext_value.

    SELECT FROM zllm_ext_value
     FIELDS mandt,
            type,
            internal_value,
            lang,
            external_value
          WHERE lang = @sy-langu
          INTO TABLE @prompt_key_ext_value.

  ENDMETHOD.


  METHOD get_prompt_chain_name.

    SELECT FROM zllm_prompt_name
           FIELDS mandt,
                  prompt_key,
                  prompt_chain,
                  prompt_name
           INTO TABLE @prompt_chain_name.

  ENDMETHOD.

  METHOD get_prompt_prop.

* First check if this is parent prompt connector
    prompt_prop = me->check_parent_connector( prompt_key = prompt_key ).

    IF prompt_prop IS INITIAL.
* Otherwise this is should be a child connector
      prompt_prop = me->check_child_connector( prompt_key = prompt_key ).

    ENDIF.


  ENDMETHOD.

  METHOD check_parent_connector.

    SELECT FROM zllm_prompt_conn
         FIELDS parent_app_id AS app_id,
                parent_business_function AS business_function,
                parent_prompt_key AS prompt_key,
                parent_prompt_chain AS prompt_chain
         WHERE  parent_app_id = @gt_app_id
           AND  parent_business_function = @gt_business_function
           AND  parent_prompt_key = @prompt_key
       INTO TABLE @prompt_prop.
    IF sy-subrc EQ 0.

      LOOP AT prompt_prop ASSIGNING FIELD-SYMBOL(<fs_prompt_prop>).
        <fs_prompt_prop>-connected = abap_true.
        <fs_prompt_prop>-child = abap_false.
        <fs_prompt_prop>-bidirectional = abap_true.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD check_child_connector.

    SELECT FROM zllm_prompt_conn
         FIELDS child_app_id AS app_id,
                child_business_function AS business_function,
                child_prompt_key AS prompt_key,
                child_prompt_chain AS prompt_chain
         WHERE  child_app_id = @gt_app_id
           AND  child_business_function = @gt_business_function
           AND  child_prompt_key = @prompt_key
       INTO TABLE @prompt_prop.
    IF sy-subrc EQ 0.

      LOOP AT prompt_prop ASSIGNING FIELD-SYMBOL(<fs_prompt_prop>).
        <fs_prompt_prop>-connected = abap_true.
        <fs_prompt_prop>-child = abap_true.
        <fs_prompt_prop>-bidirectional = abap_true.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD update_prompt_connector_chain.

    DATA: fin_prompt_conn TYPE TABLE OF zllm_prompt_conn,
          lt_parent_conn  TYPE TABLE OF zllm_prompt_conn.

* Fetch the parents connectors
    SELECT FROM zllm_prompt_conn
         FIELDS *
         FOR ALL ENTRIES IN @prompt_connector_chain
         WHERE  parent_app_id = @prompt_connector_chain-app_id
           AND  parent_business_function = @prompt_connector_chain-business_function
         INTO TABLE @lt_parent_conn.
    IF sy-subrc EQ 0.

      LOOP AT prompt_connector_chain INTO DATA(ls_prompt_connector_chain).

        DATA(ls_parent_conn) = VALUE zllm_prompt_conn( lt_parent_conn[ parent_app_id = ls_prompt_connector_chain-app_id
                                                                       parent_business_function = ls_prompt_connector_chain-business_function
                                                                       parent_prompt_key = ls_prompt_connector_chain-prompt_key
                                                                       parent_prompt_chain = ls_prompt_connector_chain-old_prompt_chain ] OPTIONAL ).
        IF ls_parent_conn IS NOT INITIAL.
* Update only the prompt chain which was changed from UI
          ls_parent_conn-parent_prompt_chain = ls_prompt_connector_chain-new_prompt_chain.
          APPEND ls_parent_conn TO fin_prompt_conn.
        ENDIF.
      ENDLOOP.
    ENDIF.


* Fetch the child connectors
    SELECT FROM zllm_prompt_conn
         FIELDS *
         FOR ALL ENTRIES IN @prompt_connector_chain
         WHERE  child_app_id = @prompt_connector_chain-app_id
           AND  child_business_function = @prompt_connector_chain-business_function
         INTO TABLE @DATA(lt_child_conn).
    IF sy-subrc EQ 0.
      LOOP AT prompt_connector_chain INTO ls_prompt_connector_chain.

        DATA(ls_child_conn) = VALUE zllm_prompt_conn( lt_parent_conn[ child_app_id = ls_prompt_connector_chain-app_id
                                                                       child_business_function = ls_prompt_connector_chain-business_function
                                                                       child_prompt_key = ls_prompt_connector_chain-prompt_key
                                                                       child_prompt_chain = ls_prompt_connector_chain-old_prompt_chain ] OPTIONAL ).
        IF ls_child_conn IS NOT INITIAL.
* Update only the prompt chain which was changed from UI
          ls_child_conn-child_prompt_chain = ls_prompt_connector_chain-new_prompt_chain.
          APPEND ls_child_conn TO fin_prompt_conn.
        ENDIF.
      ENDLOOP.
    ENDIF.


    DATA(lcl_llm_crud) = NEW zcl_llm_crud_control_util( app_id            = gt_app_id
                                                        business_function = gt_business_function ).


    TRY.
        lcl_llm_crud->update_prompt_connector_chain( prompt_conn = fin_prompt_conn ).

      CATCH zcx_llm INTO DATA(lcx_llm).
* Raise exception.
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.


    ENDTRY.

  ENDMETHOD.

  METHOD save_prompt_through_connector.

    TYPES: BEGIN OF ty_final_hier,
             app_id            TYPE   zllm_app_id,
             business_function TYPE   zllm_buss_function_name,
             prompt_key        TYPE   zllm_prompt_key,
             prompt_chain      TYPE   zllm_prompt_chain,
           END OF ty_final_hier,
           tt_final_hier TYPE TABLE OF ty_final_hier.

    DATA: lt_final_hier TYPE tt_final_hier,
          ls_final_hier TYPE ty_final_hier,
          prompts       TYPE zllm_prompts_tt.

    LOOP AT prompt INTO DATA(ls_prompt).

      DATA(lt_prop) = me->get_prompt_prop( prompt_key = ls_prompt-prompt_key ).

      DATA(ls_prop) = VALUE zllm_prompt_properties( lt_prop[ app_id = ls_prompt-app_id
                                                             business_function = ls_prompt-business_function
                                                             prompt_key = ls_prompt-prompt_key
                                                             prompt_chain = ls_prompt-prompt_chain ] ).

      IF ls_prop-child = abap_true AND ls_prop-bidirectional = abap_false.
        "Do nothing
      ELSEIF ls_prop-connected = abap_false.
        "Do nothing
      ELSEIF ls_prop-child = abap_true AND ls_prop-bidirectional = abap_true.

        SELECT FROM zllm_prompt_conn
             FIELDS *
              WHERE  child_app_id = @ls_prompt-app_id
                AND  child_business_function = @ls_prompt-business_function
                AND  child_prompt_chain = @ls_prompt-prompt_chain
                AND  child_prompt_key = @ls_prompt-prompt_key
                INTO TABLE @DATA(lt_child_hier).
        IF sy-subrc EQ 0.
          SELECT FROM zllm_prompt_conn
             FIELDS *
             FOR ALL ENTRIES IN @lt_child_hier
              WHERE  parent_app_id = @lt_child_hier-parent_app_id
                AND  parent_business_function = @lt_child_hier-parent_business_function
                AND  parent_prompt_chain = @lt_child_hier-parent_prompt_chain
                AND  parent_prompt_key = @lt_child_hier-parent_prompt_key
                INTO TABLE @DATA(lt_parent_hier).
        ENDIF.
      ELSE.
        SELECT FROM zllm_prompt_conn
             FIELDS *
              WHERE  parent_app_id = @ls_prompt-app_id
                AND  parent_business_function = @ls_prompt-business_function
                AND  parent_prompt_chain = @ls_prompt-prompt_chain
                AND  parent_prompt_key = @ls_prompt-prompt_key
                INTO TABLE @lt_parent_hier.
      ENDIF.


      LOOP AT lt_parent_hier INTO DATA(ls_parent_hier).
        IF sy-index = 1.
          ls_final_hier-app_id = ls_parent_hier-parent_app_id.
          ls_final_hier-business_function = ls_parent_hier-parent_business_function.
          ls_final_hier-prompt_chain = ls_parent_hier-parent_prompt_chain.
          ls_final_hier-prompt_key = ls_parent_hier-parent_prompt_key.
          APPEND ls_final_hier TO lt_final_hier.
          CLEAR ls_final_hier.
        ENDIF.
        ls_final_hier-app_id = ls_parent_hier-child_app_id.
        ls_final_hier-business_function = ls_parent_hier-child_business_function.
        ls_final_hier-prompt_chain = ls_parent_hier-child_prompt_chain.
        ls_final_hier-prompt_key = ls_parent_hier-child_prompt_key.
        APPEND ls_final_hier TO lt_final_hier.
        CLEAR ls_final_hier.
      ENDLOOP.

      SELECT FROM zllm_prompts
      FIELDS *
      FOR ALL ENTRIES IN @lt_final_hier
      WHERE version = @ls_prompt-version
        AND app_id = @lt_final_hier-app_id
        AND business_function = @lt_final_hier-business_function
        AND prompt_chain = @lt_final_hier-prompt_chain
        AND prompt_key = @lt_final_hier-prompt_key
        INTO TABLE @prompts.
    ENDLOOP.

    LOOP AT prompts ASSIGNING FIELD-SYMBOL(<fs_prompts>).

      <fs_prompts>-prompt_text = ls_prompt-prompt_text.

    ENDLOOP.

    TRY.
        DATA(lcl_llm_crud_util) = NEW zcl_llm_crud_control_util( app_id            = gt_app_id
                                                                 business_function = gt_business_function ).

        lcl_llm_crud_util->save_prompt( prompt = prompts
                                        commit = abap_true ).

      CATCH zcx_llm INTO DATA(lcx_llm).
* Raise exception.
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
