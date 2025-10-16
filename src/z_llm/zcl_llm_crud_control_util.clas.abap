CLASS zcl_llm_crud_control_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC

**********************************************************************
* Only allow friends to perform change or related operations.
**********************************************************************
  GLOBAL FRIENDS zcl_llm_version_control_util
                 zcl_fiori_llm_util
                 zcl_llm_prompt_util
                 zcl_llm_agents_util
                 zcl_llm_config_util
                 zcl_llm_auths_util
                 zcl_llm_bot_handler
                 zcl_llm_conversation_util
                 zcl_llm_util
                 zcl_llm_gcs_util
                 zcl_llm_agent_catalog_util
                 zcl_llm_mcp_config_util.


  PUBLIC SECTION.

    METHODS:
      constructor IMPORTING app_id            TYPE zllm_app_id
                            business_function TYPE zllm_buss_function_name.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:

**********************************************************************
* Method to save prompt connector table
**********************************************************************
      save_prompt_connector IMPORTING prompt_connector TYPE zllm_prompt_connector_tt
                            RAISING   zcx_llm,


      save_prompt_properties IMPORTING prompt_prop TYPE zllm_prompt_prop_tt
                             RAISING   zcx_llm,

      update_prompt_connector_chain IMPORTING prompt_conn TYPE zllm_prompt_conn_tt
                                    RAISING   zcx_llm,
**********************************************************************
* Method to update prompt history table
**********************************************************************
      update_prompt_history IMPORTING prompt TYPE zllm_prompts_tt
                            RAISING   zcx_llm,

**********************************************************************
* Method to update prompt name / label
**********************************************************************
      update_prompt_name IMPORTING prompt_name  TYPE zllm_prompt_label
                                   prompt_key   TYPE zllm_prompt_key
                                   prompt_chain TYPE zllm_prompt_chain
                         RAISING   zcx_llm,

**********************************************************************
* Method to save new app version
**********************************************************************
      save_app_version IMPORTING prompt        TYPE zllm_prompts_tt
                                 func_declar   TYPE zllm_func_declar_tt
                                 func_parame   TYPE zllm_func_parame_tt
                                 version_ctrl  TYPE zllm_version_ctr
                                 agent_map     TYPE zllm_agent_map_tt
                       RETURNING VALUE(status) TYPE boolean
                       RAISING   zcx_llm,
      generate_uuid RETURNING VALUE(uuid) TYPE guid16,


**********************************************************************
* Method to save or update prompts
* This includes delete as delete is an indicator field
* External value will allow the bot manager app to display user friendly descriptions
* It is not recomended to leave external_value blank, but it is permitted.
**********************************************************************
      save_prompt IMPORTING VALUE(prompt)  TYPE zllm_prompts_tt
                            external_value TYPE zllm_ext_value_tt OPTIONAL
                            commit         TYPE boolean DEFAULT abap_true
                  EXPORTING prompts_saved  TYPE zllm_prompts_tt
                  RETURNING VALUE(status)  TYPE boolean
                  RAISING   zcx_llm,


**********************************************************************
* Make sure that the version being saved makes sense. e.g.
* If current version is 2, beta version is 3, but attempting to save
* version 0, throw an exception.
**********************************************************************
      check_version IMPORTING version      TYPE zllm_version
                    RETURNING VALUE(valid) TYPE boolean
                    RAISING   zcx_llm,
**********************************************************************
* Method to save or update function declarations
**********************************************************************
      save_function_declaration IMPORTING func_declar    TYPE zllm_func_declar_tt
                                          func_parame    TYPE zllm_func_parame_tt
                                          external_value TYPE zllm_ext_value OPTIONAL
                                          func_index     TYPE zllm_func_index OPTIONAL
                                          agent_catalog  TYPE zllm_agent_cat OPTIONAL
                                          commit         TYPE boolean DEFAULT abap_true
                                RETURNING VALUE(status)  TYPE boolean
                                RAISING   zcx_llm,

**********************************************************************
* Method to save new version control record
**********************************************************************
      save_version_control IMPORTING version_control TYPE zllm_version_ctr
                                     commit          TYPE boolean DEFAULT abap_true
                           RETURNING VALUE(status)   TYPE boolean
                           RAISING   zcx_llm,

**********************************************************************
* Method to change active version.
* THis could be rolling back to a previous version or
* setting a beta version to active.
**********************************************************************
      save_app_active_version IMPORTING version       TYPE zllm_version
                              RETURNING VALUE(status) TYPE boolean
                              RAISING   zcx_llm,

**********************************************************************
* Method to set the passed in version as active.
* This should not be called outside of save_app_active_version
**********************************************************************
      set_active_version IMPORTING version               TYPE zllm_version
                         RETURNING VALUE(active_version) TYPE zllm_version_ctr
                         RAISING   zcx_llm,

**********************************************************************
* Method to remove the current active version
* This is to be called as part of save_app_active_version
**********************************************************************
      set_inactive_version RETURNING VALUE(inactive_version) TYPE zllm_version_ctr
                           RAISING   zcx_llm,

**********************************************************************
* Method to save the config for temperature.
**********************************************************************
      save_config IMPORTING config TYPE zllm_config
                  RAISING   zcx_llm,

**********************************************************************
* Method to create new roles and auths
**********************************************************************
      create_authorisation_role IMPORTING VALUE(role)   TYPE zllm_auths_roles
                                RETURNING VALUE(status) TYPE boolean
                                RAISING   zcx_llm,

**********************************************************************
* Method to create new roles and auths for bot management app.
**********************************************************************
      create_authorisation_tabid IMPORTING VALUE(tabs)   TYPE zllm_auths_tabs
                                 RETURNING VALUE(status) TYPE boolean
                                 RAISING   zcx_llm,
**********************************************************************
* Method to assign a user to a role
**********************************************************************
      assign_role_to_user IMPORTING roleid        TYPE     zllm_auth_role
                                    user          TYPE     uname
                                    deleted       TYPE     zllm_auth_deleted
                          RETURNING VALUE(status) TYPE boolean
                          RAISING   zcx_llm,
**********************************************************************
* Method to create a new bot/change details of an existing bot
**********************************************************************
      save_bot    IMPORTING bot            TYPE zllm_bots
                            external_value TYPE zllm_ext_value_tt OPTIONAL
                            commit         TYPE boolean DEFAULT abap_true
                  RETURNING VALUE(status)  TYPE boolean
                  RAISING   zcx_llm,

**********************************************************************
* Method to save prompt example
**********************************************************************
      save_prompt_example IMPORTING VALUE(prompt_example) TYPE zllm_prompt_eg
                          RETURNING VALUE(status)         TYPE boolean
                          RAISING   zcx_llm,

**********************************************************************
* Method to save user filters
**********************************************************************
      save_user_filter IMPORTING filter_type  TYPE zllm_usr_filter
                                 filter_value TYPE zllm_usr_filter_value
                       RAISING   zcx_llm,

**********************************************************************
* Method to change the model version.
**********************************************************************
      save_model_per_version IMPORTING VALUE(version)  TYPE zllm_version
                                       VALUE(model_id) TYPE zllm_model_id
                             RETURNING VALUE(status)   TYPE boolean
                             RAISING   zcx_llm,

**********************************************************************
* Method to save conversation
**********************************************************************
      save_conversation IMPORTING VALUE(conversation) TYPE string
                        RETURNING VALUE(status)       TYPE boolean
                        RAISING   zcx_llm,

**********************************************************************
* Method to update sub agents
**********************************************************************
      save_sub_agent IMPORTING supv_function_name     TYPE zllm_function_name
                               VALUE(sub_agent)       TYPE zllm_func_declar
                               VALUE(sub_agent_param) TYPE zllm_func_parame
                               agent_map              TYPE zllm_agent_map
                               external_value         TYPE zllm_ext_value OPTIONAL
                               function_index         TYPE zllm_func_index OPTIONAL
                               commit                 TYPE boolean DEFAULT abap_true
                     RETURNING VALUE(status)          TYPE boolean
                     RAISING   zcx_llm,

**********************************************************************
* Update prompt usage to reflect accurate sentiment.
**********************************************************************
      update_prompt_usage IMPORTING guid          TYPE guid
                                    sentiment     TYPE zllm_sentiment OPTIONAL
                                    resolved      TYPE boolean OPTIONAL
                                    resolved_note TYPE string OPTIONAL
                          RETURNING VALUE(status) TYPE boolean,

**********************************************************************
* Rotate log
**********************************************************************
      roate_log IMPORTING start_date    TYPE datum
                          end_date      TYPE datum
                          type          TYPE zllm_log_types_tt
                          commit        TYPE boolean DEFAULT abap_true
                RETURNING VALUE(status) TYPE boolean
                RAISING   zcx_llm,


**********************************************************************
* Save GCS mapping
**********************************************************************
      save_gcs_map IMPORTING VALUE(gcs_map) TYPE zllm_gcs_map
                             commit         TYPE boolean DEFAULT abap_true
                   RETURNING VALUE(status)  TYPE boolean
                   RAISING   zcx_llm,


**********************************************************************
* Save prompt chain to RAG agents
**********************************************************************
      save_prompt_chain_to_rag IMPORTING prompt_to_rag TYPE zllm_rag_odata_tt
                                         commit        TYPE boolean DEFAULT abap_true
                               RETURNING VALUE(status) TYPE boolean
                               RAISING   zcx_llm,

**********************************************************************
* Save RAG agent
**********************************************************************
      save_catalog_map IMPORTING catalog_map   TYPE zllm_catalog_map
                                 commit        TYPE boolean DEFAULT abap_true
                       RETURNING VALUE(status) TYPE boolean
                       RAISING   zcx_llm,

**********************************************************************
* Save RAG agent
**********************************************************************
      save_bot_to_catalog IMPORTING catalog       TYPE zllm_catalog_map
                                    commit        TYPE boolean DEFAULT abap_true
                          RETURNING VALUE(status) TYPE boolean
                          RAISING   zcx_llm,

**********************************************************************
* Save agent to catalog
**********************************************************************
      save_agent_to_catalog IMPORTING catalog       TYPE zllm_agent_cat
                                      commit        TYPE boolean DEFAULT abap_true
                            RETURNING VALUE(status) TYPE boolean
                            RAISING   zcx_llm,

**********************************************************************
* Save RAG class configuration details
**********************************************************************
      save_rag_class_details IMPORTING app_id             TYPE zllm_app_id
                                       business_function  TYPE zllm_buss_function_name
                                       agent_name         TYPE zllm_function_name
                                       class_name         TYPE zllm_class_name
                                       parent_method_name TYPE zllm_method_name
                                       child_method_name  TYPE zllm_method_name
                                       response_structure TYPE zllm_response_structure
                                       deleted            TYPE zllm_deletion_flag
                                       commit             TYPE boolean DEFAULT abap_true
                             RETURNING VALUE(status)      TYPE boolean
                             RAISING   zcx_llm,

      save_agent_map         IMPORTING agent_map     TYPE zllm_agent_map_tt
                                       commit        TYPE boolean DEFAULT abap_true
                             RETURNING VALUE(status) TYPE boolean
                             RAISING   zcx_llm.

**********************************************************************
* Class attributes
**********************************************************************
    DATA: gv_app_id            TYPE zllm_app_id,
          gv_business_function TYPE zllm_buss_function_name,
          gcl_llm_log          TYPE REF TO zcl_llm_log.

ENDCLASS.



CLASS zcl_llm_crud_control_util IMPLEMENTATION.


  METHOD constructor.

    gv_app_id = app_id.
    gv_business_function = business_function.

    gcl_llm_log = NEW zcl_llm_log( app_id            = app_id
                                   business_function = business_function ).

  ENDMETHOD.


  METHOD save_prompt_connector.

    DATA: lt_prompt_conn TYPE TABLE OF zllm_prompt_conn,
          lt_prompt_prop TYPE TABLE OF zllm_prompt_prop.

    LOOP AT prompt_connector INTO DATA(ls_prompt_connector).

* Log entry in prompt connector table
      APPEND INITIAL LINE TO lt_prompt_conn ASSIGNING FIELD-SYMBOL(<fs_prompt_conn>).
      <fs_prompt_conn>       = CORRESPONDING #( ls_prompt_connector ).
      <fs_prompt_conn>-mandt = sy-mandt.

* Log entry in prompt properties table
      APPEND INITIAL LINE TO lt_prompt_prop ASSIGNING FIELD-SYMBOL(<fs_prompt_prop>).
      <fs_prompt_prop>-mandt                   = sy-mandt.
      "Maintaining here the foreign key relationship
      <fs_prompt_prop>-uuid                    = ls_prompt_connector-uuid.
      <fs_prompt_prop>-bidirectional           = ls_prompt_connector-bidirectional.

* Create or update entries based on UUID
      IF ls_prompt_connector-uuid IS INITIAL.
        <fs_prompt_conn>-uuid  = me->generate_uuid(  ).
        <fs_prompt_prop>-uuid  = <fs_prompt_conn>-uuid.
      ENDIF.
    ENDLOOP.

    MODIFY zllm_prompt_conn FROM TABLE lt_prompt_conn.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      gcl_llm_log->write_general_log( object_type = 'SAVE PROMPT CONN'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'SUCCESS'
                                      new_value   = '' ).

    ELSE.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>prompt_connector_failed.

    ENDIF.

* Save the properties of the prompt connector
    me->save_prompt_properties( prompt_prop = lt_prompt_prop ).

  ENDMETHOD.

  METHOD save_prompt_properties.

    MODIFY zllm_prompt_prop FROM TABLE prompt_prop.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      gcl_llm_log->write_general_log( object_type = 'PROMPT PROPERTIES'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'SUCCESS'
                                      new_value   = '' ).

    ELSE.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>prompt_properties_failed.

    ENDIF.

  ENDMETHOD.

  METHOD update_prompt_connector_chain.

    MODIFY zllm_prompt_conn FROM TABLE prompt_conn.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      gcl_llm_log->write_general_log( object_type = 'UPDATE PROMPT CONN'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'SUCCESS'
                                      new_value   = '' ).

    ELSE.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>prompt_connector_failed.

    ENDIF.

  ENDMETHOD.

  METHOD update_prompt_name.

    DATA: ls_prompt_name TYPE zllm_prompt_name.

    ls_prompt_name-mandt = sy-mandt.
    ls_prompt_name-prompt_chain = prompt_chain.
    ls_prompt_name-prompt_key = prompt_key.
    ls_prompt_name-prompt_name = prompt_name.


    MODIFY zllm_prompt_name FROM ls_prompt_name.


    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      gcl_llm_log->write_general_log( object_type = 'PROMPT NAME'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'SUCCESS'
                                      new_value   = CONV string( prompt_name ) ).

    ELSE.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>prompt_name_failed
          msg1   = CONV string( prompt_name ).

    ENDIF.

  ENDMETHOD.

  METHOD save_rag_class_details.

    DATA: ls_rag_config TYPE zllm_rag_config.

    ls_rag_config-mandt = sy-mandt.
    ls_rag_config-app_id = app_id.
    ls_rag_config-business_function = business_function.
    ls_rag_config-agent_name = agent_name.
    ls_rag_config-class_name = class_name.
    ls_rag_config-parent_method_name = parent_method_name.
    ls_rag_config-child_method_name = child_method_name.
    ls_rag_config-response_structure = response_structure.
    ls_rag_config-deleted = deleted.


    MODIFY zllm_rag_config FROM ls_rag_config.

    IF sy-subrc = 0.
      IF commit = abap_true.
        COMMIT WORK AND WAIT.
      ENDIF.

      status = abap_true.

      gcl_llm_log->write_general_log( object_type = 'RAG CLASS'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'SUCCESS'
                                      new_value   = |{ agent_name } - { class_name } -> { parent_method_name }->{ child_method_name }->{ response_structure }| ).


    ELSE.

      ROLLBACK WORK.
      status = abap_false.

      gcl_llm_log->write_general_log( object_type = 'RAG CLASS'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'FAILED'
                                      new_value   = |{ agent_name } - { class_name } -> { parent_method_name }->{ child_method_name }| ).

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>save_rag_class_failed
          msg1   = CONV string( class_name )
          msg2   = |{ parent_method_name }-{ child_method_name }|.

    ENDIF.


  ENDMETHOD.


  METHOD save_agent_to_catalog.

    MODIFY zllm_agent_cat FROM catalog.

    IF sy-subrc = 0.
      IF commit = abap_true.
        COMMIT WORK AND WAIT.


        gcl_llm_log->write_general_log( object_type = 'RAG CATALOG'
                                        object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                        sentiment   = 'SUCCESS'
                                        new_value   = |{ catalog-catalog_name } -> { catalog-catalog_app_id }| ).

      ENDIF.
      status = abap_true.

    ELSE.
      ROLLBACK WORK.
      status = abap_false.

      gcl_llm_log->write_general_log( object_type = 'RAG CATALOG'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'FAILED'
                                      new_value   = |{ catalog-catalog_name } -> { catalog-catalog_app_id }| ).

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>save_rag_catalog_failed
          msg1   = CONV string( catalog-catalog_app_id ).

    ENDIF.



  ENDMETHOD.

  METHOD save_bot_to_catalog.

    MODIFY zllm_catalog_map FROM catalog.

    IF sy-subrc = 0.
      IF commit = abap_true.
        COMMIT WORK AND WAIT.
        status = abap_true.


        gcl_llm_log->write_general_log( object_type = 'RAG CATALOG'
                                        object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                        sentiment   = 'SUCCESS'
                                        new_value   = |{ catalog-app_id } -> { catalog-catalog_app_id }| ).

      ENDIF.


    ELSE.
      ROLLBACK WORK.
      status = abap_false.

      gcl_llm_log->write_general_log( object_type = 'PROMPT CHAIN'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'FAILED'
                                      new_value   = |{ catalog-app_id } -> { catalog-catalog_app_id }| ).

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>save_rag_catalog_failed
          msg1   = CONV string( catalog-catalog_app_id ).

    ENDIF.


  ENDMETHOD.

  METHOD save_catalog_map.


    MODIFY zllm_catalog_map FROM catalog_map.

    IF sy-subrc = 0.
      IF commit = abap_true.
        COMMIT WORK AND WAIT.
        status = abap_true.


        gcl_llm_log->write_general_log( object_type = 'RAG CATALOG'
                                        object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                        sentiment   = 'SUCCESS'
                                        new_value   = |{ catalog_map-catalog_app_id } -> { catalog_map-catalog_business_function }| ).

      ENDIF.


    ELSE.
      ROLLBACK WORK.
      status = abap_false.

      gcl_llm_log->write_general_log( object_type = 'PROMPT CHAIN'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'FAILED'
                                      new_value   = |{ catalog_map-catalog_app_id } -> { catalog_map-catalog_business_function }| ).

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>save_rag_catalog_failed
          msg1   = CONV string( catalog_map-catalog_app_id ).

    ENDIF.



  ENDMETHOD.

  METHOD save_prompt_chain_to_rag.

    DATA(lcl_version_control) = NEW zcl_llm_version_control_util(  ).

    DATA(versions) = lcl_version_control->get_active_app_version( app_id            = gv_app_id
                                                                  business_function = gv_business_function ).


    DATA: ls_agent_map TYPE zllm_agent_map,
          lt_agent_map TYPE TABLE OF zllm_agent_map.


    LOOP AT prompt_to_rag ASSIGNING FIELD-SYMBOL(<fs_prompt_to_rag>).

      ls_agent_map-mandt = sy-mandt.
      ls_agent_map-app_id  = <fs_prompt_to_rag>-app_id.
      ls_agent_map-business_function  = <fs_prompt_to_rag>-business_function.
      ls_agent_map-parent = <fs_prompt_to_rag>-agent. "The prompt link
      ls_agent_map-child = <fs_prompt_to_rag>-child. " The RAG agent
      ls_agent_map-type = 'RAG'.
      ls_agent_map-deletion_indicator = <fs_prompt_to_rag>-delete.


*If there is a beta version. Save to Beta.
      IF versions-beta_version IS NOT INITIAL.
        ls_agent_map-version = versions-beta_version.
      ELSE.
        ls_agent_map-version = versions-active_version.
      ENDIF.


      APPEND ls_agent_map TO lt_agent_map.


      gcl_llm_log->write_general_log( object_type = 'PROMPT CHAIN'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'PREPARED'
                                      new_value   = |{ <fs_prompt_to_rag>-agent } - { <fs_prompt_to_rag>-child }| ).

    ENDLOOP.


    MODIFY zllm_agent_map FROM TABLE lt_agent_map.

    IF sy-subrc = 0.
      IF commit = abap_true.
        COMMIT WORK AND WAIT.
        status = abap_true.


        gcl_llm_log->write_general_log( object_type = 'PROMPT CHAIN'
                                        object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                        sentiment   = 'SUCCESS'
                                        new_value   = |{ ls_agent_map-parent }->{ ls_agent_map-child } ; { COND char20( WHEN ls_agent_map-deletion_indicator = 'X' THEN 'Relationship deleted' ) }| ).

      ENDIF.


    ELSE.
      ROLLBACK WORK.
      status = abap_false.

      gcl_llm_log->write_general_log( object_type = 'PROMPT CHAIN'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'FAILED' ).

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>save_agent_map_failed.

    ENDIF.


  ENDMETHOD.

  METHOD save_gcs_map.

* Assign unique key.
    gcs_map-uuid = me->generate_uuid(  ).
    gcs_map-app_id = me->gv_app_id.
    gcs_map-business_function = me->gv_business_function.

    MODIFY zllm_gcs_map FROM gcs_map.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      status = abap_true.

      gcl_llm_log->write_general_log( object_type = 'SAVE GCS MAP'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'SUCCESS'
                                      new_value   = |{ gcs_map-agent } - { gcs_map-agent }| ).

    ELSE.
      ROLLBACK WORK.
      status = abap_false.

      gcl_llm_log->write_general_log( object_type = 'SAVE GCS MAP'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'FAILED'
                                      new_value   = |{ gcs_map-agent } - { gcs_map-agent }| ).

* Boo ....
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>save_gcs_failed
          msg1   = CONV string( gcs_map-agent ).
    ENDIF.


  ENDMETHOD.

  METHOD roate_log.

    IF type IS NOT INITIAL.

      DELETE FROM zllm_log
      WHERE dat BETWEEN start_date AND end_date
      AND object_type IN type.


      IF commit = abap_true.
        COMMIT WORK AND WAIT.
        status = abap_true.
      ENDIF.

    ELSE.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>invalid_log_deletion_config.

    ENDIF.

  ENDMETHOD.

  METHOD update_prompt_usage.

    SELECT SINGLE *
    FROM zllm_log
    INTO @DATA(ls_log)
    WHERE uuid = @guid.

    ls_log-sentiment = sentiment.
    ls_log-resolved = resolved.
    ls_log-resolved_note = resolved_note.

    MODIFY zllm_log FROM ls_log.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      status = abap_true.

    ELSE.
      ROLLBACK WORK.
      status = abap_false.
    ENDIF.


  ENDMETHOD.

  METHOD save_sub_agent.

*** Global rules:
*   -- 1. When a sub agent is saved against supervisor, the parent function's type has to be saved as 'SUPERVISOR'

    IF sub_agent IS NOT INITIAL.

      SELECT SINGLE *
        FROM zllm_func_declar
        INTO @DATA(ls_supervisor_agent)
       WHERE app_id = @sub_agent-app_id
         AND business_function = @sub_agent-business_function
         AND function_name = @supv_function_name
         AND version = @sub_agent-version.

      IF sy-subrc = 0.

        ls_supervisor_agent-type = 'SUPERVISOR'.

      ENDIF.

***   --Save Sub agent declaration--
*     Update the sub agent
      MODIFY zllm_func_declar FROM sub_agent.

      IF sy-subrc <> 0.
        ROLLBACK WORK.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>save_sub_agent_failed
            msg1   = CONV string( gv_app_id )
            msg2   = CONV string( gv_business_function ).

      ENDIF.

***   --Save Sub agent parameters--
      IF sub_agent_param IS NOT INITIAL.

        MODIFY zllm_func_parame FROM sub_agent_param.

        IF sy-subrc <> 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>save_sub_agent_failed
              msg1   = CONV string( gv_app_id )
              msg2   = CONV string( gv_business_function ).

        ENDIF.

      ENDIF.

*       Save the external name of the sub function/agent
      IF external_value IS NOT INITIAL.

        MODIFY zllm_ext_value FROM external_value.

        IF sy-subrc <> 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>save_ext_name_failed
              msg1   = CONV string( gv_app_id )
              msg2   = CONV string( gv_business_function ).

        ENDIF.

      ENDIF.

      IF agent_map IS NOT INITIAL.

        MODIFY zllm_agent_map FROM agent_map.

        IF sy-subrc <> 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>save_agent_map_failed
              msg1   = CONV string( agent_map-parent )
              msg2   = CONV string( agent_map-child ).

        ENDIF.

      ENDIF.

      IF ls_supervisor_agent IS NOT INITIAL.

        MODIFY zllm_func_declar FROM ls_supervisor_agent.

        IF sy-subrc <> 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>save_func_delcar_failed
              msg1   = CONV string( ls_supervisor_agent-app_id )
              msg2   = CONV string( ls_supervisor_agent-business_function ).

        ENDIF.


      ENDIF.


      IF function_index IS NOT INITIAL.

        MODIFY zllm_func_index FROM function_index.

        IF sy-subrc <> 0.
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>save_func_index_failed
              msg1   = CONV string( function_index-function_name ).

        ENDIF.

      ENDIF.

*       No exception by this point, assume it went well.
      IF commit = abap_true.
        COMMIT WORK AND WAIT.
        status = abap_true.
      ENDIF.


**********************************************************************
* Logging
**********************************************************************

      gcl_llm_log->write_general_log( object_type = 'SAVE SUB FUNC CALL'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'SUCCESS'
                                      new_value   = |{ sub_agent-function_name } - { sub_agent-function_description }| ).


      gcl_llm_log->write_general_log( object_type = 'SAVE SUB FUNC PARAME'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'SUCCESS'
                                      new_value   = |{ sub_agent_param-parameter_name } - { sub_agent_param-parameter_description }| ).

      gcl_llm_log->write_general_log( object_type = 'SAVE SUB FUNC EXTNAM'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'SUCCESS'
                                      new_value   = |{ sub_agent-function_name } - { external_value-external_value }| ).


      gcl_llm_log->write_general_log( object_type = 'SAVE AGENT MAP'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'SUCCESS'
                                      new_value   = |{ agent_map-parent } - { agent_map-child }| ).

    ELSE.

      RAISE EXCEPTION TYPE zcx_llm.

    ENDIF.


  ENDMETHOD.


  METHOD save_conversation.


    DATA: ls_conversation TYPE zllm_conv.

    ls_conversation-mandt = sy-mandt.
    ls_conversation-uuid = me->generate_uuid(  ).
    ls_conversation-app_id = gv_app_id.
    ls_conversation-business_function = gv_business_function.
    ls_conversation-conversation = conversation.
    ls_conversation-dat = sy-datum.
    ls_conversation-time = sy-uzeit.
    ls_conversation-user_name = sy-uname.


    MODIFY zllm_conv FROM ls_conversation.

    IF sy-subrc <> 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>save_conversation_failed
          msg1   = CONV string( sy-uname ).

    ELSE.
      COMMIT WORK AND WAIT.
      status = abap_true.

    ENDIF.



  ENDMETHOD.

  METHOD save_user_filter.

    DATA: ls_user_filter TYPE zllm_user_filter.

    ls_user_filter-mandt = sy-mandt.
    ls_user_filter-filter = filter_type.
    ls_user_filter-user_name = sy-uname.
    ls_user_filter-value = filter_value.
    ls_user_filter-app_id = gv_app_id.
    ls_user_filter-business_function = gv_business_function.

    MODIFY zllm_user_filter FROM ls_user_filter.

    IF sy-subrc <> 0.

      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>user_filter_save_failed
          msg1   = CONV string( sy-uname ).
    ELSE.

      COMMIT WORK AND WAIT.

    ENDIF.


    CLEAR: ls_user_filter.


  ENDMETHOD.

  METHOD save_prompt_example.

    prompt_example-mandt = sy-mandt.

    IF prompt_example-uuid IS INITIAL.

      prompt_example-uuid = me->generate_uuid(  ).

    ENDIF.

* Update the prompt example table.
    MODIFY zllm_prompt_eg FROM prompt_example.

    IF sy-subrc <> 0.
      ROLLBACK WORK.
      status = abap_false.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>failed_to_update_prompt_eg.

    ELSE.
      COMMIT WORK AND WAIT.
      status = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD assign_role_to_user.


    DATA ls_role TYPE zllm_auths.

    ls_role-mandt = sy-mandt.
    ls_role-roleid = roleid.
    ls_role-user_name = user.
    ls_role-deleted = deleted.

    MODIFY zllm_auths FROM ls_role.

    IF sy-subrc <> 0.

      ROLLBACK WORK.
      status = abap_false.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>failed_to_update_user_role
          msg1   = CONV string( user ).

    ELSE.

      COMMIT WORK AND WAIT.
      status = abap_true.


    ENDIF.

  ENDMETHOD.


  METHOD create_authorisation_role.


    role-roleid = me->generate_uuid( ).

    MODIFY zllm_auths_roles FROM role.

    IF sy-subrc <> 0.

      ROLLBACK WORK.
      status = abap_false.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>failed_to_update_role
          msg1   = CONV string( role-roleid ).

    ELSE.
      COMMIT WORK AND WAIT.
      status = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD save_config.

    MODIFY zllm_config FROM config.

    IF sy-subrc = 0.

      COMMIT WORK AND WAIT.

    ELSE.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>config_save_failed
          msg1   = CONV string( gv_app_id )
          msg2   = CONV string( gv_business_function )
          msg3   = CONV string( config-param ).
    ENDIF.

  ENDMETHOD.



  METHOD save_version_control.

    IF version_control IS NOT INITIAL.

      MODIFY zllm_version_ctr FROM version_control.

      IF sy-subrc = 0.
        IF commit = abap_true.
          COMMIT WORK AND WAIT.
        ENDIF.

      ELSE.
* It didnt work. Rollback
        ROLLBACK WORK.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>version_control_record_failed.
      ENDIF.


    ENDIF.


  ENDMETHOD.


  METHOD check_version.

    DATA(lcl_version_control) = NEW zcl_llm_version_control_util(  ).

    DATA(versions) = lcl_version_control->get_active_app_version( app_id            = gv_app_id
                                                                  business_function = gv_business_function ).

* First check if this is a beta version.
    IF versions-beta_version = version.
* Allow
      RETURN.
* Now check if this is saving to the activeversion.
    ELSEIF versions-active_version = version.
* Allow
      RETURN.
* Now check if this is a Beta version. If so it should be version > than the active version and Beta should be blank.
    ELSEIF version > versions-active_version AND
           versions-beta_version = 0.
* Allow
      RETURN.
    ELSE.
* We are attempting to save a version that does not match beta or active. Raise exception.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>version_miss_match
          msg1   = CONV string( version ).
    ENDIF.



  ENDMETHOD.


  METHOD set_active_version.

* Select the provided version and set the active flag.
    SELECT SINGLE *
    FROM zllm_version_ctr
    INTO @active_version
    WHERE app_id = @gv_app_id
    AND business_function = @gv_business_function
    AND version = @version
    AND version_active = @abap_false.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>invalid_version
          msg1   = CONV string( version ).

    ENDIF.


    active_version-version_active = abap_true.
    active_version-dat = sy-datum.
    active_version-time = sy-uzeit.
    active_version-usr = sy-uname.

* All active app versions must be GLOBAL table
    IF active_version-version_type = 'BETA'.
      active_version-version_type = 'GLOBAL'.
    ENDIF.

  ENDMETHOD.


  METHOD set_inactive_version.

* Identify the current version of the app / business function and set the active flag as false.
    DATA(lcl_version_control) = NEW zcl_llm_version_control_util(  ).
    DATA(active_version) = lcl_version_control->get_active_app_version( app_id            = gv_app_id
                                                                        business_function = gv_business_function ).

    SELECT SINGLE *
    FROM zllm_version_ctr
    INTO @inactive_version
    WHERE app_id = @gv_app_id
    AND business_function = @gv_business_function
    AND version = @active_version-active_version
    AND version_active = @abap_true.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>failed_to_determine_version
          msg1   = CONV string( gv_app_id )
          msg2   = CONV string( gv_business_function ).
    ENDIF.

* Set properties for inactive version
    inactive_version-version_active = abap_false.
    inactive_version-dat = sy-datum.
    inactive_version-time = sy-uzeit.
    inactive_version-usr = sy-uname.

  ENDMETHOD.

  METHOD save_app_active_version.


* Remove active version.
    DATA(ls_inactive_version) = me->set_inactive_version(  ).

* Set the active version.
    DATA(ls_active_version) = me->set_active_version( version = version ).

    DATA: lt_version_change TYPE TABLE OF zllm_version_ctr.

    APPEND ls_inactive_version TO lt_version_change.
    APPEND ls_active_version TO lt_version_change.


* Update table for both active / inactive.
    MODIFY zllm_version_ctr FROM TABLE lt_version_change.

    IF sy-subrc <> 0.
      ROLLBACK WORK.
      status = abap_false.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>version_update_failed
          msg1   = CONV string( gv_app_id )
          msg2   = CONV string( gv_business_function )
          msg3   = CONV string( version ).

    ELSE.

      COMMIT WORK AND WAIT.
      status = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD save_prompt.

    TRY.
        LOOP AT prompt ASSIGNING FIELD-SYMBOL(<fs_prompts>).
* Ensure there is no version miss match on saving.
          me->check_version( version = <fs_prompts>-version ).

* Also assign a chain sequence if its blank.
          IF <fs_prompts>-prompt_chain = 0 AND <fs_prompts>-prompt_manual_chain = abap_false.


*   Get the prompt chain number
            CALL FUNCTION 'NUMBER_GET_NEXT'
              EXPORTING
                nr_range_nr             = '01'
                object                  = 'ZLLM_CHAIN'
              IMPORTING
                number                  = <fs_prompts>-prompt_chain
              EXCEPTIONS
                interval_not_found      = 1
                number_range_not_intern = 2
                object_not_found        = 3
                quantity_is_0           = 4
                quantity_is_not_1       = 5
                interval_overflow       = 6
                buffer_overflow         = 7
                OTHERS                  = 8.


            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zcx_llm
                EXPORTING
                  textid = zcx_llm=>version_interval_failed
                  msg1   = CONV string( <fs_prompts>-app_id )
                  msg2   = CONV string( <fs_prompts>-business_function ).
            ENDIF.


          ENDIF.

        ENDLOOP.

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.

* Update prompt history table before updating.
    me->update_prompt_history( prompt = prompt ).


* Update prompt table
    MODIFY zllm_prompts FROM TABLE prompt.

    IF external_value IS NOT INITIAL.
* Update external name.
      MODIFY zllm_ext_value FROM TABLE external_value.
    ENDIF.

    IF sy-subrc <> 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>save_prompt_failed
          msg1   = CONV string( gv_app_id )
          msg2   = CONV string( gv_business_function ).

    ELSE.

      IF commit = abap_true.
        COMMIT WORK AND WAIT.
        status = abap_true.
      ENDIF.

* Make a general log entry.
      LOOP AT prompt ASSIGNING FIELD-SYMBOL(<fs_prompt>).


        gcl_llm_log->write_general_log( object_type = 'SAVE PROMPT'
                                        object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                        sentiment   = 'SUCCESS'
                                        new_value   = |{ <fs_prompt>-prompt_key } - { <fs_prompt>-prompt_text }| ).

      ENDLOOP.


    ENDIF.

*   Exporting the amended input, to read the generated prompt chain numbers outside
    prompts_saved = prompt.


  ENDMETHOD.


  METHOD save_function_declaration.

    MODIFY zllm_func_declar FROM TABLE func_declar.

    IF sy-subrc <> 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>save_func_delcar_failed
          msg1   = CONV string( gv_app_id )
          msg2   = CONV string( gv_business_function ).

    ENDIF.

    IF func_parame IS NOT INITIAL.

      MODIFY zllm_func_parame FROM TABLE func_parame.

      IF sy-subrc <> 0.
        ROLLBACK WORK.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>save_func_delcar_failed
            msg1   = CONV string( gv_app_id )
            msg2   = CONV string( gv_business_function ).

      ENDIF.

    ENDIF.

*   Save the external name of the function/agent
    IF external_value IS NOT INITIAL.

      MODIFY zllm_ext_value FROM external_value.

      IF sy-subrc <> 0.
        ROLLBACK WORK.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>save_ext_name_failed
            msg1   = CONV string( external_value-internal_value ).

      ENDIF.

    ENDIF.

*   Save index for the function/agent
    IF func_index IS NOT INITIAL.

      MODIFY zllm_func_index FROM func_index.

      IF sy-subrc <> 0.
        ROLLBACK WORK.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>save_func_index_failed
            msg1   = CONV string( func_index-function_name ).

      ENDIF.

    ENDIF.

* No exception by this point, assume it went well.
    IF commit = abap_true.
      COMMIT WORK AND WAIT.
      status = abap_true.
    ENDIF.

* Make a general log entry.
    LOOP AT func_declar ASSIGNING FIELD-SYMBOL(<func_declar>).


      gcl_llm_log->write_general_log( object_type = 'SAVE FUNCTION CALL'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'SUCCESS'
                                      new_value   = |{ <func_declar>-function_name } - { <func_declar>-function_description }| ).

* Get the corresponding parame changes.
      TRY.
          DATA(ls_parame_change) = func_parame[ parameters_id = <func_declar>-function_parameters_id ].

          gcl_llm_log->write_general_log( object_type = 'SAVE FUNCTION PARAME'
                                          object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                          sentiment   = 'SUCCESS'
                                          new_value   = |{ ls_parame_change-parameter_name } - { ls_parame_change-parameter_description }| ).
        CATCH cx_sy_itab_line_not_found.
* If line not found, thats ok
      ENDTRY.

    ENDLOOP.


  ENDMETHOD.


  METHOD save_app_version.

    TRY.
* Save prompts.
        me->save_prompt( prompt = prompt
                         commit = abap_false ).

* Save the function declaration
        me->save_function_declaration( func_declar = func_declar
                                       func_parame = func_parame
*                                       external_value = external_value
                                       commit      = abap_false ).

* Save agent map
        me->save_agent_map( agent_map = agent_map
                            commit    = abap_false ).

* Save version control.
        me->save_version_control( version_control = version_ctrl
                                  commit          = abap_false ).
* Commit all changes
        COMMIT WORK AND WAIT.

* Generate a general log entry
        gcl_llm_log->write_general_log( object_type = 'SAVE APP VERSION'
                                        object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                        sentiment   = 'SUCCESS'
                                        new_value   = CONV zllm_new_value( me->gv_business_function ) ).


        status = abap_true.
      CATCH zcx_llm INTO DATA(lcx_llm).
* If an exception was thrown, roll back
        ROLLBACK WORK.

        status = abap_false.

* Generate a general log entry
        gcl_llm_log->write_general_log( object_type = 'SAVE APP VERSION'
                                        object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                        sentiment   = 'FAIL' ).


        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.


    ENDTRY.

  ENDMETHOD.


  METHOD update_prompt_history.

    DATA: lt_prompt_history TYPE TABLE OF zllm_prompt_hist,
          ls_prompt_history TYPE zllm_prompt_hist.

    IF prompt IS NOT INITIAL.

      SELECT *
      FROM zllm_prompts
      FOR ALL ENTRIES IN @prompt
      WHERE prompt_key = @prompt-prompt_key
      AND app_id = @prompt-app_id
      AND business_function = @prompt-business_function
      AND version = @prompt-version
      INTO TABLE @DATA(lt_old_prompt).


* Loop at the results, updating records for the prompt history table.

      LOOP AT lt_old_prompt ASSIGNING FIELD-SYMBOL(<fs_old_prompt>).


        ls_prompt_history-mandt = sy-mandt.
        ls_prompt_history-uuid = me->generate_uuid(  ).
        ls_prompt_history-app_id = <fs_old_prompt>-app_id.
        ls_prompt_history-business_function = <fs_old_prompt>-business_function.
        ls_prompt_history-prompt_chain = <fs_old_prompt>-prompt_chain.
        ls_prompt_history-is_signed_off = <fs_old_prompt>-is_signed_off.
        ls_prompt_history-prompt_key = <fs_old_prompt>-prompt_key.
        ls_prompt_history-prompt_text = <fs_old_prompt>-prompt_text.
        ls_prompt_history-prompt_type = <fs_old_prompt>-prompt_type.
        ls_prompt_history-prompt_chain_enabled = <fs_old_prompt>-prompt_chain_enabled.
        ls_prompt_history-prompt_manual_chain = <fs_old_prompt>-prompt_manual_chain.
        ls_prompt_history-think_link = <fs_old_prompt>-think_link.
        ls_prompt_history-final_link = <fs_old_prompt>-final_link.
        ls_prompt_history-usr = sy-uname.
        ls_prompt_history-dat = sy-datum.
        ls_prompt_history-time = sy-uzeit.


* Generate a general log entry
        gcl_llm_log->write_general_log( object_type = 'PROMPT HISTORY'
                                        object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                        sentiment   = 'PREPARE'
                                        new_value   = CONV zllm_new_value( ls_prompt_history-prompt_key ) ).

        APPEND ls_prompt_history TO lt_prompt_history.

      ENDLOOP.

      IF lt_prompt_history IS NOT INITIAL.

        MODIFY zllm_prompt_hist FROM TABLE lt_prompt_history.

        IF sy-subrc <> 0.
* Buggar it....
          ROLLBACK WORK.

          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>save_prompt_history_failed
              msg1   = CONV string( gv_app_id )
              msg2   = CONV string( gv_business_function ).

        ELSE.

          COMMIT WORK AND WAIT.

* Generate a general log entry
          gcl_llm_log->write_general_log( object_type = 'PROMPT HISTORY'
                                          object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                          sentiment   = 'SUCCESS'
                                          new_value   = 'Prompt history updated' ).

        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD generate_uuid.

    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = uuid.

  ENDMETHOD.

  METHOD save_bot.

    IF bot IS NOT INITIAL.

      MODIFY zllm_bots FROM bot.

      IF sy-subrc <> 0.

        ROLLBACK WORK.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>bot_creation_failed
            msg1   = CONV string( bot-app_id ).

*     Not forcing a commit yet as another save operation needs to happen after this
      ELSE.

        IF external_value IS NOT INITIAL.

*         Update external name.
          MODIFY zllm_ext_value FROM TABLE external_value.


          IF sy-subrc <> 0.

            ROLLBACK WORK.

            RAISE EXCEPTION TYPE zcx_llm
              EXPORTING
                textid = zcx_llm=>save_prompt_failed
                msg1   = CONV string( gv_app_id )
                msg2   = CONV string( gv_business_function ).

          ELSE.

            IF commit = abap_true.
              COMMIT WORK.
              status = abap_true.
            ENDIF.

*           Logging
            DATA(lref_llm_log_util) = NEW zcl_llm_log( app_id            = gv_app_id
                                                       business_function = gv_business_function ).

            IF bot-deleted IS INITIAL.
              DATA(log_value) = |{ gv_app_id }-{ gv_business_function }-{ external_value[ 1 ]-external_value }-Enabled:{ bot-enabled }-{ sy-uname }|.
            ELSE.
              log_value = |{ gv_app_id }-{ gv_business_function }-{ external_value[ 1 ]-external_value }-Enabled:{ bot-enabled }-Deleted:{ bot-deleted }-{ sy-uname }|.
            ENDIF.

            lref_llm_log_util->write_general_log( object_type = 'SAVE_BOT'
                                                  object_key  = |App: { gv_app_id }, Business function: { gv_business_function }|
                                                  sentiment   = 'SUCCESS'
                                                  new_value   = log_value ).

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD save_model_per_version.

    SELECT SINGLE *
    FROM zllm_version_ctr
    INTO @DATA(ls_version)
    WHERE business_function = @gv_business_function
    AND app_id = @gv_app_id
    AND version = @version.

    ls_version-model_id = model_id.

* Update table
    MODIFY zllm_version_ctr FROM ls_version.

    IF sy-subrc <> 0.
      ROLLBACK WORK.
      status = abap_false.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>version_update_failed
          msg1   = CONV string( gv_app_id )
          msg2   = CONV string( gv_business_function )
          msg3   = CONV string( version ).

    ELSE.

      COMMIT WORK AND WAIT.
      status = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD save_agent_map.

*   Save agent map
    IF agent_map IS NOT INITIAL.

      MODIFY zllm_agent_map FROM TABLE agent_map.

      IF sy-subrc <> 0.
        ROLLBACK WORK.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>save_agent_map_failed.

      ENDIF.

    ENDIF.

* No exception by this point, assume it went well.
    IF commit = abap_true.
      COMMIT WORK AND WAIT.
      status = abap_true.
    ENDIF.

* Make a general log entry.
    LOOP AT agent_map ASSIGNING FIELD-SYMBOL(<agent_map>).


      gcl_llm_log->write_general_log( object_type = 'SAVE AGENT MAP'
                                      object_key  = |App: { me->gv_app_id }, Business function: { me->gv_business_function }|
                                      sentiment   = 'SUCCESS'
                                      new_value   = |{ <agent_map>-parent }->{ <agent_map>-child }| ).

    ENDLOOP.

  ENDMETHOD.

  METHOD create_authorisation_tabid.

    MODIFY zllm_auths_tabs FROM tabs.

    IF sy-subrc <> 0.

      ROLLBACK WORK.
      status = abap_false.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>failed_to_update_role
          msg1   = CONV string( tabs-tab_id ).

    ELSE.
      COMMIT WORK AND WAIT.
      status = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
