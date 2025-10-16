CLASS zcl_fiori_llm_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS: constructor IMPORTING app_id            TYPE zllm_app_id OPTIONAL
                                   business_function TYPE zllm_buss_function_name OPTIONAL
                                   temperature       TYPE zllm_temperature OPTIONAL
                         RAISING   zcx_llm,
      call_model  IMPORTING prompt          TYPE string
                            system_key      TYPE string OPTIONAL
                  RETURNING VALUE(response) TYPE zllm_model_response
                  RAISING   zcx_llm,

**********************************************************************
* Checks that a user has authorisations for app / business function
* This check includes usage of the AI bot manager Fiori app
**********************************************************************
      check_auths RETURNING VALUE(authorised) TYPE boolean,

**********************************************************************
* Method to get list of apps.
* v1 - Always returns both active(global) and beta versions (Beta may not exist)
* v2 - Will now require version type to be passed
**********************************************************************
      get_apps    IMPORTING iv_version_type TYPE zllm_version_type DEFAULT 'GLOBAL'
                  RETURNING VALUE(llm_apps) TYPE zllm_app_odata_tt
                  RAISING   zcx_llm,

**********************************************************************
* Method to get list of prompts.
* If no prompt_key provided, returns all prompts for a given app id / business function
* If no version is provided, active prompt is selected
**********************************************************************
      get_prompt  IMPORTING app_id            TYPE zllm_app_id
                            business_function TYPE zllm_buss_function_name
                            prompt_key        TYPE zllm_prompt_key OPTIONAL
                            version           TYPE zllm_version OPTIONAL
                  RETURNING VALUE(prompt)     TYPE zllm_prompt_odata_tt
                  RAISING   zcx_llm,

**********************************************************************
* Method to save prompt.
* If no version provided, active version will be saved.
* Handles deletions as a deletion is a record indicator
**********************************************************************
      save_prompt IMPORTING app_id                        TYPE zllm_app_id
                            business_function             TYPE zllm_buss_function_name
                            prompt_key                    TYPE zllm_prompt_key
                            prompt_chain                  TYPE zllm_prompt_chain
                            prompt_manual_chain           TYPE zllm_prompt_manual_chain
                            prompt_chain_enabled          TYPE zllm_prompt_chain_enabled DEFAULT abap_false
                            prompt_type                   TYPE zllm_prompt_type
                            prompt                        TYPE zllm_prompt_text
                            is_signed_off                 TYPE zllm_prompt_sign_off DEFAULT abap_false
                            deleteable                    TYPE zllm_prompt_deletable DEFAULT abap_true
                            deleted                       TYPE zllm_prompt_deleted DEFAULT abap_false
                            version                       TYPE zllm_version OPTIONAL
                            override_func_declar          TYPE boolean DEFAULT abap_false
                            external_value                TYPE zllm_ext_value OPTIONAL
                            think_link                    TYPE zllm_think_link OPTIONAL
                            final_link                    TYPE zllm_final_link OPTIONAL
                            prompt_name                   TYPE zllm_prompt_label OPTIONAL
                            model_id                      TYPE zllm_model_id OPTIONAL
                            decision_link                 TYPE zllm_decision_link OPTIONAL
                  RETURNING VALUE(generated_prompt_chain) TYPE zllm_prompt_chain
                  RAISING   zcx_llm,

**********************************************************************
* Get the AI bot configuration from ZLLM_AGENT_PARAM table
**********************************************************************
      get_config IMPORTING app_id            TYPE zllm_app_id
                           business_function TYPE zllm_buss_function_name
                           param             TYPE zllm_agent_parameter
                           type              TYPE zllm_config_type
                 RETURNING VALUE(value)      TYPE zllm_agent_param_value
                 RAISING   zcx_llm,

**********************************************************************
* Get list of function declartions.
**********************************************************************
      get_function_declarations IMPORTING app_id                          TYPE zllm_app_id
                                          business_function               TYPE zllm_buss_function_name
                                          function_name                   TYPE zllm_function_name
                                          version                         TYPE zllm_version OPTIONAL
                                          type                            TYPE zllm_function_type OPTIONAL
                                RETURNING VALUE(rt_function_declarations) TYPE zllm_agent_cds_tt
                                RAISING   zcx_llm,

**********************************************************************
* Method to get prompt history
**********************************************************************
      get_prompt_history IMPORTING app_id            TYPE zllm_app_id
                                   business_function TYPE zllm_buss_function_name
                                   prompt_key        TYPE zllm_prompt_key
                         RETURNING VALUE(prompt)     TYPE zllm_prompt_history_odata_tt
                         RAISING   zcx_llm,

**********************************************************************
* Publish a prompt from a function declaration
**********************************************************************
      publish_prompt IMPORTING app_id            TYPE zllm_app_id
                               business_function TYPE zllm_buss_function_name
                               function_name     TYPE zllm_function_name
                     RAISING   zcx_llm,

**********************************************************************
* Get all versions for a given app id / business function.
**********************************************************************
      get_app_versions  IMPORTING app_id            TYPE zllm_app_id
                                  business_function TYPE zllm_buss_function_name
                        RETURNING VALUE(versions)   TYPE zllm_versions_odata_tt
                        RAISING   zcx_llm,

**********************************************************************
* Set the selected version as the active version.
* This maybe a roll back, or setting beta as active.
**********************************************************************
      set_app_active_version IMPORTING app_id            TYPE zllm_app_id
                                       business_function TYPE zllm_buss_function_name
                                       version           TYPE zllm_version
                                       model_id          TYPE zllm_model_id OPTIONAL
                             RAISING   zcx_llm,

**********************************************************************
* Update agent. This could be creating/editing details of an agent
**********************************************************************
      update_agent           IMPORTING it_agent_details TYPE zllm_agent_cds_tt
                             RAISING   zcx_llm,

**********************************************************************
* Update rag agent. This could be creating/editing details of an agent
**********************************************************************
      update_rag_agent       IMPORTING it_agent_details TYPE zllm_rag_func_odata_tt
                             RAISING   zcx_llm,

**********************************************************************
* Get the next agent ID from the number range object
**********************************************************************
      get_next_agent_id      IMPORTING iv_app_id          TYPE zllm_app_id
                                       iv_buss_function   TYPE zllm_buss_function_name
                             RETURNING VALUE(rv_agent_id) TYPE zllm_function_parameters_id
                             RAISING   zcx_llm,

**********************************************************************
* Get all the external values maintained
**********************************************************************
      get_external_values    "IMPORTING it_type_range      TYPE
        RETURNING VALUE(rt_external_values) TYPE zllm_ext_value_tt
        RAISING   zcx_llm,

**********************************************************************
* Method to update bot configuration
**********************************************************************
      update_bot             IMPORTING iv_app_id                TYPE zllm_app_id
                                       iv_buss_func             TYPE zllm_buss_function_name
                                       iv_external_name         TYPE zllm_external_name
                                       iv_enable                TYPE zllm_enabled_indicator
                                       iv_delete                TYPE zllm_deletion_flag OPTIONAL
                                       iv_helper_app_id         TYPE zllm_helper_app_id OPTIONAL
                                       iv_helper_bus_function   TYPE zllm_helper_bus_func_name OPTIONAL
                                       iv_eval_bot_app_id       TYPE zllm_eval_app_id OPTIONAL
                                       iv_eval_bot_bus_function TYPE zllm_eval_bus_func_name OPTIONAL
                                       iv_bot_number_range      TYPE zllm_agent_param_value OPTIONAL
                                       iv_fc_number_range       TYPE zllm_agent_param_value OPTIONAL
                             RAISING   zcx_llm,

**********************************************************************
* Method to update roles and auths.
**********************************************************************
      create_update_role IMPORTING roleid                 TYPE zllm_auth_role
                                   roledesc               TYPE zllm_auth_role_desc
                                   region                 TYPE zllm_region
                                   app_id                 TYPE zllm_app_id
                                   business_function      TYPE zllm_buss_function_name
                                   agent                  TYPE zllm_function_name
                                   tabid                  TYPE zllm_auth_tabid
                                   deleted                TYPE zllm_auth_deleted
                                   exclude_from_reporting TYPE zllm_exclude_from_report
                         RAISING   zcx_llm,

**********************************************************************
* Method to update tabs and permission
**********************************************************************
      create_update_tabs IMPORTING tabs TYPE zllm_auths_tabs_odata
                         RAISING   zcx_llm,

**********************************************************************
* Method to assign users to a role.
**********************************************************************
      assign_user_role IMPORTING user              TYPE uname
                                 roleid            TYPE zllm_auth_role
                                 deleted           TYPE zllm_auth_deleted
                                 app_id            TYPE zllm_app_id
                                 business_function TYPE zllm_buss_function_name
                       RAISING   zcx_llm,
**********************************************************************
* Method to get user and their roles.
**********************************************************************
      get_auth_roles IMPORTING app_id            TYPE zllm_app_id
                               business_function TYPE zllm_buss_function_name
                     RETURNING VALUE(roles)      TYPE zllm_roles_auth_odata_tt,

**********************************************************************
* Method to get user roles
**********************************************************************
      get_user_roles RETURNING VALUE(user_roles)  TYPE zllm_user_roles_odata_tt,

**********************************************************************
* Method to get user roles and authorization
**********************************************************************
      get_roles_and_authorization RETURNING VALUE(roles_auths) TYPE zllm_roles_auths_tt
                                  RAISING   zcx_llm,

**********************************************************************
* Method to get tabs and their permissions
**********************************************************************
      get_tabs_and_permission RETURNING VALUE(auth_tabs)  TYPE zllm_auths_tabs_odata_tt,

**********************************************************************
* Method to set user override version
**********************************************************************
      set_user_version IMPORTING app_id            TYPE zllm_app_id
                                 business_function TYPE zllm_buss_function_name
                                 version           TYPE zllm_version
                       RETURNING VALUE(status)     TYPE boolean
                       RAISING   zcx_llm,

**********************************************************************
* Method to remove user override version. Version will now be global version
**********************************************************************
      delete_user_version IMPORTING app_id            TYPE zllm_app_id
                                    business_function TYPE zllm_buss_function_name
                                    version           TYPE zllm_version
                          RETURNING VALUE(status)     TYPE boolean
                          RAISING   zcx_llm,

**********************************************************************
* Method to get the usage of a prompt, with source and response
**********************************************************************
      get_prompt_usage IMPORTING app_id              TYPE zllm_app_id
                                 business_function   TYPE zllm_buss_function_name
                                 prompt_key          TYPE zllm_prompt_key
                       RETURNING VALUE(prompt_usage) TYPE zllm_prompt_usage_tt
                       RAISING   zcx_llm,

**********************************************************************
* Method to get the prompt, by the star rating
**********************************************************************
      get_prompt_by_rating IMPORTING app_id                TYPE zllm_app_id
                                     business_function     TYPE zllm_buss_function_name
                                     prompt_key_range      TYPE zllm_prompt_key_range_tt
                                     rating_range          TYPE zllm_rating_range_tt
                           RETURNING VALUE(prompts_rating) TYPE zllm_ratingsearch_odata_tt
                           RAISING   zcx_llm,

**********************************************************************
* Method to calculate a prompts average star rating.
**********************************************************************
      get_prompt_avg_rating IMPORTING prompt_key        TYPE zllm_prompt_key
                                      app_id            TYPE zllm_app_id
                                      business_function TYPE zllm_buss_function_name
                            RETURNING VALUE(avg_rating) TYPE zllm_prompt_avg_rating
                            RAISING   zcx_llm,

**********************************************************************
* Method to save a prompt example
**********************************************************************
      save_prompt_example IMPORTING prompt_key        TYPE zllm_prompt_key
                                    example           TYPE string
                                    app_id            TYPE zllm_app_id
                                    business_function TYPE zllm_buss_function_name
                                    uuid              TYPE guid OPTIONAL
                                    delete            TYPE zllm_deletion_flag OPTIONAL
                          RAISING   zcx_llm,

**********************************************************************
* Method to get prompt examples for a given prompt
**********************************************************************
      get_prompt_example IMPORTING prompt_key            TYPE zllm_prompt_key
                                   app_id                TYPE zllm_app_id
                                   business_function     TYPE zllm_buss_function_name
                         RETURNING VALUE(prompt_example) TYPE zllm_prompt_eg_tt
                         RAISING   zcx_llm,

**********************************************************************
* Method to get prompt function details.
**********************************************************************
      get_prompt_fm_config IMPORTING prompt_key        TYPE zllm_prompt_key
                                     app_id            TYPE zllm_app_id
                                     business_function TYPE zllm_buss_function_name
                           RETURNING VALUE(parmeters)  TYPE /goog/t_function_parameters
                           RAISING   zcx_llm,

**********************************************************************
* Method to trigger prompt FM
**********************************************************************
      trigger_prompt IMPORTING function          TYPE zllm_function_name
                               app_id            TYPE zllm_app_id
                               business_function TYPE zllm_buss_function_name
                               parameters        TYPE /goog/t_function_parameters
                     RETURNING VALUE(response)   TYPE string
                     RAISING   zcx_llm,

**********************************************************************
* Method to get the feedback, by the object key (ex. freshdesk ticket id)
**********************************************************************
      get_feedb_by_object_key IMPORTING it_filter_select_options TYPE /iwbep/t_mgw_select_option
                              RETURNING VALUE(rs_feedback)       TYPE zllm_feedback_search_odata
                              RAISING   zcx_llm,

**********************************************************************
* Method to save user filters for ratings.
**********************************************************************
      save_user_filters IMPORTING filter_type       TYPE zllm_usr_filter
                                  filter_value      TYPE zllm_usr_filter_value
                                  app_id            TYPE zllm_app_id
                                  business_function TYPE zllm_buss_function_name
                        RAISING   zcx_llm,

**********************************************************************
* Method to get filters
      get_user_filters IMPORTING app_id            TYPE zllm_app_id
                                 business_function TYPE zllm_buss_function_name
                       RETURNING VALUE(filter)     TYPE zllm_user_filter_odata_tt
                       RAISING   zcx_llm,

**********************************************************************
* Method to get models
      get_models IMPORTING VALUE(app_id)            TYPE zllm_app_id
                           VALUE(business_function) TYPE zllm_buss_function_name
                 RETURNING VALUE(rt_models)         TYPE zllm_user_model_odata_tt

                 RAISING   zcx_llm,


**********************************************************************
* Method to set user model version
**********************************************************************
      set_user_model IMPORTING app_id            TYPE zllm_app_id
                               business_function TYPE zllm_buss_function_name
                               version           TYPE zllm_version
                               model_id          TYPE zllm_model_id
                     RETURNING VALUE(status)     TYPE boolean
                     RAISING   zcx_llm,

**********************************************************************
* Method to remove user model version. Version will now be global version
**********************************************************************
      delete_user_model IMPORTING app_id            TYPE zllm_app_id
                                  business_function TYPE zllm_buss_function_name
                                  version           TYPE zllm_version
                                  model_id          TYPE zllm_model_id
                        RETURNING VALUE(status)     TYPE boolean
                        RAISING   zcx_llm,

**********************************************************************
* Method to save notes
**********************************************************************
      save_log_notes IMPORTING app_id            TYPE zllm_app_id
                               business_function TYPE zllm_buss_function_name
                               uuid_key          TYPE guid
                               note              TYPE zllm_note,

**********************************************************************
* Method for multi interactive chat
**********************************************************************
      multi_chat IMPORTING app_id            TYPE zllm_app_id
                           business_function TYPE zllm_buss_function_name
                           chat              TYPE string
                           chat_type         TYPE zllm_conversation_type
                 RETURNING VALUE(response)   TYPE string
                 RAISING   zcx_llm,

**********************************************************************
* Method for multi interactive chat
**********************************************************************
      get_signature IMPORTING app_id            TYPE zllm_app_id
                              business_function TYPE zllm_buss_function_name
                    RETURNING VALUE(signature)  TYPE string,

**********************************************************************
* Get list of sub agents for the supervisor agent
**********************************************************************
      get_sub_agents IMPORTING app_id            TYPE zllm_app_id
                               business_function TYPE zllm_buss_function_name
                               supv_agent        TYPE zllm_function_name
                               version           TYPE zllm_version OPTIONAL
                     RETURNING VALUE(sub_agents) TYPE zllm_sub_func_odata_tt
                     RAISING   zcx_llm,

**********************************************************************
* Save sub agent and its parameters
**********************************************************************
      save_sub_agent IMPORTING sub_agent TYPE zllm_sub_func_odata
                     RAISING   zcx_llm,

**********************************************************************
* Human agents are not always logging things correctly. This allows us to correct those errors
**********************************************************************
      update_prompt_usage IMPORTING app_id            TYPE zllm_app_id
                                    business_function TYPE zllm_buss_function_name
                                    guid              TYPE string
                                    sentiment         TYPE zllm_sentiment OPTIONAL
                                    resolved          TYPE boolean OPTIONAL
                                    resolved_note     TYPE string OPTIONAL
                          RETURNING VALUE(status)     TYPE boolean,


**********************************************************************
* Get current status(Open/Not open) of the ticket from Freshdesk
**********************************************************************
      get_freshdesk_ticket_status IMPORTING ticket          TYPE zcs_freshdesk_ticket_id
                                  RETURNING VALUE(not_open) TYPE xfeld,

**********************************************************************
* Handle locking and APC broadcast
**********************************************************************
      handle_locking IMPORTING type   TYPE string
                               key    TYPE string
                               delete TYPE boolean
                     RAISING   zcx_llm,

**********************************************************************
* Get agent level customisation from the GCS map table
**********************************************************************
      get_gcs_map IMPORTING app_id                     TYPE zllm_app_id
                            business_function          TYPE zllm_buss_function_name
                            agent                      TYPE string OPTIONAL
                            user                       TYPE uname OPTIONAL
                  RETURNING VALUE(agent_customisation) TYPE zllm_custom_agent_odata_tt
                  RAISING   zcx_llm,


**********************************************************************
* Get agent level customisation from the GCS map table
**********************************************************************
      save_gcs_map IMPORTING app_id                     TYPE zllm_app_id
                             business_function          TYPE zllm_buss_function_name
                             agent                      TYPE string OPTIONAL
                             user                       TYPE uname OPTIONAL
                             uri                        TYPE zllm_gcs_kb_uri
                             mime                       TYPE zllm_mime
                   RETURNING VALUE(agent_customisation) TYPE zllm_custom_agent_odata_tt
                   RAISING   zcx_llm,

**********************************************************************
* Get prompt chain to RAG agents. Agent must be a supervisor agent
**********************************************************************
      get_prompt_chain_to_rag IMPORTING app_id            TYPE zllm_app_id
                                        business_function TYPE zllm_buss_function_name
                                        agent             TYPE string
                                        version           TYPE zllm_version
                              RETURNING VALUE(rag)        TYPE zllm_rag_odata_tt
                              RAISING   zcx_llm,

**********************************************************************
* Save prompt chain to RAG
**********************************************************************
      save_prompt_chain_to_rag IMPORTING prompt_chain  TYPE zllm_rag_odata_tt
                               RETURNING VALUE(status) TYPE boolean
                               RAISING   zcx_llm,

**********************************************************************
* Save RAG catalog (this should also include a UI call to save agent)
**********************************************************************
      save_catalog_map IMPORTING catalog       TYPE zllm_catalog_map
                       RETURNING VALUE(status) TYPE boolean
                       RAISING   zcx_llm,

**********************************************************************
* Get list of agents along with their catalog details
**********************************************************************
      get_agent_catalog_menu RETURNING VALUE(rt_agent_catalog_menu) TYPE zllm_agent_catalog_menu_tt
                             RAISING   zcx_llm,

**********************************************************************
* Save bot to rag catalog
**********************************************************************
      save_bot_to_catalog IMPORTING app_id            TYPE zllm_app_id
                                    business_function TYPE zllm_buss_function_name
                                    catalog           TYPE zllm_bot_to_catalog_odata
                          RETURNING VALUE(status)     TYPE boolean
                          RAISING   zcx_llm,

**********************************************************************
* Get bot to catalog
**********************************************************************
      get_bot_to_catalog IMPORTING app_id            TYPE zllm_app_id
                                   business_function TYPE zllm_buss_function_name
                         RETURNING VALUE(catalog)    TYPE zllm_bot_to_catalog_odata_tt
                         RAISING   zcx_llm,

**********************************************************************
* Get catalogs for bots.
**********************************************************************
      get_agent_catalog   RETURNING VALUE(catalog) TYPE zllm_bot_to_catalog_odata_tt
                          RAISING   zcx_llm,

**********************************************************************
* Get a list of agents from a catalog name used in a prompt chain
**********************************************************************
      get_catalog_to_prompt_chain IMPORTING app_id               TYPE zllm_app_id
                                            business_function    TYPE zllm_buss_function_name
                                            catalog_name         TYPE zllm_rag_catalog_name
                                  RETURNING VALUE(prompt_chains) TYPE zllm_prompt_chain_tt,

**********************************************************************
* Save agent catalog
**********************************************************************
      save_agent_to_catalog IMPORTING catalog       TYPE zllm_rag_agent_catalog_odata
                            RETURNING VALUE(status) TYPE boolean
                            RAISING   zcx_llm,

**********************************************************************
* Save the RAG class configurations
**********************************************************************
      save_rag_class_details IMPORTING app_id             TYPE zllm_app_id
                                       business_function  TYPE zllm_buss_function_name
                                       agent_name         TYPE zllm_function_name
                                       class_name         TYPE zllm_class_name
                                       parent_method_name TYPE zllm_method_name
                                       child_method_name  TYPE zllm_method_name
                                       response_structure TYPE zllm_response_structure
                                       deleted            TYPE zllm_deletion_flag
                             RETURNING VALUE(config)      TYPE zllm_rag_config_tt
                             RAISING   zcx_llm,

**********************************************************************
* Get MCP agent's response structure
**********************************************************************
      get_mcp_agent_response_struct IMPORTING app_id                    TYPE zllm_app_id
                                              business_function         TYPE zllm_buss_function_name
                                              agent_name                TYPE zllm_function_name
                                    RETURNING VALUE(response_structure) TYPE string
                                    RAISING   zcx_llm,

      get_mcp_class_config          IMPORTING app_id                  TYPE zllm_app_id
                                              business_function       TYPE zllm_buss_function_name
                                              agent_name              TYPE zllm_function_name
                                    RETURNING VALUE(mcp_class_config) TYPE zllm_rag_config_tt
                                    RAISING   zcx_llm,
**********************************************************************
* Method to save prompt connector
* This will connect the prompt text between multiple bot's
* If prompt text is changed in parent, child gets changed too
**********************************************************************
      save_prompt_connector         IMPORTING prompt_connector TYPE zllm_prompt_connector_tt
                                    RAISING   zcx_llm,

**********************************************************************
* Method to get prompt connector
* This will give back the list of available connectors
* The query can be made to check the connection both from parent/child
**********************************************************************
      get_prompt_connector          IMPORTING app_id                  TYPE zllm_app_id
                                              business_function       TYPE zllm_buss_function_name
                                              prompt_chain            TYPE zllm_prompt_chain
                                              prompt_key              TYPE zllm_prompt_key
                                              source                  TYPE char10
                                    RETURNING VALUE(prompt_connector) TYPE zllm_prompt_connector_tt
                                    RAISING   zcx_llm,

************************************************************************
* Method to get prompt properties
* This will give back the properties: Connected, Child and Bidirectional
* The properties are set to True/False
************************************************************************
      get_prompt_prop               IMPORTING app_id             TYPE zllm_app_id
                                              business_function  TYPE zllm_buss_function_name
                                              prompt_key         TYPE zllm_prompt_key
                                    RETURNING VALUE(prompt_prop) TYPE zllm_prompt_properties_tt
                                    RAISING   zcx_llm,


      get_agent_list                IMPORTING app_id            TYPE zllm_app_id
                                              business_function TYPE zllm_buss_function_name
                                    RETURNING VALUE(agents)     TYPE zllm_agents_list_tt
                                    RAISING   zcx_llm,



      update_prompt_connector_chain IMPORTING prompt_connector_chain TYPE zllm_prompt_connector_chain_tt
                                    RAISING   zcx_llm.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:

      check_function_declaration IMPORTING app_id                 TYPE zllm_app_id
                                           business_function      TYPE zllm_buss_function_name
                                           function_name          TYPE zllm_function_name
                                 RETURNING VALUE(function_exists) TYPE boolean,
      build_external_value       IMPORTING external_value   TYPE zllm_ext_value
                                 RETURNING VALUE(ext_value) TYPE zllm_ext_value_tt,
      build_prompt               IMPORTING app_id                 TYPE zllm_app_id
                                           business_function      TYPE zllm_buss_function_name
                                           prompt_key             TYPE zllm_prompt_key
                                           prompt_chain           TYPE zllm_prompt_chain
                                           prompt_chain_enabled   TYPE zllm_prompt_chain_enabled
                                           prompt_type            TYPE zllm_prompt_type
                                           prompt                 TYPE zllm_prompt_text
                                           is_signed_off          TYPE zllm_prompt_sign_off DEFAULT abap_false
                                           deleteable             TYPE zllm_prompt_deletable DEFAULT abap_true
                                           deleted                TYPE zllm_prompt_deleted DEFAULT abap_false
                                           version                TYPE zllm_version OPTIONAL
                                           override_func_declar   TYPE boolean DEFAULT abap_false
                                           think_link             TYPE zllm_think_link OPTIONAL
                                           final_link             TYPE zllm_final_link OPTIONAL
                                           model_id               TYPE zllm_model_id OPTIONAL
                                           decision_link          TYPE zllm_decision_link OPTIONAL
                                 RETURNING VALUE(response_prompt) TYPE zllm_prompts_tt
                                 RAISING   zcx_llm,
      get_user_version          IMPORTING app_id            TYPE zllm_app_id
                                          business_function TYPE zllm_buss_function_name
                                RETURNING VALUE(version)    TYPE zllm_version,

      get_user_model            IMPORTING app_id            TYPE zllm_app_id
                                          business_function TYPE zllm_buss_function_name
                                          version           TYPE zllm_version
                                RETURNING VALUE(model_id)   TYPE zllm_model_id,

      trigger_apc IMPORTING app_id         TYPE zapp_id
                            type           TYPE ztype
                            username       TYPE uname
                            message        TYPE zuser_msg
                            param          TYPE zuser_param_name
                            param_value    TYPE zuser_param_value
                            read_indicator TYPE zmessage_read DEFAULT abap_true.

    DATA: app_id            TYPE zllm_app_id,
          business_function TYPE zllm_buss_function_name,
          gv_version        TYPE zllm_version,
          request           TYPE string,
          lcl_llm_util      TYPE REF TO zcl_llm_util.

ENDCLASS.



CLASS zcl_fiori_llm_util IMPLEMENTATION.


  METHOD constructor.


    me->app_id = app_id.
    me->business_function = business_function.


* It is possible not to invoke the LLM util if we are wanting to access none LLM functions.
    IF app_id IS NOT INITIAL OR business_function IS NOT INITIAL.

      TRY.
          lcl_llm_util = NEW zcl_llm_util( app_id            = app_id
                                           business_function = business_function
                                           temperature       = temperature ).


* Get version
          DATA(lcl_llm_version_controller) = NEW zcl_llm_version_control_util(  ).
          DATA(versions) = lcl_llm_version_controller->get_active_app_version( app_id            = me->app_id
                                                                               business_function = me->business_function ).

* User version will always override current version.
          IF versions-user_version IS NOT INITIAL.
            me->gv_version = versions-user_version.
          ELSE.
            me->gv_version = versions-active_version.
          ENDIF.


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


  METHOD save_rag_class_details.


    DATA(lcl_rag_util) = NEW zcl_llm_mcp_config_util(  ).

    TRY.
        lcl_rag_util->save_rag_class_details( app_id             = app_id
                                              business_function  = business_function
                                              agent_name         = agent_name
                                              class_name         = class_name
                                              parent_method_name = parent_method_name
                                              child_method_name  = child_method_name
                                              response_structure = response_structure
                                              deleted            = deleted ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.


  ENDMETHOD.


  METHOD save_agent_to_catalog.

    DATA(lcl_llm_agent_catalog) = NEW zcl_llm_agent_catalog_util( app_id            = app_id
                                                                  business_function = business_function ).


    DATA: ls_catalog TYPE zllm_agent_cat.

    ls_catalog-agent_name = to_upper( catalog-agent_name ).
    ls_catalog-catalog_app_id = catalog-catalog_app_id.
    ls_catalog-catalog_business_function = catalog-catalog_business_function.
    ls_catalog-catalog_name = catalog-catalog_name.
    ls_catalog-deleted = catalog-deleted.
    ls_catalog-mandt = sy-mandt.
    ls_catalog-type = catalog-type.

    TRY.

        lcl_llm_agent_catalog->save_agent_to_catalog( catalog = ls_catalog ).


      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.


  ENDMETHOD.


  METHOD get_catalog_to_prompt_chain.

    DATA(lcl_llm_catalog_util) = NEW zcl_llm_agent_catalog_util( app_id            = app_id
                                                                 business_function = business_function ).

    DATA(lcl_llm_prompt_util) = NEW zcl_llm_prompt_util( app_id            = app_id
                                                         business_function = business_function ).

    DATA(catalog_agents) = lcl_llm_catalog_util->get_agents_in_catalog( catalog_name = catalog_name ).

* Check if any of these exist in a prompt chain.
    LOOP AT catalog_agents ASSIGNING FIELD-SYMBOL(<fs_cat_agents>).

      DATA(lt_prompt_chain) = lcl_llm_prompt_util->get_prompt_chain( prompt_key = CONV zllm_prompt_key( <fs_cat_agents>-agent_name )
                                                                     version    = me->gv_version ).

      IF lt_prompt_chain IS NOT INITIAL.
        SORT lt_prompt_chain ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_prompt_chain COMPARING prompt_key.

        MOVE-CORRESPONDING lt_prompt_chain TO prompt_chains.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_bot_to_catalog.

    DATA(lcl_llm_rag_catalog) = NEW zcl_llm_agent_catalog_util( app_id            = app_id
                                                                business_function = business_function ).

    DATA(lt_catalog) = lcl_llm_rag_catalog->get_catalog_map( ).

    DATA: ls_catalog TYPE zllm_bot_to_catalog_odata.

    LOOP AT lt_catalog ASSIGNING FIELD-SYMBOL(<fs_catalog>).

      ls_catalog-app_id = <fs_catalog>-app_id.
      ls_catalog-business_function = <fs_catalog>-business_function.
      ls_catalog-rag_app_id = <fs_catalog>-catalog_app_id.
      ls_catalog-rag_business_function = <fs_catalog>-catalog_business_function.
      ls_catalog-catalog_name = <fs_catalog>-catalog_name.

      APPEND ls_catalog TO catalog.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_agent_catalog.

    DATA(lcl_llm_rag_catalog) = NEW zcl_llm_agent_catalog_util( app_id            = me->app_id
                                                                business_function = me->business_function ).

    DATA(lt_catalog) = lcl_llm_rag_catalog->get_catalogs(  ).

    DATA: ls_catalog TYPE zllm_bot_to_catalog_odata.

    LOOP AT lt_catalog ASSIGNING FIELD-SYMBOL(<fs_catalog>).

      ls_catalog-rag_app_id = <fs_catalog>-catalog_app_id.
      ls_catalog-rag_business_function = <fs_catalog>-catalog_business_function.
      ls_catalog-catalog_name = <fs_catalog>-catalog_name.

      APPEND ls_catalog TO catalog.

    ENDLOOP.


  ENDMETHOD.


  METHOD save_bot_to_catalog.


    DATA(lcl_llm_agent_catalog) = NEW zcl_llm_agent_catalog_util( app_id            = app_id
                                                                  business_function = business_function ).

* Check if the delete flag is set. If it is, we must check if the catalog being removed from the bot has agents assigned to a prompt chain.
    IF catalog-deleted = abap_true.

      DATA(lt_prompt_chains) = me->get_catalog_to_prompt_chain( app_id            = app_id
                                                                business_function = business_function
                                                                catalog_name      = catalog-catalog_name ).

      IF lt_prompt_chains IS NOT INITIAL.

        status = abap_false.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>prompt_chain_exists
            msg1   = CONV string( catalog-catalog_name ).

      ENDIF.



    ENDIF.


    DATA: ls_catalog TYPE zllm_catalog_map.

    ls_catalog-client = sy-mandt.
    ls_catalog-app_id = app_id.
    ls_catalog-business_function = business_function.
    ls_catalog-catalog_app_id = catalog-rag_app_id.
    ls_catalog-catalog_business_function = catalog-rag_business_function.
    ls_catalog-catalog_name = catalog-catalog_name.
    ls_catalog-deleted = catalog-deleted.


    TRY.

        lcl_llm_agent_catalog->save_catalog_map( catalog = ls_catalog ).


      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.


  ENDMETHOD.


  METHOD save_catalog_map.

    DATA(lcl_llm_rag_catalog) = NEW zcl_llm_agent_catalog_util( app_id            = me->app_id
                                                                business_function = me->business_function ).

    DATA: ls_catalog TYPE zllm_catalog_map.

    ls_catalog-client = sy-mandt.
    ls_catalog-catalog_app_id = catalog-catalog_app_id.
    ls_catalog-catalog_business_function = catalog-catalog_business_function.
    ls_catalog-catalog_name = catalog-catalog_name.
    ls_catalog-deleted = catalog-deleted.



    TRY.
        lcl_llm_rag_catalog->save_catalog_map( catalog = ls_catalog ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.


  ENDMETHOD.


  METHOD save_gcs_map.


    DATA(lcl_gcs_util) = NEW zcl_llm_gcs_util( app_id            = app_id
                                               business_function = business_function ).

    DATA: ls_gcs_map TYPE zllm_gcs_map.

    ls_gcs_map-agent = agent.
    ls_gcs_map-gcs_uri = uri.
    ls_gcs_map-mime = mime.
    ls_gcs_map-usr = user.
    ls_gcs_map-mandt = sy-mandt.


    TRY.
        lcl_gcs_util->save_gcs_map( gcs_map = ls_gcs_map ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.

  ENDMETHOD.


  METHOD save_prompt_chain_to_rag.

* We need app id and business function. It will be the same for all entries.
    TRY.
        DATA(ls_rag) = prompt_chain[ 1 ].

        IF ls_rag-app_id IS INITIAL.
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>save_agent_map_failed.
        ENDIF.


        IF ls_rag-business_function IS INITIAL.
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>save_agent_map_failed.
        ENDIF.


      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>save_agent_map_failed.
    ENDTRY.


    DATA(lcl_llm_rag_util) = NEW zcl_llm_agents_util( app_id            = ls_rag-app_id
                                                      business_function = ls_rag-business_function ).

    TRY.
        lcl_llm_rag_util->save_prompt_chain_to_rag( rag = prompt_chain ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.

  ENDMETHOD.


  METHOD get_prompt_chain_to_rag.


    DATA(lcl_llm_rag_util) = NEW zcl_llm_agents_util( app_id            = app_id
                                                      business_function = business_function ).

    rag = lcl_llm_rag_util->get_prompt_to_rag( agent   = agent
                                               version = version ).

  ENDMETHOD.


  METHOD get_gcs_map.

    DATA(lcl_gcs_map_util) = NEW zcl_llm_gcs_util( app_id            = app_id
                                                   business_function = business_function ).

    agent_customisation = lcl_gcs_map_util->get_gcs_map( agent = agent
                                                         user  = user ).



  ENDMETHOD.


  METHOD handle_locking.

    RETURN.
    DATA: ls_locking TYPE zllm_locking.

    ls_locking-mandt = sy-mandt.
    ls_locking-username = sy-uname.
    ls_locking-type = type.
    ls_locking-type_key = key.


* Check if this lock entry already exists.
    SELECT SINGLE *
    FROM zllm_locking
    INTO @DATA(ls_user_lock)
    WHERE username = @ls_locking-username
    AND type = @ls_locking-type
    AND type_key = @ls_locking-type_key.

    IF sy-subrc = 0.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>lock_exists
          msg1   = CONV string( ls_user_lock-username ).


    ENDIF.


    IF delete = abap_false.

      ls_locking-dat = sy-datum.
      ls_locking-time = sy-uzeit.

      MODIFY zllm_locking FROM ls_locking.

* Trigger the APC.
      me->trigger_apc( app_id         = 'CSHUB'
                       message        = |{ type },{ key },LOCKED,{ sy-uname }|
                       param          = CONV zuser_param_name( type )
                       param_value    = CONV zuser_param_value( type )
                       read_indicator = abap_true
                       username       = sy-uname
                       type           = CONV ztype( type ) ).


    ELSE.

      DELETE zllm_locking FROM ls_locking.


* Trigger the APC.
      me->trigger_apc( app_id         = 'CSHUB'
                       message        = |{ type },{ key },UNLOCKED,{ sy-uname }|
                       param          = CONV zuser_param_name( type )
                       param_value    = CONV zuser_param_value( type )
                       read_indicator = abap_true
                       username       = sy-uname
                       type           = CONV ztype( type ) ).

    ENDIF.


    COMMIT WORK AND WAIT.


  ENDMETHOD.


  METHOD trigger_apc.

    DATA(lcl_message_handler) = NEW zcl_user_message_handler(  ).

    lcl_message_handler->register_message( iv_app_id             = app_id
                                           iv_type               = type
                                           iv_user               = sy-uname
                                           iv_language           = 'E'
                                           iv_message            = message
                                           iv_param_name         = param
                                           iv_param_value        = param_value
                                           iv_region             = 'UK'
                                           iv_flash_article_card = abap_false ).


  ENDMETHOD.


  METHOD update_prompt_usage.


    DATA(lcl_crud_util) = NEW zcl_llm_crud_control_util( app_id            = app_id
                                                         business_function = business_function ).

    status = lcl_crud_util->update_prompt_usage( guid          = CONV guid( guid )
                                                 sentiment     = sentiment
                                                 resolved      = resolved
                                                 resolved_note = resolved_note ).

  ENDMETHOD.


  METHOD get_sub_agents.

    DATA(lcl_sub_agents) = NEW zcl_llm_agents_util( app_id            = app_id
                                                    business_function = business_function ).

    TRY.

        sub_agents = lcl_sub_agents->get_sub_agents( app_id            = app_id
                                                     business_function = business_function
                                                     supv_agent        = supv_agent
                                                     version           = version ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.

  ENDMETHOD.


  METHOD save_sub_agent.
    TRY.

        DATA(lcl_sub_agents) = NEW zcl_llm_agents_util( app_id            = app_id
                                                        business_function = business_function ).

        lcl_sub_agents->update_sub_agent( sub_agent = sub_agent ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.

  ENDMETHOD.


  METHOD get_signature.

    SELECT SINGLE signature
    FROM zllm_signature
    INTO @signature
    WHERE app_id = @app_id
    AND business_function = @business_function.


  ENDMETHOD.


  METHOD multi_chat.

* Build the conversation and then append the chat question.
    DATA(lcl_conversation) = NEW zcl_llm_conversation_util( app_id            = app_id
                                                            business_function = business_function ).


* Add new chat to the history.
    lcl_conversation->addto_conversation( conversation = chat
                                          type         = chat_type ).

* Get the chat history
    DATA(conv_history) = lcl_conversation->fetch_conversation(  ).


* Build prompt.

* Submit chat to model.
    DATA(lcl_llm) = NEW zcl_llm_util( app_id            = app_id
                                      business_function = business_function ).

    response = lcl_llm->call_model( prompt = conv_history )-response.


  ENDMETHOD.


  METHOD get_user_filters.

    SELECT app_id, business_function, user_name, filter, value
    FROM zllm_user_filter
    INTO TABLE @filter
    WHERE app_id = @app_id
    AND business_function = @business_function
    AND user_name = @sy-uname.

  ENDMETHOD.


  METHOD save_log_notes.


  ENDMETHOD."


  METHOD save_user_filters.

    TRY.
        DATA(lcl_llm_crud) = NEW zcl_llm_crud_control_util( app_id            = app_id
                                                            business_function = business_function ).


        lcl_llm_crud->save_user_filter( filter_type  = filter_type
                                        filter_value = filter_value ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.


  ENDMETHOD.


  METHOD trigger_prompt.

    TRY.
        DATA(lcl_llm_prompt_util) = NEW zcl_llm_prompt_util( app_id            = app_id
                                                             business_function = business_function ).

        lcl_llm_prompt_util->trigger_prompt( function   = function
                                             parameters = parameters ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.
  ENDMETHOD.


  METHOD get_prompt_fm_config.

    DATA(lcl_prompt_util) = NEW zcl_llm_prompt_util( app_id            = app_id
                                                     business_function = business_function ).

    parmeters = lcl_prompt_util->get_prompt_fm_config( prompt_key = prompt_key ).

  ENDMETHOD.


  METHOD get_prompt_example.

    TRY.
        DATA(lcl_prompt_util) = NEW zcl_llm_prompt_util( app_id            = app_id
                                                         business_function = business_function ).

        prompt_example = lcl_prompt_util->get_prompt_example( app_id            = app_id
                                                              business_function = business_function
                                                              prompt_key        = prompt_key ).
      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.


  ENDMETHOD.


  METHOD save_prompt_example.

    DATA(lcl_prompt_util) = NEW zcl_llm_prompt_util( app_id            = app_id
                                                     business_function = business_function ).

    TRY.
        lcl_prompt_util->save_prompt_example( prompt_key = prompt_key
                                              example    = example
                                              uuid       = uuid
                                              delete     = delete ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.

  ENDMETHOD.


  METHOD get_prompt_avg_rating.

    TRY.
        DATA(lcl_llm_prompt_util) = NEW zcl_llm_prompt_util( app_id            = app_id
                                                             business_function = business_function ).

        avg_rating = lcl_llm_prompt_util->get_prompt_avg_rating( prompt_key = prompt_key ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.


  ENDMETHOD.


  METHOD get_prompt_usage.

    TRY.
        DATA(lcl_llm_prompt_util) = NEW zcl_llm_prompt_util( app_id            = app_id
                                                             business_function = business_function ).

        prompt_usage = lcl_llm_prompt_util->get_prompt_usage( prompt_key = prompt_key ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.



  ENDMETHOD.


  METHOD get_user_version.

    SELECT SINGLE version
    FROM zllm_usr_version
    INTO @version
    WHERE app_id = @app_id
    AND business_function = @business_function
    AND usr = @sy-uname.

  ENDMETHOD.


  METHOD get_user_model.

    SELECT SINGLE model_id
    FROM zllm_usr_version
    INTO @model_id
    WHERE app_id = @app_id
    AND business_function = @business_function
    AND usr = @sy-uname
    AND version = @version.

  ENDMETHOD.


  METHOD set_user_version.


    DATA(lcl_version_control) = NEW zcl_llm_version_control_util(  ).

    TRY.

        lcl_version_control->set_user_version( app_id            = app_id
                                               business_function = business_function
                                               version           = version ).
      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.
  ENDMETHOD.


  METHOD delete_user_version.

    DATA(lcl_version_control) = NEW zcl_llm_version_control_util(  ).

    TRY.

        lcl_version_control->delete_user_version( app_id            = app_id
                                                  business_function = business_function
                                                  version           = version ).
      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.


  ENDMETHOD.


  METHOD get_auth_roles.

    DATA(lcl_llm_roles) = NEW zcl_llm_auths_util(  ).

    roles = lcl_llm_roles->get_auth_roles( app_id            = app_id
                                           business_function = business_function ).


  ENDMETHOD.


  METHOD get_user_roles.

    DATA(lcl_llm_roles) = NEW zcl_llm_auths_util(  ).

    user_roles = lcl_llm_roles->get_users_and_their_roles( ).


  ENDMETHOD.


  METHOD assign_user_role.

    DATA(lcl_llm_roles) = NEW zcl_llm_auths_util(  ).

    TRY.

        lcl_llm_roles->assign_role_to_user( user              = user
                                            roleid            = roleid
                                            deleted           = deleted
                                            app_id            = app_id
                                            business_function = business_function ).
      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.

  ENDMETHOD.


  METHOD create_update_role.

    DATA(lcl_llm_roles) = NEW zcl_llm_auths_util(  ).

    TRY.

        DATA: ls_role TYPE zllm_auths_roles.

* Assign fields to role structure.
        ls_role-client = sy-mandt.
        ls_role-agent = agent.
        ls_role-app_id = app_id.
        ls_role-business_function = business_function.
        ls_role-deleted = deleted.
        ls_role-region = region.
        ls_role-roleid = roleid.
        ls_role-role_desc = roledesc.
        ls_role-tab_id = tabid.
        ls_role-exclude_from_reporting = exclude_from_reporting.


* Create the role.
        lcl_llm_roles->create_role( role = ls_role ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.


    ENDTRY.

  ENDMETHOD.


  METHOD set_app_active_version.

    TRY.
        DATA(lcl_version_controller) = NEW zcl_llm_version_control_util(  ).
        lcl_version_controller->set_app_active_version( app_id            = app_id
                                                        business_function = business_function
                                                        version           = version
                                                        model_id          = model_id ).


* Now we must create a new beta.
        TRY.
            DATA(lv_version) = lcl_version_controller->create_new_app_version( app_id            = app_id
                                                                               business_function = business_function ).
          CATCH zcx_llm INTO DATA(lcx_llm).
            RAISE EXCEPTION TYPE zcx_llm
              EXPORTING
                textid = lcx_llm->if_t100_message~t100key
                msg1   = lcx_llm->msg1
                msg2   = lcx_llm->msg2
                msg3   = lcx_llm->msg3.
        ENDTRY.
    ENDTRY.

  ENDMETHOD.


  METHOD get_app_versions.

    TRY.
        DATA(lcl_llm_verion_control) = NEW zcl_llm_version_control_util(  ).
        DATA(app_versions) = lcl_llm_verion_control->get_all_app_versions( app_id            = app_id
                                                                           business_function = business_function ).


        DATA: ls_app_versions TYPE zllm_versions_odata.

        LOOP AT app_versions ASSIGNING FIELD-SYMBOL(<fs_app_versions>).

          ls_app_versions-app_id = <fs_app_versions>-app_id.
          ls_app_versions-business_function = <fs_app_versions>-business_function.
          ls_app_versions-version = <fs_app_versions>-version.
          ls_app_versions-active = <fs_app_versions>-version_active.
          ls_app_versions-dat = <fs_app_versions>-dat.
          ls_app_versions-time = <fs_app_versions>-time.
          ls_app_versions-model_id = <fs_app_versions>-model_id.
          ls_app_versions-user_model_id = <fs_app_versions>-model_id.

* If version type is beta, then this is a beta version.
          IF <fs_app_versions>-version_type = 'BETA'.
            ls_app_versions-beta = abap_true.

          ELSE.
            ls_app_versions-beta = abap_false.
          ENDIF.

* Check if the user has a user override.
          DATA(user_version) = me->get_user_version( app_id            = app_id
                                                     business_function = business_function ).

          IF user_version = ls_app_versions-version.
            ls_app_versions-user_version = user_version.
          ENDIF.


* Check if the user has a user override.
          DATA(user_model) = me->get_user_model( app_id            = app_id
                                                 business_function = business_function
                                                 version           = ls_app_versions-version ).

          IF user_model IS NOT INITIAL.
            ls_app_versions-user_model_id = user_model.
          ENDIF.


          APPEND ls_app_versions TO versions.
          CLEAR: ls_app_versions.

        ENDLOOP.

        SORT versions BY version DESCENDING.

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.


    ENDTRY.


  ENDMETHOD.


  METHOD publish_prompt.

    TRY.
        DATA(lcl_llm_prompt_util) = NEW zcl_llm_prompt_util( app_id            = app_id
                                                             business_function = business_function ).

        lcl_llm_prompt_util->publish_prompt( func_name = function_name ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.


    ENDTRY.
  ENDMETHOD.


  METHOD get_prompt_history.

    TRY.
        DATA(lcl_llm_prompt_util) = NEW zcl_llm_prompt_util( app_id            = app_id
                                                             business_function = business_function ).

        prompt = lcl_llm_prompt_util->get_prompt_history( prompt_key = prompt_key ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.


    ENDTRY.
  ENDMETHOD.


  METHOD get_config.

    SELECT SINGLE value
    FROM zllm_config
    INTO @value
    WHERE app_id = @app_id
    AND buss_function = @business_function
    AND param = @param
    AND type = @type.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>no_number_range_interval
          msg1   = CONV string( app_id )
          msg2   = CONV string( business_function ).
    ENDIF.

  ENDMETHOD.


  METHOD get_function_declarations.
*** Method implementation to get function declaration & parameters for the agent passed

* Assign locking.
    me->handle_locking( type   = 'AGENT'
                        key    = CONV string( function_name )
                        delete = abap_false ).


    DATA(lref_llm_agents_util) = NEW zcl_llm_agents_util( app_id            = app_id
                                                          business_function = business_function ).

    TRY.

        rt_function_declarations = lref_llm_agents_util->get_agent( iv_app_id        = app_id
                                                                    iv_buss_function = business_function
                                                                    iv_function_name = function_name
                                                                    iv_version       = version
                                                                    type             = type ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2.


    ENDTRY.

  ENDMETHOD.


  METHOD get_prompt.

    DATA(lcl_prompt_util) = NEW zcl_llm_prompt_util( app_id            = app_id
                                                     business_function = business_function ).

    prompt = lcl_prompt_util->get_prompt( prompt_key = prompt_key
                                          version    = version ).

* Assign locking.
    me->handle_locking( type   = 'PROMPT'
                        key    = CONV string( prompt_key )
                        delete = abap_false ).

  ENDMETHOD.


  METHOD build_external_value.

    IF external_value IS NOT INITIAL.

      DATA: ls_ext_value TYPE zllm_ext_value,
            lt_ext_value TYPE zllm_ext_value_tt.

      ls_ext_value-mandt = sy-mandt.
      ls_ext_value-external_value = external_value-external_value.
      ls_ext_value-internal_value = external_value-internal_value.

* If language key is not supplied, default to english.
      IF external_value-lang IS INITIAL.
        ls_ext_value-lang = 'EN'.
      ELSE.
        ls_ext_value-lang = external_value-lang.
      ENDIF.

* Append to structure
      APPEND external_value TO ext_value.

    ENDIF.

  ENDMETHOD.


  METHOD build_prompt.

    DATA: lt_prompt TYPE zllm_prompts_tt,
          ls_prompt TYPE zllm_prompts.

    ls_prompt-mandt = sy-mandt.
    ls_prompt-app_id = app_id.
    ls_prompt-business_function = business_function.
    ls_prompt-deletable = deleteable.
    ls_prompt-is_signed_off = is_signed_off.
    ls_prompt-prompt_key = prompt_key.
    ls_prompt-prompt_chain = prompt_chain.
    ls_prompt-prompt_chain_enabled = prompt_chain_enabled.
    ls_prompt-prompt_text = prompt.
    ls_prompt-prompt_type = prompt_type.
    ls_prompt-deleted = deleted.
    ls_prompt-version = version.
    ls_prompt-think_link = think_link.
    ls_prompt-final_link = final_link.
    ls_prompt-model_id = model_id.
    ls_prompt-decision_link = decision_link.


* Append the prompt to needed structure for saving.
    APPEND ls_prompt TO response_prompt.


  ENDMETHOD.


  METHOD save_prompt.


* Check if deleting a prompt that a corresponding function declaration doesnt exist.
    IF override_func_declar = abap_false AND
       deleted = abap_true.

      DATA(function_exists) = me->check_function_declaration( app_id            = app_id
                                                              business_function = business_function
                                                              function_name     = CONV zllm_function_name( prompt_key ) ).

      IF function_exists = abap_true.

* Cannot / should not delete if the function call still exists.
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>func_call_exists
            msg1   = CONV string( prompt_key ).
      ENDIF.
    ENDIF.


    TRY.
        DATA(lcl_llm_crud_util) = NEW zcl_llm_crud_control_util( app_id            = app_id
                                                                 business_function = business_function ).


* Build prompt structure needed for saving.
**********************************************************************
        DATA: lt_prompt TYPE zllm_prompts_tt.

        lt_prompt = me->build_prompt( app_id               = app_id
                                      business_function    = business_function
                                      deleteable           = deleteable
                                      is_signed_off        = is_signed_off
                                      prompt_key           = prompt_key
                                      prompt_chain         = prompt_chain
                                      prompt_chain_enabled = prompt_chain_enabled
                                      prompt               = prompt
                                      prompt_type          = prompt_type
                                      deleted              = deleted
                                      version              = version
                                      think_link           = think_link
                                      final_link           = final_link
                                      model_id             = model_id
                                      decision_link        = decision_link ).

* Build external value structure for saving.
**********************************************************************
        DATA: lt_ext_value TYPE zllm_ext_value_tt.

        lt_ext_value =  me->build_external_value( external_value = external_value ).



* Save ...
**********************************************************************
        lcl_llm_crud_util->save_prompt( EXPORTING prompt         = lt_prompt
                                                  external_value = lt_ext_value
                                        IMPORTING prompts_saved  = DATA(lt_prompts_saved) ).

        lcl_llm_crud_util->update_prompt_name( prompt_name  = prompt_name
                                               prompt_chain = prompt_chain
                                               prompt_key   = prompt_key ).

*       Since only a single prompt can be saved using this method, we will read the prompts saved from above and
*       pass the prompt chain number back to UI. This is particularly useful when saving a link inside the prompt chain,
*       and the same prompt link is re-edited after the first save without refreshing.
        IF lt_prompts_saved IS NOT INITIAL.
          generated_prompt_chain = lt_prompts_saved[ 1 ]-prompt_chain.
        ENDIF.

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.

  ENDMETHOD.


  METHOD get_apps.

*   Fetch all the bots registered for use. This is the start of the flow.
    SELECT zllm_bots~app_id,
           zllm_bots~business_function,
           zllm_bots~enabled
      FROM zllm_bots
      INTO TABLE @DATA(lt_bots)
      WHERE deleted = @abap_false.


*   Get the list of apps / business functions / agents for the bots that are available to use
    IF lt_bots IS NOT INITIAL.

*     NOTE : This is the highest level for agents. Hence, there could be two types - SUPERVISOR / ''(Blank)
      DATA(lt_function_type_range_tt) = VALUE zllm_function_type_range_tt( sign   = 'I'
                                                                           option = 'EQ'
                                                                         ( low    =  '' )
                                                                         ( low    = 'SUPERVISOR' ) ).

      SELECT zllm_func_declar~app_id,
             zllm_func_declar~business_function,
             zllm_func_declar~function_name,
             zllm_func_declar~version,
             zllm_func_declar~deletion_indicator,
             zllm_func_declar~automate,
             zllm_func_declar~web_enabled
      FROM zllm_func_declar
      INNER JOIN zllm_version_ctr ON zllm_func_declar~app_id = zllm_version_ctr~app_id AND
                                     zllm_func_declar~business_function = zllm_version_ctr~business_function AND
                                     zllm_func_declar~version = zllm_version_ctr~version
      FOR ALL ENTRIES IN @lt_bots
      WHERE zllm_func_declar~app_id = @lt_bots-app_id
        AND zllm_func_declar~business_function = @lt_bots-business_function
        AND ( zllm_version_ctr~version_active = @abap_true OR
              zllm_version_ctr~version_type = 'BETA' )
      AND zllm_func_declar~deletion_indicator = @abap_false "We dont want to display deleted agents
      AND zllm_func_declar~type IN @lt_function_type_range_tt
      AND zllm_func_declar~app_id <> 'LLM'
      INTO TABLE @DATA(lt_llm_apps).


      LOOP AT lt_bots ASSIGNING FIELD-SYMBOL(<fs_bot>).

* For each bot, get the feedback percentage.
        IF NOT line_exists( lt_llm_apps[ app_id = <fs_bot>-app_id
                                         business_function = <fs_bot>-business_function ] ).
          APPEND INITIAL LINE TO lt_llm_apps ASSIGNING FIELD-SYMBOL(<fs_apps>).
          <fs_apps>-app_id = <fs_bot>-app_id.
          <fs_apps>-business_function = <fs_bot>-business_function.

        ENDIF.

      ENDLOOP.

      SORT lt_llm_apps BY app_id business_function function_name version ASCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_llm_apps COMPARING app_id business_function function_name version.

*     Get all the external values for passing to UI
      TRY.
          DATA(lt_external_values) = me->get_external_values( ).

        CATCH zcx_llm INTO DATA(lcx_llm).
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.

      ENDTRY.

*     Get the current version of the app.
      DATA(zcl_llm_version_util) = NEW zcl_llm_version_control_util( ).


      DATA: ls_llm          TYPE zllm_app_odata,
            lv_beta_version TYPE zllm_version.

*     Get active and beta versions for each app
      LOOP AT lt_llm_apps ASSIGNING <fs_apps>.

        AT NEW function_name.

          TRY.
              DATA(versions) = zcl_llm_version_util->get_active_app_version( app_id            = <fs_apps>-app_id
                                                                             business_function = <fs_apps>-business_function ).

              IF versions-beta_version IS NOT INITIAL.

                lv_beta_version = versions-beta_version.

              ENDIF.

            CATCH zcx_llm INTO lcx_llm.
              RAISE EXCEPTION TYPE zcx_llm
                EXPORTING
                  textid = lcx_llm->if_t100_message~t100key
                  msg1   = lcx_llm->msg1
                  msg2   = lcx_llm->msg2
                  msg3   = lcx_llm->msg3.
          ENDTRY.


          ls_llm-agent = <fs_apps>-function_name.
          ls_llm-business_function = <fs_apps>-business_function.
          ls_llm-app = <fs_apps>-app_id.
          ls_llm-automate = <fs_apps>-automate.
          ls_llm-enabled_for_web = <fs_apps>-web_enabled.

*         If this is an active version
          IF <fs_apps>-version = versions-active_version.

            IF <fs_apps>-deletion_indicator = abap_true.
              ls_llm-active_version = 0. "If delete indicator on set active version to 0
            ELSE.
              ls_llm-active_version = versions-active_version.

            ENDIF.
          ENDIF.


* Get the filter requirements.
          DATA(filter) = me->get_user_filters( app_id            = <fs_apps>-app_id
                                               business_function = <fs_apps>-business_function ).

* Even though filter is a table, there can only be 1 entry.
          TRY.

              DATA(ls_filter) = filter[ 1 ].

              DATA: start_date TYPE datum,
                    end_date   TYPE datum.

              CASE ls_filter-value.
                WHEN 'TODAY'.

                  start_date = sy-datum.
                  end_date = sy-datum.


                WHEN 'YESTERDAY'.

                  start_date = sy-datum - 1.
                  end_date = sy-datum - 1.

                WHEN '7DAY'.

                  start_date = sy-datum - 8.
                  end_date = sy-datum - 1.

                WHEN 'ACTIVATION'.

                  SELECT SINGLE dat
                  FROM zllm_version_ctr
                  INTO @DATA(version_date)
                  WHERE app_id = @<fs_apps>-app_id
                  AND business_function = @<fs_apps>-business_function
                  AND version = @<fs_apps>-version.

                  start_date = version_date.
                  end_date = sy-datum.


                WHEN 'ALLTIME'.

                  start_date = '20241101'.
                  end_date = sy-datum.

              ENDCASE.

            CATCH cx_sy_itab_line_not_found.
* On initial load, there may be no entries here. Default is all time.

              start_date = sy-datum.
              end_date = sy-datum.

          ENDTRY.


* How many times has the bot generated a response?
          SELECT SINGLE COUNT( uuid_key )
          FROM zllm_log
          WHERE object_type = 'GENERATE_CONTENT'
          AND app_id = @<fs_apps>-app_id
          AND buss_function = @<fs_apps>-business_function
          AND dat BETWEEN @start_date AND @end_date
          AND user_name NOT IN ( 'ASEXTON', 'OPUGLISI','DRAMACHANDRA','NFARRELL','VHARIHARAN','BABBOTT' )
          INTO @DATA(count_generate).

* Now count how many times feedback was provided
          SELECT SINGLE COUNT( uuid_key )
          FROM zllm_log
          WHERE object_type = 'Freshdesk'
          AND app_id = @<fs_apps>-app_id
          AND buss_function = @<fs_apps>-business_function
          AND dat BETWEEN @start_date AND @end_date
          AND user_name NOT IN ( 'ASEXTON', 'OPUGLISI','DRAMACHANDRA','NFARRELL','VHARIHARAN','BABBOTT' )
          INTO @DATA(count_feedback).


* How many times rating was given for this agent / prompt
          SELECT SINGLE COUNT( uuid_key )
          FROM zllm_log
          WHERE zllm_log~object_type = 'FUNCTION_CALL'
          AND app_id = @<fs_apps>-app_id
          AND buss_function = @<fs_apps>-business_function
          AND zllm_log~object_key = @<fs_apps>-function_name
          AND zllm_log~dat BETWEEN @start_date AND @end_date
          AND user_name NOT IN ( 'ASEXTON', 'OPUGLISI','DRAMACHANDRA','NFARRELL','VHARIHARAN','BABBOTT' )
          INTO @ls_llm-rating_count.


          DATA: lv_feedback_percent TYPE p DECIMALS 2.

          TRY.
              lv_feedback_percent = ( count_feedback / count_generate ) * 100.
            CATCH cx_root.
          ENDTRY.

          ls_llm-feedback_percent = lv_feedback_percent.

          TRY.

              IF <fs_apps>-function_name IS INITIAL.    "New bot with no agent yet

                ls_llm-beta_version = lv_beta_version.

              ELSE.

                IF lt_llm_apps[ version = versions-beta_version function_name = <fs_apps>-function_name ]-deletion_indicator = abap_true.
                  ls_llm-beta_version = 0.
                ELSE.
                  ls_llm-beta_version = lt_llm_apps[ version = versions-beta_version ]-version.
                ENDIF.

              ENDIF.

            CATCH cx_sy_itab_line_not_found.

          ENDTRY.

          IF line_exists( lt_external_values[ mandt = sy-mandt
                                              type  = 'APP_ID'
*                                              internal_value = <fs_apps>-app_id ] ).
                                              internal_value = <fs_apps>-app_id && |_| && <fs_apps>-business_function ] ).

            ls_llm-app_external_name = lt_external_values[ mandt = sy-mandt
                                                           type  = 'APP_ID'
*                                                           internal_value = <fs_apps>-app_id ]-external_value.
                                                           internal_value = <fs_apps>-app_id && |_| && <fs_apps>-business_function ]-external_value.
          ENDIF.


          IF line_exists( lt_external_values[ mandt = sy-mandt
                                              type  = 'FUNCTION_NAME'
                                              internal_value = <fs_apps>-function_name ] ).

            ls_llm-agent_external_name = lt_external_values[ mandt = sy-mandt
                                                           type  = 'FUNCTION_NAME'
                                                           internal_value = <fs_apps>-function_name ]-external_value.

          ENDIF.

          IF line_exists( lt_bots[ app_id = <fs_apps>-app_id
                                   business_function = <fs_apps>-business_function ] ).

            ls_llm-enabled = lt_bots[ app_id = <fs_apps>-app_id
                                      business_function = <fs_apps>-business_function ]-enabled.

          ENDIF.

* Get the average prompt rating
          DATA(lcl_prompt_util) = NEW zcl_llm_prompt_util( app_id            = <fs_apps>-app_id
                                                           business_function = <fs_apps>-business_function ).

          ls_llm-rating = lcl_prompt_util->get_prompt_avg_rating( prompt_key = CONV zllm_prompt_key( <fs_apps>-function_name )
                                                                  start_date = start_date
                                                                  end_date   = end_date ).


*         Supervisor indicator
          SELECT SINGLE child
             FROM zllm_agent_map
             INTO @DATA(lv_child)
             WHERE parent = @<fs_apps>-function_name
               AND version = @<fs_apps>-version
               AND type = 'SUB'.

          IF sy-subrc = 0.

            ls_llm-supervisor = abap_true.

          ENDIF.

          APPEND ls_llm TO llm_apps.
          CLEAR: ls_llm.

        ENDAT.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD check_function_declaration.

    SELECT function_name
    FROM zllm_func_declar
    INTO TABLE @DATA(lt_func_declar)
    WHERE app_id = @app_id
    AND business_function = @business_function
    AND function_name = @function_name.

* If we find an entry the function exists.
    IF sy-subrc <> 0.
      function_exists = abap_false.
    ELSE.
      function_exists = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD check_auths.

    authorised = lcl_llm_util->check_auths( ).

  ENDMETHOD.


  METHOD call_model.


*** Check for user version
    DATA(lcl_llm_version_controller) = NEW zcl_llm_version_control_util(  ).

    DATA(versions) = lcl_llm_version_controller->get_active_app_version( app_id            = me->app_id
                                                                         business_function = me->business_function ).

*   Checking if user version is not same as active, as user version seems to be retained in the table once it's activated
    IF versions-user_version IS NOT INITIAL AND versions-user_version <> versions-active_version.
      DATA(user_version) = abap_true.
    ENDIF.

*   If user version is not active, proceed with the checks below.
*   However, if a user version exists, then skip the checks and allow for LLM to be called
    IF user_version = abap_false.

*     Check if the AI has already actioned this.
      SELECT SINGLE new_value
      FROM zllm_log
      INTO @DATA(lv_new_value)
      WHERE object_key = @system_key
      AND object_type = 'ACTIONED'.


      IF sy-subrc = 0.
        response-response = lv_new_value.
        response-actioned = abap_true.
        RETURN.
      ENDIF.

*     Check if the ticket is in Open status, if not, do no proceed further and exit
      DATA(not_open) = me->get_freshdesk_ticket_status( ticket = CONV zcs_freshdesk_ticket_id( system_key ) ).

*     If the ticket is not in Open status,
*     1. Check if the App ID and business function are that of SOCIAL, skip the check
*     2. If anything else, perform the check to fetch the response generated for the ticket from the logs
      IF not_open = abap_true.

        IF app_id = 'CSHUB' AND
           business_function = 'SOCIAL'.

*       Do nothing and proceed with the response generation

        ELSE.

          SELECT SINGLE new_value
          FROM zllm_log
          INTO @lv_new_value
          WHERE object_key = @system_key
          AND object_type = 'Freshdesk'.

          response-response = lv_new_value.

          RETURN.

        ENDIF.

      ENDIF.

    ENDIF.

    TRY.
        lcl_llm_util->add_function_call_properties(  ).

        response = lcl_llm_util->call_model( prompt     = prompt
                                             system_key = system_key ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.

    CLEAR : user_version,
            not_open.


  ENDMETHOD.


  METHOD update_rag_agent.

*** Method implementation to update(create/change) agents - one agent at a time

    DATA(lref_llm_agents_util) = NEW zcl_llm_agents_util( app_id            = app_id
                                                          business_function = business_function ).

    DATA: lt_agent_details      TYPE zllm_agent_cds_tt,
          ls_agent_details_type TYPE zllm_agent_cds.

    LOOP AT it_agent_details ASSIGNING FIELD-SYMBOL(<fs_agent_details>).

      ls_agent_details_type-agent_deletion_indicator = <fs_agent_details>-agent_deletion_indicator.
      ls_agent_details_type-agent_key = <fs_agent_details>-function_name.
      ls_agent_details_type-agent_type = <fs_agent_details>-agent_type.
      ls_agent_details_type-app_id = <fs_agent_details>-app_id.
      ls_agent_details_type-automate = abap_false.
      ls_agent_details_type-business_function = <fs_agent_details>-business_function.
      ls_agent_details_type-external_value = <fs_agent_details>-external_value.
      ls_agent_details_type-function_description = <fs_agent_details>-function_description.
      ls_agent_details_type-function_parameters_id = <fs_agent_details>-parameters_id.
      ls_agent_details_type-function_version = <fs_agent_details>-function_version.
      ls_agent_details_type-is_required = <fs_agent_details>-is_required.
      ls_agent_details_type-parameters_id = <fs_agent_details>-parameters_id.
      ls_agent_details_type-parameter_description = <fs_agent_details>-parameter_description.
      ls_agent_details_type-parameter_name = <fs_agent_details>-parameter_name.
      ls_agent_details_type-parameter_type = <fs_agent_details>-parameter_type.
      ls_agent_details_type-param_deletion_indicator = <fs_agent_details>-param_deletion_indicator.
      ls_agent_details_type-version = <fs_agent_details>-version.

      APPEND ls_agent_details_type TO lt_agent_details.

    ENDLOOP.


    TRY.

        lref_llm_agents_util->update_rag_agent( it_agent_details = lt_agent_details ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2.


    ENDTRY.


  ENDMETHOD.


  METHOD update_agent.
*** Method implementation to update(create/change) agents - one agent at a time

    DATA(lref_llm_agents_util) = NEW zcl_llm_agents_util( app_id            = app_id
                                                          business_function = business_function ).

    TRY.

        lref_llm_agents_util->update_agent( it_agent_details = it_agent_details ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2.


    ENDTRY.

  ENDMETHOD.


  METHOD get_next_agent_id.

    IF iv_app_id IS NOT INITIAL AND
       iv_buss_function IS NOT INITIAL.

      TRY.

          me->get_config(
            EXPORTING
              app_id            = iv_app_id
              business_function = iv_buss_function
              param             = 'NUMBER_RANGE_INTERVAL'
              type              = 'GLOBAL'
            RECEIVING
              value             = DATA(lv_number_range_interval)
          ).


          DATA(lref_llm_agents_util) = NEW zcl_llm_agents_util( app_id            = app_id
                                                                business_function = business_function ).

          lref_llm_agents_util->get_next_agent_id(
            EXPORTING
              iv_app_id        = iv_app_id
              iv_buss_function = iv_buss_function
              iv_nr_interval   = CONV char2( lv_number_range_interval )
            RECEIVING
              rv_agent_id      = rv_agent_id ).


        CATCH zcx_llm INTO DATA(lcx_llm).
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2.
      ENDTRY.


    ENDIF.

  ENDMETHOD.


  METHOD get_external_values.
*** Method implementation to get external values maintained

    SELECT zllm_ext_value~mandt,
           zllm_ext_value~type,
           zllm_ext_value~internal_value,
           zllm_ext_value~external_value,
           zllm_ext_value~lang
      FROM zllm_ext_value
      INTO CORRESPONDING FIELDS OF TABLE @rt_external_values.
    "WHERE type IN @it_type_range.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>external_values_not_maintained.

    ENDIF.


  ENDMETHOD.


  METHOD update_bot.
*** Method implementation to create/change bot details

* Assign locking.
    me->handle_locking( type   = 'BOT'
                        key    = |{ iv_app_id }{ iv_buss_func }|
                        delete = abap_false ).


    DATA(lref_llm_bot_handler) = NEW zcl_llm_bot_handler( ).

    TRY.

        lref_llm_bot_handler->manage_bot(
          iv_app_id                = iv_app_id
          iv_buss_func             = iv_buss_func
          iv_external_name         = iv_external_name
          iv_enabled               = iv_enable
          iv_deletion_flag         = iv_delete
          iv_helper_app_id         = iv_helper_app_id
          iv_helper_bus_function   = iv_helper_bus_function
          iv_eval_bot_app_id       = iv_eval_bot_app_id
          iv_eval_bot_bus_function = iv_eval_bot_bus_function
          iv_bot_number_range      = iv_bot_number_range
          iv_fc_number_range       = iv_fc_number_range
        ).


      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2.


    ENDTRY.


  ENDMETHOD.


  METHOD get_prompt_by_rating.


*    SELECT zllm_star_rating~*
*    FROM zllm_star_rating
*    INTO TABLE @DATA(lt_rating)
*    WHERE zllm_star_rating~app_id = @app_id
*    AND zllm_star_rating~buss_function = @business_function
*    AND zllm_star_rating~rating IN @rating_range.


  ENDMETHOD.


  METHOD get_feedb_by_object_key.

    TRY.

        DATA(lv_obj_type) = it_filter_select_options[ property = 'ObjectType' ]-select_options[ 1 ]-low.

      CATCH cx_sy_itab_line_not_found.
* Do nothing
    ENDTRY.

    TRY.

        DATA(lv_obj_key) = it_filter_select_options[ property = 'ObjectKey' ]-select_options[ 1 ]-low.


      CATCH cx_sy_itab_line_not_found.
* Do nothing
    ENDTRY.

    IF lv_obj_type <> space AND lv_obj_type <> space.

      SELECT *
      FROM zllm_log
      INTO TABLE @DATA(lt_log)
      WHERE object_type = @lv_obj_type
      AND object_key = @lv_obj_key
      AND sentiment <> 'NOFEEDBACK'
      ORDER BY dat DESCENDING,time DESCENDING.


      SELECT *
      FROM zllm_star_rating
      INTO TABLE @DATA(lt_star)
      WHERE object_type = @lv_obj_type
      AND object_key = @lv_obj_key
      ORDER BY dat DESCENDING, time DESCENDING.

      IF lt_log IS NOT INITIAL.

        rs_feedback-feedback_complete = abap_true.

        TRY.

            DATA(ls_star) = lt_star[ 1 ].

            rs_feedback-rating = ls_star-rating.

          CATCH cx_sy_itab_line_not_found.

* Do nothing

        ENDTRY.

      ELSE.
* If this was an error we would not have feedback in log so check for one star in rating table
*Read to find the 1 star entry
        TRY.
            DATA(ls_star2) = lt_star[ rating = '1' ].

            rs_feedback-feedback_complete = abap_true.
            rs_feedback-rating = ls_star2-rating.

          CATCH cx_sy_itab_line_not_found.

* Table is sorted descending by time and date so entry 1 is the most recent
            TRY.

                DATA(ls_star3) = lt_star[ 1 ].

                rs_feedback-feedback_complete = abap_false.
                rs_feedback-rating = ls_star3-rating.

              CATCH cx_sy_itab_line_not_found.

                rs_feedback-feedback_complete = abap_false.

            ENDTRY.

        ENDTRY.



      ENDIF.

*     Check if the ticket was replied to automatically, as it'll be logged differently
*     If yes, respond back with feedback complete
      SELECT *
      FROM zllm_log
      INTO TABLE @DATA(lt_auto_reply_log)
      WHERE object_type = 'ACTIONED'
        AND object_key = @lv_obj_key
      ORDER BY dat DESCENDING,time DESCENDING.

      IF lt_auto_reply_log IS NOT INITIAL.

        rs_feedback-feedback_complete = abap_true.

      ENDIF.


    ENDIF.

  ENDMETHOD.


  METHOD get_models.


    DATA(lt_config) = lcl_llm_util->get_config( ).


    IF lt_config IS NOT INITIAL.

      DATA(lv_model_count) = lines( lt_config ).

*     An attempt to display the last added model to the Google AI config table first on UI (:
      rt_models = VALUE zllm_user_model_odata_tt( FOR i = lv_model_count
                                                  THEN i - 1
                                                  UNTIL i = 0
                                                  ( app_id = app_id
                                                    business_function = business_function
                                                    model_id = lt_config[ i ]-model_id ) ).

    ENDIF.

  ENDMETHOD.


  METHOD set_user_model.


    DATA(lcl_version_control) = NEW zcl_llm_version_control_util(  ).

    TRY.

        lcl_version_control->set_user_model( app_id            = app_id
                                             business_function = business_function
                                             version           = version
                                             model_id          = model_id ).
      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.
  ENDMETHOD.


  METHOD delete_user_model.

    DATA(lcl_version_control) = NEW zcl_llm_version_control_util(  ).

    TRY.

        lcl_version_control->delete_user_model( app_id            = app_id
                                                business_function = business_function
                                                version           = version
                                                model_id          = model_id ).
      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.


  ENDMETHOD.


  METHOD get_freshdesk_ticket_status.
*** Method implementation to get current status of the freshdesk ticket passed
*   If it's not open(status 2), Not open is set to true

    IF ticket IS NOT INITIAL.


      TRY.

          DATA(lref_cs_hub_integration_util) = NEW zcl_cs_hub_integration_handler( ).

          DATA(ls_ticket_content) = lref_cs_hub_integration_util->fetch_freshdesk_ticket_content( iv_ticket_id = ticket ).

          IF ls_ticket_content IS NOT INITIAL AND
             ls_ticket_content-status <> 2.       "Status code 2 : Open

            not_open = abap_true.

          ENDIF.

*       In case of error, send it's not open as it's safer
        CATCH zcx_cs_hub_integration INTO DATA(lcx_integration).

          not_open = abap_true.

      ENDTRY.

    ENDIF.


  ENDMETHOD.


  METHOD get_agent_catalog_menu.
*** Method implementation to get Agents Catalog configured

*   Not passing App ID and business function, as it's for the menu - should fetch the whole list
    DATA(lcl_llm_agent_catalog) = NEW zcl_llm_agent_catalog_util( app_id            = ''
                                                                  business_function = '' ).

    rt_agent_catalog_menu = lcl_llm_agent_catalog->get_catalog_agents( ).

  ENDMETHOD.


  METHOD get_mcp_agent_response_struct.
*** Method iplementation to get MCP agent's response structure

    DATA(lcl_llm_mcp_config) = NEW zcl_llm_mcp_config_util( ).

    response_structure = lcl_llm_mcp_config->get_agent_response_struct_json( app_id            = app_id
                                                                             business_function = business_function
                                                                             agent_name        = agent_name ).


  ENDMETHOD.


  METHOD get_mcp_class_config.
*** Method iplementation to get MCP class config

    DATA(lcl_llm_mcp_config) = NEW zcl_llm_mcp_config_util( ).

    mcp_class_config = lcl_llm_mcp_config->get_rag_class_details( app_id            = app_id
                                                                  business_function = business_function
                                                                  agent_name        = agent_name ).


  ENDMETHOD.


  METHOD save_prompt_connector.

    TRY.
        DATA(lcl_llm_prompt_util) = NEW zcl_llm_prompt_util( app_id            = app_id
                                                             business_function = business_function ).

        lcl_llm_prompt_util->save_prompt_connector( prompt_connector = prompt_connector ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.

  ENDMETHOD.


  METHOD get_prompt_connector.

    TRY.
        DATA(lcl_llm_prompt_util) = NEW zcl_llm_prompt_util( app_id            = app_id
                                                             business_function = business_function ).

        prompt_connector = lcl_llm_prompt_util->get_prompt_connector( prompt_chain = prompt_chain
                                                                      prompt_key   = prompt_key
                                                                      source       = source ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.

  ENDMETHOD.


  METHOD get_agent_list.

    TRY.

        DATA(lcl_llm_agents_util) = NEW zcl_llm_agents_util( app_id            = app_id
                                                             business_function = business_function ).

        agents = lcl_llm_agents_util->get_agent_list( ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.

  ENDMETHOD.


  METHOD get_prompt_prop.

    TRY.
        DATA(lcl_llm_prompt_util) = NEW zcl_llm_prompt_util( app_id            = app_id
                                                             business_function = business_function ).

        prompt_prop = lcl_llm_prompt_util->get_prompt_prop( prompt_key = prompt_key ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.

  ENDMETHOD.


  METHOD update_prompt_connector_chain.

    TRY.
        DATA(lcl_llm_prompt_util) = NEW zcl_llm_prompt_util( app_id            = app_id
                                                             business_function = business_function ).

        lcl_llm_prompt_util->update_prompt_connector_chain( prompt_connector_chain = prompt_connector_chain ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.

  ENDMETHOD.


  METHOD create_update_tabs.

    DATA(lcl_llm_roles) = NEW zcl_llm_auths_util(  ).

    TRY.

        lcl_llm_roles->create_tabid( tabs = tabs ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.
  ENDMETHOD.


  METHOD get_tabs_and_permission.

    DATA(lcl_llm_roles) = NEW zcl_llm_auths_util(  ).

    auth_tabs = lcl_llm_roles->get_auth_tabid( ).

  ENDMETHOD.

  METHOD get_roles_and_authorization.

    TRY.

        DATA(lcl_llm_roles_auth) = NEW zcl_llm_roles_auth_util(  ).

        roles_auths = lcl_llm_roles_auth->get_roles_auth( ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
