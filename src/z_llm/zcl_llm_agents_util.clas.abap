CLASS zcl_llm_agents_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_fiori_llm_util
                 zcl_llm_util.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA : gref_llm_agents_sql   TYPE REF TO zcl_llm_agents_sql.

    METHODS :
      "! Constructor
      constructor IMPORTING app_id            TYPE zllm_app_id OPTIONAL
                            business_function TYPE zllm_buss_function_name OPTIONAL,

**********************************************************************
*     "! Method to fetch declarations/parameter descriptions of functions/agents
**********************************************************************
      get_agent                          IMPORTING iv_app_id                       TYPE zllm_app_id
                                                   iv_buss_function                TYPE zllm_buss_function_name
                                                   iv_function_name                TYPE zllm_function_name
                                                   VALUE(iv_version)               TYPE zllm_version OPTIONAL
                                                   type                            TYPE zllm_function_type OPTIONAL
                                         RETURNING VALUE(rt_function_declarations) TYPE zllm_agent_cds_tt
                                         RAISING   zcx_llm,

**********************************************************************
*      "! Method to update declarations/parameter descriptions of functions/agents
**********************************************************************
      update_agent                       IMPORTING it_agent_details TYPE zllm_agent_cds_tt
                                         RAISING   zcx_llm,

**********************************************************************
*      "! Method to get the next agent ID using a number range object for the bot
*      "! (App ID and business function combo) passed
**********************************************************************
      get_next_agent_id                  IMPORTING iv_app_id          TYPE zllm_app_id
                                                   iv_buss_function   TYPE zllm_buss_function_name
                                                   iv_nr_interval     TYPE char2
                                         RETURNING VALUE(rv_agent_id) TYPE zllm_function_parameters_id
                                         RAISING   zcx_llm,

**********************************************************************
* Use this method to get sub agents to be used in a composed function call
**********************************************************************
      get_sub_agents                      IMPORTING app_id            TYPE zllm_app_id
                                                    business_function TYPE zllm_buss_function_name
                                                    supv_agent        TYPE zllm_function_name
                                                    VALUE(version)    TYPE zllm_version OPTIONAL
                                          RETURNING VALUE(sub_agents) TYPE zllm_sub_func_odata_tt
                                          RAISING   zcx_llm,

**********************************************************************
* Use this method to update a sub agent
**********************************************************************
      update_sub_agent IMPORTING VALUE(sub_agent) TYPE zllm_sub_func_odata
                       RAISING   zcx_llm,

**********************************************************************
* Use this method to check the existence of function module for the agent name/function name
**********************************************************************
      check_existence_of_fm     IMPORTING iv_agent_name    TYPE zllm_function_name
                                RETURNING VALUE(rv_exists) TYPE char1
                                RAISING   zcx_llm,

**********************************************************************
* Fetch all rag agents assigned to a prompt chain
**********************************************************************
      get_prompt_to_rag IMPORTING agent      TYPE string
                                  version    TYPE zllm_version
                        RETURNING VALUE(rag) TYPE zllm_rag_odata_tt
                        RAISING   zcx_llm,

**********************************************************************
* Save RAG agents against a prompt chain.
**********************************************************************
      save_prompt_chain_to_rag IMPORTING rag TYPE zllm_rag_odata_tt
                               RAISING   zcx_llm,

**********************************************************************
* Get RAG agents
**********************************************************************
      get_rag_agents IMPORTING agent            TYPE zllm_function_name
                               version          TYPE zllm_version
                               type             TYPE zllm_type_map
                     RETURNING VALUE(agent_map) TYPE zllm_agent_map_tt
                     RAISING   zcx_llm,

**********************************************************************
* Update RAG agent
**********************************************************************
      update_rag_agent  IMPORTING it_agent_details TYPE zllm_agent_cds_tt
                        RAISING   zcx_llm,

**********************************************************************
* Get active agents
**********************************************************************
      get_agent_list RETURNING VALUE(agents) TYPE zllm_agents_list_tt
                     RAISING   zcx_llm.

    DATA: gv_app_id            TYPE zllm_app_id,
          gv_business_function TYPE zllm_buss_function_name,
          gv_version           TYPE zllm_version.

ENDCLASS.



CLASS zcl_llm_agents_util IMPLEMENTATION.

  METHOD constructor.

    me->gv_app_id = app_id.
    me->gv_business_function = business_function.

*   Instantiate SQL class for agents
    me->gref_llm_agents_sql = NEW zcl_llm_agents_sql(  ).

    IF me->gv_app_id IS NOT INITIAL AND
       me->gv_business_function IS NOT INITIAL.

      TRY.
          DATA(lcl_llm_version_controller) = NEW zcl_llm_version_control_util(  ).
          DATA(versions) = lcl_llm_version_controller->get_active_app_version( app_id            = me->gv_app_id
                                                                               business_function = me->gv_business_function ).

* User version will always override current version.
          IF versions-user_version IS NOT INITIAL.
            me->gv_version = versions-user_version.
          ELSE.
            me->gv_version = versions-active_version.
          ENDIF.

        CATCH zcx_llm.

          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>invalid_app_id
              msg1   = CONV string( app_id ).

      ENDTRY.

    ENDIF.


  ENDMETHOD.


  METHOD get_rag_agents.

    SELECT *
      FROM zllm_agent_map
      INTO TABLE @agent_map
     WHERE parent = @agent
       AND deletion_indicator = @abap_false
       AND version = @version
       AND type = 'RAG'.


    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>no_chained_agents
          msg1   = CONV string( agent ).
    ENDIF.

  ENDMETHOD.


  METHOD save_prompt_chain_to_rag.

    DATA(lcl_llm_crud_util) = NEW zcl_llm_crud_control_util( app_id            = me->gv_app_id
                                                             business_function = me->gv_business_function ).

    TRY.
        lcl_llm_crud_util->save_prompt_chain_to_rag( prompt_to_rag = rag ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.


  ENDMETHOD.


  METHOD get_prompt_to_rag.

* Get all the assigned RAG agents for a specific prompt chain.

    IF agent IS INITIAL.

      TYPES: BEGIN OF ty_agent_map,
               parent TYPE zllm_function_name,
               child  TYPE zllm_function_name,
               type   TYPE zllm_type_map,
             END OF ty_agent_map.


      DATA: ls_agent_map TYPE ty_agent_map,
            lt_agent_map TYPE TABLE OF ty_agent_map.


* If the agent is initial, display allowed RAG agents based on the catalog.

* Get the catalog assigned to the bot.
      TRY.
          DATA(lcl_agent_catalog_util) = NEW zcl_llm_agent_catalog_util( app_id            = me->gv_app_id
                                                                         business_function = me->gv_business_function ).

          DATA(lt_catalog) = lcl_agent_catalog_util->get_catalog_map( ).

        CATCH zcx_llm INTO DATA(lcx_llm).

          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.

      ENDTRY.


* For each catalog, get the agents.
      LOOP AT lt_catalog ASSIGNING FIELD-SYMBOL(<fs_catalog>).

        SELECT agent_name
        FROM zllm_agent_cat
        INTO TABLE @DATA(lt_agent_name)
        WHERE catalog_name = @<fs_catalog>-catalog_name.


        LOOP AT lt_agent_name ASSIGNING FIELD-SYMBOL(<fs_agent_name>).

          ls_agent_map-child = <fs_agent_name>-agent_name.
          APPEND ls_agent_map TO lt_agent_map.

        ENDLOOP.


      ENDLOOP.

    ELSE.

      SELECT parent, child, type
      FROM zllm_agent_map
      INTO TABLE @lt_agent_map
      WHERE parent = @agent
      AND deletion_indicator = @abap_false
      AND version = @version
      AND type = 'RAG'.                 "As it's a prompt chain to RAG mapping we are looking for

    ENDIF.

* For each prompt, get the details.
    DATA: ls_rag_odata TYPE zllm_rag_odata.


    LOOP AT lt_agent_map ASSIGNING FIELD-SYMBOL(<fs_agent_map>).

* Get the agent descriptions
      SELECT SINGLE function_name, function_description, function_parameters_id
      FROM zllm_func_declar
      INTO @DATA(ls_agent)
      WHERE app_id ='LLM'
      AND business_function = 'MCP'
      AND function_name = @<fs_agent_map>-child
      AND version = 1.

* Get the agent parameters
      SELECT parameter_name, parameter_description, parameter_type, is_required
      FROM zllm_func_parame
      INTO TABLE @DATA(lt_params)
      WHERE app_id = 'LLM'
      AND business_function = 'MCP'
      AND parameters_id = @ls_agent-function_parameters_id.


      ls_rag_odata-agent = <fs_agent_map>-child.
      ls_rag_odata-agent_description = ls_agent-function_description.

      LOOP AT lt_params ASSIGNING FIELD-SYMBOL(<fs_params>).

        CASE sy-tabix.
          WHEN 1.
            ls_rag_odata-param1 = <fs_params>-parameter_name.
            ls_rag_odata-param1_desc = <fs_params>-parameter_description.
            ls_rag_odata-param1_type = <fs_params>-parameter_type.
            ls_rag_odata-param1_required = <fs_params>-is_required.

          WHEN 2.
            ls_rag_odata-param2 = <fs_params>-parameter_name.
            ls_rag_odata-param2_desc = <fs_params>-parameter_description.
            ls_rag_odata-param2_type = <fs_params>-parameter_type.
            ls_rag_odata-param2_required = <fs_params>-is_required.

          WHEN 3.
            ls_rag_odata-param3 = <fs_params>-parameter_name.
            ls_rag_odata-param3_desc = <fs_params>-parameter_description.
            ls_rag_odata-param3_type = <fs_params>-parameter_type.
            ls_rag_odata-param3_required = <fs_params>-is_required.

          WHEN 4.
            ls_rag_odata-param4 = <fs_params>-parameter_name.
            ls_rag_odata-param4_desc = <fs_params>-parameter_description.
            ls_rag_odata-param4_type = <fs_params>-parameter_type.
            ls_rag_odata-param4_required = <fs_params>-is_required.

          WHEN 5.
            ls_rag_odata-param5 = <fs_params>-parameter_name.
            ls_rag_odata-param5_desc = <fs_params>-parameter_description.
            ls_rag_odata-param5_type = <fs_params>-parameter_type.
            ls_rag_odata-param5_required = <fs_params>-is_required.

        ENDCASE.

      ENDLOOP.

      APPEND ls_rag_odata TO rag.
      CLEAR : ls_rag_odata.

    ENDLOOP.


  ENDMETHOD.


  METHOD update_sub_agent.

    DATA: ls_sub_agent          TYPE zllm_func_declar,
          ls_sub_agent_param    TYPE zllm_func_parame,
          ls_sub_agent_ext_name TYPE zllm_ext_value,
          ls_agent_map          TYPE zllm_agent_map.

*   Agent must be in upper case.
    sub_agent-sub_function_name = to_upper( sub_agent-sub_function_name ).

*   Ensure that the sub function/agent name is prefixed with 'Z_LLM' as well
    IF sub_agent-sub_function_name+0(6) <> 'Z_LLM_'.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>wrong_prefix_for_agent_name.

    ENDIF.


    TRY.
* Ensure that the sub agent FM exists.
        DATA(lv_fm_exists) = me->check_existence_of_fm( iv_agent_name = sub_agent-sub_function_name ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.


    TRY.
*         Check if indexing for the agent exists already. If yes, do not do anything.
        DATA(lv_agent_index) = me->gref_llm_agents_sql->check_if_agent_index_exists( iv_app_id        = sub_agent-app_id
                                                                                     iv_buss_function = sub_agent-business_function
                                                                                     iv_function_name = sub_agent-sub_function_name ).

        IF lv_agent_index IS INITIAL.

          DATA: ls_function_index TYPE zllm_func_index.

          DATA(lv_latest_agent_index) = me->gref_llm_agents_sql->fetch_latest_agent_index( iv_app_id        = sub_agent-app_id
                                                                                           iv_buss_function = sub_agent-business_function ).

          ls_function_index-mandt = sy-mandt.
          ls_function_index-app_id = sub_agent-app_id.
          ls_function_index-business_function = sub_agent-business_function.
          ls_function_index-function_name = sub_agent-sub_function_name.
          ls_function_index-function_index = lv_latest_agent_index + 1.

        ENDIF.

*       Not raising an exception as yet, as it can be maintained manually
      CATCH zcx_llm INTO lcx_llm.
    ENDTRY.


*   --SUB AGENT DECLARATION
    ls_sub_agent-mandt = sy-mandt.
    ls_sub_agent-app_id = sub_agent-app_id.
    ls_sub_agent-business_function = sub_agent-business_function.
    ls_sub_agent-deletion_indicator = sub_agent-agent_deletion_indicator.
    ls_sub_agent-function_description = sub_agent-function_description.
    ls_sub_agent-function_name = sub_agent-sub_function_name.
    ls_sub_agent-function_parameters_id = sub_agent-function_parameters_id.
    ls_sub_agent-function_version = sub_agent-function_version.
    ls_sub_agent-version = sub_agent-version.
    ls_sub_agent-type = 'SUB'.


*   --SUB AGENT PARAMETERS
    ls_sub_agent_param-mandt = sy-mandt.
    ls_sub_agent_param-app_id = sub_agent-app_id.
    ls_sub_agent_param-business_function = sub_agent-business_function.
    ls_sub_agent_param-deletion_indicator = sub_agent-param_deletion_indicator.
    ls_sub_agent_param-is_required = sub_agent-is_required.
    ls_sub_agent_param-parameters_id = sub_agent-function_parameters_id.
    ls_sub_agent_param-parameter_description = sub_agent-parameter_description.
    ls_sub_agent_param-parameter_name = sub_agent-parameter_name.
    ls_sub_agent_param-parameter_type = sub_agent-parameter_type.
    ls_sub_agent_param-version = sub_agent-version.

*   --SUB AGENT EXTERNAL NAME
    ls_sub_agent_ext_name-mandt = sy-mandt.
    ls_sub_agent_ext_name-lang = 'E'.
    ls_sub_agent_ext_name-type = 'SUB_FUNCTION_NAME'.
    ls_sub_agent_ext_name-internal_value = sub_agent-sub_function_name.
    ls_sub_agent_ext_name-external_value = sub_agent-external_value.

*   --SUB AGENT MAP
    ls_agent_map-mandt = sy-mandt.
    ls_agent_map-app_id = sub_agent-app_id.
    ls_agent_map-business_function = sub_agent-business_function.
    ls_agent_map-parent = sub_agent-supv_function_name.
    ls_agent_map-child = sub_agent-sub_function_name.
    ls_agent_map-type = 'SUB'.
    ls_agent_map-version = sub_agent-version.
    ls_agent_map-deletion_indicator = sub_agent-agent_deletion_indicator.


    TRY.
        DATA(lcl_crud) = NEW zcl_llm_crud_control_util( app_id            = sub_agent-app_id
                                                        business_function = sub_agent-business_function ).


        lcl_crud->save_sub_agent( supv_function_name = sub_agent-supv_function_name
                                  sub_agent          = ls_sub_agent
                                  sub_agent_param    = ls_sub_agent_param
                                  external_value     = ls_sub_agent_ext_name
                                  agent_map          = ls_agent_map
                                  function_index     = ls_function_index ).

      CATCH zcx_llm INTO lcx_llm.
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.

  ENDMETHOD.

  METHOD get_sub_agents.
*** Method implementation to get sub agents under the supervisor agent passed

*   Get the agent details using CDS
    TRY.

        IF version IS INITIAL.

          DATA(lref_llm_version_util) = NEW zcl_llm_version_control_util( ).
          DATA(versions) = lref_llm_version_util->get_active_app_version( app_id            = app_id
                                                                          business_function = business_function ).

          IF versions-user_version IS NOT INITIAL.
            version = versions-user_version.
          ELSE.
            version = versions-active_version.
          ENDIF.

        ENDIF.


        sub_agents = me->gref_llm_agents_sql->fetch_sub_agents( iv_app_id           = app_id
                                                                iv_buss_function    = business_function
                                                                iv_version          = version
                                                                iv_supervisor_agent = supv_agent ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.


    ENDTRY.


  ENDMETHOD.

  METHOD get_agent.
*** Method implementation to fetch all the agents maintained for the App ID, business function & agent name passed

*   Get the agent details using CDS
    IF iv_version IS INITIAL.


      DATA(lref_llm_version_util) = NEW zcl_llm_version_control_util( ).
      DATA(versions) = lref_llm_version_util->get_active_app_version( app_id            = iv_app_id
                                                                      business_function = iv_buss_function ).

      iv_version = versions-beta_version.

    ENDIF.

    TRY.
* If the type is RAG, select only the RAG agent. All RAG agents are version 1.
        IF type = 'RAG'.
          iv_version = 1.
          DATA(lt_function_type_range_tt) = VALUE zllm_function_type_range_tt( sign   = 'I'
                                                                                      option = 'EQ'
                                                                                    ( low    =  '' )
                                                                                    ( low    = 'RAG' ) ).

        ELSEIF type = 'ACTION'.

          iv_version = 1.
          lt_function_type_range_tt = VALUE zllm_function_type_range_tt( sign   = 'I'
                                                                         option = 'EQ'
                                                                       ( low    =  '' )
                                                                       ( low    = type ) ).


        ELSE.
*       NOTE : This is the highest level for agents. Hence, there could be two types - SUPERVISOR / ''(Blank)
          lt_function_type_range_tt = VALUE zllm_function_type_range_tt( sign   = 'I'
                                                                               option = 'EQ'
                                                                             ( low    =  '' )
                                                                             ( low    = 'SUPERVISOR' ) ).
        ENDIF.

        rt_function_declarations = me->gref_llm_agents_sql->fetch_agent( iv_app_id          = iv_app_id
                                                                         iv_buss_function   = iv_buss_function
                                                                         iv_function_name   = iv_function_name
                                                                         iv_version         = iv_version
                                                                         it_func_type_range = lt_function_type_range_tt ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.


    ENDTRY.


  ENDMETHOD.

  METHOD update_agent.
*** Method implementation to update agent details(declaration or parameters)
*** This is a common method for any kind of update - Create/Change/Delete

    DATA : lt_prompts TYPE zllm_prompts_tt.
    DATA : ls_function_index TYPE zllm_func_index.

    IF it_agent_details IS NOT INITIAL.

      DATA(agent) = it_agent_details[ 1 ].

*     Agent must be in upper case.
      agent-agent_key = to_upper( agent-agent_key ).

*     Ensure that the function/agent name is prefixed with 'Z_LLM'
      IF agent-agent_key+0(6) <> 'Z_LLM_'.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>wrong_prefix_for_agent_name.

      ENDIF.

      TRY.

          DATA(lv_fm_exists) = me->check_existence_of_fm( iv_agent_name = agent-agent_key ).

        CATCH zcx_llm INTO DATA(lcx_llm).
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.

      ENDTRY.

*     Step 1 : Trigger version-ing for the changes done
*              This would return the version number that the changes would have to be saved to
*              Note: The changes will be saved only to the latest beta version
      DATA(lref_llm_version_control_util) = NEW zcl_llm_version_control_util( ).

      TRY.

* Only create new version for non RAG agents
          IF agent-agent_type <> 'RAG'.
            DATA(lv_version) = lref_llm_version_control_util->create_new_app_version( app_id            = agent-app_id
                                                                                      business_function = agent-business_function ).

          ELSE.
* For RAG agents, version doesnt matter. Its always 1.
            lv_version = 1.
          ENDIF.


        CATCH zcx_llm INTO lcx_llm.
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.

      ENDTRY.


*     Step 2 : Build data for update in accordance to the structures of interest
*              Update changed data with the version number received from the previous step
**--  FUNCTION DECLARTION
      DATA(lt_function_declar) = VALUE zllm_func_declar_tt( FOR ls_agent IN it_agent_details
                                                          ( mandt             = sy-mandt
                                                            app_id            = ls_agent-app_id
                                                            business_function = ls_agent-business_function
                                                            version           = lv_version
                                                            function_name     = to_upper( ls_agent-agent_key )
                                                            function_description = ls_agent-function_description
                                                            function_parameters_id = ls_agent-function_parameters_id
                                                            function_version = ls_agent-function_version
                                                            deletion_indicator = ls_agent-agent_deletion_indicator
                                                            automate = ls_agent-automate
                                                            type     = ls_agent-agent_type
                                                            web_enabled = ls_agent-web_enabled
                                                             ) ).

      SORT lt_function_declar ASCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_function_declar COMPARING app_id business_function function_name version.

**--  FUNCTION PARAMETERS
      DATA(lt_function_parame) = VALUE zllm_func_parame_tt( FOR ls_agent IN it_agent_details
                                                          ( mandt             = sy-mandt
                                                            app_id            = ls_agent-app_id
                                                            business_function = ls_agent-business_function
                                                            version           = lv_version
                                                            parameters_id     = ls_agent-parameters_id
                                                            parameter_name = ls_agent-parameter_name
                                                            parameter_type = to_upper( ls_agent-parameter_type )
                                                            parameter_description = ls_agent-parameter_description
                                                            is_required = ls_agent-is_required
                                                            deletion_indicator = COND zllm_f_parameter_delete( WHEN ls_agent-agent_deletion_indicator IS NOT INITIAL
                                                                                                               THEN ls_agent-agent_deletion_indicator
                                                                                                               WHEN ls_agent-param_deletion_indicator IS NOT INITIAL
                                                                                                               THEN ls_agent-param_deletion_indicator
                                                                                                              ) ) ).

      SORT lt_function_parame ASCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_function_parame COMPARING app_id business_function parameter_name version.

**--  FUNCTION/AGENT EXTERNAL NAME
      DATA(ls_agent_ext_name) = VALUE zllm_ext_value( mandt = sy-mandt
                                                      lang  = 'E'     "Change later
                                                      type  = 'FUNCTION_NAME'
                                                      internal_value = agent-agent_key
                                                      external_value = agent-external_value ).

**--  FUNCTION/AGENT INDEX
      TRY.

*         Check if indexing for the agent exists already. If yes, do not do anything.
          DATA(lv_agent_index) = me->gref_llm_agents_sql->check_if_agent_index_exists( iv_app_id        = agent-app_id
                                                                                       iv_buss_function = agent-business_function
                                                                                       iv_function_name = agent-agent_key ).

          IF lv_agent_index IS INITIAL.

            DATA(lv_latest_agent_index) = me->gref_llm_agents_sql->fetch_latest_agent_index( iv_app_id        = agent-app_id
                                                                                             iv_buss_function = agent-business_function ).

            ls_function_index-mandt = sy-mandt.
            ls_function_index-app_id = agent-app_id.
            ls_function_index-business_function = agent-business_function.
            ls_function_index-function_name = agent-agent_key.
            ls_function_index-function_index = lv_latest_agent_index + 1.

          ENDIF.

*       Not raising an exception as yet, as it can be maintained manually
        CATCH zcx_llm INTO lcx_llm.
      ENDTRY.

*     Step 3 : Save changes to the DB
      DATA(lref_llm_crud_util) = NEW zcl_llm_crud_control_util( app_id            = agent-app_id
                                                                business_function = agent-business_function ).

      TRY.

          DATA(lv_status) = lref_llm_crud_util->save_function_declaration( func_declar    = lt_function_declar
                                                                           func_parame    = lt_function_parame
                                                                           func_index     = ls_function_index
                                                                           external_value = ls_agent_ext_name ).

        CATCH zcx_llm INTO lcx_llm.
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.

      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD get_next_agent_id.
*** Method implementation to get the next number for agent ID from it's number range object

*   Get the next agent ID
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = iv_nr_interval
        object                  = 'ZLLM'
      IMPORTING
        number                  = rv_agent_id
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
          msg1   = CONV string( iv_app_id )
          msg2   = CONV string( iv_buss_function ).
    ENDIF.


  ENDMETHOD.

  METHOD check_existence_of_fm.
*** Method implementation to check the existence of corresponding FM : Agent name in the App and FM name should match

    CALL FUNCTION 'RH_FUNCTION_EXIST'
      EXPORTING
        name               = CONV rs38l_fnam( iv_agent_name )
      EXCEPTIONS
        function_not_found = 1
        OTHERS             = 2.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>function_module_does_not_exist
          msg1   = CONV string( iv_agent_name ).

    ELSE.

      rv_exists = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD update_rag_agent.
*** Method implementation to update RAG agent details(declaration or parameters)
*** This is a common method for any kind of update - Create/Change/Delete

*   --RULES/STEPS:
*   1. Every RAG agent will be assigned to a catalogue and hence will be saved into ZLLM_AGENT_MAP table
*   2. The declaration and parameters, like other agent types will be saved into ZLLM_FUNC_DECLAR and ZLLM_FUNC_PARAME
*   3. RAG agents will not adhere to version-ing. Hence it's defaulted to 1.

    DATA : lt_prompts TYPE zllm_prompts_tt.
    DATA : ls_function_index TYPE zllm_func_index.
    DATA : lv_version TYPE zllm_version.

    IF it_agent_details IS NOT INITIAL.

*     RAG agents will be defaulted to version 1 always
      lv_version = 1.

      DATA(agent) = it_agent_details[ 1 ].

*     Agent must be in upper case.
      agent-agent_key = to_upper( agent-agent_key ).

*     Ensure that the function/agent name is prefixed with 'Z_LLM'
      IF agent-agent_key+0(6) <> 'Z_LLM_'.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>wrong_prefix_for_agent_name.

      ENDIF.

      TRY.

          "    DATA(lv_fm_exists) = me->check_existence_of_fm( iv_agent_name = agent-agent_key ).

        CATCH zcx_llm INTO DATA(lcx_llm).
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.

      ENDTRY.


*     Build data for update in accordance to the structures of interest
**--  FUNCTION DECLARTION
      DATA(lt_function_declar) = VALUE zllm_func_declar_tt( FOR ls_agent IN it_agent_details
                                                          ( mandt             = sy-mandt
                                                            app_id            = ls_agent-app_id
                                                            business_function = ls_agent-business_function
                                                            version           = lv_version
                                                            function_name     = to_upper( ls_agent-agent_key )
                                                            function_description = ls_agent-function_description
                                                            function_parameters_id = ls_agent-function_parameters_id
                                                            function_version = ls_agent-function_version
                                                            deletion_indicator = ls_agent-agent_deletion_indicator
                                                            automate = ls_agent-automate
                                                            type     = ls_agent-agent_type
                                                             ) ).

      SORT lt_function_declar ASCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_function_declar COMPARING app_id business_function function_name version.

**--  FUNCTION PARAMETERS
      DATA(lt_function_parame) = VALUE zllm_func_parame_tt( FOR ls_agent IN it_agent_details
                                                          ( mandt             = sy-mandt
                                                            app_id            = ls_agent-app_id
                                                            business_function = ls_agent-business_function
                                                            version           = lv_version
                                                            parameters_id     = ls_agent-parameters_id
                                                            parameter_name = ls_agent-parameter_name
                                                            parameter_type = to_upper( ls_agent-parameter_type )
                                                            parameter_description = ls_agent-parameter_description
                                                            is_required = ls_agent-is_required
                                                            deletion_indicator = COND zllm_f_parameter_delete( WHEN ls_agent-agent_deletion_indicator IS NOT INITIAL
                                                                                                               THEN ls_agent-agent_deletion_indicator
                                                                                                               WHEN ls_agent-param_deletion_indicator IS NOT INITIAL
                                                                                                               THEN ls_agent-param_deletion_indicator
                                                                                                              ) ) ).

      SORT lt_function_parame ASCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_function_parame COMPARING app_id business_function parameter_name version.

**--  FUNCTION/AGENT EXTERNAL NAME
      DATA(ls_agent_ext_name) = VALUE zllm_ext_value( mandt = sy-mandt
                                                      lang  = 'E'     "Change later
                                                      type  = 'FUNCTION_NAME'
                                                      internal_value = agent-agent_key
                                                      external_value = agent-external_value ).

**--  FUNCTION/AGENT INDEX
      TRY.

*         Check if indexing for the agent exists already. If yes, do not do anything.
          DATA(lv_agent_index) = me->gref_llm_agents_sql->check_if_agent_index_exists( iv_app_id        = agent-app_id
                                                                                       iv_buss_function = agent-business_function
                                                                                       iv_function_name = agent-agent_key ).

          IF lv_agent_index IS INITIAL.

            DATA(lv_latest_agent_index) = me->gref_llm_agents_sql->fetch_latest_agent_index( iv_app_id        = agent-app_id
                                                                                             iv_buss_function = agent-business_function ).

            ls_function_index-mandt = sy-mandt.
            ls_function_index-app_id = agent-app_id.
            ls_function_index-business_function = agent-business_function.
            ls_function_index-function_name = agent-agent_key.
            ls_function_index-function_index = lv_latest_agent_index + 1.

          ENDIF.

*       Not raising an exception as yet, as it can be maintained manually
        CATCH zcx_llm INTO lcx_llm.
      ENDTRY.

**--  FUNCTION/AGENT MAP TO CATALOGUE
      DATA(ls_agent_cat) = VALUE zllm_agent_cat(
          mandt                     = sy-mandt
          catalog_app_id            = agent-app_id
          catalog_business_function = agent-business_function
          agent_name                = agent-agent_key
          catalog_name              = agent-catalog_name
          deleted                   = agent-agent_deletion_indicator
      ).

*     Save changes to the DB
      DATA(lref_llm_crud_util) = NEW zcl_llm_crud_control_util( app_id            = agent-app_id
                                                                business_function = agent-business_function ).

      TRY.

          DATA(lv_status) = lref_llm_crud_util->save_function_declaration( func_declar    = lt_function_declar
                                                                           func_parame    = lt_function_parame
                                                                           func_index     = ls_function_index
                                                                           external_value = ls_agent_ext_name
                                                                           agent_catalog  = ls_agent_cat ).

        CATCH zcx_llm INTO lcx_llm.
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.

      ENDTRY.

    ENDIF.


  ENDMETHOD.

  METHOD get_agent_list.

    SELECT FROM zllm_func_declar AS a
    INNER JOIN zllm_ext_value AS b
    ON a~function_name = b~internal_value
    FIELDS a~app_id,
           a~business_function,
           a~function_name AS prompt_key,
           b~external_value,
           a~type
     WHERE app_id = @me->gv_app_id
       AND business_function = @me->gv_business_function
       AND version = @me->gv_version
       AND lang = @sy-langu
    INTO TABLE @agents.

  ENDMETHOD.

ENDCLASS.
