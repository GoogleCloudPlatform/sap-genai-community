CLASS zcl_z_llm_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_z_llm_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS : /iwbep/if_mgw_appl_srv_runtime~changeset_begin REDEFINITION,
      /iwbep/if_mgw_appl_srv_runtime~changeset_process REDEFINITION,
      /iwbep/if_mgw_appl_srv_runtime~changeset_end REDEFINITION.


  PROTECTED SECTION.

    METHODS: llmset_get_entityset REDEFINITION ,
      llmset_create_entity REDEFINITION,
      llmlogset_create_entity REDEFINITION,
      llmratinglogset_create_entity REDEFINITION,
      llmappset_get_entityset REDEFINITION,
      llmpromptset_get_entityset REDEFINITION,
      llmpromptset_create_entity REDEFINITION,
      llmagentset_get_entityset REDEFINITION,
      llmprompthistory_get_entityset REDEFINITION,
      llmpublishprompt_create_entity REDEFINITION,
      llmappversionsse_get_entityset REDEFINITION,
      llmappversionsse_create_entity REDEFINITION,
      llmnewagentidset_get_entity REDEFINITION,
      llmconfigset_get_entityset REDEFINITION,
      llmbotset_update_entity REDEFINITION,
      llmroleauthsset_create_entity REDEFINITION,
      llmuserroleset_create_entity REDEFINITION,
      llmroleauthsset_get_entityset REDEFINITION,
      llmuserroleset_get_entityset REDEFINITION,
      llmuserversionse_create_entity REDEFINITION,
      llmtotalusagerep_get_entityset REDEFINITION,
      llmagentusagerep_get_entityset REDEFINITION,
      llmpromptusagese_get_entityset REDEFINITION,
      llmratingsearchs_get_entityset REDEFINITION,
      llmpromptexample_create_entity REDEFINITION,
      llmpromptexample_get_entityset REDEFINITION,
      llmprompttrigger_get_entityset REDEFINITION,
      llmprompttrigger_create_entity REDEFINITION,
      llmfeedbacksearc_get_entityset REDEFINITION,
      llminteractionre_get_entityset REDEFINITION,
      llmuserfilterset_get_entityset REDEFINITION,
      llmuserfilterset_create_entity REDEFINITION,
      llmmodelset_update_entity REDEFINITION,
      llmmodelset_get_entityset REDEFINITION,
      llmbotset_get_entityset REDEFINITION,
      llmfeedbacknotes_create_entity REDEFINITION,
      llmsubagentset_get_entityset REDEFINITION,
      llmsubagentset_create_entity REDEFINITION,
      llmsubagentset_update_entity REDEFINITION,
      llmpromptusagese_create_entity REDEFINITION,
      llmlockingset_create_entity REDEFINITION,
      llmlockingset_get_entityset REDEFINITION,
      llmrotatelogset_get_entityset REDEFINITION,
      llmcopybotset_create_entity REDEFINITION,
      llmjobset_get_entityset REDEFINITION,
      llmcustomiseagen_get_entityset REDEFINITION,
      llmpromptchainto_get_entityset REDEFINITION,
      llmbottocatalogs_get_entityset REDEFINITION,
      llmcataloglistse_get_entityset REDEFINITION,
      llmragagentscata_get_entityset REDEFINITION,
      llmcatalogtoprom_get_entityset REDEFINITION,
      llmragagentscata_create_entity REDEFINITION,
      llmragclassconfi_get_entityset REDEFINITION,
      llmmcpagentrespo_get_entityset REDEFINITION,
      llmunsafepromptr_get_entityset REDEFINITION,
      llmsentimentrepo_create_entity REDEFINITION,
      llmreportprompts_create_entity REDEFINITION,
      llmreportprompts_get_entityset REDEFINITION,
      llmmcprolesauths_get_entityset REDEFINITION,
      llmpromptconnect_get_entityset REDEFINITION,
      llmagentlistset_get_entityset  REDEFINITION,
      llmauthtabset_create_entity  REDEFINITION,
      llmauthtabset_get_entityset  REDEFINITION,
      llmpromptpropert_get_entityset REDEFINITION,
      llmcreateagentse_create_entity REDEFINITION.


  PRIVATE SECTION.

    DATA: lcl_fiori_llm TYPE REF TO zcl_fiori_llm_util,
          lcl_report    TYPE REF TO zcl_llm_report_util.


ENDCLASS.



CLASS zcl_z_llm_dpc_ext IMPLEMENTATION.

method llmcreateagentse_create_entity.

    TRY.
        DATA: ls_post TYPE zllm_create_agent_odata.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).


        DATA(lcl_create_abap_agent) = NEW zcl_llm_create_abap_agent_util( app_id = ls_post-app_id
                                                                    business_function = ls_post-business_function ).

        lcl_create_abap_agent->create_abap_agent( template_agent = 'Z_LLM_CS_HUB_SUP_ORD_STATUS'
                                                  new_agent = conv rs38l-name( ls_post-agent_name )
                                                  function_group = conv rs38l-area( ls_post-function_group ) ).


      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

    ENDTRY.


ENDMETHOD.


  METHOD llmunsafepromptr_get_entityset.

    TRY.

        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(date_range) = it_filter_select_options[ property = 'Date' ]-select_options[ 1 ].

        DATA(lt_date_range) = VALUE ztt_date_range( sign   = date_range-sign
                                                                option = date_range-option
                                                              ( low    = date_range-low
                                                                high   = date_range-high  ) ).

        DATA(lcl_report_util) = NEW zcl_llm_report_util( app_id            = CONV zllm_app_id( app_id )
                                                         business_function = CONV zllm_buss_function_name( business_function ) ).

        et_entityset = lcl_report_util->build_unsafe_prompt_report( it_date_range = lt_date_range ).

      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId, BusinessFunction, StartDate or EndDate not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

    ENDTRY.

  ENDMETHOD.


  METHOD llmragclassconfi_get_entityset.

    TRY.

        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(agent_name) = it_filter_select_options[ property = 'AgentName' ]-select_options[ 1 ]-low.

        TRY.

            DATA(lcl_fiori_llm_util) = NEW zcl_fiori_llm_util( ).

            MOVE-CORRESPONDING lcl_fiori_llm_util->get_mcp_class_config( app_id            = CONV zllm_app_id( app_id )
                                                                         business_function = CONV zllm_buss_function_name( business_function )
                                                                         agent_name        = CONV zllm_function_name( agent_name )
                                                                       ) TO et_entityset.

          CATCH zcx_llm INTO DATA(lcx_llm).
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                http_status_code = 500
                message          = CONV bapi_msg( lcx_llm->get_text( ) )
                textid           = /iwbep/cx_mgw_busi_exception=>business_error.
        ENDTRY.


      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction or AgentName not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

    ENDTRY.


  ENDMETHOD.


  METHOD llmragagentscata_create_entity.

    TRY.
        DATA: ls_post TYPE zllm_rag_agent_catalog_odata.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).


        DATA(lcl_fiori_llm) = NEW zcl_fiori_llm_util(  ).
        lcl_fiori_llm->save_agent_to_catalog( catalog = ls_post ).


      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

    ENDTRY.


  ENDMETHOD.


  METHOD llmcatalogtoprom_get_entityset.

    TRY.

        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(catalog_name) = it_filter_select_options[ property = 'CatalogName' ]-select_options[ 1 ]-low.


        DATA(lcl_fiori_llm) = NEW zcl_fiori_llm_util( app_id            = CONV zllm_app_id( app_id )
                                                      business_function = CONV zllm_buss_function_name( business_function ) ).

        DATA(lt_prompt_chains) = lcl_fiori_llm->get_catalog_to_prompt_chain( app_id            = CONV zllm_app_id( app_id )
                                                                             business_function = CONV zllm_buss_function_name( business_function )
                                                                             catalog_name      = CONV zllm_rag_catalog_name( catalog_name ) ).

        DATA: ls_entityset TYPE zllm_catalog_to_prompt_odata.

        LOOP AT lt_prompt_chains ASSIGNING FIELD-SYMBOL(<fs_prompt_chain>).

          ls_entityset-app_id = <fs_prompt_chain>-app_id.
          ls_entityset-business_function = <fs_prompt_chain>-business_function.
          ls_entityset-catalog_name = catalog_name.
          ls_entityset-prompt_key = <fs_prompt_chain>-prompt_key.

          SELECT SINGLE external_value
          FROM zllm_ext_value
          INTO ls_entityset-external_name
          WHERE type = 'FUNCTION_NAME'
          AND internal_value = <fs_prompt_chain>-prompt_key.


          APPEND ls_entityset TO et_entityset.

        ENDLOOP.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId, BusinessFunction or CatalogName not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.


      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

    ENDTRY.


  ENDMETHOD.


  METHOD llmcataloglistse_get_entityset.

    TRY.

        DATA(lcl_fiori_llm) = NEW zcl_fiori_llm_util(  ).
        et_entityset = lcl_fiori_llm->get_agent_catalog(  ).


      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

    ENDTRY.

  ENDMETHOD.


  METHOD llmbottocatalogs_get_entityset.

    TRY.

        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.

        DATA(lcl_fiori_llm) = NEW zcl_fiori_llm_util(  ).
        et_entityset = lcl_fiori_llm->get_bot_to_catalog( app_id            = CONV zllm_app_id( app_id )
                                                          business_function = CONV zllm_buss_function_name( business_function ) ).


      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

    ENDTRY.


  ENDMETHOD.


  METHOD llmpromptchainto_get_entityset.

* Agent maybe blank.
    TRY.
        DATA(agent) = it_filter_select_options[ property = 'Agent' ]-select_options[ 1 ]-low.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.


    TRY.

        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(version) = it_filter_select_options[ property = 'Version' ]-select_options[ 1 ]-low.


        DATA(lcl_fiori_llm) = NEW zcl_fiori_llm_util(  ).
        et_entityset = lcl_fiori_llm->get_prompt_chain_to_rag( app_id            = CONV zllm_app_id( app_id )
                                                               business_function = CONV zllm_buss_function_name( business_function )
                                                               agent             = agent
                                                               version           = CONV zllm_version( version ) ).


      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

    ENDTRY.
  ENDMETHOD.


  METHOD llmcustomiseagen_get_entityset.

    TRY.

        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(agent) = it_filter_select_options[ property = 'Agent' ]-select_options[ 1 ]-low.



        DATA(lcl_fiori_llm) = NEW zcl_fiori_llm_util(  ).

        et_entityset = lcl_fiori_llm->get_gcs_map( app_id            = CONV zllm_app_id( app_id )
                                                   business_function = CONV zllm_buss_function_name( business_function )
                                                   agent             = agent ).



      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

    ENDTRY.

  ENDMETHOD.


  METHOD llmjobset_get_entityset.

    TRY.

        SELECT app_id, business_function, running, updated_date AS date, updated_time AS time
        FROM zllm_job
        INTO CORRESPONDING FIELDS OF TABLE @et_entityset.

      CATCH cx_sy_itab_line_not_found INTO DATA(lcx_data).

    ENDTRY.



  ENDMETHOD.


  METHOD llmcopybotset_create_entity.

    TRY.
        DATA: ls_post TYPE zllm_copy_bot.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).


        DATA(lcl_llm_copy_bot_util) = NEW zcl_llm_copy_bot_util( from_app_id            = CONV zllm_app_id( ls_post-from_app )
                                                                 from_business_function = CONV zllm_buss_function_name( ls_post-from_buss_function )
                                                                 to_app_id              = CONV zllm_app_id( ls_post-to_app )
                                                                 to_business_function   = CONV zllm_buss_function_name( ls_post-to_buss_function ) ).
        lcl_llm_copy_bot_util->copy_bot(  ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

    ENDTRY.

  ENDMETHOD.


  METHOD llmrotatelogset_get_entityset.
    TRY.

        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

    ENDTRY.
  ENDMETHOD.


  METHOD llmlockingset_get_entityset.

    SELECT type, username
    INTO CORRESPONDING FIELDS OF TABLE @et_entityset
    FROM zllm_locking.

  ENDMETHOD.


  METHOD llmlockingset_create_entity.

    TRY.
        DATA: ls_post TYPE zllm_lock.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).

        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

* Only handle locking for deletions. Locking is otherwise held within the function
        IF ls_post-delete IS NOT INITIAL.

          lcl_fiori_llm->handle_locking( type   = ls_post-type
                                         delete = ls_post-delete
                                         key    = ls_post-key ).
        ENDIF.


      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

    ENDTRY.

  ENDMETHOD.


  METHOD llmpromptusagese_create_entity.

    TRY.

        DATA: ls_post TYPE zllm_prompt_usage.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).

        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        er_entity-sentiment = lcl_fiori_llm->update_prompt_usage( app_id            = ls_post-app_id
                                                                  business_function = ls_post-business_function
                                                                  guid              = ls_post-guid
                                                                  sentiment         = ls_post-sentiment
                                                                  resolved          = ls_post-resolved
                                                                  resolved_note     = ls_post-resolved_note ).


      CATCH zcx_llm INTO DATA(lcx_llm).
        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.
    ENDTRY.




  ENDMETHOD.


  METHOD llmsubagentset_update_entity.

  ENDMETHOD.


  METHOD llmsubagentset_create_entity.


    TRY.

        DATA: ls_post TYPE zllm_sub_func_odata.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).

        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        lcl_fiori_llm->save_sub_agent( sub_agent = ls_post ).


      CATCH zcx_llm INTO DATA(lcx_llm).
        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.
    ENDTRY.


  ENDMETHOD.


  METHOD llmsubagentset_get_entityset.

    TRY.
        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(supv_agent) = it_filter_select_options[ property = 'SupvFunctionName' ]-select_options[ 1 ]-low.
        DATA(version) = it_filter_select_options[ property = 'Version' ]-select_options[ 1 ]-low.


        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).
        et_entityset = lcl_fiori_llm->get_sub_agents( app_id            = CONV zllm_app_id( app_id )
                                                      business_function = CONV zllm_buss_function_name( business_function )
                                                      supv_agent        = CONV zllm_function_name( supv_agent )
                                                      version           = CONV zllm_version( version ) ).


      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.


  ENDMETHOD.


  METHOD llmfeedbacknotes_create_entity.

    TRY.
        DATA ls_post TYPE zllm_log_notes_odata.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).


        DATA(lcl_log) = NEW zcl_llm_log( app_id            = ls_post-app_id
                                         business_function = ls_post-buss_function ).


        lcl_log->save_log_notes( object_key  = ls_post-object_key
                                 object_type = ls_post-object_type
                                 uuid_key    = CONV guid( ls_post-uuid_key )
                                 note        = ls_post-note ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

    ENDTRY.

  ENDMETHOD.


  METHOD llmbotset_get_entityset.

    TRY.
        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.


        SELECT *
        FROM zllm_bots
        INNER JOIN zllm_config ON zllm_bots~app_id = zllm_config~app_id AND
                                  zllm_bots~business_function = zllm_config~buss_function
        INTO TABLE @DATA(bots)
        WHERE zllm_bots~app_id = @app_id
        AND zllm_bots~business_function = @business_function.


        LOOP AT bots ASSIGNING FIELD-SYMBOL(<fs_bots>).

          DATA: ls_bots TYPE zllm_bots_odata.

* Get the external name and add it in.
          SELECT SINGLE external_value
          FROM zllm_ext_value
          WHERE type = 'APP_ID'
          AND internal_value = @( |{ app_id }_{ business_function }| )
          INTO @ls_bots-external_name.


          ls_bots-app_id = <fs_bots>-zllm_bots-app_id.
          ls_bots-business_function = <fs_bots>-zllm_bots-business_function.
          ls_bots-enabled = <fs_bots>-zllm_bots-enabled.
          ls_bots-eval_app_id = <fs_bots>-zllm_bots-eval_app_id.
          ls_bots-eval_business_function = <fs_bots>-zllm_bots-eval_business_function.
          ls_bots-helper_app_id = <fs_bots>-zllm_bots-helper_app_id.
          ls_bots-helper_business_function = <fs_bots>-zllm_bots-helper_business_function.


          IF <fs_bots>-zllm_config-param = 'NUMBER_RANGE_INTERVAL'.
            ls_bots-bot_number_range = <fs_bots>-zllm_config-value.
          ENDIF.

          IF <fs_bots>-zllm_config-param = 'VER_NUMBER_RANGE_INTERVAL'.
            ls_bots-version_number_range = <fs_bots>-zllm_config-value.
          ENDIF.

          APPEND ls_bots TO et_entityset.

        ENDLOOP.




      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

  ENDMETHOD.


  METHOD llmuserfilterset_get_entityset.


    TRY.
        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.


        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).
        et_entityset = lcl_fiori_llm->get_user_filters( app_id            = CONV zllm_app_id( app_id )
                                                        business_function = CONV zllm_buss_function_name( business_function ) ).


      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.


  ENDMETHOD.


  METHOD llmuserfilterset_create_entity.


    TRY.

        DATA: ls_post TYPE zllm_user_filter_odata.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).

        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        lcl_fiori_llm->save_user_filters( filter_type       = ls_post-filter
                                          filter_value      = ls_post-value
                                          app_id            = ls_post-app_id
                                          business_function = ls_post-business_function ).


      CATCH zcx_llm INTO DATA(lcx_llm).
        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.
    ENDTRY.





  ENDMETHOD.


  METHOD llmprompttrigger_create_entity.

    TRY.
        DATA ls_post TYPE zllm_prompt_trigger_odata.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).


        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

* Convert UI params into Google Fm params.
        DATA: ls_parameters TYPE /goog/function_parameters,
              lt_parameters TYPE /goog/t_function_parameters.

        IF ls_post-parameter1 IS NOT INITIAL.

          ls_parameters-parameter_name = ls_post-parameter1.
          ls_parameters-parameter_value = ls_post-parameter1_value.
          APPEND ls_parameters TO lt_parameters.

        ELSEIF ls_post-parameter2 IS NOT INITIAL.

          ls_parameters-parameter_name = ls_post-parameter2.
          ls_parameters-parameter_value = ls_post-parameter2_value.
          APPEND ls_parameters TO lt_parameters.

        ELSEIF ls_post-parameter3 IS NOT INITIAL.

          ls_parameters-parameter_name = ls_post-parameter3.
          ls_parameters-parameter_value = ls_post-parameter3_value.
          APPEND ls_parameters TO lt_parameters.

        ELSEIF ls_post-parameter4 IS NOT INITIAL.

          ls_parameters-parameter_name = ls_post-parameter4.
          ls_parameters-parameter_value = ls_post-parameter4_value.
          APPEND ls_parameters TO lt_parameters.

        ELSEIF ls_post-parameter5 IS NOT INITIAL.

          ls_parameters-parameter_name = ls_post-parameter5.
          ls_parameters-parameter_value = ls_post-parameter5_value.
          APPEND ls_parameters TO lt_parameters.

        ENDIF.

        lcl_fiori_llm->trigger_prompt( business_function = ls_post-business_function
                                       app_id            = ls_post-app_id
                                       function          = CONV zllm_function_name( ls_post-prompt_key )
                                       parameters        = lt_parameters ).


      CATCH zcx_llm INTO DATA(lcx_llm).
        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.
    ENDTRY.


  ENDMETHOD.


  METHOD llmprompttrigger_get_entityset.

    TRY.
        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(prompt_key) = it_filter_select_options[ property = 'PromptKey' ]-select_options[ 1 ]-low.


        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).


        DATA(parameters) = lcl_fiori_llm->get_prompt_fm_config( app_id            = CONV zllm_app_id( app_id )
                                                                business_function = CONV zllm_buss_function_name( business_function )
                                                                prompt_key        = CONV zllm_prompt_key( prompt_key ) ).

        DATA: ls_param_odata TYPE zllm_prompt_trigger_odata.

        LOOP AT parameters ASSIGNING FIELD-SYMBOL(<fs_params>).

* Don't have time to do this correctly, so this will do for now ...
          IF sy-tabix = 1.

            ls_param_odata-parameter1 = <fs_params>-parameter_name.
            ls_param_odata-parameter1_type = <fs_params>-parameter_value.

          ELSEIF sy-tabix = 2.

            ls_param_odata-parameter2 = <fs_params>-parameter_name.
            ls_param_odata-parameter2_type = <fs_params>-parameter_value.

          ELSEIF sy-tabix = 3.

            ls_param_odata-parameter3 = <fs_params>-parameter_name.
            ls_param_odata-parameter3_type = <fs_params>-parameter_value.

          ELSEIF sy-tabix = 4.

            ls_param_odata-parameter4 = <fs_params>-parameter_name.
            ls_param_odata-parameter4_type = <fs_params>-parameter_value.

          ELSEIF sy-tabix = 5.

            ls_param_odata-parameter5 = <fs_params>-parameter_name.
            ls_param_odata-parameter5_type = <fs_params>-parameter_value.

          ENDIF.

        ENDLOOP.



      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.
  ENDMETHOD.


  METHOD llmpromptexample_get_entityset.

    TRY.
        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(prompt_key) = it_filter_select_options[ property = 'PromptKey' ]-select_options[ 1 ]-low.


        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        et_entityset = lcl_fiori_llm->get_prompt_example( app_id            = CONV zllm_app_id( app_id )
                                                          business_function = CONV zllm_buss_function_name( business_function )
                                                          prompt_key        = CONV zllm_prompt_key( prompt_key ) ).


      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

    ENDTRY.

  ENDMETHOD.


  METHOD llmpromptexample_create_entity.

    TRY.
        DATA ls_post TYPE zllm_prompt_eg.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).


        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        lcl_fiori_llm->save_prompt_example( business_function = ls_post-business_function
                                            app_id            = ls_post-app_id
                                            prompt_key        = ls_post-prompt_key
                                            example           = ls_post-object
                                            uuid              = ls_post-uuid
                                            delete            = ls_post-deleted ).


      CATCH zcx_llm INTO DATA(lcx_llm).
        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.
    ENDTRY.

  ENDMETHOD.


  METHOD llmpromptusagese_get_entityset.

    TRY.
        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(prompt_key) = it_filter_select_options[ property = 'PromptKey' ]-select_options[ 1 ]-low.

        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        et_entityset = lcl_fiori_llm->get_prompt_usage( app_id            = CONV zllm_app_id( app_id )
                                                        business_function = CONV zllm_buss_function_name( business_function )
                                                        prompt_key        = CONV zllm_prompt_key( prompt_key ) ).



      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

    ENDTRY.

  ENDMETHOD.


  METHOD llmagentusagerep_get_entityset.

    TRY.
        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(start_date) = it_filter_select_options[ property = 'StartDate' ]-select_options[ 1 ]-low.
        DATA(end_date) = it_filter_select_options[ property = 'EndDate' ]-select_options[ 1 ]-low.

        lcl_report = NEW zcl_llm_report_util( app_id            = CONV zllm_app_id( app_id )
                                              business_function = CONV zllm_buss_function_name( business_function ) ).

        et_entityset = lcl_report->build_agent_usage_report( start_date = CONV datum( start_date )
                                                             end_date   = CONV datum( end_date ) ).

      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

    ENDTRY.


  ENDMETHOD.


  METHOD llmtotalusagerep_get_entityset.

    TRY.
        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(start_date) = it_filter_select_options[ property = 'StartDate' ]-select_options[ 1 ]-low.
        DATA(end_date) = it_filter_select_options[ property = 'EndDate' ]-select_options[ 1 ]-low.

        lcl_report = NEW zcl_llm_report_util( app_id            = CONV zllm_app_id( app_id )
                                              business_function = CONV zllm_buss_function_name( business_function ) ).

        et_entityset = lcl_report->build_bot_usage_report( start_date = CONV datum( start_date )
                                                           end_date   = CONV datum( end_date ) ).

      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

    ENDTRY.


  ENDMETHOD.


  METHOD llmuserversionse_create_entity.

    DATA: ls_post TYPE zllm_user_version_odata.

    TRY.
        io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).


        DATA(lcl_fiori_llm) = NEW zcl_fiori_llm_util(  ).

* If model id is provided its a model id update
        IF ls_post-model_id IS NOT INITIAL.

          IF ls_post-delete = abap_true.

            lcl_fiori_llm->delete_user_model( app_id            = ls_post-app_id
                                              business_function = ls_post-business_function
                                              version           = ls_post-version
                                              model_id          = ls_post-model_id ).

          ELSE.

            lcl_fiori_llm->set_user_model( app_id            = ls_post-app_id
                                           business_function = ls_post-business_function
                                           version           = ls_post-version
                                           model_id          = ls_post-model_id ).
          ENDIF.




        ELSE.


          IF ls_post-delete = abap_true.

            lcl_fiori_llm->delete_user_version( app_id            = ls_post-app_id
                                                business_function = ls_post-business_function
                                                version           = ls_post-version ).

          ELSE.

            lcl_fiori_llm->set_user_version( app_id            = ls_post-app_id
                                             business_function = ls_post-business_function
                                             version           = ls_post-version ).
          ENDIF.


        ENDIF.

      CATCH cx_sy_itab_line_not_found.
      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.
    ENDTRY.


  ENDMETHOD.


  METHOD llmuserroleset_create_entity.

    TRY.
        DATA ls_post TYPE zllm_user_roles_odata.
        io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).


        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        lcl_fiori_llm->assign_user_role( user              = ls_post-usr
                                         roleid            = ls_post-roleid
                                         deleted           = ls_post-deleted
                                         business_function = ls_post-business_function
                                         app_id            = ls_post-app_id ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.
    ENDTRY.

  ENDMETHOD.


  METHOD llmroleauthsset_create_entity.

    TRY.
        DATA ls_post TYPE zllm_auths_roles.
        io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).


        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        lcl_fiori_llm->create_update_role( roleid                 = ls_post-roleid
                                           roledesc               = ls_post-role_desc
                                           region                 = ls_post-region
                                           app_id                 = ls_post-app_id
                                           business_function      = ls_post-business_function
                                           agent                  = ls_post-agent
                                           tabid                  = ls_post-tab_id
                                           deleted                = ls_post-deleted
                                           exclude_from_reporting = ls_post-exclude_from_reporting ).


      CATCH zcx_llm INTO DATA(lcx_llm).
        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.
    ENDTRY.

  ENDMETHOD.


  METHOD llmconfigset_get_entityset.

    TRY.
        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
*        DATA(type) = it_filter_select_options[ property = 'Type' ]-select_options[ 1 ]-low.


        DATA(lcl_config_util) = NEW zcl_llm_config_util( app_id            = CONV zllm_app_id( app_id )
                                                         business_function = CONV zllm_buss_function_name( business_function )
                                                         type              = '' ).

        et_entityset = lcl_config_util->get_all_config(  ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId, BusinessFunction or Type not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

    ENDTRY.

  ENDMETHOD.


  METHOD llmappversionsse_create_entity.

    DATA ls_post TYPE zllm_versions_odata.
    io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).

    TRY.
        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

* Set version as active (may be beta or roll back)
        lcl_fiori_llm->set_app_active_version( app_id            = ls_post-app_id
                                               business_function = ls_post-business_function
                                               version           = ls_post-version
                                               model_id          = ls_post-model_id ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.

    ENDTRY.

  ENDMETHOD.


  METHOD llmappversionsse_get_entityset.

    TRY.
        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.


        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        et_entityset = lcl_fiori_llm->get_app_versions( app_id            = CONV zllm_app_id( app_id )
                                                        business_function = CONV zllm_buss_function_name( business_function ) ).

      CATCH zcx_llm INTO DATA(lcx_llm).
* No prompt history.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

  ENDMETHOD.


  METHOD llmpublishprompt_create_entity.

    DATA ls_post TYPE zllm_publish_prompt_odata.
    io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).

    TRY.
        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

* Save the prompt
        lcl_fiori_llm->publish_prompt( app_id            = ls_post-app_id
                                       business_function = ls_post-business_function
                                       function_name     = ls_post-function_name ).


      CATCH zcx_llm INTO DATA(lcx_llm).

* Save failed. Raise exception.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

    ENDTRY.

  ENDMETHOD.


  METHOD llmprompthistory_get_entityset.

    TRY.
        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(prompt_key) = it_filter_select_options[ property = 'PromptKey' ]-select_options[ 1 ]-low.


        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        et_entityset = lcl_fiori_llm->get_prompt_history( app_id            = CONV zllm_app_id( app_id )
                                                          business_function = CONV zllm_buss_function_name( business_function )
                                                          prompt_key        = CONV zllm_prompt_key( prompt_key ) ).

      CATCH zcx_llm INTO DATA(lcx_llm).
* No prompt history.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId, BusinessFunction or PromptKey not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.


  ENDMETHOD.


  METHOD llmpromptset_create_entity.

    DATA ls_post TYPE zllm_prompt_odata.
    io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).

    TRY.
        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

* Save the prompt
        er_entity-prompt_chain = lcl_fiori_llm->save_prompt( app_id               = ls_post-app_id
                                                             business_function    = ls_post-business_function
                                                             prompt_key           = ls_post-prompt_key
                                                             prompt_chain         = ls_post-prompt_chain
                                                             prompt_manual_chain  = ls_post-prompt_manual_chain
                                                             prompt_chain_enabled = ls_post-prompt_chain_enabled
                                                             prompt               = ls_post-prompt
                                                             prompt_type          = ls_post-prompt_type
                                                             is_signed_off        = ls_post-sign_off
                                                             deleteable           = ls_post-deletable
                                                             version              = ls_post-version
                                                             deleted              = ls_post-deleted
                                                             override_func_declar = ls_post-override_func_declar
                                                             think_link           = ls_post-think_link
                                                             final_link           = ls_post-final_link
                                                             prompt_name          = ls_post-prompt_name
                                                             model_id             = ls_post-model_id
                                                             decision_link        = ls_post-decision_link ).

      CATCH zcx_llm INTO DATA(lcx_llm).

* Build custom exception message for UI.
* This scenario is where user attempted to delete without overriding the function declarations.
        IF lcx_llm->if_t100_message~t100key-msgno = '024'.

* Build HTML message.
          DATA: lv_html_message TYPE string.
          lv_html_message = |<p>Agent { lcx_llm->msg1 } still exists. | &
                            |If you continue, the agent will respond with error.</p> | &
                            |<p>Are you sure you wish to continue?</p>|.


          lcx_llm->if_t100_message~t100key-attr1 = CONV bapi_msg( lv_html_message ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              http_status_code = 500
              message          = CONV bapi_msg( lv_html_message )
              textid           = lcx_llm->if_t100_message~t100key.

        ELSE.

* Raise exception.
          lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              http_status_code = 500
              message          = CONV bapi_msg( lcx_llm->msg1 )
              textid           = lcx_llm->if_t100_message~t100key.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD llmpromptset_get_entityset.

    TRY.
        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction key not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.


* Prompt key is optional so if it doesnt exist, its ok.
    TRY.
        DATA(prompt_key) = it_filter_select_options[ property = 'PromptKey' ]-select_options[ 1 ]-low.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.


* Prompt key is optional so if it doesnt exist, its ok.
    TRY.
        DATA(version) = it_filter_select_options[ property = 'Version' ]-select_options[ 1 ]-low.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    TRY.
        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        et_entityset = lcl_fiori_llm->get_prompt( app_id            = CONV zllm_app_id( app_id )
                                                  business_function = CONV zllm_buss_function_name( business_function )
                                                  prompt_key        = CONV zllm_prompt_key( prompt_key )
                                                  version           = CONV zllm_version( version ) ).

      CATCH zcx_llm.

    ENDTRY.

  ENDMETHOD.


  METHOD llmappset_get_entityset.

    TRY.
        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

* Get the list of apps / functions / agents.
        et_entityset = lcl_fiori_llm->get_apps(  ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_longtext( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.


  ENDMETHOD.


  METHOD llmratinglogset_create_entity.

    DATA ls_post TYPE zllm_rating_log.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).

    TRY.
        DATA(lcl_llm_log) = NEW zcl_llm_log( app_id            = ls_post-app_id
                                             business_function = ls_post-buss_function ).


        lcl_fiori_llm = NEW zcl_fiori_llm_util( app_id            = ls_post-app_id
                                                business_function = ls_post-buss_function ).


        "IF lcl_fiori_llm->check_auths(  ) = abap_true.

        lcl_llm_log->write_rating_log( object_type = ls_post-object_type
                                       object_key  = ls_post-object_key
                                       rating      = ls_post-rating
                                       uuid_key    = CONV guid16( ls_post-uuid_key ) ).

    ENDTRY.

  ENDMETHOD.


  METHOD llmset_get_entityset.

  ENDMETHOD.


  METHOD llmset_create_entity.

    DATA ls_post TYPE zfiori_llm_odata.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).

    TRY.
        lcl_fiori_llm = NEW zcl_fiori_llm_util( app_id            = ls_post-app_id
                                                business_function = ls_post-business_function
                                                temperature       = ls_post-temperature ).

        DATA(lcl_cs_hub) = NEW zcl_cs_hub_delivery_util(  ).

* Until I create the auths class, force this to be true.
        IF abap_true = abap_true.
          "IF lcl_fiori_llm->check_auths(  ) = abap_true.

          DATA(response) = lcl_fiori_llm->call_model( prompt     = ls_post-request
                                                      system_key = ls_post-system_key ).


          IF sy-subrc = 0 .
            response-response = |{ response-response } { lcl_fiori_llm->get_signature( app_id = ls_post-app_id                                                                                   business_function = ls_post-business_function ) }|.
          ENDIF.

          er_entity-response = response-response.

* This key will allow us to link user interactions with the bot response to the model call
          er_entity-uuid_key = response-uuid_key.

          er_entity-actioned = response-actioned.

* It is possible to get the model to respond back with the base64. But this increases cost and slows performance. Its faster to do it this way.

          IF ls_post-hybris_order IS NOT INITIAL.

            DATA(lt_delivery) = lcl_cs_hub->get_delivery_data( iv_hybris_order = ls_post-hybris_order ).

            LOOP AT lt_delivery ASSIGNING FIELD-SYMBOL(<fs_delivery>) WHERE base64_image_url IS NOT INITIAL.
              er_entity-base64 = |data:image/png;base64,{ <fs_delivery>-base64_image_url } |.
            ENDLOOP.

          ENDIF.
        ELSE.

          er_entity-response = 'Sorry, you do not have authorisation for this agent'.

        ENDIF.
      CATCH zcx_llm INTO DATA(lcx_llm).
        er_entity-response = lcx_llm->get_longtext(  ).
    ENDTRY.

  ENDMETHOD.


  METHOD llmlogset_create_entity.


    DATA ls_post TYPE zllm_log_odata.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).

    TRY.
        DATA(lcl_llm_log) = NEW zcl_llm_log( app_id            = ls_post-app_id
                                             business_function = ls_post-buss_function ).


        lcl_fiori_llm = NEW zcl_fiori_llm_util( app_id            = ls_post-app_id
                                                business_function = ls_post-buss_function ).


        " IF lcl_fiori_llm->check_auths(  ) = abap_true.

        lcl_llm_log->write_general_log( sentiment   = ls_post-sentiment
                                        old_value   = ls_post-old_value
                                        new_value   = ls_post-new_value
                                        object_type = ls_post-object_type
                                        object_key  = ls_post-object_key
                                        request     = ls_post-request
                                        uuid_key    = CONV guid( ls_post-uuid_key ) ).


        CASE ls_post-sentiment.

*         To trigger response comparison, only during 'Edit' action
*         Sentiment 'Negative' means that user action is 'Edit', rather than 'Copy'
          WHEN 'NEGATIVE'.

            DATA(lcl_llm_response_comparer) = NEW zcl_llm_response_comparer(  ).

            DATA(lv_edit_category) = lcl_llm_response_comparer->compare_responses( iv_uuid_key          = CONV guid( ls_post-uuid_key )
                                                                                   iv_app_id            = ls_post-app_id
                                                                                   iv_business_function = ls_post-buss_function
                                                                                   iv_request           = ls_post-request
                                                                                   iv_bot_response      = ls_post-old_value
                                                                                   iv_edited_resposne   = ls_post-new_value ).


        ENDCASE.
        " ELSE.
* Do not write this log. user has no auths.
        "  ENDIF.
      CATCH zcx_llm.
    ENDTRY.



  ENDMETHOD.


  METHOD llmagentset_get_entityset.

    TRY.

        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low .
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(function_name) = it_filter_select_options[ property = 'AgentKey' ]-select_options[ 1 ]-low.

* clean the parameters.
        CONDENSE app_id NO-GAPS.
        CONDENSE business_function NO-GAPS.
        CONDENSE function_name NO-GAPS.


      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction or AgentKey key not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

*   Version is an optional parameter, hence keeping this separated from the logic for mandatory keys
    IF line_exists( it_filter_select_options[ property = 'Version' ]-select_options[ 1 ] ).
      DATA(version) = it_filter_select_options[ property = 'Version' ]-select_options[ 1 ]-low.
    ENDIF.

    IF line_exists( it_filter_select_options[ property = 'AgentType' ]-select_options[ 1 ] ).
      DATA(agent_type) = it_filter_select_options[ property = 'AgentType' ]-select_options[ 1 ]-low.
      CONDENSE agent_type NO-GAPS.
    ENDIF.


    TRY.
        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        et_entityset = lcl_fiori_llm->get_function_declarations( app_id            = CONV zllm_app_id( app_id )
                                                                 business_function = CONV zllm_buss_function_name( business_function )
                                                                 function_name     = CONV zllm_function_name( function_name )
                                                                 version           = CONV zllm_version( version )
                                                                 type              = CONV zllm_function_type( agent_type ) ).


      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_begin.

    cv_defer_mode = abap_true.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_end.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_process.
* Method implementation to handle batch calls

    DATA : lref_request_context      TYPE REF TO /iwbep/if_mgw_req_entity_c,
           ls_changeset_response     TYPE  /iwbep/if_mgw_appl_types=>ty_s_changeset_response,
           lt_agent                  TYPE STANDARD TABLE OF zllm_agent_odata,
           lt_sub_agent              TYPE STANDARD TABLE OF zllm_sub_func_odata,
           lt_prompt_chain           TYPE STANDARD TABLE OF zllm_prompt_odata,
           lt_gcs_map                TYPE STANDARD TABLE OF zllm_custom_agent_odata,
           lt_prompt_to_rag          TYPE zllm_rag_odata_tt,
           lt_bot_to_agent           TYPE zllm_bot_to_catalog_odata_tt,
           lt_rag_agent              TYPE zllm_rag_func_odata_tt,
           lt_rag_class_config       TYPE zllm_rag_config_odata_tt,
           lt_prompt_connector       TYPE zllm_prompt_connector_tt,
           lt_prompt_connector_chain TYPE zllm_prompt_connector_chain_tt.


    LOOP AT it_changeset_request ASSIGNING FIELD-SYMBOL(<fs_changeset_request>).

*     Read the name of the entity set that's been called
      lref_request_context ?= <fs_changeset_request>-request_context.
      DATA(lv_entity_set) = lref_request_context->get_entity_set_name( ).

      CASE lv_entity_set.

        WHEN 'LLMRAGClassConfigSet'.

          DATA : ls_rag_class_config      TYPE zllm_rag_config_odata.

          TRY.
              <fs_changeset_request>-entry_provider->read_entry_data( IMPORTING es_data = ls_rag_class_config ).

              APPEND ls_rag_class_config TO lt_rag_class_config.


            CATCH /iwbep/cx_mgw_tech_exception.
          ENDTRY.

          copy_data_to_ref( EXPORTING is_data = ls_rag_class_config
                            CHANGING  cr_data = ls_changeset_response-entity_data ).

          ls_changeset_response-operation_no = <fs_changeset_request>-operation_no.

          INSERT ls_changeset_response INTO TABLE ct_changeset_response.


        WHEN 'LLMRAGAgentSet'.

          DATA : ls_rag_agent      TYPE zllm_rag_func_odata.

          TRY.
              <fs_changeset_request>-entry_provider->read_entry_data( IMPORTING es_data = ls_rag_agent ).

              APPEND ls_rag_agent TO lt_rag_agent.


            CATCH /iwbep/cx_mgw_tech_exception.
          ENDTRY.

          copy_data_to_ref( EXPORTING is_data = ls_rag_agent
                            CHANGING  cr_data = ls_changeset_response-entity_data ).

          ls_changeset_response-operation_no = <fs_changeset_request>-operation_no.

          INSERT ls_changeset_response INTO TABLE ct_changeset_response.


        WHEN 'LLMBotToCatalogSet'.

          DATA : ls_catalog      TYPE zllm_bot_to_catalog_odata.

          TRY.
              <fs_changeset_request>-entry_provider->read_entry_data( IMPORTING es_data = ls_catalog ).

              APPEND ls_catalog TO lt_bot_to_agent.


            CATCH /iwbep/cx_mgw_tech_exception.
          ENDTRY.

          copy_data_to_ref( EXPORTING is_data = ls_catalog
                            CHANGING  cr_data = ls_changeset_response-entity_data ).

          ls_changeset_response-operation_no = <fs_changeset_request>-operation_no.

          INSERT ls_changeset_response INTO TABLE ct_changeset_response.


        WHEN 'LLMAgentSet'.


          DATA : ls_agent      TYPE zllm_agent_odata.

          TRY.
              <fs_changeset_request>-entry_provider->read_entry_data( IMPORTING es_data = ls_agent ).

              APPEND ls_agent TO lt_agent.


            CATCH /iwbep/cx_mgw_tech_exception.
          ENDTRY.

          copy_data_to_ref( EXPORTING is_data = ls_agent
                            CHANGING  cr_data = ls_changeset_response-entity_data ).

          ls_changeset_response-operation_no = <fs_changeset_request>-operation_no.

          INSERT ls_changeset_response INTO TABLE ct_changeset_response.

        WHEN 'LLMSubAgentSet'.

          DATA: ls_sub_agent TYPE zllm_sub_func_odata.

          TRY.
              <fs_changeset_request>-entry_provider->read_entry_data( IMPORTING es_data = ls_sub_agent ).

              APPEND ls_sub_agent TO lt_sub_agent.


            CATCH /iwbep/cx_mgw_tech_exception INTO DATA(lcx_tech_exception).
          ENDTRY.

          copy_data_to_ref( EXPORTING is_data = ls_sub_agent
                            CHANGING  cr_data = ls_changeset_response-entity_data ).

          ls_changeset_response-operation_no = <fs_changeset_request>-operation_no.

          INSERT ls_changeset_response INTO TABLE ct_changeset_response.


        WHEN 'LLMPromptSet'.

          DATA: ls_prompt TYPE zllm_prompt_odata.

          TRY.
              <fs_changeset_request>-entry_provider->read_entry_data( IMPORTING es_data = ls_prompt ).

              APPEND ls_prompt TO lt_prompt_chain.


            CATCH /iwbep/cx_mgw_tech_exception.
          ENDTRY.

          copy_data_to_ref( EXPORTING is_data = ls_sub_agent
                            CHANGING  cr_data = ls_changeset_response-entity_data ).

          ls_changeset_response-operation_no = <fs_changeset_request>-operation_no.

          INSERT ls_changeset_response INTO TABLE ct_changeset_response.


        WHEN 'LLMCustomiseAgentSet'.

          DATA: ls_gcs_map TYPE zllm_custom_agent_odata.

          TRY.
              <fs_changeset_request>-entry_provider->read_entry_data( IMPORTING es_data = ls_gcs_map ).

              APPEND ls_gcs_map TO lt_gcs_map.


            CATCH /iwbep/cx_mgw_tech_exception.
          ENDTRY.

          copy_data_to_ref( EXPORTING is_data = ls_gcs_map
                            CHANGING  cr_data = ls_changeset_response-entity_data ).

          ls_changeset_response-operation_no = <fs_changeset_request>-operation_no.

          INSERT ls_changeset_response INTO TABLE ct_changeset_response.


        WHEN 'LLMPromptChainToRAGSet'.

          DATA : ls_prompt_to_rag      TYPE zllm_rag_odata.

          TRY.
              <fs_changeset_request>-entry_provider->read_entry_data( IMPORTING es_data = ls_prompt_to_rag ).

              APPEND ls_prompt_to_rag TO lt_prompt_to_rag.


            CATCH /iwbep/cx_mgw_tech_exception.
          ENDTRY.

          copy_data_to_ref( EXPORTING is_data = ls_prompt_to_rag
                            CHANGING  cr_data = ls_changeset_response-entity_data ).

          ls_changeset_response-operation_no = <fs_changeset_request>-operation_no.

          INSERT ls_changeset_response INTO TABLE ct_changeset_response.

        WHEN 'LLMPromptConnectorSet'.

          DATA ls_prompt_connector TYPE zllm_prompt_connector.

          TRY.
              <fs_changeset_request>-entry_provider->read_entry_data( IMPORTING es_data = ls_prompt_connector ).

              APPEND ls_prompt_connector TO lt_prompt_connector.

            CATCH /iwbep/cx_mgw_tech_exception.
          ENDTRY.

          copy_data_to_ref( EXPORTING is_data = ls_prompt_connector
                            CHANGING  cr_data = ls_changeset_response-entity_data ).

          ls_changeset_response-operation_no = <fs_changeset_request>-operation_no.

          INSERT ls_changeset_response INTO TABLE ct_changeset_response.

        WHEN 'LLMPromptConnectorChainSet'.

          DATA ls_prompt_connector_chain TYPE zllm_prompt_connector_chain.

          TRY.
              <fs_changeset_request>-entry_provider->read_entry_data( IMPORTING es_data = ls_prompt_connector_chain ).

              APPEND ls_prompt_connector_chain TO lt_prompt_connector_chain.

            CATCH /iwbep/cx_mgw_tech_exception.
          ENDTRY.

          copy_data_to_ref( EXPORTING is_data = ls_prompt_connector_chain
                            CHANGING  cr_data = ls_changeset_response-entity_data ).

          ls_changeset_response-operation_no = <fs_changeset_request>-operation_no.

          INSERT ls_changeset_response INTO TABLE ct_changeset_response.

      ENDCASE.

    ENDLOOP.


    CASE lv_entity_set.

      WHEN 'LLMRAGClassConfigSet'.

        IF lt_rag_class_config IS NOT INITIAL.
          ls_rag_class_config = lt_rag_class_config[ 1 ].
        ENDIF.

        TRY.

            lcl_fiori_llm = NEW zcl_fiori_llm_util( app_id            = ls_gcs_map-app_id
                                                    business_function = ls_gcs_map-business_function ).

            LOOP AT lt_rag_class_config ASSIGNING FIELD-SYMBOL(<fs_class_config>).

              lcl_fiori_llm->save_rag_class_details( app_id             = <fs_class_config>-app_id
                                                     business_function  = <fs_class_config>-business_function
                                                     agent_name         = <fs_class_config>-agent_name
                                                     class_name         = <fs_class_config>-class_name
                                                     parent_method_name = <fs_class_config>-parent_method_name
                                                     child_method_name  = <fs_class_config>-child_method_name
                                                     response_structure = <fs_class_config>-response_structure
                                                     deleted            = <fs_class_config>-deleted ).

            ENDLOOP.

          CATCH zcx_llm INTO DATA(lcx_llm).
            lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                http_status_code = 500
                message          = CONV bapi_msg( lcx_llm->msg1 )
                textid           = lcx_llm->if_t100_message~t100key.
        ENDTRY.


      WHEN 'LLMRAGAgentSet'.

        IF lt_rag_agent IS NOT INITIAL.
          ls_rag_agent = lt_rag_agent[ 1 ].
        ENDIF.

        TRY.

            DATA(lref_fiori_llm_util) = NEW zcl_fiori_llm_util( app_id            = ls_rag_agent-app_id
                                                                business_function = ls_rag_agent-business_function ).

            lref_fiori_llm_util->update_rag_agent( it_agent_details = lt_rag_agent ).

          CATCH zcx_llm INTO lcx_llm.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                http_status_code = 500
                message          = CONV bapi_msg( lcx_llm->get_longtext( ) )
                textid           = /iwbep/cx_mgw_busi_exception=>business_error.
        ENDTRY.


      WHEN 'LLMAgentSet'.

        IF lt_agent IS NOT INITIAL.
          ls_agent = lt_agent[ 1 ].
        ENDIF.

        TRY.

            lref_fiori_llm_util = NEW zcl_fiori_llm_util( app_id            = ls_agent-app_id
                                                          business_function = ls_agent-business_function ).

            lref_fiori_llm_util->update_agent( it_agent_details = lt_agent ).

          CATCH zcx_llm INTO lcx_llm.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                http_status_code = 500
                message          = CONV bapi_msg( lcx_llm->get_longtext( ) )
                textid           = /iwbep/cx_mgw_busi_exception=>business_error.
        ENDTRY.



      WHEN 'LLMCustomiseAgentSet'.

        IF lt_gcs_map IS NOT INITIAL.
          ls_gcs_map = lt_gcs_map[ 1 ].
        ENDIF.

        TRY.

            lcl_fiori_llm = NEW zcl_fiori_llm_util( app_id            = ls_gcs_map-app_id
                                                    business_function = ls_gcs_map-business_function ).

            LOOP AT lt_gcs_map ASSIGNING FIELD-SYMBOL(<fs_gcs_map>).

              lcl_fiori_llm->save_gcs_map( agent = CONV string( <fs_gcs_map>-agent )
                                           app_id = <fs_gcs_map>-app_id
                                           business_function = <fs_gcs_map>-business_function
                                           uri = <fs_gcs_map>-gcs_uri
                                           mime = <fs_gcs_map>-mime
                                           user = <fs_gcs_map>-username  ).

            ENDLOOP.

          CATCH zcx_llm INTO lcx_llm.
            lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                http_status_code = 500
                message          = CONV bapi_msg( lcx_llm->msg1 )
                textid           = lcx_llm->if_t100_message~t100key.
        ENDTRY.

      WHEN 'LLMSubAgentSet'.
        TRY.

            lcl_fiori_llm = NEW zcl_fiori_llm_util( app_id            = ls_sub_agent-app_id
                                                    business_function = ls_sub_agent-business_function ).

            lcl_fiori_llm->save_sub_agent( sub_agent = ls_sub_agent ).


          CATCH zcx_llm INTO lcx_llm.
            lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                http_status_code = 500
                message          = CONV bapi_msg( lcx_llm->msg1 )
                textid           = lcx_llm->if_t100_message~t100key.
        ENDTRY.


      WHEN 'LLMPromptSet'.

        TRY.

            LOOP AT lt_prompt_chain ASSIGNING FIELD-SYMBOL(<fs_prompt_chain>).

              lcl_fiori_llm = NEW zcl_fiori_llm_util( app_id            = <fs_prompt_chain>-app_id
                                                      business_function = <fs_prompt_chain>-business_function ).

              lcl_fiori_llm->save_prompt( app_id               = <fs_prompt_chain>-app_id
                                          business_function    = <fs_prompt_chain>-business_function
                                          prompt_key           = <fs_prompt_chain>-prompt_key
                                          prompt_chain         = <fs_prompt_chain>-prompt_chain
                                          prompt_manual_chain  = <fs_prompt_chain>-prompt_manual_chain
                                          prompt_chain_enabled = <fs_prompt_chain>-prompt_chain_enabled
                                          prompt_type          = <fs_prompt_chain>-prompt_type
                                          prompt               = <fs_prompt_chain>-prompt
                                          is_signed_off        = <fs_prompt_chain>-sign_off
                                          deleteable           = <fs_prompt_chain>-deletable
                                          deleted              = <fs_prompt_chain>-deleted
                                          version              = <fs_prompt_chain>-version
                                          override_func_declar = <fs_prompt_chain>-override_func_declar
                                          think_link           = <fs_prompt_chain>-think_link
                                          final_link           = <fs_prompt_chain>-final_link
                                          prompt_name          = <fs_prompt_chain>-prompt_name ).

            ENDLOOP.

          CATCH zcx_llm INTO lcx_llm.
            lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                http_status_code = 500
                message          = CONV bapi_msg( lcx_llm->msg1 )
                textid           = lcx_llm->if_t100_message~t100key.
        ENDTRY.


      WHEN 'LLMBotToCatalogSet'.

        TRY.
            DATA: ls_bot_to_catalog TYPE zllm_bot_to_catalog_odata .


            LOOP AT lt_bot_to_agent ASSIGNING FIELD-SYMBOL(<fs_bot_to_agent>).

              ls_bot_to_catalog-app_id = <fs_bot_to_agent>-app_id.
              ls_bot_to_catalog-business_function = <fs_bot_to_agent>-business_function.
              ls_bot_to_catalog-rag_app_id = <fs_bot_to_agent>-rag_app_id.
              ls_bot_to_catalog-rag_business_function = <fs_bot_to_agent>-rag_business_function.
              ls_bot_to_catalog-deleted = <fs_bot_to_agent>-deleted.
              ls_bot_to_catalog-catalog_name = <fs_bot_to_agent>-catalog_name.

              lcl_fiori_llm = NEW zcl_fiori_llm_util( app_id            = <fs_bot_to_agent>-app_id
                                                      business_function = <fs_bot_to_agent>-business_function ).

              lcl_fiori_llm->save_bot_to_catalog( app_id            = <fs_bot_to_agent>-app_id
                                                  business_function = <fs_bot_to_agent>-business_function
                                                  catalog           = ls_bot_to_catalog ).

            ENDLOOP.

          CATCH zcx_llm INTO lcx_llm.
            lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                http_status_code = 500
                message          = CONV bapi_msg( lcx_llm->msg1 )
                textid           = lcx_llm->if_t100_message~t100key.
        ENDTRY.


      WHEN 'LLMPromptChainToRAGSet'.

        IF lt_prompt_to_rag IS NOT INITIAL.
          ls_prompt_to_rag = lt_prompt_to_rag[ 1 ].
        ENDIF.

        TRY.

            lcl_fiori_llm = NEW zcl_fiori_llm_util( app_id            = ls_prompt_to_rag-app_id
                                                    business_function = ls_prompt_to_rag-business_function ).

            lcl_fiori_llm->save_prompt_chain_to_rag( prompt_chain = lt_prompt_to_rag ).


          CATCH zcx_llm INTO lcx_llm.
            lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                http_status_code = 500
                message          = CONV bapi_msg( lcx_llm->msg1 )
                textid           = lcx_llm->if_t100_message~t100key.
        ENDTRY.


      WHEN 'LLMPromptConnectorSet'.

        TRY.

            lcl_fiori_llm = NEW zcl_fiori_llm_util( app_id            = ls_prompt_connector-parent_app_id
                                                    business_function = ls_prompt_connector-parent_business_function ).

            lcl_fiori_llm->save_prompt_connector( prompt_connector = lt_prompt_connector ).


          CATCH zcx_llm INTO lcx_llm.
            lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                http_status_code = 500
                message          = CONV bapi_msg( lcx_llm->msg1 )
                textid           = lcx_llm->if_t100_message~t100key.
        ENDTRY.


      WHEN 'LLMPromptConnectorChainSet'.

        TRY.

            lcl_fiori_llm = NEW zcl_fiori_llm_util( app_id            = ls_prompt_connector_chain-app_id
                                                    business_function = ls_prompt_connector_chain-business_function ).

            lcl_fiori_llm->update_prompt_connector_chain( prompt_connector_chain = lt_prompt_connector_chain ).


          CATCH zcx_llm INTO lcx_llm.
            lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                http_status_code = 500
                message          = CONV bapi_msg( lcx_llm->msg1 )
                textid           = lcx_llm->if_t100_message~t100key.
        ENDTRY.


    ENDCASE.


  ENDMETHOD.


  METHOD llmnewagentidset_get_entity.
*** Service implementation to get the next agent ID, when creating a new agent

    TRY.

        DATA(app_id) = it_key_tab[ name = 'AppId' ]-value .
        DATA(business_function) = it_key_tab[ name = 'BusinessFunction' ]-value.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction key not provided from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.


    TRY.
        DATA(lref_fiori_llm_util) = NEW zcl_fiori_llm_util(  ).

        DATA(lv_new_agent_id) = lref_fiori_llm_util->get_next_agent_id( iv_app_id        = CONV zllm_app_id( app_id )
                                                                        iv_buss_function = CONV zllm_buss_function_name( business_function ) ).

        er_entity-app_id = app_id.
        er_entity-business_function = business_function.
        er_entity-next_agent_id = lv_new_agent_id.

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.


  ENDMETHOD.


  METHOD llmbotset_update_entity.
*** Service implementation to create/change bot details

    DATA : ls_bot      TYPE zllm_bots_odata.

    TRY.

        DATA(app_id) = it_key_tab[ name = 'AppId' ]-value .
        DATA(business_function) = it_key_tab[ name = 'BusinessFunction' ]-value.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction key not provided from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

*   Gather all the required data passed from UI
    TRY.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_bot ).

      CATCH /iwbep/cx_mgw_tech_exception INTO DATA(lcx_input).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_input->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

    ENDTRY.

*   Instantiate Fiori class to carry out the creation/update action
    TRY.

        DATA(lref_fiori_llm_util) = NEW zcl_fiori_llm_util( ).

        lref_fiori_llm_util->update_bot( iv_app_id                = ls_bot-app_id
                                         iv_buss_func             = ls_bot-business_function
                                         iv_external_name         = ls_bot-external_name
                                         iv_enable                = ls_bot-enabled
                                         iv_delete                = ls_bot-deletion_flag
                                         iv_helper_app_id         = ls_bot-helper_app_id
                                         iv_helper_bus_function   = ls_bot-helper_business_function
                                         iv_eval_bot_app_id       = ls_bot-eval_app_id
                                         iv_eval_bot_bus_function = ls_bot-eval_business_function
                                         iv_bot_number_range      = ls_bot-bot_number_range
                                         iv_fc_number_range       = ls_bot-version_number_range ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.


  ENDMETHOD.


  METHOD llmroleauthsset_get_entityset.
*** Service implementation to get all roles and authorisation maintained for the App ID and Business function passed

*   Ensure the required keys are passed
    TRY.

        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low .
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction or AgentKey key not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

*   Instantiate Fiori class to fetch all the roles maintained for the key combination passed
    TRY.

        DATA(lref_fiori_llm_util) = NEW zcl_fiori_llm_util( ).

        DATA(lt_roles) = lref_fiori_llm_util->get_auth_roles( app_id            = CONV zllm_app_id( app_id )
                                                              business_function = CONV zllm_buss_function_name( business_function ) ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

    et_entityset = lt_roles.

  ENDMETHOD.


  METHOD llmuserroleset_get_entityset.

    TRY.

        DATA(lref_fiori_llm_util) = NEW zcl_fiori_llm_util( ).

        et_entityset = lref_fiori_llm_util->get_user_roles( ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

  ENDMETHOD.


  METHOD llmratingsearchs_get_entityset.

    DATA: lt_rating_range TYPE zllm_rating_range_tt,
          lt_key_range    TYPE zllm_prompt_key_range_tt.

    TRY.

        DATA(rating_range) = it_filter_select_options[ property = 'Rating' ]-select_options.

        lt_rating_range = VALUE zllm_rating_range_tt( FOR ls_rating_range IN rating_range
                                                     ( sign = ls_rating_range-sign
                                                       option = ls_rating_range-option
                                                       low = ls_rating_range-low
                                                       high = ls_rating_range-high ) ).

      CATCH cx_sy_itab_line_not_found.
* Do nothing
    ENDTRY.

    TRY.

        DATA(key_range) = it_filter_select_options[ property = 'Key' ]-select_options.


        lt_key_range = VALUE zllm_prompt_key_range_tt( FOR ls_key_range IN key_range
                                                      ( sign = ls_key_range-sign
                                                        option = ls_key_range-option
                                                        low = ls_key_range-low
                                                        high = ls_key_range-high ) ).

      CATCH cx_sy_itab_line_not_found.
* Do nothing
    ENDTRY.


    TRY.

        DATA(lcl_fiori_llm_util) = NEW zcl_fiori_llm_util( ).

        DATA(lt_entityset) = lcl_fiori_llm_util->get_prompt_by_rating( EXPORTING app_id            = 'CSHUB'
                                                                                 business_function = 'EMAIL'
                                                                                 prompt_key_range  = lt_key_range
                                                                                 rating_range      = lt_rating_range ).



        et_entityset = lt_entityset.

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

    ENDTRY.


  ENDMETHOD.


  METHOD llmfeedbacksearc_get_entityset.


    TRY.

        DATA(lcl_fiori_llm_util) = NEW zcl_fiori_llm_util( ).

        DATA(ls_entityset) = lcl_fiori_llm_util->get_feedb_by_object_key( EXPORTING it_filter_select_options = it_filter_select_options ).

        APPEND ls_entityset TO et_entityset.

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

    ENDTRY.


  ENDMETHOD.


  METHOD llminteractionre_get_entityset.
*** Service implementation to fetch results for bot interactions

    TRY.
        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(interaction_date_range) = it_filter_select_options[ property = 'InteractionDate' ]-select_options[ 1 ].

        DATA(lt_interaction_date_range) = VALUE ztt_date_range( sign   = interaction_date_range-sign
                                                                option = interaction_date_range-option
                                                              ( low    = interaction_date_range-low
                                                                high   = interaction_date_range-high  ) ).

        lcl_report = NEW zcl_llm_report_util( app_id            = CONV zllm_app_id( app_id )
                                              business_function = CONV zllm_buss_function_name( business_function ) ).

        et_entityset = lcl_report->build_interaction_report( it_date_range = lt_interaction_date_range ).

      CATCH cx_sy_itab_line_not_found.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

    ENDTRY.


  ENDMETHOD.


  METHOD llmmodelset_update_entity.

    DATA : ls_model_config      TYPE zllm_user_model_odata.

    TRY.

        DATA(app_id) = it_key_tab[ name = 'AppId' ]-value .
        DATA(business_function) = it_key_tab[ name = 'BusinessFunction' ]-value.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction key not provided from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.


    TRY.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_model_config ).

      CATCH /iwbep/cx_mgw_tech_exception INTO DATA(lcx_input).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_input->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

    ENDTRY.

  ENDMETHOD.


  METHOD llmmodelset_get_entityset.

    TRY.

        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction key not provided from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.


    TRY.

        DATA(lcl_fiori_llm_util) = NEW zcl_fiori_llm_util( app_id            = CONV zllm_app_id( app_id )
                                                           business_function = CONV zllm_buss_function_name( business_function ) ).


        et_entityset = lcl_fiori_llm_util->get_models( app_id            = CONV zllm_app_id( app_id )
                                                       business_function = CONV zllm_buss_function_name( business_function ) ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

  ENDMETHOD.


  METHOD llmragagentscata_get_entityset.

    TRY.

        DATA(lcl_fiori_llm_util) = NEW zcl_fiori_llm_util( ).

        et_entityset = lcl_fiori_llm_util->get_agent_catalog_menu( ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

  ENDMETHOD.


  METHOD llmmcpagentrespo_get_entityset.

    DATA : app_id            TYPE zllm_app_id,
           business_function TYPE zllm_buss_function_name,
           agent_name        TYPE zllm_function_name.

    TRY.

        app_id = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        business_function = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        agent_name = it_filter_select_options[ property = 'AgentName' ]-select_options[ 1 ]-low.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId/BusinessFunction/AgentName key not provided from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.


    TRY.

        DATA(lcl_fiori_llm_util) = NEW zcl_fiori_llm_util( ).

        DATA(response_structure_json) = lcl_fiori_llm_util->get_mcp_agent_response_struct( app_id            = app_id
                                                                                           business_function = business_function
                                                                                           agent_name        = agent_name
                                                                                           ).

        DATA(ls_entity) = VALUE zllm_mcp_agent_response_odata(
            app_id             = app_id
            business_function  = business_function
            agent_name         = agent_name
            response_structure = response_structure_json
        ).

        APPEND ls_entity TO et_entityset.

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.


  ENDMETHOD.


  METHOD llmsentimentrepo_create_entity.

    DATA ls_post TYPE zllm_sentiment_report.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).

    TRY.
        DATA(lcl_llm_sentiment_report) = NEW zcl_llm_sentiment_report( app_id            = ls_post-app_id
                                                                       business_function = ls_post-business_function
                                                                       from_date         = ls_post-from_date
                                                                       to_date           = ls_post-to_date
                                                                       from_time         = ls_post-from_time
                                                                       to_time           = ls_post-to_time
                                                                       query_type        = ls_post-query_type ).

        er_entity-response = lcl_llm_sentiment_report->generate_response( prompt     = ls_post-request
                                                                          query_type = ls_post-query_type ).
      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

  ENDMETHOD.


  METHOD llmreportprompts_create_entity.

    DATA ls_post TYPE zllm_prompt_fav.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).

    TRY.
        DATA(lcl_llm_report_prompt_util) = NEW zcl_llm_report_prompt_util( app_id            = ls_post-app_id
                                                                           business_function = ls_post-business_function
                                                                           prompt_id         = ls_post-prompt_id
                                                                           prompt            = ls_post-prompt
                                                                           deleted           = ls_post-deleted ).

        lcl_llm_report_prompt_util->save_prompt(  ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

  ENDMETHOD.


  METHOD llmreportprompts_get_entityset.

    TRY.

        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction key not provided from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

    TRY.
        DATA(lcl_llm_report_prompt_util) = NEW zcl_llm_report_prompt_util( app_id            = CONV zllm_app_id( app_id )
                                                                           business_function = CONV zllm_buss_function_name( business_function ) ).

        et_entityset = lcl_llm_report_prompt_util->get_prompt(  ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

  ENDMETHOD.


  METHOD llmmcprolesauths_get_entityset.

    TRY.

        DATA(lref_fiori_llm_util) = NEW zcl_fiori_llm_util( ).

        et_entityset = lref_fiori_llm_util->get_roles_and_authorization( ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

  ENDMETHOD.


  METHOD llmpromptconnect_get_entityset.

    TRY.

        DATA(app_id) = it_filter_select_options[ property = 'ParentAppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'ParentBusinessFunction' ]-select_options[ 1 ]-low.
        DATA(prompt_chain) = it_filter_select_options[ property = 'ParentPromptChain' ]-select_options[ 1 ]-low.
        DATA(prompt_key) = it_filter_select_options[ property = 'ParentPromptKey' ]-select_options[ 1 ]-low.
        DATA(source) = 'Parent'.

      CATCH cx_sy_itab_line_not_found.

        TRY.

            app_id = it_filter_select_options[ property = 'ChildAppId' ]-select_options[ 1 ]-low.
            business_function = it_filter_select_options[ property = 'ChildBusinessFunction' ]-select_options[ 1 ]-low.
            prompt_chain = it_filter_select_options[ property = 'ChildPromptChain' ]-select_options[ 1 ]-low.
            prompt_key = it_filter_select_options[ property = 'ChildPromptKey' ]-select_options[ 1 ]-low.
            source = 'Child'.

          CATCH cx_sy_itab_line_not_found.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                http_status_code = 500
                message          = 'AppId or BusinessFunction or PromptChain or PromptKey not found from UI Odata call'
                textid           = /iwbep/cx_mgw_busi_exception=>business_error.
        ENDTRY.
    ENDTRY.

* clean the parameters.
    CONDENSE app_id NO-GAPS.
    CONDENSE business_function NO-GAPS.

    TRY.
        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        et_entityset = lcl_fiori_llm->get_prompt_connector( app_id            = CONV zllm_app_id( app_id )
                                                            business_function = CONV zllm_buss_function_name( business_function )
                                                            prompt_chain      = CONV zllm_prompt_chain( prompt_chain )
                                                            prompt_key        = CONV zllm_prompt_key( prompt_key )
                                                            source            = CONV char10( source ) ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

  ENDMETHOD.

  METHOD llmagentlistset_get_entityset.

    TRY.

        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

* clean the parameters.
    CONDENSE app_id NO-GAPS.
    CONDENSE business_function NO-GAPS.

    TRY.
        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        et_entityset = lcl_fiori_llm->get_agent_list( app_id            = CONV zllm_app_id( app_id )
                                                      business_function = CONV zllm_buss_function_name( business_function ) ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

  ENDMETHOD.

  METHOD llmpromptpropert_get_entityset.

    TRY.

        DATA(app_id) = it_filter_select_options[ property = 'AppId' ]-select_options[ 1 ]-low.
        DATA(business_function) = it_filter_select_options[ property = 'BusinessFunction' ]-select_options[ 1 ]-low.
        DATA(prompt_key) = it_filter_select_options[ property = 'PromptKey' ]-select_options[ 1 ]-low.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'AppId or BusinessFunction not found from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

* clean the parameters.
    CONDENSE app_id NO-GAPS.
    CONDENSE business_function NO-GAPS.

    TRY.
        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        et_entityset = lcl_fiori_llm->get_prompt_prop( app_id            = CONV zllm_app_id( app_id )
                                                       business_function = CONV zllm_buss_function_name( business_function )
                                                       prompt_key        = CONV zllm_prompt_key( prompt_key ) ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

  ENDMETHOD.

  METHOD llmauthtabset_create_entity.

    TRY.
        DATA ls_post TYPE zllm_auths_tabs_odata.
        io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).


        lcl_fiori_llm = NEW zcl_fiori_llm_util(  ).

        lcl_fiori_llm->create_update_tabs( tabs = ls_post ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        lcx_llm->if_t100_message~t100key-attr1 = lcx_llm->msg1.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->msg1 )
            textid           = lcx_llm->if_t100_message~t100key.
    ENDTRY.

  ENDMETHOD.

  METHOD llmauthtabset_get_entityset.

    TRY.

        DATA(lref_fiori_llm_util) = NEW zcl_fiori_llm_util( ).

        et_entityset = lref_fiori_llm_util->get_tabs_and_permission( ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_llm->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
