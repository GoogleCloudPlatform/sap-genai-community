CLASS zcl_llm_agent_catalog_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_fiori_llm_util
                 zcl_llm_agents_util.

  PUBLIC SECTION.


    METHODS: constructor             IMPORTING app_id            TYPE zllm_app_id
                                               business_function TYPE zllm_buss_function_name.

  PROTECTED SECTION.
  PRIVATE SECTION.


    METHODS: save_catalog_map           IMPORTING catalog TYPE zllm_catalog_map
                                        RAISING   zcx_llm,
**********************************************************************
* Save a bot to catalog
**********************************************************************
      save_bot_to_catalog        IMPORTING catalog TYPE zllm_catalog_map
                                 RAISING   zcx_llm,

**********************************************************************
* Get all agents that belong to a catalog
**********************************************************************
      get_agents_in_catalog IMPORTING catalog_name  TYPE zllm_rag_catalog_name
                            RETURNING VALUE(agents) TYPE zllm_agent_catalog_tt,

**********************************************************************
* Get the catalog name from a bot ID
**********************************************************************
      get_catalog_map        RETURNING VALUE(catalog) TYPE zllm_catalog_map_tt
                             RAISING   zcx_llm,

**********************************************************************
* Get all catalogs
**********************************************************************
      get_catalogs          RETURNING VALUE(catalog) TYPE zllm_agent_catalog_tt
                            RAISING   zcx_llm,

**********************************************************************
* Get all agents of a catalog
**********************************************************************
      get_catalog_agents     RETURNING VALUE(rt_agent_catalog_menu) TYPE zllm_agent_catalog_menu_tt,

**********************************************************************
* Save agent to catalog
**********************************************************************
      save_agent_to_catalog IMPORTING catalog       TYPE zllm_agent_cat
                            RETURNING VALUE(status) TYPE boolean
                            RAISING zcx_llm.


    DATA: gv_app_id            TYPE zllm_app_id,
          gv_business_function TYPE zllm_buss_function_name.

ENDCLASS.



CLASS zcl_llm_agent_catalog_util IMPLEMENTATION.

  METHOD constructor.

    gv_app_id = app_id.
    gv_business_function = business_function.

  ENDMETHOD.


  METHOD save_agent_to_catalog.

    TRY.
        DATA(lcl_llm_crud_util) = NEW zcl_llm_crud_control_util( app_id            = me->gv_app_id
                                                                 business_function = me->gv_business_function ).


        lcl_llm_crud_util->save_agent_to_catalog( catalog = catalog ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.



  ENDMETHOD.

  METHOD get_agents_in_catalog.

    SELECT *
    FROM zllm_agent_cat
    INTO TABLE agents
    WHERE catalog_name = catalog_name
    AND deleted = abap_false.


  ENDMETHOD.

  METHOD get_catalog_map.

    SELECT *
    FROM zllm_catalog_map
    INTO TABLE catalog
    WHERE app_id = me->gv_app_id
    AND business_function = me->gv_business_function
    AND deleted = abap_false.

  ENDMETHOD.


  METHOD get_catalogs.


    SELECT DISTINCT catalog_app_id, catalog_business_function, catalog_name
    INTO CORRESPONDING FIELDS OF TABLE @catalog
    FROM zllm_agent_cat
    WHERE deleted = @abap_false.


  ENDMETHOD.

  METHOD save_bot_to_catalog.

    TRY.
        DATA(lcl_llm_crud_util) = NEW zcl_llm_crud_control_util( app_id            = me->gv_app_id
                                                                 business_function = me->gv_business_function ).


        lcl_llm_crud_util->save_bot_to_catalog( catalog = catalog ).

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

    TRY.
        DATA(lcl_llm_crud_util) = NEW zcl_llm_crud_control_util( app_id            = me->gv_app_id
                                                                 business_function = me->gv_business_function ).


        lcl_llm_crud_util->save_catalog_map( catalog_map = catalog ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.

  ENDMETHOD.

  METHOD get_catalog_agents.
*** Method implementation to get the list of catalogues available for displaying in the Catalogue menu

    SELECT catalog_app_id,
           catalog_business_function,
           agent_name,
           catalog_name,
           external_value AS agent_external_name,
           deleted,
           zllm_agent_cat~type
    FROM zllm_agent_cat
    LEFT OUTER JOIN zllm_ext_value ON zllm_ext_value~internal_value = zllm_agent_cat~agent_name
                                  AND zllm_ext_value~lang = 'E'
    INTO TABLE @rt_agent_catalog_menu
    WHERE deleted = @abap_false.

  ENDMETHOD.

ENDCLASS.

