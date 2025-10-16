CLASS zcl_llm_agents_sql DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_llm_agents_util.

  PUBLIC SECTION.

    METHODS :
      "! Constructor
      constructor.


  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS :
      "! Method to fetch declarations/prompt descriptions of functions/agents
      fetch_zllm_func_declar                IMPORTING iv_app_id                       TYPE zllm_app_id
                                                      iv_buss_function                TYPE zllm_buss_function_name
                                            RETURNING VALUE(rt_function_declarations) TYPE zllm_func_declar_tt
                                            RAISING   zcx_llm,

      fetch_agent                           IMPORTING iv_app_id                       TYPE zllm_app_id
                                                      iv_buss_function                TYPE zllm_buss_function_name
                                                      iv_function_name                TYPE zllm_function_name
                                                      iv_version                      TYPE zllm_version
                                                      it_func_type_range              TYPE zllm_function_type_range_tt OPTIONAL

                                            RETURNING VALUE(rt_function_declarations) TYPE zllm_agent_cds_tt
                                            RAISING   zcx_llm,

      fetch_latest_agent_index              IMPORTING iv_app_id                    TYPE zllm_app_id
                                                      iv_buss_function             TYPE zllm_buss_function_name
                                            RETURNING VALUE(rv_latest_agent_index) TYPE zllm_function_index
                                            RAISING   zcx_llm,

      check_if_agent_index_exists           IMPORTING iv_app_id             TYPE zllm_app_id
                                                      iv_buss_function      TYPE zllm_buss_function_name
                                                      iv_function_name      TYPE zllm_function_name
                                            RETURNING VALUE(rv_agent_index) TYPE zllm_function_index,

      fetch_sub_agents                      IMPORTING iv_app_id                           TYPE zllm_app_id
                                                      iv_buss_function                    TYPE zllm_buss_function_name
                                                      iv_version                          TYPE zllm_version
                                                      iv_supervisor_agent                 TYPE zllm_function_name
                                            RETURNING VALUE(rt_sub_function_declarations) TYPE zllm_sub_func_cds_tt
                                            RAISING   zcx_llm.



ENDCLASS.



CLASS zcl_llm_agents_sql IMPLEMENTATION.

  METHOD constructor.


  ENDMETHOD.



  METHOD fetch_zllm_func_declar.

    IF iv_app_id IS NOT INITIAL AND
       iv_buss_function IS NOT INITIAL.

      SELECT mandt,
             app_id,
             business_function,
             function_name,
             version,
             function_description,
             function_parameters_id,
             function_version
        FROM zllm_func_declar
        INTO TABLE @rt_function_declarations
             WHERE app_id = @iv_app_id
               AND business_function = @iv_buss_function.

      IF sy-subrc <> 0.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>no_function_declaration
            msg1   = CONV string( iv_app_id )
            msg2   = CONV string( iv_buss_function ).


      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD fetch_agent.
*** Method implementation to fetch agent details using the CDS ZCDS_LLM_AGENT, that
*** that combines the results of two tables ZLLM_FUNC_DELCAR & ZLLM_FUNC_PARAME

*   Check for the keys that are required, as the App is designed to fetch results for
*   one agent at a time
    IF iv_app_id        IS NOT INITIAL AND
       iv_buss_function IS NOT INITIAL AND
       iv_function_name IS NOT INITIAL AND
       iv_version IS NOT INITIAL.

      SELECT app_id ,
             business_function,
             function_name AS agent_key,
             function_description,
             function_parameters_id,
             parameters_id,
             parameter_name,
             parameter_description,
             parameter_type,
             is_required,
             function_version,
             version,
             external_value,
             automate,
             web_enabled
        FROM zcds_llm_agent
        INTO CORRESPONDING FIELDS OF TABLE @rt_function_declarations
             WHERE app_id = @iv_app_id
               AND business_function = @iv_buss_function
               AND function_name  = @iv_function_name
               AND version = @iv_version
               AND type IN @it_func_type_range.

    ENDIF.


  ENDMETHOD.

  METHOD fetch_latest_agent_index.
*** Method implementation to fetch index of the last agent created

    SELECT MAX( function_index )
      FROM zllm_func_index
      INTO @rv_latest_agent_index
     WHERE app_id = @iv_app_id
       AND business_function = @iv_buss_function.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>no_function_index
          msg1   = CONV string( iv_app_id )
          msg2   = CONV string( iv_buss_function ).

    ENDIF.


  ENDMETHOD.

  METHOD check_if_agent_index_exists.
*** Method implementation check if index for an agent exists

    SELECT SINGLE function_index
      FROM zllm_func_index
      INTO @rv_agent_index
     WHERE app_id = @iv_app_id
       AND business_function = @iv_buss_function
       AND function_name = @iv_function_name.

  ENDMETHOD.

  METHOD fetch_sub_agents.

*** Method implementation to fetch agent details using the CDS ZCDS_LLM_SUB_AGENT, that
*** that combines the results of two tables ZLLM_SUB_FUNC & ZLLM_SUB_FUNC_P

*   Check for the keys that are required, as the App is designed to fetch results for
*   one agent at a time
    IF iv_app_id        IS NOT INITIAL AND
        iv_buss_function IS NOT INITIAL AND
        iv_version       IS NOT INITIAL AND
        iv_supervisor_agent IS NOT INITIAL.

      SELECT app_id ,
             business_function,
             @iv_supervisor_agent AS supv_function_name,
             function_name AS sub_function_name,
             function_description,
             function_parameters_id,
             parameters_id,
             parameter_name,
             parameter_description,
             parameter_type,
             is_required,
             function_version,
             version,
             external_value
        FROM zcds_llm_sub_agent( p_supv_agent = @iv_supervisor_agent )
        INTO CORRESPONDING FIELDS OF TABLE @rt_sub_function_declarations
             WHERE app_id = @iv_app_id
               AND business_function = @iv_buss_function
               AND version = @iv_version.

    ENDIF.


  ENDMETHOD.

ENDCLASS.
