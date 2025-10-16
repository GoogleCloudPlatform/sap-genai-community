CLASS zcl_llm_config_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: constructor    IMPORTING app_id            TYPE zllm_app_id
                                      business_function TYPE zllm_buss_function_name
                                      type              TYPE zllm_config_type
                            RAISING   zcx_llm,
      set_temperature IMPORTING temperature   TYPE string
                      RETURNING VALUE(status) TYPE boolean
                      RAISING   zcx_llm,
      set_number_range_interval IMPORTING interval      TYPE zllm_number_interval
                                          interval_type TYPE zllm_interval_type
                                RETURNING VALUE(status) TYPE boolean,
      set_gcs_bucket IMPORTING bucket        TYPE string
                     RETURNING VALUE(status) TYPE boolean
                     RAISING   zcx_llm,
      set_gcs_file IMPORTING file          TYPE string
                             mime          TYPE string
                   RETURNING VALUE(status) TYPE boolean
                   RAISING   zcx_llm,
      get_config IMPORTING parameter     TYPE zllm_agent_parameter
                 RETURNING VALUE(config) TYPE zllm_config,
      get_all_config RETURNING VALUE(config) TYPE zllm_config_tt.


  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: gv_app_id            TYPE zllm_app_id,
          gv_business_function TYPE zllm_buss_function_name,
          gv_config_type       TYPE zllm_config_type,
          gs_config            TYPE zllm_config.

ENDCLASS.



CLASS zcl_llm_config_util IMPLEMENTATION.


  METHOD constructor.

    gv_app_id = app_id.
    gv_business_function = business_function.
    gv_config_type = type.

  ENDMETHOD.


  METHOD get_all_config.

    IF me->gv_config_type IS NOT INITIAL.

      SELECT *
      FROM zllm_config
      INTO TABLE @config
      WHERE app_id = @gv_app_id
      AND buss_function = @gv_business_function
      AND type = @gv_config_type.

    ELSE.

      SELECT *
        FROM zllm_config
        INTO TABLE @config
       WHERE app_id = @gv_app_id
         AND buss_function = @gv_business_function.

    ENDIF.

  ENDMETHOD.


  METHOD set_temperature.

    TRY.

        DATA(lcl_crud_util) = NEW zcl_llm_crud_control_util( app_id            = gv_app_id
                                                             business_function = gv_business_function ).


        CLEAR: gs_config.

        gs_config-mandt = sy-mandt.
        gs_config-app_id = gv_app_id.
        gs_config-buss_function = gv_business_function.
        gs_config-param = 'TEMPERATURE'.
        gs_config-value = temperature.

* Save config.
        lcl_crud_util->save_config( config = gs_config ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.

  ENDMETHOD.


  METHOD set_number_range_interval.

    TRY.

        DATA(lcl_crud_util) = NEW zcl_llm_crud_control_util( app_id            = gv_app_id
                                                             business_function = gv_business_function ).


        CLEAR: gs_config.

        gs_config-mandt = sy-mandt.
        gs_config-app_id = gv_app_id.
        gs_config-buss_function = gv_business_function.
        gs_config-param = |{ interval }-{ interval_type }|.
        gs_config-value = interval.

* Save config.
        lcl_crud_util->save_config( config = gs_config ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2.
    ENDTRY.


  ENDMETHOD.

  METHOD set_gcs_bucket.

    TRY.

        DATA(lcl_crud_util) = NEW zcl_llm_crud_control_util( app_id            = gv_app_id
                                                             business_function = gv_business_function ).


        CLEAR: gs_config.

        gs_config-mandt = sy-mandt.
        gs_config-app_id = gv_app_id.
        gs_config-buss_function = gv_business_function.
        gs_config-param = 'GCS_BUCKET'.
        gs_config-value = bucket.

* Save config.
        lcl_crud_util->save_config( config = gs_config ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2.
    ENDTRY.



  ENDMETHOD.

  METHOD set_gcs_file.


    TRY.

        DATA(lcl_crud_util) = NEW zcl_llm_crud_control_util( app_id            = gv_app_id
                                                             business_function = gv_business_function ).


        CLEAR: gs_config.

        gs_config-mandt = sy-mandt.
        gs_config-app_id = gv_app_id.
        gs_config-buss_function = gv_business_function.
        gs_config-param = 'GCS_BUCKET'.
        gs_config-value = file.

* Save config.
        lcl_crud_util->save_config( config = gs_config ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2.
    ENDTRY.


  ENDMETHOD.


  METHOD get_config.

    SELECT SINGLE *
    FROM zllm_config
    INTO @config
    WHERE app_id = @gv_app_id
    AND buss_function = @gv_business_function
    AND param = @parameter.

  ENDMETHOD.

ENDCLASS.
