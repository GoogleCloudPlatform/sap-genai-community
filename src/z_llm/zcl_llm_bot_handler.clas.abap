CLASS zcl_llm_bot_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_fiori_llm_util .

  PUBLIC SECTION.

    METHODS : constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.


    METHODS : get_bot                        IMPORTING iv_app_id     TYPE zllm_app_id
                                                       iv_buss_func  TYPE zllm_buss_function_name
                                             RETURNING VALUE(rs_bot) TYPE zllm_bots
                                             RAISING   zcx_llm.

    METHODS : manage_bot                     IMPORTING iv_app_id                TYPE zllm_app_id
                                                       iv_buss_func             TYPE zllm_buss_function_name
                                                       iv_external_name         TYPE zllm_external_name
                                                       iv_enabled               TYPE zllm_enabled_indicator
                                                       iv_deletion_flag         TYPE zllm_deletion_flag
                                                       iv_helper_app_id         TYPE zllm_helper_app_id OPTIONAL
                                                       iv_helper_bus_function   TYPE zllm_helper_bus_func_name OPTIONAL
                                                       iv_eval_bot_app_id       TYPE zllm_eval_app_id OPTIONAL
                                                       iv_eval_bot_bus_function TYPE zllm_eval_bus_func_name OPTIONAL
                                                       iv_bot_number_range      TYPE zllm_agent_param_value OPTIONAL
                                                       iv_fc_number_range       TYPE zllm_agent_param_value OPTIONAL
                                             RAISING   zcx_llm.
ENDCLASS.



CLASS zcl_llm_bot_handler IMPLEMENTATION.

  METHOD constructor.


  ENDMETHOD.


  METHOD get_bot.
*** Method implementation to get details of an existing bot

    IF iv_app_id IS NOT INITIAL AND
       iv_buss_func IS NOT INITIAL.

      SELECT SINGLE *
               FROM zllm_bots
               INTO @rs_bot
              WHERE app_id = @iv_app_id
                AND business_function = @iv_buss_func.

      IF sy-subrc <> 0.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>bot_not_found
            msg1   = CONV string( iv_app_id )
            msg2   = CONV string( iv_buss_func ).


      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD manage_bot.


    DATA : ls_external_bot_name TYPE zllm_ext_value,
           lt_external_bot_name TYPE zllm_ext_value_tt.

    DATA: ls_config TYPE zllm_config.


    DATA(lref_crud_control_util) = NEW zcl_llm_crud_control_util( app_id            = iv_app_id
                                                                  business_function = iv_buss_func ).

    ls_config-app_id = iv_app_id.
    ls_config-buss_function = iv_buss_func.
    ls_config-mandt = sy-mandt.


    IF iv_bot_number_range IS NOT INITIAL.

      ls_config-param = 'NUMBER_RANGE_INTERVAL'.
      ls_config-type = ''.
      ls_config-value = iv_bot_number_range.

      lref_crud_control_util->save_config( config = ls_config ).


      ls_config-type = 'GLOBAL'.
      lref_crud_control_util->save_config( config = ls_config ).

    ENDIF.

    IF iv_fc_number_range IS NOT INITIAL.

      ls_config-param = 'VER_NUMBER_RANGE_INTERVAL'.
      ls_config-type = ''.
      ls_config-value = iv_fc_number_range.

      lref_crud_control_util->save_config( config = ls_config ).

      ls_config-type = 'GLOBAL'.
      lref_crud_control_util->save_config( config = ls_config ).

      CLEAR : ls_config.

    ENDIF.

    TRY.

        DATA(lref_version_control_util) = NEW zcl_llm_version_control_util( ).

        DATA(lv_version) = lref_version_control_util->create_new_app_version( app_id            = iv_app_id
                                                                              business_function = iv_buss_func ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.


    TRY.
*       Ensure to grab all the fields in the table for the App ID and business function
        DATA(ls_bot) = me->get_bot( iv_app_id    = iv_app_id
                                    iv_buss_func = iv_buss_func ).

*       The action is 'UPDATE', if an entry is found for the keys passed
        IF ls_bot IS NOT INITIAL.

          ls_bot-client = sy-mandt.
          ls_bot-enabled = iv_enabled.
          ls_bot-deleted = iv_deletion_flag.
          ls_bot-helper_app_id = iv_helper_app_id.
          ls_bot-helper_business_function = iv_helper_bus_function.
          ls_bot-eval_app_id = iv_eval_bot_app_id.
          ls_bot-eval_business_function = iv_eval_bot_bus_function.


          ls_external_bot_name-mandt = sy-mandt.
          ls_external_bot_name-type = 'APP_ID'.
*          ls_external_bot_name-internal_value = iv_app_id.
          ls_external_bot_name-internal_value = iv_app_id && |_| && iv_buss_func.
          ls_external_bot_name-external_value = iv_external_name.
          ls_external_bot_name-lang = 'E'.   "To change

          APPEND ls_external_bot_name TO lt_external_bot_name.

          DATA(lv_status) = lref_crud_control_util->save_bot( bot            = ls_bot
                                                              external_value = lt_external_bot_name
                                                              commit         = abap_true ).

        ENDIF.

*     Exception will be raised if a bot is not found for the app id and business function passed
*     It means, it's a CREATION
      CATCH zcx_llm INTO lcx_llm.

        IF iv_app_id IS NOT INITIAL AND
           iv_buss_func IS NOT INITIAL AND
           iv_external_name IS NOT INITIAL.

*         Build data for saving the bot that has just been created
          ls_bot-client = sy-mandt.
          ls_bot-app_id = to_upper( iv_app_id ).
          ls_bot-business_function = to_upper( iv_buss_func ).
          ls_bot-enabled = abap_true.                "Will be true during creation

          ls_external_bot_name-mandt = sy-mandt.
          ls_external_bot_name-type = 'APP_ID'.
          ls_external_bot_name-internal_value = ls_bot-app_id && |_| && ls_bot-business_function.
          ls_external_bot_name-external_value = iv_external_name.
          ls_external_bot_name-lang = 'E'.   "To change

          APPEND ls_external_bot_name TO lt_external_bot_name.

          lv_status = lref_crud_control_util->save_bot( bot            = ls_bot
                                                        external_value = lt_external_bot_name
                                                        commit         = abap_true ).

        ENDIF.


    ENDTRY.

  ENDMETHOD.

ENDCLASS.
