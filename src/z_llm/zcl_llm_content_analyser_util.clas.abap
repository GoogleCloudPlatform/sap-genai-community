CLASS zcl_llm_content_analyser_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS : constructor         IMPORTING iv_requester_app_id        TYPE zllm_app_id
                                            iv_requester_buss_function TYPE zllm_buss_function_name
                                  RAISING zcx_llm.

    METHODS : analyse_content     IMPORTING iv_content                 TYPE string OPTIONAL
                                            it_function_parameters     TYPE /goog/t_function_parameters OPTIONAL
                                            iv_uuid_key                TYPE guid OPTIONAL
                                  RETURNING VALUE(rv_content_analysis) TYPE string.

PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS : gc_app_id TYPE zllm_app_id VALUE 'ANALYSER',
                gc_business_function TYPE zllm_buss_function_name VALUE 'CONTENT'.


    DATA : gref_llm_util  TYPE REF TO zcl_llm_util,
           gv_bot_enabled TYPE char1.

    METHODS : check_config_enabled_for_bot    IMPORTING iv_app_id TYPE zllm_app_id
                                                        iv_business_function type zllm_buss_function_name
                                              RETURNING VALUE(rv_enabled) TYPE char1
                                              RAISING zcx_llm.


  ENDCLASS.



CLASS zcl_llm_content_analyser_util IMPLEMENTATION.

  METHOD constructor.

    IF me->gref_llm_util IS NOT BOUND.

      TRY.

          me->gref_llm_util = NEW zcl_llm_util( app_id            = me->gc_app_id
                                                business_function = me->gc_business_function ).

          me->gv_bot_enabled = me->check_config_enabled_for_bot( iv_app_id            = iv_requester_app_id
                                                                 iv_business_function = iv_requester_buss_function ).


        CATCH zcx_llm INTO DATA(lcx_llm).
            RAISE EXCEPTION TYPE zcx_llm
              EXPORTING
                textid   = lcx_llm->if_t100_message~t100key
                msg1     = lcx_llm->msg1
                msg2     = lcx_llm->msg2.

      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD analyse_content.
*** Method implementation to analyse the content passed and extract images if provided

*   Notes: With regards to logging, since UUID is stored and retrieved from memory - expecting this piece of logic
*   to share the same UUID as the calling agent

    TYPES : BEGIN OF ty_llm_input,
              task    TYPE string,
              content TYPE string,
            END OF ty_llm_input.


    DATA : ls_input_query TYPE ty_llm_input.

    IF me->gv_bot_enabled = abap_true.

*     Analyse the text content passed to summarise it
      IF iv_content IS NOT INITIAL.

        TRY.

            DATA(ls_text_reader_prompt) = me->gref_llm_util->fetch_prompt( iv_prompt_key = 'TEXT_READER'
                                                                           prompt_type   = 'GLOBAL' ).

            ls_input_query-task = ls_text_reader_prompt-prompt_text.
            ls_input_query-content = iv_content.

            DATA(lref_json_util) = NEW zcl_llm_json_converter_util( ).

            DATA(lv_input_query_json) = lref_json_util->convert_to_json( input   = ls_input_query ).

            DATA(ls_response) = me->gref_llm_util->call_model( prompt     = lv_input_query_json ).

            rv_content_analysis = ls_response-response.

*         Logging


            CLEAR : ls_input_query,
                    lv_input_query_json.


          CATCH zcx_llm INTO DATA(lcx_llm).
          CATCH cx_root INTO DATA(lcx_root).
        ENDTRY.


      ENDIF.

*     Analyse the image URLs extracted and passed as function parameters, and get detailed descriptions of the images
      IF it_function_parameters IS NOT INITIAL.

        DATA(lt_function_parameters) = it_function_parameters.

        TRY.

            DATA(ls_image_reader_prompt) = me->gref_llm_util->fetch_prompt( iv_prompt_key = 'IMAGE_READER'
                                                                            prompt_type   = 'GLOBAL' ).

            ls_input_query-task = ls_image_reader_prompt-prompt_text.
*          ls_input_query-content = iv_content.

            IF lref_json_util IS NOT BOUND.
              lref_json_util = NEW zcl_llm_json_converter_util( ).
            ENDIF.

            lv_input_query_json = lref_json_util->convert_to_json( input   = ls_input_query ).


            LOOP AT lt_function_parameters ASSIGNING FIELD-SYMBOL(<fs_parameter>) WHERE parameter_name = 'IMAGE_URL'.

              me->gref_llm_util->set_gcs_file( mime      = 'image/webp'
                                               file_name = <fs_parameter>-parameter_value ).

            ENDLOOP.

            ls_response = me->gref_llm_util->call_model( prompt     = lv_input_query_json ).

            rv_content_analysis = |{ rv_content_analysis }\n\n\n{ ls_response-response }|.

            CLEAR : ls_input_query,
                    lv_input_query_json.

*         Logging!


          CATCH zcx_llm INTO lcx_llm.
          CATCH cx_root INTO lcx_root.
        ENDTRY.


      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD check_config_enabled_for_bot.
*** Method implementation to check if the content analyser bot is enabled for use by another external bot

    IF iv_app_id IS NOT INITIAL AND
       iv_business_function IS NOT INITIAL.

      SELECT SINGLE enable_content_analysis
      FROM zllm_bots
      INTO @rv_enabled
      WHERE app_id = @iv_app_id
      AND business_function = @iv_business_function.

      IF sy-subrc <> 0.

        rv_enabled = abap_false.

      ENDIF.

    ELSE.

      rv_enabled = abap_false.

*    RAISE EXCEPTION TYPE zcx_llm
*      EXPORTING
*        textid   = zcx_llm=>bot_not_found
*        msg1     =
*        msg2     =
*    .

    ENDIF.

  ENDMETHOD.

ENDCLASS.
