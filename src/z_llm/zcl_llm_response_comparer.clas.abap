CLASS zcl_llm_response_comparer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS : constructor.

    METHODS : compare_responses                   IMPORTING iv_uuid_key          TYPE guid16
                                                            iv_app_id            TYPE zllm_app_id
                                                            iv_business_function TYPE zllm_buss_function_name
                                                            iv_request           TYPE zllm_request_value
                                                            iv_bot_response      TYPE zllm_old_value
                                                            iv_edited_resposne   TYPE zllm_new_value
                                                  RETURNING VALUE(rv_category)   TYPE zllm_level_of_edit
                                                  RAISING   zcx_llm.

  PROTECTED SECTION.
  PRIVATE SECTION.

  METHODS : log_comparison_result                 IMPORTING iv_uuid_key          TYPE guid16
                                                            iv_app_id            TYPE zllm_app_id
                                                            iv_business_function TYPE zllm_buss_function_name
                                                            iv_edit_category     TYPE zllm_level_of_edit
                                                            iv_comments          TYPE zllm_comparison_comments
                                                  RAISING zcx_llm.

ENDCLASS.



CLASS zcl_llm_response_comparer IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.

  METHOD compare_responses.


    DATA : lv_edit_category TYPE string,
           lv_comments      TYPE string.


    TRY.

        DATA(lref_llm_util) = NEW zcl_llm_util( app_id            = iv_app_id
                                                business_function = iv_business_function ).

        DATA(ls_prompt) = lref_llm_util->fetch_prompt( iv_prompt_key = 'RESPONSECOMPARER' ).


        DATA(lv_prompt) = |{ ls_prompt-prompt_text } \n\n RESPONSE1:{ iv_bot_response } \n RESPONSE2: { iv_edited_resposne }|.


        DATA(ls_response) = lref_llm_util->call_simple_model( prompt   = lv_prompt ).

        TRY.

            CALL TRANSFORMATION zllm_cs_hub_xml_transform
            SOURCE XML ls_response-response
            RESULT edit_category = lv_edit_category
                   comments = lv_comments.

            me->log_comparison_result( iv_uuid_key          = iv_uuid_key
                                       iv_app_id            = iv_app_id
                                       iv_business_function = iv_business_function
                                       iv_edit_category     = CONV zllm_level_of_edit( lv_edit_category )
                                       iv_comments          = lv_comments ).

          CATCH cx_st_error INTO DATA(lcx_st_error).
        ENDTRY.

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.


  ENDMETHOD.

  METHOD log_comparison_result.

    DATA(lref_llm_log) = NEW zcl_llm_log( app_id            = iv_app_id
                                          business_function = iv_business_function ).

    lref_llm_log->write_response_compare_log(
      iv_uuid_key          = iv_uuid_key
      iv_app_id            = iv_app_id
      iv_business_function = iv_business_function
      iv_edit_category     = iv_edit_category
      iv_comments          = iv_comments
    ).
*    CATCH zcx_llm.

  ENDMETHOD.

ENDCLASS.
