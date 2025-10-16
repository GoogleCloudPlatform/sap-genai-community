CLASS zcl_llm_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_llm_util
                 zcl_ai_video_util.

  PUBLIC SECTION.

    METHODS: constructor IMPORTING app_id            TYPE zllm_app_id
                                   business_function TYPE zllm_buss_function_name,
      write_general_log   IMPORTING request              TYPE zllm_request_value OPTIONAL
                                    request_no_html      TYPE zllm_request_value OPTIONAL
                                    sentiment            TYPE zllm_sentiment OPTIONAL
                                    object_type          TYPE zllm_log_object_type OPTIONAL
                                    object_key           TYPE  zllm_log_object_key OPTIONAL
                                    old_value            TYPE zllm_old_value OPTIONAL
                                    new_value            TYPE zllm_new_value OPTIONAL
                                    request_token_count  TYPE zllm_request_token OPTIONAL
                                    response_token_count TYPE zllm_response_token OPTIONAL
                                    uuid_key             TYPE guid16 OPTIONAL,
      write_rating_log   IMPORTING object_type TYPE zllm_log_object_type OPTIONAL
                                   object_key  TYPE  zllm_log_object_key OPTIONAL
                                   rating      TYPE zllm_rating
                                   uuid_key    TYPE guid16 OPTIONAL,

      create_log_from_llm_fm IMPORTING iv_function_name       TYPE zllm_function_name
                                       iv_uuid                TYPE guid16
                                       iv_prompt              TYPE string
                                       iv_function_response   TYPE string
                                       it_function_parameters TYPE /goog/t_function_parameters,

      write_response_compare_log IMPORTING iv_uuid_key          TYPE guid16
                                           iv_app_id            TYPE zllm_app_id
                                           iv_business_function TYPE zllm_buss_function_name
                                           iv_edit_category     TYPE zllm_level_of_edit
                                           iv_comments          TYPE zllm_comparison_comments
                                 RAISING   zcx_llm,

      save_log_notes IMPORTING object_key  TYPE zllm_log_object_key
                               object_type TYPE zllm_log_object_type
                               uuid_key    TYPE guid
                               note        TYPE zllm_note
                     RAISING   zcx_llm,

      save_for_action_log IMPORTING uuid_key TYPE guid16
                                    agent    TYPE zllm_function_name
                                    rag_data TYPE zllm_rag_data
                          RAISING   zcx_llm.


  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS: generate_uuid RETURNING VALUE(uuid) TYPE guid16.

    METHODS : check_uuid_key_exists IMPORTING iv_uuid_key                    TYPE guid16
                                    RETURNING VALUE(rs_existing_star_rating) TYPE zllm_star_rating.

    DATA: app_id            TYPE zllm_app_id,
          business_function TYPE zllm_buss_function_name.


ENDCLASS.



CLASS zcl_llm_log IMPLEMENTATION.


  METHOD constructor.

    me->app_id = app_id.
    me->business_function = business_function.


  ENDMETHOD.


  METHOD save_log_notes.

    DATA: ls_log_note TYPE zllm_log_notes.

    ls_log_note-mandt = sy-mandt.
    ls_log_note-app_id = app_id.
    ls_log_note-buss_function = business_function.
    ls_log_note-uuid = me->generate_uuid( ).
    ls_log_note-uuid_key = uuid_key.
    ls_log_note-dat = sy-datum.
    ls_log_note-time = sy-uzeit.
    ls_log_note-object_key = object_key.
    ls_log_note-object_type = object_type.
    ls_log_note-user_name = sy-uname.
    ls_log_note-note = note.


    MODIFY zllm_log_notes FROM ls_log_note.

    IF sy-subrc <> 0.

      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>log_note_save_failed.

    ELSE.
      COMMIT WORK AND WAIT.
    ENDIF.



  ENDMETHOD.


  METHOD write_rating_log.

    DATA: log TYPE zllm_star_rating.

    DATA(ls_existing_star_rating) = me->check_uuid_key_exists( iv_uuid_key = uuid_key ).

    IF ls_existing_star_rating IS INITIAL.

      log-mandt = sy-mandt.
      log-uuid = me->generate_uuid(  ).
      log-app_id = me->app_id.
      log-buss_function = me->business_function.
      log-user_name = sy-uname.
      log-dat = sy-datum.
      log-time = sy-uzeit.
      log-object_key = object_key.
      log-object_type = object_type.
      log-rating = rating.
      log-uuid_key = uuid_key.

    ELSE.

      log = ls_existing_star_rating.
      log-dat = sy-datum.
      log-time = sy-uzeit.
      log-rating = rating.

    ENDIF.


    MODIFY zllm_star_rating FROM log.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.

  METHOD write_general_log.

    DATA: log TYPE zllm_log.

    log-mandt = sy-mandt.
    log-uuid = me->generate_uuid(  ).
    log-uuid_key = uuid_key.
    log-user_name = sy-uname.
    log-app_id = me->app_id.
    log-buss_function = me->business_function.
    log-dat = sy-datum.
    log-time = sy-uzeit.
    log-sentiment = sentiment.
    log-new_value = new_value.
    log-old_value = old_value.
    log-request = request.
    log-request_no_html = request_no_html.
    log-object_key = object_key.
    log-object_type = object_type.
    log-request_token_count = request_token_count.
    log-response_token_count = response_token_count.

    IF request IS NOT INITIAL.
      log-request_char_count = strlen( request ).
    ENDIF.

    IF new_value IS NOT INITIAL.
      log-response_char_count = strlen( new_value ).
    ENDIF.


    MODIFY zllm_log FROM log.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
    ENDIF.


  ENDMETHOD.

  METHOD generate_uuid.

    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = uuid.


  ENDMETHOD.

  METHOD create_log_from_llm_fm.
*** Method implementation that can be called from LLM FMs to log their details during every call

*** Three log entries are to be generated:
*   1. The name of the function and parameters identified by the LLM
*   2. The prompt document containing instructions for the how the end response is to be generated
*   3. Helper data fetched from SAP/other sources that's needed for the response - referred to as RAG Data



    DATA(lv_function_parameters_for_log) = REDUCE string( INIT log TYPE string   sep = ' '
                                                          FOR param IN it_function_parameters
                                                          NEXT log = |{ log }{ sep }{ param-parameter_name } : { param-parameter_value }| sep = ',' ).

* Log function call parameters
    me->write_general_log( uuid_key    = iv_uuid
                           request     = ''
                           object_type = 'FUNCTION_CALL'
                           object_key  = |{ iv_function_name }|
                           new_value   = |Parameters: { lv_function_parameters_for_log }| ).

* Log the function call response
    me->write_general_log( uuid_key    = iv_uuid
                           request     = ''
                           object_type = 'RESPONSE_PROMPT'
                           object_key  = |{ iv_function_name }|
                           new_value   = |{ iv_prompt }| ).

* Log the function call RAG
    me->write_general_log( uuid_key    = iv_uuid
                           request     = ''
                           object_type = 'RAG_DATA'
                           object_key  = |{ iv_function_name }|
                           new_value   = |{ iv_function_response }| ).



  ENDMETHOD.

  METHOD write_response_compare_log.

    DATA : ls_llm_res_compare TYPE zllm_res_compare.

    IF iv_app_id IS NOT INITIAL AND
       iv_business_function IS NOT INITIAL AND
       iv_uuid_key IS NOT INITIAL.

      ls_llm_res_compare-client = sy-mandt.
      ls_llm_res_compare-app_id = iv_app_id.
      ls_llm_res_compare-buss_function = iv_business_function.
      ls_llm_res_compare-uuid = me->generate_uuid(  ).
      ls_llm_res_compare-uuid_key = iv_uuid_key.
      ls_llm_res_compare-edit_category = iv_edit_category.
      ls_llm_res_compare-comparison_comments = iv_comments.
      ls_llm_res_compare-dat = sy-datum.
      ls_llm_res_compare-time = sy-uzeit.

      MODIFY zllm_res_compare FROM ls_llm_res_compare.

      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
      ENDIF.


    ENDIF.

  ENDMETHOD.

  METHOD check_uuid_key_exists.

*   Check if UUID key isn't initial, as there is no point in checking otherwise
*   Not raising an error here as it's okay for there to not be a star rating entry already
    IF iv_uuid_key IS NOT INITIAL.

      SELECT SINGLE *
       FROM zllm_star_rating
       INTO @rs_existing_star_rating
       WHERE uuid_key = @iv_uuid_key.

    ENDIF.

  ENDMETHOD.

  METHOD save_for_action_log.
*** Method implementation to log RAG data for queries that may require further action

    DATA: log TYPE zllm_for_act_log.

    log-client = sy-mandt.
    log-uuid = me->generate_uuid(  ).
    log-uuid_key = uuid_key.
    log-app_id = me->app_id.
    log-buss_function = me->business_function.
    log-agent = agent.
    log-rag_data = rag_data.
    log-username = sy-uname.
    log-dat = sy-datum.
    log-time = sy-uzeit.


    MODIFY zllm_for_act_log FROM log.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
