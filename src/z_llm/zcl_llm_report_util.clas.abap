CLASS zcl_llm_report_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.


    TYPES : tt_llm_auths TYPE STANDARD TABLE OF zllm_auths WITH EMPTY KEY.

    METHODS: constructor                IMPORTING app_id            TYPE zllm_app_id
                                                  business_function TYPE zllm_buss_function_name,

      build_bot_usage_report     IMPORTING start_date          TYPE datum
                                           end_date            TYPE datum
                                 RETURNING VALUE(usage_report) TYPE zllm_total_usage_report_tt,

      build_agent_usage_report   IMPORTING start_date          TYPE datum
                                           end_date            TYPE datum
                                 RETURNING VALUE(usage_report) TYPE zllm_agent_usage_report_tt,

      build_interaction_report   IMPORTING it_date_range             TYPE ztt_date_range
                                 RETURNING VALUE(interaction_report) TYPE zllm_interaction_report_tt,

      build_sentiment_report     IMPORTING it_date_range   TYPE ztt_date_range
                                           loop_count      TYPE i OPTIONAL
                                 RETURNING VALUE(response) TYPE string,

      build_unsafe_prompt_report IMPORTING it_date_range TYPE ztt_date_range
                                 RETURNING VALUE(report) TYPE zllm_unsafe_prompt_report_tt.


  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: gv_app_id            TYPE zllm_app_id,
          gv_business_function TYPE zllm_buss_function_name,
          gt_uname_role_excl   TYPE tt_llm_auths.

    METHODS : get_fd_tickets_searched   IMPORTING it_search_date_range      TYPE ztt_date_range
                                        RETURNING VALUE(rt_fd_ticket_range) TYPE ztt_cs_hub_ticket_range.

ENDCLASS.



CLASS zcl_llm_report_util IMPLEMENTATION.

  METHOD constructor.

    gv_app_id = app_id.
    gv_business_function = business_function.

    DATA(lref_llm_auths_util) = NEW zcl_llm_auths_util(  ).

    me->gt_uname_role_excl = lref_llm_auths_util->get_users_to_excl_from_report( app_id            = me->gv_app_id
                                                                                 business_function = me->gv_business_function ).

  ENDMETHOD.

  METHOD build_unsafe_prompt_report.

    SELECT app_id, buss_function AS business_function, dat AS date, time, request, new_value AS reason
    FROM zllm_log
    INTO TABLE @report
    WHERE app_id = @gv_app_id
    AND buss_function = @gv_business_function
    AND object_type = 'UNSAFE_PROMPT'
    AND dat IN @it_date_range.


  ENDMETHOD.


  METHOD build_sentiment_report.

    SELECT request
    FROM zllm_log
    INTO TABLE @DATA(lt_requests)
    WHERE dat IN @it_date_range
    AND object_type = 'Freshdesk'.


    IF sy-subrc = 0.

      DATA(lcl_llm_util) = NEW zcl_llm_util( app_id            = 'CSHUB'
                                             business_function = 'EMAIL' ).


      DATA(ls_prompt) = lcl_llm_util->fetch_prompt( iv_prompt_key = 'TRENDREPORT' ).

      DATA: lv_counter TYPE i.

      LOOP AT lt_requests ASSIGNING FIELD-SYMBOL(<fs_requests>).

        lv_counter = lv_counter + 1.
        IF lv_counter = loop_count.
          EXIT.
        ENDIF..

        DATA(ls_removehtml) = lcl_llm_util->fetch_prompt( iv_prompt_key = 'REMOVEHTML' ).
        ls_removehtml-prompt_text = |{ ls_removehtml-prompt_text } \n\n { <fs_requests>-request }|.
        DATA(removed_html) = lcl_llm_util->call_model( prompt = ls_removehtml-prompt_text )-response.

        ls_prompt-prompt_text = |{ ls_prompt-prompt_text } \n<EMAIL>\n { removed_html }|.

        CLEAR: removed_html,
               ls_removehtml.


      ENDLOOP.


      response = lcl_llm_util->call_model( prompt = ls_prompt-prompt_text )-response.

    ENDIF.


  ENDMETHOD.


  METHOD build_agent_usage_report.


    SELECT object_key, COUNT( object_key ) AS usage
    FROM zllm_log
    WHERE app_id = @gv_app_id
    AND buss_function = @gv_business_function
    AND dat BETWEEN @start_date AND @end_date
    AND object_type = 'FUNCTION_CALL'
    GROUP BY object_key
    INTO TABLE @DATA(lt_log).

    DATA: ls_report TYPE zllm_agent_usage_report.


    LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<fs_log>).

      ls_report-agent = <fs_log>-object_key.
      ls_report-total_count = <fs_log>-usage.

      APPEND ls_report TO usage_report.

    ENDLOOP.

  ENDMETHOD.


  METHOD build_bot_usage_report.

    SELECT user_name, dat, time, request_token_count, response_token_count
    FROM zllm_log
    INTO TABLE @DATA(lt_log)
    WHERE app_id = @gv_app_id
    AND buss_function = @gv_business_function
    AND dat BETWEEN @start_date AND @end_date
    AND object_type = 'GENERATE_CONTENT'.

    DATA: total_token TYPE i,
          total_usage TYPE i,
          ls_report   TYPE zllm_total_usage_report.


* Provide the detail if we want to see it, otherwise we will display only total figures.
    LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<fs_log>).

      ls_report-date = <fs_log>-dat.
      ls_report-time = <fs_log>-time.
      ls_report-user = <fs_log>-user_name.

      total_token = total_token + ( <fs_log>-request_token_count + <fs_log>-response_token_count ).

      APPEND ls_report TO usage_report.

    ENDLOOP.

    ls_report-total_token = total_token.
    ls_report-total_usage = lines( lt_log ).

    APPEND ls_report TO usage_report.


  ENDMETHOD.

  METHOD build_interaction_report.
*** Method implementation to build a report for interactions with the bot

    IF me->gv_app_id IS NOT INITIAL AND
       me->gv_business_function IS NOT INITIAL.


*      DATA(lt_fd_ticket_range) = me->get_fd_tickets_searched( it_search_date_range = it_date_range ).

      SELECT zcds_llm_ia_summary_report~*
        FROM zcds_llm_ia_summary_report "( p_app_id = @me->gv_app_id,
                                        " p_business_function = @me->gv_business_function )
        WHERE   app_id IN ( @me->gv_app_id, 'LLM'  )
           AND  buss_function IN ( @me->gv_business_function, 'MCP' )
           and interaction_date IN @it_date_range
          "AND interacted_by NOT IN @me->gt_uname_role_excl   "To exclude certain unames like developers from the report
          AND ( interaction_type = 'CS Hub' OR
                interaction_type = 'Automation' )
          "AND interaction_key IN @lt_fd_ticket_range
         INTO TABLE @DATA(lt_report).

      SELECT uuid_key,
             zllm_res_compare~edit_category AS ai_edit_category,
             zllm_res_compare~comparison_comments AS ai_comparison_comments
        FROM zllm_res_compare
        FOR ALL ENTRIES IN @lt_report
        WHERE uuid_key = @lt_report-uuid_key
        INTO TABLE @DATA(lt_ai_response_comparison).

      LOOP AT lt_report ASSIGNING FIELD-SYMBOL(<fs_report>).

        APPEND INITIAL LINE TO interaction_report ASSIGNING FIELD-SYMBOL(<fs_i_report>).
        <fs_i_report>-app_id = <fs_report>-app_id.
        <fs_i_report>-business_function      = <fs_report>-buss_function.
        <fs_i_report>-uuid_key               = <fs_report>-uuid_key.
        <fs_i_report>-interaction_type       = <fs_report>-interaction_type.
        <fs_i_report>-interaction_key        = <fs_report>-interaction_key.
        <fs_i_report>-interacted_by          = <fs_report>-interacted_by.
        <fs_i_report>-agent_used             = <fs_report>-agent_used.
        <fs_i_report>-interaction_date       = <fs_report>-interaction_date.
        <fs_i_report>-interaction_time       = <fs_report>-interaction_time.
        <fs_i_report>-sentiment              = <fs_report>-sentiment.
        <fs_i_report>-auto_replied_indicator = <fs_report>-auto_replied.
*        <fs_i_report>-star_rating            = <fs_report>-star_rating.

        IF line_exists( lt_ai_response_comparison[ uuid_key = <fs_report>-uuid_key ] ).
          DATA(ls_ai_res_compare) = lt_ai_response_comparison[ uuid_key = <fs_report>-uuid_key ].
          <fs_i_report>-ai_edit_category       = ls_ai_res_compare-ai_edit_category.
          <fs_i_report>-ai_comparison_comments = ls_ai_res_compare-ai_comparison_comments.
        ENDIF.

        CLEAR : ls_ai_res_compare.

      ENDLOOP.

      SORT interaction_report BY interaction_date interaction_time ASCENDING.

    ENDIF.

*** To exclude users in all circumstances, irrespective of sentiment
    DATA(lt_users_to_exclude_range) = VALUE ztt_username_range( FOR ls_user IN me->gt_uname_role_excl
                                                                WHERE ( roleid <> 'CS_NO_FEEDBACK' )
                                                              ( sign = 'I'      option = 'EQ'
                                                                low  = ls_user-user_name ) ).

    IF lt_users_to_exclude_range IS NOT INITIAL.
      DELETE interaction_report WHERE interacted_by IN lt_users_to_exclude_range.
    ENDIF.

*** To exclude a few CS agents only when they click on NO FEEDBACK
    DATA(lt_no_feedback_excl_range) = VALUE ztt_username_range( FOR ls_user IN me->gt_uname_role_excl
                                                                WHERE ( roleid = 'CS_NO_FEEDBACK' )
                                                              ( sign = 'I'      option = 'EQ'
                                                                low  = ls_user-user_name ) ).

    IF lt_no_feedback_excl_range IS NOT INITIAL.
      DELETE interaction_report WHERE interacted_by IN lt_no_feedback_excl_range AND sentiment = 'NOFEEDBACK'.
    ENDIF.

  ENDMETHOD.

  METHOD get_fd_tickets_searched.
*** Method implementation to get all the Freshdesk tickets searched on CS Hub App, from log tables

    IF it_search_date_range IS NOT INITIAL.

*     Using this field, as this would be the only field that will get updated if ticket was researched,
*     and there's been no change to the ticket
      SELECT DISTINCT ( identifier ) AS identifier,
                      new_value,
                      log_date,
                      log_time
        FROM zcs_hub_fd_log
        INTO TABLE @DATA(lt_cs_hub_search)
        WHERE log_date IN @it_search_date_range
          AND field_name = 'SAP_UPDATED_DATE'
        ORDER BY log_date, log_time, identifier ASCENDING.

*     Delete duplicates as there could be multiple entries for the same ticket
      DELETE ADJACENT DUPLICATES FROM lt_cs_hub_search COMPARING identifier.

*     Create a range of FD tickets that were searched on the hub App
      LOOP AT lt_cs_hub_search ASSIGNING FIELD-SYMBOL(<fs_hub_search>).

        APPEND INITIAL LINE TO rt_fd_ticket_range ASSIGNING FIELD-SYMBOL(<fs_fd_ticket>).
        <fs_fd_ticket>-sign = 'I'.
        <fs_fd_ticket>-option = 'EQ'.
        SPLIT <fs_hub_search>-identifier AT ': ' INTO DATA(id) DATA(ticket).
        <fs_fd_ticket>-low = ticket.

        CLEAR : id, ticket.

      ENDLOOP.


    ENDIF.

  ENDMETHOD.

ENDCLASS.
