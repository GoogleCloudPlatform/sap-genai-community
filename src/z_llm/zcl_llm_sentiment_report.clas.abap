CLASS zcl_llm_sentiment_report DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: constructor IMPORTING app_id            TYPE zllm_app_id
                                   business_function TYPE zllm_buss_function_name
                                   from_date         TYPE datum
                                   to_date           TYPE datum
                                   from_time         TYPE uzeit
                                   to_time           TYPE uzeit
                                   query_type        TYPE char20,

* Generate response from Gemini
      generate_response IMPORTING prompt             TYPE string
                                  query_type         TYPE char20
                        RETURNING VALUE(rv_response) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

* Local table type declaration
    TYPES: BEGIN OF ty_llm_log,
             uuid     TYPE zllm_log-uuid,
             uuid_key TYPE zllm_log-uuid_key,
             time     TYPE zllm_log-time,
             value    TYPE string,
           END OF ty_llm_log,
           tt_llm_log TYPE TABLE OF ty_llm_log WITH EMPTY KEY.

* Global attribute
    DATA: gt_llm_log   TYPE tt_llm_log,
          gcl_llm_util TYPE REF TO zcl_llm_util.

    METHODS
* Fetch data from log table
      fetch_llm_log IMPORTING app_id            TYPE zllm_app_id
                              business_function TYPE zllm_buss_function_name
                              from_date         TYPE datum
                              to_date           TYPE datum
                              from_time         TYPE uzeit
                              to_time           TYPE uzeit
                              query_type        TYPE char20.
ENDCLASS.



CLASS zcl_llm_sentiment_report IMPLEMENTATION.

  METHOD constructor.

    me->fetch_llm_log( app_id            = app_id
                       business_function = business_function
                       from_date         = from_date
                       to_date           = to_date
                       from_time         = from_time
                       to_time           = to_time
                       query_type        = query_type ).

    me->gcl_llm_util = NEW zcl_llm_util( app_id            = app_id
                                         business_function = business_function ).

  ENDMETHOD.


  METHOD fetch_llm_log.
* Fetch the customer queries
    IF query_type = 'CUSTOMER_QUERY'.
      SELECT FROM zllm_log
           FIELDS uuid,
                  uuid_key,
                  time,
                  request AS value
           WHERE app_id IN ( @app_id, 'LLM'  )
           AND   buss_function IN ( @business_function, 'MCP' )
           AND   object_type IN ( 'Freshdesk', 'ACTIONED' )
           AND   dat BETWEEN @from_date AND @to_date
           AND   time BETWEEN @from_time AND @to_time
           INTO TABLE @gt_llm_log.

* Fetch the AI response
    ELSEIF query_type = 'AI_RESPONSE'.
      SELECT FROM zllm_log
           FIELDS uuid,
                  uuid_key,
                  time,
                  new_value AS value
            WHERE app_id IN ( @app_id, 'LLM'  )
            AND   buss_function IN ( @business_function, 'MCP' )
            AND   object_type IN ( 'Freshdesk', 'ACTIONED' )
            AND   dat BETWEEN @from_date AND @to_date
            AND   time BETWEEN @from_time AND @to_time
            INTO TABLE @gt_llm_log.
    ENDIF.

  ENDMETHOD.

  METHOD generate_response.

    DATA lv_rag TYPE string.

    IF gt_llm_log IS NOT INITIAL.

      SORT gt_llm_log DESCENDING BY uuid uuid_key time.

* Build the rag data
      LOOP AT gt_llm_log ASSIGNING FIELD-SYMBOL(<fs_log>).

        IF query_type = 'CUSTOMER_QUERY'.
          lv_rag = | { lv_rag } | & |\{ Customer question: { sy-tabix }: { <fs_log>-value },\n |.
        ELSEIF query_type = 'AI_RESPONSE'.
          lv_rag = | { lv_rag } | & |\{ AI response: { sy-tabix }: { <fs_log>-value },\n |.
        ENDIF.

      ENDLOOP.

      lv_rag = |{ lv_rag } \}|.
    ENDIF.

* Return the response back to the calling program
    rv_response = gcl_llm_util->call_simple_model( prompt = |{ lv_rag } \n\n { prompt }| )-response.

    REPLACE ALL OCCURRENCES OF '```html' IN rv_response WITH ''.
    REPLACE ALL OCCURRENCES OF '```' IN rv_response WITH ''.

  ENDMETHOD.

ENDCLASS.
