CLASS zcl_z_llm_poc_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_z_llm_poc_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS: zllmset_create_entity REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_z_llm_poc_dpc_ext IMPLEMENTATION.

  METHOD zllmset_create_entity.


    DATA ls_post TYPE zllm_poc_odata.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).

    DATA(lcl_llm_util) = NEW zcl_llm_util( app_id            = 'BUYING'
                                           business_function = 'ASSISTANT' ).

    lcl_llm_util->add_function_call_properties( ).

*    Fetch version!!!!

    IF ls_post-request IS INITIAL.
*Get the POC articles for the demo based on username.
      SELECT article, prompt, user_full_name, initial_insight, updated_date, updated_time
      FROM zllm_article_poc
      INTO TABLE @DATA(lt_articles)
      WHERE username = @sy-uname.



      SELECT SINGLE prompt_text
      FROM zllm_prompts
      INTO @DATA(lv_greeting)
      WHERE prompt_key = 'GREETING_POC_DEMO'
      AND app_id = 'BUYING'
      AND business_function = 'ASSISTANT'.

      LOOP AT lt_articles ASSIGNING FIELD-SYMBOL(<fs_articles>).

        IF <fs_articles>-updated_date = sy-datum AND
           ( sy-uzeit - <fs_articles>-updated_time ) <= 86400 AND    "1 hour
           <fs_articles>-initial_insight IS NOT INITIAL.

          er_entity-response = <fs_articles>-initial_insight.

          WAIT UP TO 20 SECONDS.

          EXIT.

        ELSE.

          SELECT SINGLE prompt_text
          FROM zllm_prompts
          INTO @DATA(lv_prompt_text)
          WHERE prompt_key = @<fs_articles>-prompt
          AND app_id = 'BUYING'
          AND business_function = 'ASSISTANT'
          AND version = '20331'.

          DATA(partial_response) = lcl_llm_util->call_model( prompt = |{ lv_prompt_text }\n\nARTICLE(S): { <fs_articles>-article ALPHA = OUT }\n\n USERNAME:{ <fs_articles>-user_full_name }| )-response.

          er_entity-response = |{ partial_response }|.

***       ---FOR THE DEMO---
          IF partial_response IS NOT INITIAL.

            UPDATE zllm_article_poc SET initial_insight = partial_response
                                        updated_date = sy-datum
                                        updated_time = sy-uzeit
                                    WHERE username = sy-uname.

            IF sy-subrc = 0.
              COMMIT WORK AND WAIT.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDLOOP.

      er_entity-response = |<WELCOME_MESSAGE>{ er_entity-response }<WELCOME_MESSAGE>|.

    ELSE.

      er_entity-response = lcl_llm_util->call_model( prompt = ls_post-request )-response.

    ENDIF.

    REPLACE ALL OCCURRENCES OF '```html' IN er_entity-response WITH ''.
    REPLACE ALL OCCURRENCES OF '**' IN er_entity-response WITH ''.
    REPLACE ALL OCCURRENCES OF '###' IN er_entity-response WITH ''.

  ENDMETHOD.

ENDCLASS.
