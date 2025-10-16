*&---------------------------------------------------------------------*
*& Report zrep_llm_trend_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrep_llm_trend_test.

PARAMETERS: p_date  TYPE datum DEFAULT sy-datum,
            p_lc    TYPE i,
            p_query AS CHECKBOX.


* As of now look at E-Mail in CS HUB
DATA(lcl_llm_util) = NEW zcl_llm_util( app_id            = 'CSHUB'
                                       business_function = 'EMAIL' ).


* RAG data from ZLLM_LOG table
SELECT FROM zllm_log
FIELDS uuid,
       request,
       new_value
WHERE app_id = 'CSHUB'
AND   buss_function = 'EMAIL'
AND   object_type = 'GENERATE_CONTENT'
AND   dat = @p_date
INTO TABLE @DATA(lt_llm_log).

* Loop at results
* { QUESTION 1: <customer question>,
*   QUESTION 2: <customer question>,
*   QUESTION 3: <customer question>}


DATA: lv_rag TYPE string.

LOOP AT lt_llm_log ASSIGNING FIELD-SYMBOL(<fs_log>).

  IF p_query = abap_true.
    lv_rag = |\{ Customer question: { sy-tabix }: { <fs_log>-request },\n |.
  ELSE.
    lv_rag = |\{ AI response: { sy-tabix }: { <fs_log>-new_value },\n |.
  ENDIF.

ENDLOOP.

    lv_rag = |{ lv_rag } \}|.


* This prompt would come from the gateway from the UI
DATA(lv_prompt) = '{ User query: Categorise all the data in the document into 10 best fit categorys. Put the output into a table using HTML }'.
DATA(lv_response)   = lcl_llm_util->call_simple_model( prompt = |{ lv_rag } \n\n { lv_prompt }| )-response.


cl_demo_output=>display( data = lv_response ).
