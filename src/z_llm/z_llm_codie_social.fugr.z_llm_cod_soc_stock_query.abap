FUNCTION z_llm_cod_soc_stock_query.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_FUNCTION_PARAMETERS) TYPE
*"        /GOOG/T_FUNCTION_PARAMETERS
*"  EXPORTING
*"     REFERENCE(EV_FUNCTION_RESPONSE) TYPE  STRING
*"  CHANGING
*"     REFERENCE(CV_PROMPT) TYPE  STRING
*"  RAISING
*"      /GOOG/CX_SDK
*"----------------------------------------------------------------------
  DATA : function_call_uuid TYPE guid16,
         app_id             TYPE zllm_app_id VALUE 'CSHUB',
         business_function  TYPE zllm_buss_function_name VALUE 'SOCIAL'.

  DATA : lv_memory_id       TYPE char50.

  lv_memory_id = |ZLLM_{ app_id }_{ business_function }|.

  IMPORT p1 = function_call_uuid FROM MEMORY ID lv_memory_id.

* Get the function module name. Due to the nature of ABAP call stack using system functions doesnt work.
  DATA: lt_callstack TYPE abap_callstack,
        ls_callstack TYPE abap_callstack_line,
        lv_fname     TYPE rs38l_fnam.

* Get the ABAP stack.
  CALL FUNCTION 'SYSTEM_CALLSTACK'
    EXPORTING
      max_level = 0
    IMPORTING
      callstack = lt_callstack.


  DATA(lcl_llm_log) = NEW zcl_llm_log( app_id            = app_id
                                       business_function = business_function ).


* NOTE: If more than one article/description has been extracted from the query, they will be passed
* passed back as comma separated values. Ensure to separate them out

* Get the article description
  IF line_exists( it_function_parameters[ parameter_name = 'ARTICLEDESC' ] ).
    DATA(articledesc) = to_upper( it_function_parameters[ parameter_name = 'ARTICLEDESC' ]-parameter_value ).
  ENDIF.

* Get the article ID
  IF line_exists( it_function_parameters[ parameter_name = 'ARTICLEID' ] ).
    DATA(articleid) = to_upper( it_function_parameters[ parameter_name = 'ARTICLEID' ]-parameter_value ).
  ENDIF.

* Get the store name
  IF line_exists( it_function_parameters[ parameter_name = 'STORENAME' ] ).
    DATA(storename) = to_upper( it_function_parameters[ parameter_name = 'STORENAME' ]-parameter_value ).
  ENDIF.

* Get the country of interest
  IF line_exists( it_function_parameters[ parameter_name = 'COUNTRY' ] ).
    DATA(country) = to_upper( it_function_parameters[ parameter_name = 'COUNTRY' ]-parameter_value ).
  ENDIF.


  TRY.
*     Instantiate classes
      DATA(lcl_llm_util) = NEW zcl_llm_util( app_id            = app_id
                                             business_function = business_function ).

      DATA(lcl_cs_llm_util) = NEW zcl_cs_hub_llm_util(  ).


***   ------MAIN PROMPT DOCUMENT------
*     Set the global context of this FM.
      DATA(ls_prompt) = lcl_llm_util->fetch_prompt( iv_prompt_key = 'INTROCONTEXT' ).
      cv_prompt = |{ cv_prompt } \n\n { ls_prompt-prompt_text }|.


      ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'SYSTEM' ).
      cv_prompt = |{ cv_prompt } \n\n { ls_prompt-prompt_text }|.

*     Set the global context of this FM.
      TRY.
          ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = CONV zllm_prompt_key( lt_callstack[ 1 ]-blockname ) ).
          cv_prompt = |\n{ cv_prompt }','{ ls_prompt-prompt_text } |.

        CATCH cx_sy_itab_line_not_found.
*       If nothing found, don't dump.
          cv_prompt = |Sorry, my configuration is incomplete|.

      ENDTRY.

*     Set the global context of this FM.
      ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'SIGNOFFCONTEXT' ).
      cv_prompt = |\n{ cv_prompt }','{ ls_prompt-prompt_text } |.

***   ------END OF MAIN PROMPT DOCUMENT------


***   ------RAG DATA------

*     1. ARTICLE ID
*     Check if article ID has been extracted as a parameter from the customer query. If yes,
*     proceed to find find the expected date. Else, attempt to figure out the article ID
*     from the description passed
      IF articleid IS INITIAL.  "No article ID extracted by LLM

        IF articledesc IS NOT INITIAL.

          DATA(response) = lcl_cs_llm_util->llm_article_from_desc( query = articledesc ).

          REPLACE ALL OCCURRENCES OF '```xml' IN response WITH ''.
          REPLACE ALL OCCURRENCES OF '```' IN response WITH ''.

*         Passing XML article extraction response as RAG data
          ev_function_response = |Current date: { sy-datum } \n\n| &
                                 |{ response } \n\n|.

          TRY.

              DATA : lt_articles TYPE TABLE OF zst_llm_cs_hub_art_extraction.
*            DATA : lo_xml_document TYPE REF TO cl_xml_document.
*
*            CREATE OBJECT lo_xml_document.
*
*            DATA(lv_return_code) = lo_xml_document->parse_string( response ).
*            IF lv_return_code = 4.
*              RETURN.
*            ENDIF.

              CALL TRANSFORMATION zllm_cs_hub_art_xml_transform
              SOURCE XML response
              RESULT article_extractions = lt_articles.


            CATCH cx_st_error INTO DATA(lcx_st_error).

          ENDTRY.

        ENDIF.

      ELSE.

        TYPES : tt_art_extraction TYPE STANDARD TABLE OF zst_llm_cs_hub_art_extraction WITH EMPTY KEY.

        SPLIT articleid AT ',' INTO TABLE DATA(lt_article_ids).
        SPLIT articledesc AT ',' INTO TABLE DATA(lt_article_desc).

        lt_articles = VALUE tt_art_extraction( FOR article IN lt_article_ids
                                               FOR desc IN lt_article_desc
                                             ( article_id = article
                                               article_description = desc ) ).



      ENDIF.


      IF lt_articles IS NOT INITIAL.

        DATA(lt_article_range) = VALUE ztt_article_range( FOR ls_response IN lt_articles
                                                          ( sign   = 'I'
                                                            option = 'EQ'
                                                            low    = zcl_alpha_in=>alpha_in( data = CONV string( ls_response-article_id ) ) ) ).
        IF country IS NOT INITIAL.

          DATA(lt_region_range) = VALUE ztt_region_range( sign = 'I'       option = 'EQ'
                                                ( low  = country ) ).

        ELSE.

          lt_region_range = VALUE ztt_region_range( sign = 'I'       option = 'EQ'
                                                        ( low  = 'UK' )
                                                        ( low = 'NI' )
                                                        ( low = 'ROI' ) ).

        ENDIF.

*       2. STORE IDENTIFICATION
*       This isn't of much use for now, but will be helpful when expanding this agent
        IF storename IS NOT INITIAL.

          DATA(store_identification_response) = lcl_cs_llm_util->llm_active_store_list( it_region_range = lt_region_range
                                                                                        query           = storename ).

          SPLIT store_identification_response AT ',' INTO DATA(store_id) DATA(store_name) DATA(store_country).

*          ev_function_response = |{ ev_function_response } - Store ID: { store_id } \n| &
*                                 |- Store name: { store_name } \n\n|.

        ENDIF.

***     Fetching and bringing together all the needed information
        IF lt_article_range IS NOT INITIAL.

          DATA(lcl_llm_cs_hub_stock_query) =  NEW zcl_llm_cs_hub_stock_query(  ).

          DATA(lt_country_range) = COND ztt_country_range( WHEN lt_region_range IS NOT INITIAL
                                                           THEN CONV ztt_country_range( lt_region_range )
                                                           "WHEN store_country IS NOT INITIAL
                                                           "THEN VALUE ztt_country_range( sign = 'I'
                                                                                         "option = 'EQ'
                                                                                       "( low = CONV zcountry( store_country ) ) )
                                                           ).

*         Get the article descriptions(web name and SAP description) for the article(s) extracted
          DATA(lt_article_descriptions) = lcl_llm_cs_hub_stock_query->get_article_descriptions( it_article_range = lt_article_range
                                                                                                it_country_range = lt_country_range ).

          DATA(lv_article_descriptions) = REDUCE string( INIT desc TYPE string sep = ''
                                                         FOR ls_art_desc IN lt_article_descriptions
                                                         NEXT desc = desc &&
                                                            |- Article: { ls_art_desc-article ALPHA = OUT } \n| &
                                                            |- Webname: { ls_art_desc-webname } \n| &
                                                            |- SAP Description:{ ls_art_desc-sap_description }\n|
                                                         sep = ',' ).

          ev_function_response =  |{ ev_function_response } \n <ARTICLE DESCRIPTIONS> \n { lv_article_descriptions }\n\n|.


*         ---CURRENT STOCK POSITION---
          IF store_id IS NOT INITIAL.

            DATA(lt_current_stock) = lcl_llm_cs_hub_stock_query->check_stock_availability( it_article_range = lt_article_range
                                                                                           it_store_range   = VALUE zsites_range_tt( sign = 'I'    option = 'EQ'
                                                                                                                                   ( low   = store_id ) ) ).
            DATA(lv_current_stock) = REDUCE string( INIT stock TYPE string sep = ''
                                                     FOR ls_stock IN lt_current_stock
                                                    NEXT stock = stock &&
                                                              |- Article: { ls_stock-matnr ALPHA = OUT }| &
                                                              |- Store ID: { ls_stock-werks } \n| &
                                                              |- Store name: { store_name } \n| &
                                                              |- Available stock:{ ls_stock-labst }\n|
                                                           sep = ',' ).

            ev_function_response =  |{ ev_function_response } \n <CURRENT STOCK POSITION> \n { lv_current_stock }\n\n|.


          ENDIF.


*         ---EXPECTED DATE TEXT from ZART_CTRY_FIELDS---
          DATA(lt_expected_date) = lcl_llm_cs_hub_stock_query->get_expected_date_text( it_article_range = lt_article_range
                                                                                       it_country_range = lt_country_range ).

          DATA(lv_expected_date_text) =  REDUCE string( INIT output TYPE string sep = ''
                                                FOR ls_expected_date IN lt_expected_date
                                                WHERE ( expected_date_text IS NOT INITIAL )  "To ensure empty text is not passed back
                                                NEXT output = output &&
                                                |- Article: { ls_expected_date-article ALPHA = OUT } \n| &
*                                                |- WEBNAME: { ls_expected_date-webname } \n| &
*                                                |- SAPDESCRIPTION:{ ls_expected_date-sap_description }\n| &
                                                |- Expected stock date: { ls_expected_date-expected_date_text }\n| &
                                                |- Country: { ls_expected_date-country }\n|
                                                sep = '\n' ).

          IF lv_expected_date_text IS NOT INITIAL.

            ev_function_response = |{ ev_function_response } <EXPECTED STOCK ARRIVAL INFORMATION>  { lv_expected_date_text }|.

          ENDIF.

*         Upon unavailability, check PO schedule lines
*         ---PO SCHEDULE LINES---
          IF lv_expected_date_text IS INITIAL.

            DATA(lt_po_delivery_dates) = lcl_llm_cs_hub_stock_query->get_po_schedule_lines( it_article_range = lt_article_range
                                                                                            it_country_range = lt_country_range ).

*           To avoid sending irrelevant information
            IF store_id IS NOT INITIAL.

              DELETE lt_po_delivery_dates WHERE store <> store_id.

            ENDIF.


            DATA(lv_po_sch_lines) = REDUCE string( INIT output TYPE string sep = ''
                                                   FOR ls_po_del_date IN lt_po_delivery_dates
                                                   NEXT output = output &&
                                                   |- Article: { ls_po_del_date-article ALPHA = OUT } \n| &
*                                                   |- WEBNAME: { ls_po_del_date-webname } \n| &
*                                                   |- SAPDESCRIPTION:{ ls_po_del_date-sap_description }\n| &
                                                   |- Expected stock date: { ls_po_del_date-expected_date_text }\n| &
                                                   |- Store ID: { ls_po_del_date-store }\n| &
                                                   |- Store name: { ls_po_del_date-store_name }\n| &
                                                   |- Country: { ls_po_del_date-country },\n|
                                                   sep = '\n' ).

            ev_function_response = |{ ev_function_response } <EXPECTED STOCK ARRIVAL INFORMATION FROM P.O.>  { lv_po_sch_lines }|.


          ENDIF.

        ENDIF.

      ELSE.

        ev_function_response = 'Apologies, but I was not able to understand which article you are referring to.'.

      ENDIF.

      "CONDENSE ev_function_response NO-GAPS.

*     The below IF block fetches the relevant prompt when the expected date of stock in unavailable
*     for the article or articles in the customer query
      IF lv_expected_date_text IS INITIAL AND
         lv_po_sch_lines IS INITIAL.


        ev_function_response =  ev_function_response &&
                                REDUCE string( INIT output TYPE string sep = ''
                                                FOR ls_article IN lt_articles
                                               NEXT output = output && |Expected date not known for:| &&
                                               |{ ls_article-article_id ALPHA = OUT } - { ls_article-article_description }\n|
                                                    sep = ',' ).

***     Log the query where stock arrival date is unknown
***     Note: If expected date text maintained is 'No Longer Available', it won't be captured in this log
        lcl_llm_log->save_for_action_log( uuid_key = function_call_uuid
                                          agent    = CONV zllm_function_name( lt_callstack[ 1 ]-blockname )
                                          rag_data = ev_function_response ).
      ENDIF.


    CATCH /goog/cx_sdk.
    CATCH zcx_llm.
    CATCH zcx_cs_hub.
    CATCH cx_sy_itab_line_not_found.
    CATCH cx_root. "if all else fails ....

***   ------PROMPT DOCUMENT - GLOBAL ERROR------
*     Update the global context to error context.
      TRY.
          ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'ERRORCONTEXT' ).
        CATCH zcx_llm.
      ENDTRY.

      cv_prompt = ls_prompt-prompt_text.

      CLEAR: ls_prompt.
***   ------END OF PROMPT DOCUMENT - GLOBAL ERROR------

  ENDTRY.

*** LOGGING
  TRY.
      lcl_llm_log->create_log_from_llm_fm( it_function_parameters = it_function_parameters
                                           iv_function_name       = CONV zllm_function_name( lt_callstack[ 1 ]-blockname )
                                           iv_uuid                = function_call_uuid
                                           iv_prompt              = cv_prompt
                                           iv_function_response   = ev_function_response ).


    CATCH cx_sy_itab_line_not_found.
* If nothing found, don't dump.
  ENDTRY.

ENDFUNCTION.
