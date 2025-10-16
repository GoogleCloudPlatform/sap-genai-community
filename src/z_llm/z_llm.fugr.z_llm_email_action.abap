FUNCTION z_llm_email_action.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(PROMPT) TYPE  STRING
*"     REFERENCE(REQUEST) TYPE  STRING OPTIONAL
*"     REFERENCE(FD_TICKET) TYPE  ZCS_FRESHDESK_TICKET_ID
*"     REFERENCE(UUID_KEY) TYPE  GUID
*"  EXPORTING
*"     REFERENCE(ACTIONED) TYPE  XFELD
*"----------------------------------------------------------------------
* --CHECK IF RESPONSE IS EMPTY(if it just contains Codie's sign off signature)
  IF prompt IS INITIAL OR
     strlen( prompt ) <= 70.  "Codie's signature length is 64, hence 70

    DATA(lcl_no_response_log) = NEW zcl_llm_log( app_id            = 'CSHUB'
                                                 business_function = 'EMAIL' ).

*   Generate log
    lcl_no_response_log->write_general_log( uuid_key    = uuid_key
                                            request     = request
                                            object_type = 'AUTO_REPLY_FAILED'
                                            object_key  = CONV zllm_log_object_key( fd_ticket )
                                            new_value   = prompt
                                            old_value   = |Auto reply failed as the response is not fully formed.| ).

    FREE : lcl_no_response_log.

    RETURN.

  ENDIF.

**********************************************************************
* Get the customers email to see if we have contacted this customer recently
**********************************************************************

* Load the ticket into ZCS_TICKET_HDR & DTL, as it might not be available already since
* it's going through the automated flow
  TRY.
      DATA(lref_cs_hub_ticket_handler) = NEW zcl_cs_hub_ticket_handler( ).


      DATA(lt_ticket_id_range) = VALUE ztt_cs_hub_ticket_range( sign   = 'I'
                                                                option = 'EQ'
                                                              ( low    = fd_ticket ) ).

      lref_cs_hub_ticket_handler->update_tickets( it_ticket_id_range = lt_ticket_id_range ).

*   Not raising an exception here
    CATCH zcx_cs_hub.
  ENDTRY.

*** Logic to look for <NOAUTORES> tag in Freshdesk ticket
*   Change to proceed with auto reply even if the query was handled by CodieWeb
  DATA(lref_integration_handler) = NEW zcl_cs_hub_integration_handler( ).

  TRY.

      DATA(lt_ticket_content) = lref_integration_handler->fetch_freshdesk_ticket_content( iv_ticket_id      = fd_ticket ).

      IF lt_ticket_content IS NOT INITIAL.

*       Codie web may have already performed a response. Check for tag NOAUTORES.
        TRY.
            IF lt_ticket_content-tags[ 1 ] = 'NOAUTORES'.

*             Log this
              DATA(lcl_codieweb_handled_log) = NEW zcl_llm_log( app_id            = 'CSHUB'
                                                                business_function = 'EMAIL' ).

*             Generate log
              lcl_codieweb_handled_log->write_general_log( uuid_key    = uuid_key
                                                           request     = request
                                                           object_type = 'AUTO_REPLY'
                                                           object_key  = CONV zllm_log_object_key( fd_ticket )
                                                           new_value   = prompt
                                                           old_value   = |This query was handled by CodieWeb and ticket was logged anyway by customer.| ).

              FREE : lcl_codieweb_handled_log.

            ENDIF.

          CATCH cx_sy_itab_line_not_found.
*         If no line found, then Codie web didnt provide a response.
        ENDTRY.

      ENDIF.

    CATCH zcx_cs_hub_integration INTO DATA(lcx_cs_hub_integration).

  ENDTRY.

  SELECT SINGLE customer_email
  FROM zcs_ticket_hdr
  INTO @DATA(customer_email)
  WHERE freshdesk_ticket_id = @fd_ticket.

  IF customer_email IS NOT INITIAL.

* Get all the tickets with the customers email, excluding the current one.
    SELECT freshdesk_ticket_id
    FROM zcs_ticket_hdr
    INTO TABLE @DATA(lt_customer_tickets)
    WHERE customer_email = @customer_email.


  ENDIF.

**********************************************************************
* Check if a sent entry already exists
**********************************************************************
  DATA: lv_contacted    TYPE boolean,
        lv_contact_date TYPE datum.

* Allow a check window of 48 hours before
  lv_contact_date = sy-datum - 2.

  LOOP AT lt_customer_tickets ASSIGNING FIELD-SYMBOL(<fs_customer_tickets>).

    SELECT SINGLE object_key
    FROM zllm_log
    INTO @DATA(lv_sent)
    WHERE object_key = @<fs_customer_tickets>-freshdesk_ticket_id
    AND object_type = 'ACTIONED'
    AND dat BETWEEN @lv_contact_date AND @sy-datum.


    IF sy-subrc = 0.
      lv_contacted = abap_true.
      EXIT.
    ENDIF.

  ENDLOOP.


* If the customer has been contacted at any point in the last 48 hours, return.
  IF lv_contacted = abap_true.
    DATA(lcl_terminate_log) = NEW zcl_llm_log( app_id            = 'CSHUB'
                                               business_function = 'EMAIL' ).

*   Generate log
    lcl_terminate_log->write_general_log( uuid_key    = uuid_key
                                          request     = request
                                          object_type = 'AUTO_REPLY_TERMINATE'
                                          object_key  = CONV zllm_log_object_key( fd_ticket )
                                          new_value   = prompt
                                          old_value   = |Terminating auto reply flow as the customer { customer_email } was auto replied to in the last 2 days.| ).

    RETURN.

  ENDIF.

**********************************************************************
* Check the current status(Open/Not Open) of the ticket in Freshdesk
* If it is NOT OPEN, RETURN, do no proceed further
**********************************************************************

  TRY.

      DATA(lcl_fiori_llm_util) = NEW zcl_fiori_llm_util( app_id            = 'CSHUB'
                                                         business_function = 'EMAIL' ).

      DATA(not_open) = lcl_fiori_llm_util->get_freshdesk_ticket_status( ticket = fd_ticket ).

      IF not_open = abap_true.
        RETURN.
      ENDIF.

    CATCH zcx_llm INTO DATA(lcx_llm).
  ENDTRY.

**********************************************************************
* Get the agent
**********************************************************************
  SELECT SINGLE object_key
  FROM zllm_log
  INTO @DATA(lv_agent)
  WHERE uuid_key = @uuid_key
  AND object_type = 'FUNCTION_CALL'.


* If nothing maintained, return false.
  IF sy-subrc = 4.
* Automation is false.
    RETURN.
  ENDIF.


**********************************************************************
* Check if agent is configured for auto send
**********************************************************************
* Get the active version of the bot.
  DATA(ls_version) = NEW zcl_llm_version_control_util( )->get_active_app_version( app_id = 'CSHUB'
                                                                                  business_function = 'EMAIL' ).

  SELECT SINGLE automate
  FROM zllm_func_declar
  INTO @DATA(lv_automate)
  WHERE app_id = 'CSHUB'
  AND business_function = 'EMAIL'
  AND version = @ls_version-active_version
  AND function_name = @lv_agent.


  IF lv_automate = abap_false.
    RETURN.
  ENDIF.

* Only continue if nothing found
  IF lv_automate = abap_true.
**********************************************************************
* Send email
**********************************************************************
    TRY.


* Get the type, and sub types for the FD email call.
        SELECT SINGLE *
        FROM zllm_fd_agents
        INTO @DATA(ls_fd_config)
        WHERE agent = @lv_agent.

* If no config we cannot send the email.
        IF sy-subrc = 4.
          RETURN.
        ENDIF.

        TRY.

*           Instantiate log class
            DATA(lcl_log) = NEW zcl_llm_log( app_id            = 'CSHUB'
                                             business_function = 'EMAIL' ).

            IF lv_agent = 'Z_LLM_CS_HUB_DISPUTES'.    "To ensure to proceed with this logic only for Disputes agent

*             Logic to get sub type 2, if required. X means it has to be replaced.
              IF ls_fd_config-fd_sub_type2 = 'X'.

                DATA(lcl_cs_hub_llm_util) = NEW zcl_cs_hub_llm_util(  ).

                DATA(courier_name) = lcl_cs_hub_llm_util->llm_extract_courier_name( query = request ).

                CONDENSE courier_name NO-GAPS.

                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN courier_name WITH ''.

*               Check if the extracted courier name is in the list of acceptable list of values for sub type 2 in Freshdesk
                SELECT subtype_level_2
                  FROM zfd_ticket_types
                  INTO TABLE @DATA(lt_sub_type_2)
                 WHERE typ = @ls_fd_config-fd_type
                   AND subtype = @ls_fd_config-fd_sub_type
                   AND subtype_level_2 = @courier_name.

                IF sy-subrc = 0.

                  ls_fd_config-fd_sub_type2 = lt_sub_type_2[ 1 ]-subtype_level_2.

                ELSE.

                  CLEAR : ls_fd_config-fd_sub_type2.

                  ls_fd_config-fd_sub_type = 'General'.

*                 Generate log
                  lcl_log->write_general_log( uuid_key    = uuid_key
                                              request     = request
                                              object_type = 'AUTO_REPLY_SUBTYPE2'
                                              object_key  = CONV zllm_log_object_key( fd_ticket )
                                              new_value   = prompt
                                              old_value   = | Changed sub type to General, as sub type 2 could not be extracted. Extracted sub type 2 { courier_name } | ). "OLD VALUE will contain the Error text
*
*                  RETURN. "Terminate as the API call is going to fail
                ENDIF.

              ENDIF.

            ELSEIF lv_agent = 'Z_LLM_CS_HUB_RECEIPT'.

              DATA(lv_request_uppercase) = to_upper( request ).

              IF lv_request_uppercase CS 'VAT'.

                ls_fd_config-fd_sub_type = 'VAT'.

              ELSE.

                ls_fd_config-fd_sub_type = 'Request'.

              ENDIF.

            ELSEIF lv_agent = 'Z_LLM_CS_HUB_ORD_MISSING_RET'.

              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util(  ).

              DATA(return_reason) = lcl_cs_hub_llm_util->llm_classify_return_reason( query = request ).

              IF return_reason IS NOT INITIAL.

                CONDENSE return_reason NO-GAPS.

                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN return_reason WITH ''.

                ls_fd_config-fd_sub_type = return_reason.

                lv_request_uppercase = to_upper( request ).

                IF lv_request_uppercase CS 'HOME DELIVERY'.
                  ls_fd_config-fd_sub_type2 = 'Online order'.
                ELSE.
                  ls_fd_config-fd_sub_type2 = 'Instore purchase'.
                ENDIF.

                CLEAR : lv_request_uppercase.

              ENDIF.

            ELSEIF lv_agent = 'Z_LLM_CS_HUB_SPARE_PARTS'.

              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util(  ).

              DATA(response) = lcl_cs_hub_llm_util->llm_classify_spare_part( query = request ).

              IF response IS NOT INITIAL.

                CONDENSE response NO-GAPS.

                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN response WITH ''.
                DATA(lv_response) = to_upper( response ).


                ls_fd_config-fd_type = 'Product'.
                ls_fd_config-fd_sub_type = 'Spare parts'.

                IF lv_response <> 'NURSERY' AND
                   lv_response <> 'OUTDOOR'.

                  ls_fd_config-fd_sub_type2 = 'Other depts'.

                ELSE.
                  ls_fd_config-fd_sub_type2 = response.
                ENDIF.


                CLEAR : lv_request_uppercase.

              ENDIF.

            ELSEIF lv_agent = 'Z_LLM_CS_HUB_GIFT_CARD'.

              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util(  ).

              DATA(gc_query_sub_type) = lcl_cs_hub_llm_util->llm_classify_gift_card( query = request ).

              IF gc_query_sub_type IS NOT INITIAL.

*                CONDENSE gc_query_sub_type NO-GAPS.

                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN gc_query_sub_type WITH ''.

                ls_fd_config-fd_sub_type = gc_query_sub_type.

              ENDIF.


            ELSEIF lv_agent = 'Z_LLM_CS_HUB_PRE_ORDER'.

              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util(  ).

              DATA(preorder_query_sub_type) = lcl_cs_hub_llm_util->llm_classify_pre_order( query = request ).

              IF preorder_query_sub_type IS NOT INITIAL.

*                CONDENSE preorder_query_sub_type NO-GAPS.

                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN preorder_query_sub_type WITH ''.

                ls_fd_config-fd_sub_type = preorder_query_sub_type.

              ENDIF.

            ELSEIF lv_agent = 'Z_LLM_CS_HUB_RETURNS'.

              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util(  ).

              DATA(returns_sub_type) = lcl_cs_hub_llm_util->llm_classify_returns( query = request ).

              IF returns_sub_type IS NOT INITIAL.

                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN returns_sub_type WITH ''.

                ls_fd_config-fd_sub_type = returns_sub_type.

*               Pass the Sub type determined in the step above along with the custoemr query, to determine
*               sub type level 2
                DATA(query) = |{ request } \n| &
                              | SUB-TYPE : { returns_sub_type }|.

                DATA(returns_sub_type_2) = lcl_cs_hub_llm_util->llm_classify_returns_subtype2( query = query ).

                IF returns_sub_type_2 IS NOT INITIAL.

                  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN returns_sub_type_2 WITH ''.

                  ls_fd_config-fd_sub_type2 = returns_sub_type_2.

                ENDIF.

              ENDIF.

            ELSEIF lv_agent = 'Z_LLM_CS_HUB_CC'.

              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util(  ).

              DATA(cnc_sub_type) = lcl_cs_hub_llm_util->llm_classify_cnc( query = request ).

              IF cnc_sub_type IS NOT INITIAL.

                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN cnc_sub_type WITH ''.

                ls_fd_config-fd_sub_type = cnc_sub_type.

              ENDIF.

            ELSEIF lv_agent = 'Z_LLM_CS_HUB_PROMOTION'.

*             Determine/classify query sub type for Promotion queries
              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util(  ).

              DATA(promo_sub_type) = lcl_cs_hub_llm_util->llm_classify_promotion( query = request ).

              IF promo_sub_type IS NOT INITIAL.

                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN promo_sub_type WITH ''.
                ls_fd_config-fd_sub_type = promo_sub_type.

              ENDIF.


*            ELSEIF lv_agent = 'Z_LLM_CS_HUB_POKEMON_TCG'.
*
**             Determine/classify query sub type for Pokemon TCG queries
*              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util(  ).
*
*              DATA(pokemon_sub_type) = lcl_cs_hub_llm_util->llm_classify_pokemon( query = request ).
*
*              IF pokemon_sub_type IS NOT INITIAL.
*
*                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN pokemon_sub_type WITH ''.
*                ls_fd_config-fd_sub_type = pokemon_sub_type.
*
*              ENDIF.


            ELSEIF lv_agent = 'Z_LLM_CS_HUB_PAYMENT'.

*             Determine/classify query sub type for Payment queries
              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util(  ).

              DATA(payment_sub_type) = lcl_cs_hub_llm_util->llm_classify_payment( query = request ).

              IF payment_sub_type IS NOT INITIAL.

                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN payment_sub_type WITH ''.
                ls_fd_config-fd_sub_type = payment_sub_type.

              ENDIF.

*            ELSEIF lv_agent = 'Z_LLM_CS_HUB_PRODUCT_INFO'.
*
**             Determine/classify query sub type for Product information queries
*              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util(  ).
*
*              DATA(product_sub_type) = lcl_cs_hub_llm_util->llm_classify_product_info( query = request ).
*
*              IF product_sub_type IS NOT INITIAL.
*
*                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN product_sub_type WITH ''.
*                ls_fd_config-fd_sub_type = product_sub_type.
*
**              Product info queries can have sub type 2 as well, pass sub type to determine sub type 2
*                query = |{ request } \n| & | SUB-TYPE : { product_sub_type }|.
*
*                DATA(product_sub_type_2) = lcl_cs_hub_llm_util->llm_classify_product_subtype2( query   = request ).
*
*                IF product_sub_type_2 IS NOT INITIAL.
*
*                  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN product_sub_type_2 WITH ''.
*                  ls_fd_config-fd_sub_type2 = product_sub_type_2.
*
*                ENDIF.
*
*              ENDIF.


            ELSEIF lv_agent = 'Z_LLM_CS_HUB_MISSING_ITEM'.
*           Determine/classify query sub type for Missing item queries
*           FD types missing in FD ticket types table
              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util(  ).

              DATA(missing_item_sub_type) = lcl_cs_hub_llm_util->llm_classify_missitm_subtype( query = request ).

              IF missing_item_sub_type IS NOT INITIAL.

                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN missing_item_sub_type WITH ''.
                ls_fd_config-fd_sub_type = missing_item_sub_type.

              ENDIF.

            ELSEIF lv_agent = 'Z_LLM_CS_HUB_STOCK_QUERY'.
*              Determine/classify query sub type for stock Queries
              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util( ).

              DATA(stock_type) = lcl_cs_hub_llm_util->llm_classify_stockq_type( query = request ).
              IF stock_type IS NOT INITIAL.

                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN stock_type WITH ''.
                ls_fd_config-fd_type = stock_type.

                query = |{ request } \n| & | Type : { stock_type } |.

                DATA(stock_sub_type) = lcl_cs_hub_llm_util->llm_classify_stock_query( query = query ).
                IF stock_sub_type IS NOT INITIAL.

                  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN stock_sub_type WITH ''.
                  ls_fd_config-fd_sub_type = stock_sub_type.

                ENDIF.

              ENDIF.

            ELSEIF lv_agent = 'Z_LLM_CS_HUB_SOFTWARE_RETURNS'.
*              Determine/classify query sub type for software return Queries
              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util( ).

              DATA(swreturn_sub_type) = lcl_cs_hub_llm_util->llm_classify_swreturn( query = request ).
              IF swreturn_sub_type IS NOT INITIAL.

                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN swreturn_sub_type WITH ''.
                ls_fd_config-fd_sub_type = swreturn_sub_type.

*               Get Sub type 2 for Software return query
                query = |{ request } \n| & | SUB-TYPE : { swreturn_sub_type }|.

                DATA(swreturn_sub_type2) = lcl_cs_hub_llm_util->llm_classify_swret_subtype2( query = query ).
                IF swreturn_sub_type2 IS NOT INITIAL.

                  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN swreturn_sub_type2 WITH ''.
                  ls_fd_config-fd_sub_type2 = swreturn_sub_type2.

                ENDIF.

              ENDIF.

            ELSEIF lv_agent = 'Z_LLM_CS_HUB_POKEMON_TCG'.
*           Determine/classify query sub type for Pokemon queries
              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util(  ).

              DATA(pokemon_type) = lcl_cs_hub_llm_util->llm_classify_pokemontype( query = request ).
              IF pokemon_type IS NOT INITIAL.

                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN pokemon_type WITH ''.
                ls_fd_config-fd_type = pokemon_type.

                query = |{ request } \n| & | Type : { pokemon_type } |.

                DATA(pokemon_sub_type) = lcl_cs_hub_llm_util->llm_classify_pokemon( query = query ).

                IF pokemon_sub_type IS NOT INITIAL.

                  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN pokemon_sub_type WITH ''.
                  ls_fd_config-fd_sub_type = pokemon_sub_type.

*               Get Sub type 2 for Software return query
                  query = |{ request } \n| & | SUB-TYPE : { pokemon_sub_type }|.

                  DATA(pokemon_sub_type2) = lcl_cs_hub_llm_util->llm_classify_pokemon_subtype2( query = query ).
                  IF pokemon_sub_type2 IS NOT INITIAL.

                    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN pokemon_sub_type2 WITH ''.
                    ls_fd_config-fd_sub_type2 = pokemon_sub_type2.

                  ENDIF.

                ENDIF.

              ENDIF.
            ENDIF.

          CATCH zcx_llm.
          CATCH zcx_cs_hub.
        ENDTRY.


        DATA: lv_status TYPE zfd_object_key.
        lv_status = ls_fd_config-fd_status.
        CONDENSE lv_status NO-GAPS.



**********************************************************************
* Determine what FD status
**********************************************************************
* We need to keep the status as 'OPEN' if the reply text to the customer suggests some type of investigation will take place.

        IF lcl_cs_hub_llm_util IS INITIAL.
* Its possible this was not instantiated yet.
          TRY.
              lcl_cs_hub_llm_util = NEW zcl_cs_hub_llm_util(  ).
            CATCH zcx_llm INTO lcx_llm.
            CATCH zcx_cs_hub INTO DATA(lcx_cs_hub).
          ENDTRY.
        ENDIF.


        TRY.
            DATA(fd_status) = lcl_cs_hub_llm_util->llm_classify_fd_status( query = prompt ).
* Clean the response as best we can.
            REPLACE ALL OCCURRENCES OF '```html' IN fd_status WITH ''.
            REPLACE ALL OCCURRENCES OF '```' IN fd_status WITH ''.
            CONDENSE fd_status NO-GAPS.

            IF fd_status(4) = 'OPEN'.
* Change the status, otherwise leave it.
              lv_status = 2.
            ENDIF.

          CATCH zcx_llm INTO lcx_llm.
        ENDTRY.



**********************************************************************
* Determine the customers sentiment. If positive. Keep the ticket open
**********************************************************************
        TRY.
            DATA(fd_sentiment) = lcl_cs_hub_llm_util->llm_classify_sentiment( query = request ).
* Clean the response as best we can.
            REPLACE ALL OCCURRENCES OF '```html' IN fd_status WITH ''.
            REPLACE ALL OCCURRENCES OF '```' IN fd_status WITH ''.
            CONDENSE fd_status NO-GAPS.

            IF fd_sentiment(8) = 'POSITIVE'.
* Change the status, otherwise leave it.
              lv_status = 2.
            ENDIF.

          CATCH zcx_llm INTO lcx_llm.
        ENDTRY.

*       Ensure that there are no spaces
        CONDENSE lv_status NO-GAPS.


*** Log the types and status selected for the ticket, before calling Freshdesk API as
*** there'd be no visibility of this if/when the API fails

        IF lcl_log IS NOT BOUND.
          lcl_log = NEW zcl_llm_log( app_id            = 'CSHUB'
                                     business_function = 'EMAIL' ).
        ENDIF.

        DATA(freshdesk_type_selection) = | Type: { ls_fd_config-fd_type }\n | &
                                         | Sub-type: { ls_fd_config-fd_sub_type }\n| &
                                         | Sub-type level 2: { ls_fd_config-fd_sub_type2 }\n| &
                                         | Status code: { lv_status }|.

        lcl_log->write_general_log( uuid_key    = uuid_key
                                    request     = request
                                    object_type = 'FD_TYPES_SELECTED'
                                    object_key  = CONV zllm_log_object_key( fd_ticket )
                                    new_value   = freshdesk_type_selection ).

        CLEAR : freshdesk_type_selection.



**********************************************************************
* Fetch the email signature picture and text
**********************************************************************

* prompt can not be changed so define new variable and assign the prompt value into the new variable
        DATA: lv_prompt_new LIKE prompt.
        lv_prompt_new = prompt.

        DATA(lv_signature_image) = lcl_fiori_llm_util->get_signature( app_id = 'CSHUB' business_function = 'EMAIL' ).
        IF lv_signature_image IS NOT INITIAL.
          lv_prompt_new = |{ lv_prompt_new } { lv_signature_image }|.
        ENDIF.



**********************************************************************
* Send email
**********************************************************************
        DATA(lcl_fd_integration) = NEW zcl_cs_hub_integration_handler(  ).
        lcl_fd_integration->reply_update_freshdesk_ticket( iv_ticket_id  = fd_ticket
                                                           iv_status     = lv_status
                                                           iv_type       = ls_fd_config-fd_type
                                                           iv_subtype    = ls_fd_config-fd_sub_type
                                                           iv_subtype2   = ls_fd_config-fd_sub_type2
                                                           iv_reply_text = lv_prompt_new
                                                           iv_uuid_key   = uuid_key ).

        actioned = abap_true.
**********************************************************************
*Logging
**********************************************************************

        IF lcl_log IS NOT BOUND.
          lcl_log = NEW zcl_llm_log( app_id            = 'CSHUB'
                                     business_function = 'EMAIL' ).
        ENDIF.


*       There will be two log entries
*       1. With the actual request query and response
*       2. For the Freshdesk sub types determined(some use LLM for classification)/selected
        lcl_log->write_general_log( uuid_key    = uuid_key
                                    request     = request
                                    object_type = 'ACTIONED'
                                    object_key  = CONV zllm_log_object_key( fd_ticket )
                                    new_value   = lv_prompt_new ).



      CATCH zcx_cs_hub_integration INTO DATA(lcx_integration).
* If an error is thrown, log it

        IF lcl_log IS NOT BOUND.

          lcl_log = NEW zcl_llm_log( app_id            = 'CSHUB'
                                     business_function = 'EMAIL' ).
        ENDIF.

        lcl_log->write_general_log( uuid_key    = uuid_key
                                    request     = request
                                    object_type = 'AUTO_REPLY_FAILED'
                                    object_key  = CONV zllm_log_object_key( fd_ticket )
                                    new_value   = prompt
                                    old_value   = lcx_integration->get_text( ) ).           "OLD VALUE will contain the Error text

    ENDTRY.
  ENDIF.



ENDFUNCTION.
