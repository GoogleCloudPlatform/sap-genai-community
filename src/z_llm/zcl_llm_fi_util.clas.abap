CLASS zcl_llm_fi_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      constructor IMPORTING app_id            TYPE zllm_app_id
                            business_function TYPE zllm_buss_function_name
                  RAISING   zcx_llm,

**********************************************************************
* Method to get the staging are for FI invoices that have been processed
* by the LLM model.
**********************************************************************
      fetch_ai_staging RETURNING VALUE(staging) TYPE zllm_fi_staging_odata_tt,

**********************************************************************
* Method to determine invoice vendor
**********************************************************************
      determine_vendor IMPORTING gcs_file      TYPE string
                       RETURNING VALUE(vendor) TYPE string
                       RAISING   zcx_llm,

**********************************************************************
* Call the LLM model
**********************************************************************
      call_model IMPORTING prompt          TYPE string
                           system_key      TYPE string OPTIONAL
                 RETURNING VALUE(response) TYPE string
                 RAISING   zcx_llm,

**********************************************************************
* Get the files in the GCS bucket
**********************************************************************
      get_gcs_files RETURNING VALUE(files) TYPE zllm_gcs_files_tt
                    RAISING   zcx_llm,

**********************************************************************
* Get the prompt
**********************************************************************
      get_prompt  IMPORTING prompt_key    TYPE zllm_prompt_key OPTIONAL
                            version       TYPE zllm_version OPTIONAL
                  RETURNING VALUE(prompt) TYPE zllm_prompt_odata_tt
                  RAISING   zcx_llm,

**********************************************************************
* Save the FI AI stating data
**********************************************************************
      save_ai_staging IMPORTING data          TYPE zllm_fi_tt
                      RETURNING VALUE(status) TYPE boolean,

**********************************************************************
* Convert XML string to ABAP structure.
**********************************************************************
      convert_string_xml IMPORTING VALUE(data)    TYPE string
                         RETURNING VALUE(invoice) TYPE zllm_fi_staging_tt
                         RAISING   zcx_llm,

**********************************************************************
* Apply invoice business logic / validation
**********************************************************************
      apply_business_validation IMPORTING invoice         TYPE zllm_fi_staging_tt
                                RETURNING VALUE(response) TYPE zllm_fi_tt,

**********************************************************************
* Add function declarations.
**********************************************************************
      add_function_declarations RAISING zcx_llm,

**********************************************************************
* Set the GCS file URI to process invoice.
**********************************************************************
      set_gcs_file IMPORTING gcs_file TYPE string
                             simple   TYPE boolean DEFAULT abap_false
                   RAISING   zcx_llm,

**********************************************************************
* Set check table values, indicating a match or no match.
**********************************************************************
      set_check_table IMPORTING field_name  TYPE string
                                check_value TYPE boolean.



  PROTECTED SECTION.
  PRIVATE SECTION.

**********************************************************************
* Method to get the configuration of the bot.
**********************************************************************
    METHODS: get_bot_config.


    DATA: gs_bot_config        TYPE zllm_bots,
          gv_app_id            TYPE zllm_app_id,
          gv_business_function TYPE zllm_buss_function_name,
          gcl_llm_util         TYPE REF TO zcl_llm_util.


ENDCLASS.



CLASS zcl_llm_fi_util IMPLEMENTATION.

  METHOD constructor.

    gv_app_id = app_id.
    gv_business_function = business_function.

    TRY.
        gcl_llm_util = NEW zcl_llm_util( app_id            = gv_app_id
                                         business_function = gv_business_function ).

* Load the bot config.
        me->get_bot_config(  ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.

  ENDMETHOD.

  METHOD set_check_table.



  ENDMETHOD.


  METHOD set_gcs_file.

    TRY.
        gcl_llm_util->set_gcs_file( file_name = gcs_file
                                    mime      = 'application/pdf'
                                    simple    = simple ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.

  ENDMETHOD.


  METHOD add_function_declarations.
    TRY.
        gcl_llm_util->add_function_call_properties(  ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.
  ENDMETHOD.


  METHOD apply_business_validation.
* This method needs to be broken out to individual vendor logic method calls. For now, do nothing but basics.

    DATA(lcl_alpha_in) = NEW zcl_alpha_in(  ).
    DATA: ls_ai_staging TYPE zllm_fi_staging.

    LOOP AT invoice ASSIGNING FIELD-SYMBOL(<fs_invoice>).

      ls_ai_staging-mandt = sy-mandt.
      ls_ai_staging-currency = <fs_invoice>-currency.
      ls_ai_staging-customer_vat_number = <fs_invoice>-customer_vat_number.
      ls_ai_staging-discount = <fs_invoice>-discount.
      ls_ai_staging-doc_date = <fs_invoice>-doc_date.
      ls_ai_staging-external_code = <fs_invoice>-external_code.
      ls_ai_staging-external_description = <fs_invoice>-external_description.
      ls_ai_staging-gross_unit_price = <fs_invoice>-gross_unit_price.
      ls_ai_staging-gross_weight = <fs_invoice>-gross_weight.
      ls_ai_staging-internal_code = zcl_alpha_in=>alpha_in( data = <fs_invoice>-internal_code ).
      ls_ai_staging-internal_description = <fs_invoice>-internal_description.
      ls_ai_staging-invoice_number = <fs_invoice>-invoice_number.
      ls_ai_staging-item_total_value = <fs_invoice>-item_total_value.
      ls_ai_staging-net_weight = <fs_invoice>-net_weight.
      ls_ai_staging-pack_size = <fs_invoice>-pack_size.
      ls_ai_staging-payment_terms = <fs_invoice>-payment_terms.
      ls_ai_staging-po_number = <fs_invoice>-po_number.
      ls_ai_staging-supplier_vat_number = <fs_invoice>-supplier_vat_number.
      ls_ai_staging-total_car_gross_weight = <fs_invoice>-total_car_gross_weight.
      ls_ai_staging-total_car_qty = <fs_invoice>-total_car_qty.
      ls_ai_staging-total_vat = <fs_invoice>-total_vat.
      ls_ai_staging-units_shipped = <fs_invoice>-units_shipped.
      ls_ai_staging-unit_price = <fs_invoice>-unit_price.
      ls_ai_staging-uom = <fs_invoice>-uom.
      ls_ai_staging-vendor_name = <fs_invoice>-vendor_name.

      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_16 = ls_ai_staging-uuid.


      SELECT SINGLE maktx
      FROM makt
      INTO ls_ai_staging-internal_description
      WHERE matnr = ls_ai_staging-internal_code
      AND spras = sy-langu.

      APPEND ls_ai_staging TO response.

      CLEAR: ls_ai_staging.

    ENDLOOP.


  ENDMETHOD.



  METHOD convert_string_xml.


* Clean out any characters the LLM may inject
    REPLACE ALL OCCURRENCES OF '```xml' IN data WITH ''.
    REPLACE ALL OCCURRENCES OF 'xml' IN data WITH ''.
    REPLACE ALL OCCURRENCES OF '```' IN data WITH ''.
    REPLACE ALL OCCURRENCES OF '#' IN data WITH ''.
    REPLACE ALL OCCURRENCES OF 'html' IN data WITH ''.

    DATA: lt_invoice    TYPE TABLE OF zllm_fi_staging_st,
          ls_ai_staging TYPE zllm_fi_staging,
          lt_ai_staging TYPE TABLE OF zllm_fi_staging.

    "Transform the results into an ABAP structure.
    TRY.
        CALL TRANSFORMATION zllm_fi_ap_invoice_transform
        SOURCE XML data
        RESULT invoice_line = invoice.

      CATCH cx_xslt_format_error INTO DATA(lcx_error).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>xlst_format_error
            msg1   = lcx_error->get_text( ).


    ENDTRY.

  ENDMETHOD.


  METHOD save_ai_staging.

    MODIFY zllm_fi_staging FROM TABLE data.
    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD get_prompt.

    DATA(lcl_prompt_util) = NEW zcl_llm_prompt_util( app_id            = me->gv_app_id
                                                     business_function = me->gv_business_function ).

    prompt = lcl_prompt_util->get_prompt( prompt_key = prompt_key
                                          version    = version ).


  ENDMETHOD.

  METHOD get_gcs_files.

    DATA(lcl_gcs_util) = NEW zcl_gcs_util(  ).


    DATA: bucket TYPE string,
          search TYPE string.

    bucket = me->gs_bot_config-gcs_kb_uri.
    search = '*'.

    TRY.
        files = lcl_gcs_util->search_bucket( bucket = bucket
                                             search = search ).

      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.
    ENDTRY.



  ENDMETHOD.

  METHOD call_model.

    TRY.

        DATA(model_response) = gcl_llm_util->call_model( prompt     = prompt
                                                         system_key = system_key ).

        response = model_response-response.


      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.


  ENDMETHOD.


  METHOD determine_vendor.

    TRY.
        gcl_llm_util->set_gcs_file( file_name = gcs_file
                                    mime      = 'application/pdf'
                                    simple    = abap_true ).


        DATA(ls_prompt) = gcl_llm_util->fetch_prompt( iv_prompt_key = 'FI_VENDOR_DET'
                                                      prompt_type   = 'HELPER' ).

        DATA(response) = gcl_llm_util->call_simple_model( prompt = ls_prompt-prompt_text ).

        vendor = response-response.

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.

  ENDMETHOD.


  METHOD get_bot_config.

    SELECT SINGLE *
    FROM zllm_bots
    INTO @me->gs_bot_config
    WHERE app_id = @gv_app_id
    AND business_function = @gv_business_function.

  ENDMETHOD.


  METHOD fetch_ai_staging.

    SELECT vendor_name, doc_date, invoice_number, customer_vat_number, supplier_vat_number, payment_terms, po_number, external_code,
           external_description, internal_code, internal_description, net_weight, gross_weight, total_car_qty, total_car_gross_weight,
           units_shipped, uom, unit_price, item_total_value, total_vat, currency, discount, gross_unit_price, pack_size
    FROM zllm_fi_staging
    INTO CORRESPONDING FIELDS OF TABLE @staging.

  ENDMETHOD.


ENDCLASS.
