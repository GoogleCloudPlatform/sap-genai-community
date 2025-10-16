*&---------------------------------------------------------------------*
*& Report zllm_fi_ap_invoicce
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zllm_fi_ap_invoice.

PARAMETERS: p_gcs  TYPE string LOWER CASE DEFAULT 'gs://fi_ap_invoice/',
            p_file TYPE string LOWER CASE,
            p_case AS CHECKBOX,
            p_test AS CHECKBOX.

TRY.
    DATA(lcl_fi_llm_util) = NEW zcl_llm_fi_util( app_id            = 'FI'
                                                 business_function = 'AP_INVOICE' ).

**********************************************************************
* Initialise logging
**********************************************************************
    DATA(lcl_llm_log) = NEW zcl_llm_log( app_id            = 'FI'
                                         business_function = 'AP_INVOICE' ).

**********************************************************************
* Get the invoices from the GCS bucket.
**********************************************************************
    DATA(files) = lcl_fi_llm_util->get_gcs_files(  ).

* if no files, thats ok.
    IF files IS INITIAL.

      lcl_llm_log->write_general_log( object_type = 'NO_FILES'
                                      new_value   = 'No files found at configured GCS location' ).

      RETURN.
    ENDIF.

**********************************************************************
* Load global prompts
**********************************************************************
    DATA(lt_ap_prompt) = lcl_fi_llm_util->get_prompt( prompt_key = 'AP_INVOICE' ).

* For this bot there will only ever be 1 prompt.
    TRY.
        DATA(lv_ap_prompt) = lt_ap_prompt[ 1 ]-prompt.

      CATCH cx_sy_itab_line_not_found.
* If no prompt found stop processing.
        MESSAGE 'Could not find the AP_INVOICE prompt' TYPE 'E'.

    ENDTRY.

**********************************************************************
* Process files
**********************************************************************

*if an individual file has been provided, process only this file.
    IF p_file IS NOT INITIAL.
      PERFORM process_file.
    ELSE.
      PERFORM process_bucket.
    ENDIF.

  CATCH zcx_llm INTO DATA(lcx_llm).
ENDTRY.


FORM check_file USING file CHANGING file_exists.

  IF p_test IS NOT INITIAL.
    file_exists = abap_false.

  ELSE.
    SELECT SINGLE object_key
      FROM zllm_log
      INTO @DATA(lv_object_key)
      WHERE object_type = 'FILE'
      AND object_key = @file.

    IF sy-subrc = 0.
      file_exists = abap_true.
    ELSE.
      file_exists = abap_false.
    ENDIF.

  ENDIF.

ENDFORM.


FORM process_file.

  DATA: lv_file_exists TYPE xfeld.
  PERFORM check_file USING p_file CHANGING lv_file_exists.

  IF lv_file_exists = abap_true.
    RETURN.
  ENDIF.

  DATA: lv_log TYPE string.

* Some buckets are setup differently and are camel case.
  IF p_case = abap_false.
    p_file = to_lower( p_file ).
    p_gcs = to_lower( p_gcs ).
  ENDIF.

  lv_log = |Processing file { p_file }|.

  lcl_llm_log->write_general_log( object_type = 'FILE'
                                  object_key  = CONV zllm_log_object_key( p_file )
                                  new_value   = lv_log ).


  TRY.
      DATA(vendor) = lcl_fi_llm_util->determine_vendor( gcs_file = |{ p_gcs }{ p_file }| ).

      IF vendor IS NOT INITIAL.

        lv_ap_prompt = |{ lv_ap_prompt }{ vendor }|.


* Set the gcs uri for the invoice.
        lcl_fi_llm_util->set_gcs_file( gcs_file = |{ p_gcs }{ p_file }| ).

* Build function declarations.
        lcl_fi_llm_util->add_function_declarations(  ).

* Call the LLM model
        DATA(xml_response) = lcl_fi_llm_util->call_model( prompt     = lv_ap_prompt
                                                          system_key = p_file ).

* response should be an XML string. Convert this into the ABAP structure.
        DATA(invoice) = lcl_fi_llm_util->convert_string_xml( data = xml_response ).

* Perform business logic based on vendor.
        DATA(final_invoice) = lcl_fi_llm_util->apply_business_validation( invoice = invoice ).

* Save final invoice to staging
        lcl_fi_llm_util->save_ai_staging( data = final_invoice ).

      ENDIF.

    CATCH zcx_llm INTO DATA(lcx_llm).
      lv_log = |Failed to process file { p_file }. { lcx_llm->get_text(  ) }|.

      lcl_llm_log->write_general_log( object_type = 'FILE_FAIL'
                                      object_key  = CONV zllm_log_object_key( p_file )
                                      new_value   = lv_log ).

      IF p_test = abap_true.
        MESSAGE lv_log TYPE 'E'.
      ENDIF.

  ENDTRY.
ENDFORM.


FORM process_bucket.

  TRY.

      p_gcs = to_lower( p_gcs ).

      DATA: lv_log TYPE string.

* Build function declarations.
      lcl_fi_llm_util->add_function_declarations(  ).

* If no individual file was provided, then do this.
      LOOP AT files ASSIGNING FIELD-SYMBOL(<fs_files>).


        DATA: lv_file_exists TYPE xfeld.
        PERFORM check_file USING p_file CHANGING lv_file_exists.

        IF lv_file_exists = abap_true.

          lv_log = |File { <fs_files>-files } already processed|.

          lcl_llm_log->write_general_log( object_type = 'FILE'
                                          object_key  = CONV zllm_log_object_key( <fs_files>-files )
                                          new_value   = lv_log ).

          CONTINUE.
        ENDIF.


        lv_log = |Processing file { <fs_files>-files }|.

        lcl_llm_log->write_general_log( object_type = 'FILE'
                                        object_key  = CONV zllm_log_object_key( <fs_files>-files )
                                        new_value   = lv_log ).


        DATA(vendor) = lcl_fi_llm_util->determine_vendor( gcs_file = |{ p_gcs }{ <fs_files>-files }| ).

        IF vendor IS NOT INITIAL.

          lv_ap_prompt = |{ lv_ap_prompt }{ vendor }|.

* Set the gcs uri for the invoice.
          lcl_fi_llm_util->set_gcs_file( gcs_file = <fs_files>-files ).


          DATA(xml_response) = lcl_fi_llm_util->call_model( prompt     = lv_ap_prompt
                                                            system_key = <fs_files>-files ).

* response should be an XML string. Convert this into the ABAP structure.
          TRY.

              DATA(invoice) = lcl_fi_llm_util->convert_string_xml( data = xml_response ).

            CATCH zcx_llm INTO DATA(lcx_transform).

              lv_log = |Failed to process file { p_file }. { lcx_transform->get_text(  ) }|.

              lcl_llm_log->write_general_log( object_type = 'FILE_FAILED'
                                              object_key  = CONV zllm_log_object_key( <fs_files>-files )
                                              new_value   = lv_log ).

              CONTINUE.
          ENDTRY.

* Perform business logic based on vendor.
          DATA(final_invoice) = lcl_fi_llm_util->apply_business_validation( invoice = invoice ).

* Save final invoice to staging
          lcl_fi_llm_util->save_ai_staging( data = final_invoice ).

        ENDIF.

      ENDLOOP.

    CATCH zcx_llm INTO DATA(lcx_llm).

      lv_log = |Failed to process file { p_file }. { lcx_llm->get_text(  ) }|.

      lcl_llm_log->write_general_log( object_type = 'FILE_FAILED'
                                      object_key  = CONV zllm_log_object_key( <fs_files>-files )
                                      new_value   = lv_log ).
  ENDTRY.

ENDFORM.
