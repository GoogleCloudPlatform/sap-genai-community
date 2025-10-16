*&---------------------------------------------------------------------*
*& Report zllm_finance_poc
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zllm_finance_poc.


* Generate my table as XML.
DATA lv_string TYPE xstring.
DATA lv_structure TYPE zllm_fi_staging.

"call transformation id
"      source root = lv_structure
"      result xml data(lv_output).

"call function 'DISPLAY_XML_STRING'
"   exporting
"       xml_string      = lv_output
"     exceptions
"       no_xml_document = 1
"       others          = 2.

DATA(gv_gcs_uri) = |gs://adam_test2/385334.pdf|.

DATA(lcl_llm_util) = NEW zcl_llm_util( app_id            = 'CSHUB'
                                       business_function = 'EMAIL' ).


lcl_llm_util->set_gcs_file( file_name = gv_gcs_uri
                            mime      = 'application/pdf'
                            simple    = abap_true ).


DATA(prompt) = |<SYSTEM> \n You work within finance accounts payable. You must read all invoices and prepare them to be entered into an SAP S4/HANA system.| &
               |<INSTRUCTION> \n Read the document and outputting as only fully formed XML (without any downmarking or hidden characters such as #),  generate: \n | &
               |Do not create a root XML tag| &
               |Create a root tag <INVOICE>| &
               |Create another tag for each line <ZLLM_FI_STAGING_ST>| &
               |extract all lines,| &
               |generate an XML tag <UUID> with value 123456,| &
               |get the vendor name (as XML tag <VENDOR_NAME>,| &
               |get the invoice date (as XML tag <DOC_DATE>,| &
               |get the customer purchase order number, this will start with either 44 or 45 (as XML tag <PO_NUMBER>,| &
               |get the invoice number (as XML tag <INVOICE_NUMBER>,| &
               |get the customer VAT number (as XML tag <CUSTOMER_VAT_NUMBER>,| &
               |get the VAT Reg No. (as XML tag <SUPPLIER_VAT_NUMBER>,| &
               |get the payment terms (as XML tag <PAYMENT_TERMS>,| &
               |item code (as XML tag <EXTERNAL_CODE>),| &
               |item description (as XML tag <EXTERNAL_DESCRIPTION>),| &
               |customer item code (as XML tag <INTERNAL_CODE>),| &
               |create XML tag <INTERNAL_DESCRIPTION> with value -)| &
               |get the net weight (as XLM tag <NET_WEIGHT>,| &
               |get the gross weight (as XLM tag <GROSS_WEIGHT>,| &
               |get the total carton qty (as XLM tag <TOTAL_CAR_QTY>,| &
               |get the total gross carton weight (as XLM tag <TOTAL_CAR_GROSS_WEIGHT>,| &
               |get the quantity of units_shipped (as XLM tag <UNITS_SHIPPED>,| &
               |get the UOM (as XLM tag <UOM>,| &
               |get the unit price (as XLM tag <UNIT_PRICE>,| &
               |get the total value (as XLM tag <ITEM_TOTAL_VALUE>,| &
               |get the VAT paid (as XLM tag <TOTAL_VAT>,| &
               |get the currency of the ivoice (as XLM tag <CURRENCY>,| &
               |say discount applied is - (as XLM tag <DISCOUNT>,| &
               |calculate gross unit price by taking the unit price and deducting VAT percentage   (as XLM tag <GROSS_UNIT_PRICE>,| &
               |say pack size is - (as XLM tag <PACK_SIZE>|.


DATA(response) = lcl_llm_util->call_simple_model( prompt = prompt )-response.

* Clean out any characters the LLM may inject
REPLACE ALL OCCURRENCES OF '```xml' IN response WITH ''.
REPLACE ALL OCCURRENCES OF '```' IN response WITH ''.
REPLACE ALL OCCURRENCES OF '#' IN response WITH ''.
REPLACE ALL OCCURRENCES OF 'html' IN response WITH ''.

DATA: lt_invoice    TYPE TABLE OF zllm_fi_staging_st,
      ls_ai_staging TYPE zllm_fi_staging,
      lt_ai_staging TYPE TABLE OF zllm_fi_staging.

"Transform the results into an ABAP structure.
TRY.
    CALL TRANSFORMATION zllm_fi_moose_invoice
    SOURCE XML response
    RESULT invoice_line = lt_invoice.

    IF lt_invoice IS INITIAL.
      WRITE 'failed to convert xml'.
    ELSE.

data(lcl_alpha_in) = new zcl_alpha_in(  ).
      LOOP AT lt_invoice ASSIGNING FIELD-SYMBOL(<fs_invoice>).

        ls_ai_staging-mandt = sy-mandt.
        ls_ai_staging-gcs_file = gv_gcs_uri.
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

        APPEND ls_ai_staging TO lt_ai_staging.

        CLEAR: ls_ai_staging.

      ENDLOOP.


      MODIFY zllm_fi_staging FROM TABLE lt_ai_staging.
      COMMIT WORK AND WAIT.

    ENDIF.

  CATCH cx_xslt_format_error INTO DATA(lcx_error).
    WRITE lcx_error->get_text(  ).
ENDTRY.
