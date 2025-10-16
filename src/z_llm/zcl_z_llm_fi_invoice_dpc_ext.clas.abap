CLASS zcl_z_llm_fi_invoice_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_z_llm_fi_invoice_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS: llmstagingset_get_entityset REDEFINITION.

  PRIVATE SECTION.

    DATA: lcl_fi_util TYPE REF TO zcl_llm_fi_util.

ENDCLASS.



CLASS zcl_z_llm_fi_invoice_dpc_ext IMPLEMENTATION.


  METHOD llmstagingset_get_entityset.

    lcl_fi_util = NEW zcl_llm_fi_util( app_id = 'FI'
                                       business_function = 'AP_INVOICE'  ).
    et_entityset = lcl_fi_util->fetch_ai_staging(  ).

  ENDMETHOD.

ENDCLASS.
