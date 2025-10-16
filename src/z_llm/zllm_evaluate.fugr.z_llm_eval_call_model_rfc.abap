FUNCTION z_llm_eval_call_model_rfc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_PROMPT) TYPE  STRING
*"     VALUE(I_REQUEST_GUID) TYPE  SYSUUID_C32
*"  EXPORTING
*"     VALUE(E_RESPONSE) TYPE  STRING
*"     VALUE(E_REQUEST_GUID) TYPE  SYSUUID_C32
*"----------------------------------------------------------------------
  TRY.
      DATA(lcl_llm_util) = NEW zcl_llm_util(
        app_id            = 'EVALUATE'
        business_function = 'CODE_REVIEW'
      ).

      lcl_llm_util->add_function_call_properties( ).

      DATA(ls_response) = lcl_llm_util->call_model( prompt = i_prompt ).

      IF sy-subrc = 0.
        e_response = |{ ls_response-response }|.
      ELSE.
        e_response = 'Error calling the model.'.
      ENDIF.

    CATCH cx_root INTO DATA(lcx_root).
      e_response = lcx_root->get_longtext( ).
  ENDTRY.

  " Pass the GUID back to the caller
  e_request_guid = i_request_guid.

ENDFUNCTION.
