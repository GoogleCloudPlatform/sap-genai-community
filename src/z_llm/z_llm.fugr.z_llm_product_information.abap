FUNCTION Z_LLM_PRODUCT_INFORMATION.
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
  TRY.

      DATA(lcl_llm_util) = NEW zcl_llm_util( app_id            = 'CSHUB'
                                             business_function = 'EMAIL' ).

* Set the global context of this FM.
      DATA(ls_prompt) = lcl_llm_util->fetch_prompt( iv_prompt_key = 'INTROCONTEXT' ).
      cv_prompt = ls_prompt-prompt_text.


* Set the global context of this FM.
      ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'Z_LLM_PRODUCT_INFORMATION' ).
      cv_prompt = |\\{ cv_prompt }','{ ls_prompt-prompt_text } |.


* Set the global context of this FM.
      ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'SIGNOFFCONTEXT' ).
      cv_prompt = |\\{ cv_prompt }','{ ls_prompt-prompt_text } |.


* LLM should have identified the order number from the prompt
      DATA(hybris_order) = to_upper( it_function_parameters[ parameter_name ='HYBRISORDER' ]-parameter_value ).

* The LLM may have identified the order but it might be formatted with spaces. Remove.
      CONDENSE hybris_order.

* First thing. Check if this is a valid order.
      SELECT SINGLE bstnk
      FROM vbak
      INTO @DATA(lv_bstnk)
      WHERE bstnk = @hybris_order.

      IF sy-subrc = 0.

        DATA(lcL_llm) = NEW zcl_cs_hub_llm_util( hybris_order = CONV bstnk( hybris_order ) ).
        ev_function_response = lcl_llm->llm_get_data( ).

      ELSE.

* Update the global context to error context.
        ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'ERRORCONTEXT' ).
        cv_prompt = ls_prompt-prompt_text.

        ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'ORDERNOTFOUND' ).
        ev_function_response = | { hybris_order }', { ls_prompt-prompt_text }|.
      ENDIF.


    CATCH /goog/cx_sdk.
    CATCH zcx_llm.
    CATCH zcx_cs_hub.
    CATCH cx_sy_itab_line_not_found.

* Update the global context to error context.
      ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'ERRORCONTEXT' ).
      cv_prompt = ls_prompt-prompt_text.

      CLEAR: ls_prompt.

      ls_prompt = lcl_llm_util->fetch_prompt( iv_prompt_key = 'ORDERNOTFOUND' ).
      ev_function_response =  ls_prompt-prompt_text.


  ENDTRY.







ENDFUNCTION.
