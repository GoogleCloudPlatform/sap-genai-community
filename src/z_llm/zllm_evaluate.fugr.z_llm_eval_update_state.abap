FUNCTION z_llm_eval_update_state.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_GUID) TYPE  SYSUUID_C32
*"     VALUE(IV_RESPONSE) TYPE  STRING OPTIONAL
*"     VALUE(IV_ACTION) TYPE  CHAR10
*"----------------------------------------------------------------------
  DATA lv_timestamp TYPE timestamp.
  GET TIME STAMP FIELD lv_timestamp.

  CASE iv_action.
    WHEN 'STORE'.
      UPDATE zllm_eval_req_st
         SET response    = @iv_response,
             status      = 'DONE',
             updated_on  = @lv_timestamp
       WHERE request_guid = @iv_guid.

    WHEN 'CLEAR'.
      DELETE FROM zllm_eval_req_st WHERE request_guid = iv_guid.

    WHEN 'TIMEOUT'.
      UPDATE zllm_eval_req_st
         SET is_timed_out = @abap_true,
             updated_on   = @lv_timestamp
       WHERE request_guid = @iv_guid.

  ENDCASE.

ENDFUNCTION.
