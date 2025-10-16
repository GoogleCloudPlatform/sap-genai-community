*&---------------------------------------------------------------------*
*& Report Z_LLM_EVAL_CALLBACK_HANDLER
*&---------------------------------------------------------------------*
REPORT z_llm_eval_callback_handler.

PARAMETERS:
  p_prompt TYPE string,
  p_guid   TYPE sysuuid_c32.

DATA: gv_callback_completed TYPE abap_bool VALUE abap_false,
      gv_ai_response        TYPE    string,
      gv_request_guid       TYPE    sysuuid_c32.

START-OF-SELECTION.
  " This part of the report remains the same.
  " It calls the aRFC and waits for the callback.
  DATA(lv_task_name) = |LLM_EVAL_{ p_guid }|.

  CALL FUNCTION 'Z_LLM_EVAL_CALL_MODEL_RFC'
    STARTING NEW TASK lv_task_name
    DESTINATION 'NONE'
    PERFORMING receive_response ON END OF TASK
    EXPORTING
      i_prompt       = p_prompt
      i_request_guid = p_guid
    EXCEPTIONS
      OTHERS         = 1.

  IF sy-subrc = 0.
    CONSTANTS lc_max_wait_time TYPE i VALUE 3600. " 1 hour
    WAIT FOR ASYNCHRONOUS TASKS UNTIL gv_callback_completed = abap_true UP TO lc_max_wait_time SECONDS.
    " Sending the result via APC to UI
    zcl_llm_evaluate_util=>submit_apc_request(
      iv_response = gv_ai_response
      iv_guid     = gv_request_guid
    ).

    COMMIT WORK.

    IF sy-subrc <> 0.
      " Handle aRFC timeout if necessary (e.g., update state to 'ERROR')
    ENDIF.
  ELSE.
    " Handle immediate aRFC start failure
    zcl_llm_evaluate_util=>store_result(
      iv_guid     = p_guid
      iv_response = |Critical Error: Failed to start aRFC task. SY-SUBRC: { sy-subrc }|
    ).
  ENDIF.

*&---------------------------------------------------------------------*
*& Form receive_response
*&---------------------------------------------------------------------*
FORM receive_response USING lv_task_name.
  DATA: lv_response     TYPE string,
        lv_request_guid TYPE sysuuid_c32.


  " 1. Receive the results from the completed background task
  RECEIVE RESULTS FROM FUNCTION 'Z_LLM_EVAL_CALL_MODEL_RFC'
    IMPORTING
      e_response     = lv_response
      e_request_guid = lv_request_guid
    EXCEPTIONS OTHERS = 1.

  IF sy-subrc <> 0.
    lv_response = |Background task '{ lv_task_name }' failed to return a valid result. SY-SUBRC: { sy-subrc }|.
  ENDIF.

  " 2. Retrieve the request state from the DB table ***
  DATA(ls_state) = zcl_llm_evaluate_util=>get_request_state( lv_request_guid ).

  " 3. Check if the original OData process already timed out
  gv_ai_response = lv_response.
  gv_request_guid = lv_request_guid.
  IF ls_state-is_timed_out = abap_true.

    " Clean up the state record
    zcl_llm_evaluate_util=>clear_request_state( lv_request_guid ).

    " If it timed out, send the result directly via email
    zcl_llm_evaluate_util=>send_email(
      iv_recipients = ls_state-user_email
      iv_subject    = 'Your Report is Ready'
      iv_body       = lv_response
    ).

  ELSE.
    " If it has NOT timed out, place the result in the DB table
    " for the waiting OData polling loop to find.
    zcl_llm_evaluate_util=>store_result(
      iv_guid     = lv_request_guid
      iv_response = lv_response
    ).

    gv_callback_completed = abap_true.

  ENDIF.

ENDFORM.
