CLASS zcl_z_llm_evaluate_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_z_llm_evaluate_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS zllmevaluateset_create_entity
        REDEFINITION .
    METHODS zllmsendevalrepo_update_entity
        REDEFINITION .
    METHODS ztransportobject_get_entityset
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_z_llm_evaluate_dpc_ext IMPLEMENTATION.


  METHOD zllmevaluateset_create_entity.

    DATA ls_post TYPE zllm_evaluate_odata.
    DATA lv_guid TYPE sysuuid_c32.
    DATA lv_timeout TYPE i.
    DATA lv_end_time TYPE timestampl.
    DATA lv_result_found TYPE abap_bool.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_post ).

    TRY.
        " 1. Get configuration
        lv_timeout = 30."zcl_llm_evaluate_util=>get_timeout( ).
        DATA(lv_user_email) = zcl_llm_evaluate_util=>get_user_email( ).

        IF lv_user_email IS INITIAL.
          er_entity-response = 'Error: User email address could not be determined.'.
          RETURN.
        ENDIF.

        " 2. Generate a unique ID for the entire process
        lv_guid = cl_system_uuid=>create_uuid_c32_static( ).

        " 3. Create the initial state record in the DB table ***
        zcl_llm_evaluate_util=>store_request_info(
          iv_guid      = lv_guid
          iv_email     = lv_user_email
          iv_timed_out = abap_false
        ).

        " 4. Submit the report as a background job
        DATA lv_job_name  TYPE tbtcjob-jobname.
        DATA lv_job_count TYPE tbtcjob-jobcount.
        lv_job_name = lv_guid.

        CALL FUNCTION 'JOB_OPEN' EXPORTING jobname = lv_job_name IMPORTING jobcount = lv_job_count EXCEPTIONS OTHERS = 1.
        IF sy-subrc = 0.
          SUBMIT z_llm_eval_callback_handler
            WITH p_prompt = ls_post-request
            WITH p_guid   = lv_guid
            VIA JOB lv_job_name NUMBER lv_job_count
            AND RETURN.
          CALL FUNCTION 'JOB_CLOSE' EXPORTING jobcount = lv_job_count jobname = lv_job_name strtimmed = 'X' EXCEPTIONS OTHERS = 1.
        ENDIF.

        " 5. Polling loop now checks the DB table for the response ***
        GET TIME STAMP FIELD lv_end_time.
        lv_end_time = lv_end_time + lv_timeout.

        DO.
          " Check if the result is available in the database
          DATA(ls_state) = zcl_llm_evaluate_util=>get_request_state( lv_guid ).
          IF ls_state-response IS NOT INITIAL.
            er_entity-response = ls_state-response.
            er_entity-uuid = lv_guid.
            lv_result_found = abap_true.
            EXIT.
          ENDIF.

          " Check for timeout
          DATA lv_current_time TYPE timestampl.
          GET TIME STAMP FIELD lv_current_time.
          IF lv_current_time >= lv_end_time.
            EXIT. " Timeout reached
          ENDIF.

          WAIT UP TO 1 SECONDS.
        ENDDO.

        " 6. Check the outcome and set the final OData response
        IF lv_result_found = abap_true.
          " Success: Result arrived in time. Clean up the state record.
          zcl_llm_evaluate_util=>clear_request_state( lv_guid ).
        ELSE.
          " Timeout: Inform the user and update the state for the callback
          er_entity-response = 'The process is taking longer than expected. We will send the report to your email address.'.
          er_entity-uuid = lv_guid.
          zcl_llm_evaluate_util=>set_timed_out_flag( lv_guid ).
        ENDIF.

      CATCH cx_root INTO DATA(lcx_root).
        er_entity-response = lcx_root->get_longtext(  ).
    ENDTRY.

  ENDMETHOD.


  METHOD zllmsendevalrepo_update_entity.

*** Service implementation to send the code evaluation report

    DATA : ls_send_eval_rep      TYPE zllm_send_eval_rep_odata.

    TRY.

        DATA(lv_team) = it_key_tab[ name = 'Team' ]-value .

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = 'Team key not provided from UI Odata call'
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.
    ENDTRY.

*   Gather all the required data passed from UI
    TRY.

        io_data_provider->read_entry_data( IMPORTING es_data = ls_send_eval_rep ).

      CATCH /iwbep/cx_mgw_tech_exception INTO DATA(lcx_input).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            http_status_code = 500
            message          = CONV bapi_msg( lcx_input->get_text( ) )
            textid           = /iwbep/cx_mgw_busi_exception=>business_error.

    ENDTRY.

*   Instantiate the evaluation UTIL class

    DATA(lref_llm_eval_util) = NEW zcl_llm_evaluate_util( ).

*   Sending the code evaluation report
    lref_llm_eval_util->send_evaluation_report(
      iv_team    = ls_send_eval_rep-team
      iv_subject = ls_send_eval_rep-email_subject
      iv_body    = ls_send_eval_rep-email_body
    ).


  ENDMETHOD.


  METHOD ztransportobject_get_entityset.

*** Service implementation to get the list of open transports for the users

    DATA(lv_tr_owner) = it_filter_select_options[ property = 'Owner' ]-select_options[ 1 ]-low .

    IF lv_tr_owner IS INITIAL.
      lv_tr_owner = sy-uname.
    ENDIF.

*   Instantiate the evaluation UTIL class

    DATA(lref_llm_eval_util) = NEW zcl_llm_evaluate_util( ).

    DATA(lt_transports) = lref_llm_eval_util->get_transports_by_user( iv_user_name = CONV #( lv_tr_owner ) ).

    et_entityset = CORRESPONDING #( lt_transports ).

  ENDMETHOD.
ENDCLASS.
