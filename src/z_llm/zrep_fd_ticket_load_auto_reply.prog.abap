*&---------------------------------------------------------------------*
*& Report zrep_fd_ticket_load_auto_reply
*&---------------------------------------------------------------------*
*& Report program to pull Open tickets from Freshdesk and process them for auto-replying using
*& LLM
*&---------------------------------------------------------------------*
REPORT zrep_fd_ticket_load_auto_reply.


DATA : gc_app_id        TYPE zllm_app_id VALUE 'CSHUB',
       gc_buss_function TYPE zllm_buss_function_name VALUE 'EMAIL',
       lv_fd_source_id  TYPE char2.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: source FOR lv_fd_source_id.
SELECTION-SCREEN END OF BLOCK b1.

IF source IS INITIAL.
  WRITE:/ |Please provide the source ID for the tickets that have to be auto responded|.
ENDIF.


**********************************************************************
* Ensure that this isnt already running.
**********************************************************************

DATA: ls_job TYPE zllm_job.

SELECT SINGLE running,
              updated_date,
              updated_time
FROM zllm_job
INTO @DATA(ls_running)
WHERE app_id = 'CSHUB'
AND business_function = 'EMAIL'.

IF sy-subrc = 4.
* Make first entry.
  ls_job-mandt = sy-mandt.
  ls_job-app_id = 'CSHUB'.
  ls_job-business_function = 'EMAIL'.
  ls_job-running = abap_true.
  ls_job-updated_date = sy-datum.
  ls_job-updated_time = sy-uzeit.

  ls_running-running = abap_true.

  MODIFY zllm_job FROM ls_job.
  COMMIT WORK AND WAIT.
ENDIF.


IF ls_running-running = abap_true.

  SELECT SINGLE value
           FROM zllm_config
           INTO @DATA(lv_job_run_threshold)
          WHERE app_id = @gc_app_id
            AND buss_function = @gc_buss_function
            AND param = 'JOB_RUN_THRESHOLD'.

  IF lv_job_run_threshold IS NOT INITIAL.

    TRY.

        cl_abap_tstmp=>td_subtract(
          EXPORTING
            date1    = sy-datum
            time1    = sy-uzeit
            date2    = ls_running-updated_date
            time2    = ls_running-updated_time
          IMPORTING
            res_secs = DATA(lv_time_difference)
        ).

*       If the record hasn't updated in the time maintained
*       set job running as false
        IF lv_time_difference >= lv_job_run_threshold.

*       Update job status
          ls_job-mandt = sy-mandt.
          ls_job-app_id = 'CSHUB'.
          ls_job-business_function = 'EMAIL'.
          ls_job-running = abap_false.
          ls_job-updated_date = sy-datum.
          ls_job-updated_time = sy-uzeit.

          MODIFY zllm_job FROM ls_job.
          COMMIT WORK AND WAIT.

        ENDIF.

      CATCH cx_parameter_invalid_type.
      CATCH cx_parameter_invalid_range.
    ENDTRY.

  ENDIF.

* An instance is already running, exit.
  RETURN.

ELSE.

* Update job status
  ls_job-mandt = sy-mandt.
  ls_job-app_id = 'CSHUB'.
  ls_job-business_function = 'EMAIL'.
  ls_job-running = abap_true.
  ls_job-updated_date = sy-datum.
  ls_job-updated_time = sy-uzeit.

  MODIFY zllm_job FROM ls_job.
  COMMIT WORK AND WAIT.
ENDIF.

* Instantiate Integration handler class to pull Freshdesk tickets
DATA(lref_integration_handler) = NEW zcl_cs_hub_integration_handler( ).


TRY.
    DATA(lt_fd_open_tickets) = lref_integration_handler->fetch_freshdesk_open_tickets( ).

  CATCH zcx_cs_hub_integration INTO DATA(lcx_cs_hub_int).

    WRITE:/ | Failed to load tickets from Freshdesk. Error description: { lcx_cs_hub_int->get_text( ) }|.

ENDTRY.


DATA: lcl_log TYPE REF TO zcl_llm_log,
      lv_log  TYPE string.

DATA(lv_lines) = lines( lt_fd_open_tickets ).

**********************************************************************
* Set up logging
**********************************************************************

lcl_log = NEW zcl_llm_log( app_id            = 'CSHUB'
                           business_function = 'EMAIL' ).

lv_log = |Begining processing. { lv_lines } open tickets|.


* Log
lcl_log->write_general_log( request     = lv_log
                            object_type = 'FD_AUTOMATION' ).


IF lt_fd_open_tickets IS NOT INITIAL.

  TRY.

*     Ensure that that source ID is passed
      IF source IS NOT INITIAL.

*       Source: 1 - Email ; 2: Portal
        LOOP AT lt_fd_open_tickets ASSIGNING FIELD-SYMBOL(<fs_fd_ticket>) WHERE source IN source.

* Codie web may have already performed a response. Check for tag NOAUTORES.
          TRY.
              IF <fs_fd_ticket>-tags[ 1 ] = 'NOAUTORES'.
                CONTINUE.
              ENDIF.

            CATCH cx_sy_itab_line_not_found.
* If no line found, then Codie web didnt provide a response.
          ENDTRY.


          TRY.

*             Instantiate Fiori LLM class for every ticket
              DATA(lref_fiori_llm) = NEW zcl_fiori_llm_util( app_id            = gc_app_id
                                                             business_function = gc_buss_function ).

***---        Given the nature of the program usage, ensure to check if the ticket is open within this loop before processing
              DATA(not_open) = lref_fiori_llm->get_freshdesk_ticket_status( ticket = CONV zcs_freshdesk_ticket_id( <fs_fd_ticket>-id ) ).

              IF not_open = abap_true.

                CONTINUE.
                CLEAR : not_open,
                        lref_fiori_llm.

              ENDIF.

* Log
              lv_log = |Processing ticket: {  <fs_fd_ticket>-id } |.
              lcl_log->write_general_log( request     = lv_log
                                          object_type = 'FD_AUTOMATION'
                                          object_key  = CONV zllm_log_object_key( <fs_fd_ticket>-id ) ).


              DATA(lv_query) = |{ <fs_fd_ticket>-subject } \n { <fs_fd_ticket>-description }|.


              DATA(response) = lref_fiori_llm->call_model( prompt     = lv_query
                                                           system_key = <fs_fd_ticket>-id ).
            CATCH cx_root INTO DATA(lcx_root).

* Log
              lv_log = |Error occured processing ticket: {  <fs_fd_ticket>-id } - { lcx_root->get_text( ) }  |.
              lcl_log->write_general_log( request     = lv_log
                                          object_type = 'AUTOMATION_ERROR'
                                          object_key  = CONV zllm_log_object_key( <fs_fd_ticket>-id ) ).

              CONTINUE.

          ENDTRY.

          CLEAR : lv_query, lv_log, response.

          FREE lref_fiori_llm.

        ENDLOOP.

      ENDIF.

    CATCH zcx_llm INTO DATA(lcx_llm).

* Log
      lv_log = |Error occured processing ticket: {  <fs_fd_ticket>-id } - { lcx_llm->get_text( ) }  |.
      lcl_log->write_general_log( request     = lv_log
                                  object_type = 'AUTOMATION_ERROR'
                                  object_key  = CONV zllm_log_object_key( <fs_fd_ticket>-id ) ).

  ENDTRY.

ENDIF.

**********************************************************************
* Update job status. Job has finished.
**********************************************************************
* Update job status
ls_job-mandt = sy-mandt.
ls_job-app_id = 'CSHUB'.
ls_job-business_function = 'EMAIL'.
ls_job-running = abap_false.
ls_job-updated_date = sy-datum.
ls_job-updated_time = sy-uzeit.

MODIFY zllm_job FROM ls_job.
COMMIT WORK AND WAIT.
