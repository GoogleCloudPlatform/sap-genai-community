*&---------------------------------------------------------------------*
*& Report zrep_fetch_codieweb_log_bq
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrep_fetch_codieweb_log_bq.

PARAMETERS:
  p_key    TYPE /goog/keyname OBLIGATORY MATCHCODE OBJECT /goog/sh_gcp_key_nm,
  p_loc_id TYPE string OBLIGATORY DEFAULT 'europe-west1' LOWER CASE,
  p_tod    AS CHECKBOX DEFAULT 'X',
  p_tes    AS CHECKBOX DEFAULT 'X'.


DATA:
  lv_project_id TYPE string,
  ls_input      TYPE /goog/cl_bigquery_v2=>ty_103,
  ls_output     TYPE zllm_log,
  lt_output     TYPE TABLE OF zllm_log,
  lx_uuid_error TYPE REF TO cx_uuid_error.

CONSTANTS:
  lc_newline TYPE c VALUE cl_abap_char_utilities=>newline.

TRY.
    "Initialize Bigquery object, pass the client key name that you have configured in /GOOG/CLIENT_KEY table
    DATA(lo_bq) = NEW /goog/cl_bigquery_v2( iv_key_name = p_key ).

    "Populate relevant parameters
    lv_project_id = lo_bq->gv_project_id.

    ls_input-default_dataset-project_id = lv_project_id.         "Project ID that contains the public datasets
    ls_input-default_dataset-dataset_id = 'smyths_sap_bot_raw'.    "Dataset ID

*    DATA(lv_query_only_today) = |SELECT * FROM `smyths_sap_bot_raw.codieweb_log` WHERE dat = CURRENT_DATE() ORDER BY dat DESC, time DESC|.
     DATA(lv_query_only_today) = |SELECT * FROM `smyths_sap_bot_raw.codieweb_log` WHERE dat = '| &&
                                  sy-datum+0(4) && |-| && sy-datum+4(2) && |-| && sy-datum+6(2)  &&
                                |' ORDER BY dat DESC, time DESC|.

    DATA(lv_query_all_entries) = |SELECT * FROM `smyths_sap_bot_raw.codieweb_log` ORDER BY dat DESC, time DESC|.


    ls_input-query = lv_query_only_today.


    "Call API method: bigquery.jobs.query
    CALL METHOD lo_bq->query_jobs
      EXPORTING
        iv_p_projects_id = lv_project_id
        is_input         = ls_input
      IMPORTING
        es_output        = DATA(ls_response)
        ev_ret_code      = DATA(lv_ret_code)
        ev_err_text      = DATA(lv_err_text)
        es_err_resp      = DATA(ls_err_resp).

    IF lo_bq->is_success( lv_ret_code ) AND ls_response-job_complete = abap_true.

      LOOP AT ls_response-rows ASSIGNING FIELD-SYMBOL(<ls_row>).

        DATA(lv_index) = 1.

        LOOP AT <ls_row>-f ASSIGNING FIELD-SYMBOL(<ls_value>).

          DATA lv_bq_value_str TYPE string.

          ASSIGN COMPONENT lv_index OF STRUCTURE ls_output TO FIELD-SYMBOL(<lv_output_field>).

          IF <ls_value>-v IS BOUND AND <lv_output_field> IS ASSIGNED.

            lv_bq_value_str = <ls_value>-v->*.

            CASE lv_index.

              WHEN 2. "This is uuid
                IF lv_bq_value_str IS INITIAL OR lv_bq_value_str = 'default'.

                  CLEAR <lv_output_field>.

                ELSE.

                  REPLACE ALL OCCURRENCES OF '-' IN lv_bq_value_str WITH ''.
                  TRANSLATE lv_bq_value_str TO UPPER CASE.

                  <lv_output_field> = lv_bq_value_str.

                ENDIF.


              WHEN 3. "This is uuid key
                IF lv_bq_value_str IS INITIAL OR lv_bq_value_str = 'default'.

                  CLEAR <lv_output_field>.

                ELSE.

                  REPLACE ALL OCCURRENCES OF '-' IN lv_bq_value_str WITH ''.
                  TRANSLATE lv_bq_value_str TO UPPER CASE.

                  <lv_output_field> = lv_bq_value_str.

                ENDIF.



              WHEN 7. " This is date (e.g., 'YYYY-MM-DD')
                IF lv_bq_value_str IS NOT INITIAL.
                  " Remove hyphens for DATUM format (YYYYMMDD)
                  REPLACE ALL OCCURRENCES OF '-' IN lv_bq_value_str WITH ''.
                  " Assign to DATUM field
                  <lv_output_field> = lv_bq_value_str.
                ELSE.
                  CLEAR <lv_output_field>.
                ENDIF.



              WHEN 8. " This is time (e.g., 'HH:MM:SS' or 'HH:MM:SS.ffffff')
                IF lv_bq_value_str IS NOT INITIAL.
                  " Remove colons for UZEIT format (HHMMSS)
                  REPLACE ALL OCCURRENCES OF ':' IN lv_bq_value_str WITH ''.
                  " If BigQuery time has milliseconds, truncate to HHMMSS
                  IF strlen( lv_bq_value_str ) > 6.
                    lv_bq_value_str = lv_bq_value_str(6).
                  ENDIF.
                  " Assign to UZEIT field
                  <lv_output_field> = lv_bq_value_str.
                ELSE.
                  CLEAR <lv_output_field>.
                ENDIF.



              WHEN OTHERS. " All other fields, assign directly
                <lv_output_field> = lv_bq_value_str.


            ENDCASE.



          ENDIF.

          ADD 1 TO lv_index.

        ENDLOOP.

        APPEND ls_output TO lt_output.

        CLEAR ls_output.

      ENDLOOP.



    ELSE.
      "Raise exception in case the API call fails

    ENDIF.

    "Close HTTP Connection
    lo_bq->close( ).

    IF lt_output IS NOT INITIAL.

      SELECT *
      FROM zllm_log
      INTO TABLE @DATA(lt_log)
      WHERE app_id = 'CODIEWEB'
      AND buss_function = 'WEB'
      AND user_name = 'CODIEWEB'
      AND dat = @sy-datum.

      LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<fs_log>).
        DELETE lt_output
        WHERE uuid = <fs_log>-uuid
        AND uuid_key = <fs_log>-uuid_key
        AND app_id = <fs_log>-app_id
        AND buss_function = <fs_log>-buss_function
        AND user_name = <fs_log>-user_name.
      ENDLOOP.

      IF lt_output IS NOT INITIAL.

        IF p_tes = abap_true.

          CALL FUNCTION 'Z_VERY_SIMPLE_ALV'
            TABLES
              it_alv = lt_output.

        ELSE.

          MODIFY zllm_log FROM TABLE lt_output  .

        ENDIF.

        MESSAGE |I have added { lines( lt_output ) } records. Thank you!| TYPE 'S' DISPLAY LIKE 'I'.

      ELSE.

        MESSAGE |Nothing to process. Thank you!| TYPE 'S' DISPLAY LIKE 'I'.

      ENDIF.

    ELSE.

      MESSAGE |Nothing to process. Thank you!| TYPE 'S' DISPLAY LIKE 'I'.

    ENDIF.

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
