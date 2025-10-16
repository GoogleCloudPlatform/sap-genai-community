CLASS zcl_llm_evaluate_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES : zif_fiori_generic_apc.

    "----------------------------------------------------------------------
    " TYPES
    "----------------------------------------------------------------------
    TYPES:
      BEGIN OF ty_request_info,
        email     TYPE ad_smtpadr, " Stores the user's email address for notifications
        timed_out TYPE abap_bool,  " Flag: 'X' if the main process timed out, '' otherwise
      END OF ty_request_info,

      BEGIN OF ty_transport_object,
        transport_number        TYPE trkorr,
        parent_transport_number TYPE trkorr,
        owner                   TYPE as4user,
        transport_description   TYPE as4text,
        object_type             TYPE ddtext,
        object_name             TYPE sobj_name,
        is_parent_transport     TYPE abap_bool,
      END OF ty_transport_object,

      tt_transport_object TYPE STANDARD TABLE OF ty_transport_object WITH KEY transport_number.

    "----------------------------------------------------------------------
    " CLASS-METHODS
    "----------------------------------------------------------------------
    "Reads the timeout value from ZLLM_CONFIG.
    CLASS-METHODS get_timeout
      RETURNING
        VALUE(rv_timeout) TYPE zllm_agent_param_value.

    "Retrieves the current user's email address.
    CLASS-METHODS get_user_email
      RETURNING
        VALUE(rv_email) TYPE ad_smtpadr.

    "Sends an email using BCS classes.
    CLASS-METHODS send_email
      IMPORTING
        !iv_recipients TYPE ad_smtpadr
        !iv_subject    TYPE so_obj_des
        !iv_body       TYPE string.

    CLASS-METHODS store_request_info
      IMPORTING
        !iv_guid      TYPE sysuuid_c32
        !iv_email     TYPE ad_smtpadr
        !iv_timed_out TYPE abap_bool.

    " Updates the record with the final response from the RFC
    CLASS-METHODS store_result
      IMPORTING
        !iv_guid     TYPE sysuuid_c32
        !iv_response TYPE string.

    " Reads the entire current state from the database
    CLASS-METHODS get_request_state
      IMPORTING
        !iv_guid        TYPE sysuuid_c32
      RETURNING
        VALUE(rs_state) TYPE zllm_eval_req_st.

    " Sets the timeout flag in the database
    CLASS-METHODS set_timed_out_flag
      IMPORTING
        !iv_guid TYPE sysuuid_c32.

    " Deletes the state record from the database
    CLASS-METHODS clear_request_state
      IMPORTING
        !iv_guid TYPE sysuuid_c32.

    " Send the code evaluation report to reviewers
    CLASS-METHODS send_evaluation_report
      IMPORTING
        !iv_subject TYPE string
        !iv_body    TYPE string
        !iv_team    TYPE char20.

    " Submit the response via APC
    CLASS-METHODS submit_apc_request
      IMPORTING
        !iv_response TYPE string
        !iv_guid     TYPE sysuuid_c32.

    "This method fetches all unreleased workbench and customizing requests for a given user, including their tasks and the objects contained within.
    CLASS-METHODS get_transports_by_user
      IMPORTING
        iv_user_name         TYPE sy-uname
      RETURNING
        VALUE(rt_transports) TYPE tt_transport_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS get_object_description
      IMPORTING
        iv_object_type        TYPE trobjtype
      RETURNING
        VALUE(rv_description) TYPE ddtext.
ENDCLASS.



CLASS zcl_llm_evaluate_util IMPLEMENTATION.

  METHOD get_timeout.
    SELECT SINGLE value
      FROM zllm_config
      INTO @rv_timeout
     WHERE param = 'CALL_MODEL_TIMEOUT' AND app_id = 'EVALUATE' AND buss_function = 'CODE_REVIEW'.

    IF sy-subrc <> 0 OR rv_timeout IS INITIAL.
      rv_timeout = 180. " Default to 3 minutes
    ENDIF.
  ENDMETHOD.

  METHOD get_user_email.
    SELECT SINGLE smtp_addr
      FROM usr21
      INNER JOIN adr6 ON usr21~persnumber = adr6~persnumber
      INTO rv_email
      WHERE usr21~bname = sy-uname.
  ENDMETHOD.

  METHOD store_request_info.
    DATA ls_state TYPE zllm_eval_req_st.

    ls_state-request_guid = iv_guid.
    ls_state-user_email   = iv_email.
    ls_state-is_timed_out = iv_timed_out.
    ls_state-status       = 'NEW'.
    GET TIME STAMP FIELD ls_state-created_on.
    ls_state-updated_on = ls_state-created_on.

    MODIFY zllm_eval_req_st FROM ls_state.
    COMMIT WORK. " Ensure the DB record is visible to the new job
  ENDMETHOD.

  METHOD get_request_state.
    SELECT SINGLE *
      FROM zllm_eval_req_st
      WHERE request_guid = @iv_guid
      INTO @rs_state.
  ENDMETHOD.

  METHOD set_timed_out_flag.
    CALL FUNCTION 'Z_LLM_EVAL_UPDATE_STATE' IN UPDATE TASK
      EXPORTING
        iv_guid   = iv_guid
        iv_action = 'TIMEOUT'.
  ENDMETHOD.

  METHOD clear_request_state.
    CALL FUNCTION 'Z_LLM_EVAL_UPDATE_STATE' IN UPDATE TASK
      EXPORTING
        iv_guid   = iv_guid
        iv_action = 'CLEAR'.
  ENDMETHOD.

  METHOD send_email.
    TRY.

        TYPES : BEGIN OF ty_email,
                  email TYPE ad_smtpadr,
                END OF ty_email.

        DATA: lt_email TYPE TABLE OF ty_email,
              ls_email TYPE ty_email.

        DATA(lo_send_request) = cl_bcs=>create_persistent( ).

*         Add user as the sender to send request
        DATA(lo_email_sender) = cl_sapuser_bcs=>create( sy-uname ).
        lo_send_request->set_sender( i_sender = lo_email_sender ).

*         Add recipients to send request
        SPLIT iv_recipients AT ',' INTO TABLE lt_email.
        DELETE lt_email WHERE email IS INITIAL.
        LOOP AT lt_email INTO ls_email.
          TRANSLATE ls_email-email TO LOWER CASE.
          DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( ls_email-email ).
          lo_send_request->add_recipient(
            i_recipient = lo_recipient
            i_express   = 'X'
          ).
        ENDLOOP.

*         Build email subject and body text
        DATA(lt_body) = cl_document_bcs=>string_to_soli( iv_body ).
        DATA(lo_document) = cl_document_bcs=>create_document(
          i_type    = 'HTM' "'RAW'
          i_text    = lt_body
          i_subject = iv_subject ).

*         Add document to send request
        lo_send_request->set_document( lo_document ).

*         Send email
        lo_send_request->set_send_immediately( 'X' ).
        lo_send_request->send( ).

      CATCH cx_bcs INTO DATA(lx_bcs).
        " Handle email sending exception, e.g., log the error
        " MESSAGE lx_bcs->get_text( ) TYPE 'E'.

    ENDTRY.
  ENDMETHOD.

  METHOD store_result.
    CALL FUNCTION 'Z_LLM_EVAL_UPDATE_STATE' IN UPDATE TASK
      EXPORTING
        iv_guid     = iv_guid
        iv_response = iv_response
        iv_action   = 'STORE'.
  ENDMETHOD.

  METHOD send_evaluation_report.

*   Retrieve the reviewer's email address from the config table
    SELECT SINGLE value
      FROM zllm_config
      INTO @DATA(lv_receipients)
     WHERE param = 'CODE_REVIEWERS' AND type = @iv_team AND app_id = 'EVALUATE' AND buss_function = 'CODE_REVIEW'.

*   setting the default email address if there is no entry in the config table
    IF sy-subrc <> 0 OR lv_receipients IS INITIAL.
      lv_receipients = 'adamsexton@smythstoys.com'.
    ENDIF.

*   Calling send_email method to send the code review to the reviewers
    zcl_llm_evaluate_util=>send_email(
      iv_recipients = CONV #( lv_receipients )
      iv_subject    = CONV #( iv_subject )
      iv_body       = iv_body
    ).

  ENDMETHOD.

  METHOD get_transports_by_user.

    DATA lv_user TYPE sy-uname.
    lv_user = iv_user_name.
    TRANSLATE lv_user TO UPPER CASE.

    " 1. Find all main transport requests where the user is involved.
    SELECT strkorr AS trkorr
        FROM e070
        WHERE as4user = @lv_user
          AND trstatus IN ('D', 'L')
          AND strkorr IS NOT INITIAL
        INTO TABLE @DATA(lt_parent_reqs_from_tasks).

    SELECT trkorr
        FROM e070
        WHERE as4user = @lv_user
          AND trstatus IN ('D', 'L')
          AND strkorr IS INITIAL
        APPENDING TABLE @lt_parent_reqs_from_tasks.

    SORT lt_parent_reqs_from_tasks BY trkorr.
    DELETE ADJACENT DUPLICATES FROM lt_parent_reqs_from_tasks COMPARING trkorr.

    IF lt_parent_reqs_from_tasks IS INITIAL.
      RETURN.
    ENDIF.

    " 2. Get all headers and tasks belonging to these main requests
    SELECT trkorr, strkorr, as4user
        FROM e070
        INTO TABLE @DATA(lt_e070)
        FOR ALL ENTRIES IN @lt_parent_reqs_from_tasks
        WHERE trkorr = @lt_parent_reqs_from_tasks-trkorr
           OR strkorr = @lt_parent_reqs_from_tasks-trkorr.

    IF lt_e070 IS INITIAL.
      RETURN.
    ENDIF.

    " --- CORRECTED TEXT RETRIEVAL LOGIC ---
    " 3. Fetch descriptions for ALL transports and tasks found, not just parents.
    DATA lt_e07t TYPE HASHED TABLE OF e07t WITH UNIQUE KEY trkorr.
    SELECT trkorr, as4text
        FROM e07t
        INTO CORRESPONDING FIELDS OF TABLE @lt_e07t
        FOR ALL ENTRIES IN @lt_e070
        WHERE trkorr = @lt_e070-trkorr " Select for all transports/tasks
          AND langu  = @sy-langu.
    " --- END OF CORRECTION ---

    " 4. Fetch all corresponding objects for the transports/tasks found
    SELECT trkorr, pgmid, object, obj_name
        FROM e071
        INTO TABLE @DATA(lt_e071)
        FOR ALL ENTRIES IN @lt_e070
        WHERE trkorr = @lt_e070-trkorr.

    " 5. Process and combine all data into the final structure
    LOOP AT lt_e070 ASSIGNING FIELD-SYMBOL(<fs_e070>).

      " 5a. Determine the parent TR
      DATA(lv_parent_tr) = COND trkorr( WHEN <fs_e070>-strkorr IS INITIAL
                                        THEN <fs_e070>-trkorr
                                        ELSE <fs_e070>-strkorr ).

      " --- CORRECTED DESCRIPTION LOGIC ---
      " 5b. Get the parent's description and the current TR's description
      READ TABLE lt_e07t INTO DATA(ls_parent_text) WITH TABLE KEY trkorr = lv_parent_tr.
      READ TABLE lt_e07t INTO DATA(ls_current_text) WITH TABLE KEY trkorr = <fs_e070>-trkorr.

      " Prioritize the parent's text, but use the current TR's text if parent's is blank.
      DATA(lv_final_description) = ls_parent_text-as4text.
      IF lv_final_description IS INITIAL.
        lv_final_description = ls_current_text-as4text.
      ENDIF.
      " --- END OF CORRECTION ---

      " 5c. Prepare the common data with the correctly determined description
      DATA(ls_common_data) = VALUE ty_transport_object(
          transport_number      = <fs_e070>-trkorr
          owner                 = <fs_e070>-as4user
          parent_transport_number = lv_parent_tr
          transport_description = lv_final_description " Use the final description
          is_parent_transport   = COND #( WHEN <fs_e070>-strkorr IS INITIAL THEN abap_true ELSE abap_false )
      ).

      " 5d. Loop through objects and create a record for each one
      DATA(lv_object_found) = abap_false.
      LOOP AT lt_e071 ASSIGNING FIELD-SYMBOL(<fs_e071>) WHERE trkorr = <fs_e070>-trkorr.
        lv_object_found = abap_true.
        APPEND ls_common_data TO rt_transports ASSIGNING FIELD-SYMBOL(<fs_result_obj>).
        <fs_result_obj>-object_name = <fs_e071>-obj_name.
        <fs_result_obj>-object_type = get_object_description( <fs_e071>-object ).
      ENDLOOP.

      " 5e. If no objects were found, create the header record
      IF lv_object_found = abap_false.
        APPEND ls_common_data TO rt_transports.
      ENDIF.
    ENDLOOP.

    " Final sort for readability
    SORT rt_transports BY parent_transport_number is_parent_transport DESCENDING transport_number.

  ENDMETHOD.

  METHOD get_object_description.
    " This method provides a hard-coded but reliable mapping of technical
    " object types to their human-readable descriptions.
    " This prevents errors caused by missing tables or FMs in different system versions.
    CASE iv_object_type.
      WHEN 'PROG'. rv_description = 'Program'.
      WHEN 'CLAS'. rv_description = 'Class'.
      WHEN 'INTF'. rv_description = 'Interface'.
      WHEN 'FUGR'. rv_description = 'Function Group'.
      WHEN 'FUNC'. rv_description = 'Function Module'.
      WHEN 'TABL'. rv_description = 'Table'.
      WHEN 'VIEW'. rv_description = 'View'.
      WHEN 'DTEL'. rv_description = 'Data Element'.
      WHEN 'DOMA'. rv_description = 'Domain'.
      WHEN 'SHLP'. rv_description = 'Search Help'.
      WHEN 'TYPE'. rv_description = 'Type Group'.
      WHEN 'XSLT'. rv_description = 'XSLT Program'.
      WHEN 'SMIM'. rv_description = 'MIME Object'.
      WHEN 'SICF'. rv_description = 'ICF Service'.
      WHEN 'NROB'. rv_description = 'Number Range Object'.
      WHEN 'MSAG'. rv_description = 'Message Class'.
      WHEN 'TRAN'. rv_description = 'Transaction'.
      WHEN 'SUSO'. rv_description = 'Authorization Object'.
      WHEN 'METH'. rv_description = 'Method'.
      WHEN 'VDAT'. rv_description = 'View Data'. " Added from your example
      WHEN OTHERS.
        rv_description = iv_object_type.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_fiori_generic_apc~process.

*   Build the result in accordance to the APC result structure
    et_results = io_class_object->get_eval_response(  ).

    WAIT UP TO 2 SECONDS.

  ENDMETHOD.

  METHOD submit_apc_request.

    CALL FUNCTION 'ZLLM_EVAL_SUBMIT_APC'
      IN BACKGROUND TASK
      EXPORTING
        iv_response = iv_response
        iv_guid     = iv_guid.

  ENDMETHOD.

ENDCLASS.
