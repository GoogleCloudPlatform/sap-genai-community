CLASS zcl_llm_gcs_util DEFINITION
 PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      constructor IMPORTING app_id            TYPE zllm_app_id
                            business_function TYPE zllm_buss_function_name,

**********************************************************************
* Search a GCS bucket
**********************************************************************
      search_bucket IMPORTING search       TYPE string
                              bucket       TYPE string
                    RETURNING VALUE(files) TYPE zllm_gcs_files_tt
                    RAISING   zcx_llm,

**********************************************************************
* Get agent level customisation from the GCS map table
**********************************************************************
      save_gcs_map IMPORTING gcs_map                    TYPE zllm_gcs_map
                   RETURNING VALUE(agent_customisation) TYPE zllm_custom_agent_odata_tt
                   RAISING   zcx_llm,

**********************************************************************
* Get agent level customisation from the GCS map table
**********************************************************************
      get_gcs_map IMPORTING agent                      TYPE string OPTIONAL
                            user                       TYPE uname OPTIONAL
                  RETURNING VALUE(agent_customisation) TYPE zllm_custom_agent_odata_tt
                  RAISING   zcx_llm.


  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: lo_client            TYPE REF TO /goog/cl_storage_v1,
          gv_app_id            TYPE zllm_app_id,
          gv_business_function TYPE zllm_buss_function_name.

ENDCLASS.



CLASS zcl_llm_gcs_util IMPLEMENTATION.

  METHOD constructor.

* Open HTTP Connection
    TRY.
        me->lo_client = NEW /goog/cl_storage_v1( iv_key_name = 'VERTEX' ).

        me->gv_app_id = app_id.
        me->gv_business_function = business_function.

      CATCH /goog/cx_sdk.

    ENDTRY.

  ENDMETHOD.

  METHOD get_gcs_map.

    DATA: lv_agent TYPE zllm_function_name.

    lv_agent = to_upper( agent ).

    IF user IS INITIAL.
      SELECT usr AS username, agent, gcs_uri, mime
      FROM zllm_gcs_map
      INTO CORRESPONDING FIELDS OF TABLE @agent_customisation
      WHERE app_id = @me->gv_app_id
      AND business_function = @me->gv_business_function
      AND agent = @lv_agent.
    ELSE.
      SELECT usr AS username, agent, gcs_uri, mime
      FROM zllm_gcs_map
      INTO CORRESPONDING FIELDS OF TABLE @agent_customisation
      WHERE app_id = @me->gv_app_id
      AND business_function = @me->gv_business_function
      AND agent = @lv_agent
      AND usr = @user.
    ENDIF.

  ENDMETHOD.


  METHOD save_gcs_map.

    DATA(lcl_llm_crud_util) = NEW zcl_llm_crud_control_util( app_id            = gv_app_id
                                                             business_function = gv_business_function ).


    TRY.
        lcl_llm_crud_util->save_gcs_map( gcs_map = gcs_map ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.


  ENDMETHOD.

  METHOD search_bucket.

    DATA: lv_file TYPE string,
          ls_file TYPE zllm_gcs_files.

* Call API method: storage.objects.list
    TRY.
        CALL METHOD lo_client->list_objects
          EXPORTING
            iv_q_matchglob = search
            iv_p_bucket    = bucket
          IMPORTING
            es_output      = DATA(ls_output)
            ev_ret_code    = DATA(lv_ret_code)
            ev_err_text    = DATA(lv_err_text)
            es_err_resp    = DATA(ls_err_resp).


        IF lo_client->is_success( lv_ret_code ).
          LOOP AT ls_output-items ASSIGNING FIELD-SYMBOL(<ls_item>).
            ls_file-files = <ls_item>-name.
            APPEND ls_file TO files.
          ENDLOOP.
        ENDIF.

* Close HTTP Connection
        lo_client->close( ).


      CATCH /goog/cx_sdk.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>gcs_search_error
            msg1   = search.

    ENDTRY.
  ENDMETHOD.
ENDCLASS.
