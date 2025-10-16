class ZCL_GCS_UTIL definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods SEARCH_BUCKET
    importing
      !SEARCH type STRING
      !BUCKET type STRING
    exporting
      !SPARK_FILES type ZLLM_GCS_SPARK_FILE
    returning
      value(FILES) type ZLLM_GCS_FILES_TT
    raising
      ZCX_LLM .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: lo_client TYPE REF TO /goog/cl_storage_v1.

ENDCLASS.



CLASS ZCL_GCS_UTIL IMPLEMENTATION.


  METHOD constructor.

* Open HTTP Connection
    TRY.
        me->lo_client = NEW /goog/cl_storage_v1( iv_key_name = 'VERTEX' ).

      CATCH /goog/cx_sdk.

    ENDTRY.

  ENDMETHOD.


  METHOD search_bucket.

    DATA: lv_file       TYPE string,
          ls_file       TYPE zllm_gcs_files,
          ls_output     TYPE /goog/cl_storage_v1=>ty_016,
          ls_page_token TYPE /goog/cl_storage_v1=>ty_016-next_page_token,
          lt_page_token LIKE TABLE OF ls_page_token.

* Call API method: storage.objects.list
* PR - the videos/files are stored within bucket in pages
* everytime you make a call, GCS responds with nextPageToken,
* this nextPageToken should be passed as pageToken as part of the GCS request
* So its like, GCS responds with nextPageToken until we reach the page
* in which the file is stored within the bucket
* if nextPageToken is blank that is end of bucket - PR
    TRY.
        DO 50 TIMES.      "50 times is not correct, added as an additional check
*         Real validation for the loop should be until
*         either Items are found or next page token is blank(search until the last page)
          CALL METHOD lo_client->list_objects
            EXPORTING
              iv_q_matchglob = search
              iv_p_bucket    = bucket
              iv_q_pagetoken = ls_output-next_page_token
            IMPORTING
              es_output      = ls_output
              ev_ret_code    = DATA(lv_ret_code)
              ev_err_text    = DATA(lv_err_text)
              es_err_resp    = DATA(ls_err_resp).
          APPEND ls_output-next_page_token TO lt_page_token.
          IF ls_output-items IS NOT INITIAL OR ls_output-next_page_token IS INITIAL.
            EXIT.
          ENDIF.
        ENDDO.


        IF lo_client->is_success( lv_ret_code ).
          LOOP AT ls_output-items ASSIGNING FIELD-SYMBOL(<ls_item>).
            ls_file-files = <ls_item>-name.
            APPEND ls_file TO files.
            APPEND INITIAL LINE TO spark_files ASSIGNING FIELD-SYMBOL(<fs_file>).
            <fs_file>-file = <ls_item>-name.
            <fs_file>-id = <ls_item>-id.
            <fs_file>-media_link = <ls_item>-media_link.
            <fs_file>-size = <ls_item>-size.
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
