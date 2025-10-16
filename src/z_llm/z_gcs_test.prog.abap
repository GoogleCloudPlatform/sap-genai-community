*&---------------------------------------------------------------------*
*& Report z_gcs_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_gcs_test.

DATA: lcl_test TYPE REF TO zcl_cs_hub_llm_util,
      lt_spark_files TYPE zllm_gcs_spark_file.

DATA(lcl_gcs_util) = NEW zcl_gcs_util(  ).


DATA: bucket TYPE string,
      search TYPE string.

bucket = 'corby-spark-video'.
search = '**/*UK264344673A*'.  "'**/*UK242544245*'.


DATA(files) = lcl_gcs_util->search_bucket( EXPORTING bucket      = bucket
                                                     search      = search
                                           IMPORTING spark_files = lt_spark_files ).

IF lt_spark_files IS NOT INITIAL.
  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = DATA(lref_salv)                          " Basis Class Simple ALV Tables
    CHANGING
      t_table      = lt_spark_files
  ).

  lref_salv->display( ).

ENDIF.
