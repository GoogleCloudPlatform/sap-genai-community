CLASS zcl_veo_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: constructor IMPORTING model TYPE string DEFAULT 'VERTEX'
                         RAISING   zcx_llm,

**********************************************************************
* Set the prompt to be used to generate the video
**********************************************************************
      set_prompt IMPORTING prompt TYPE string,

**********************************************************************
* Set config
**********************************************************************
      set_config IMPORTING gcs_uri      TYPE string
                           sample_count TYPE i
                           project      TYPE string DEFAULT 'tidy-jetty-162815'
                           location     TYPE string DEFAULT 'us-central1'
                           publisher_id TYPE string DEFAULT 'google'
                           model_id     TYPE string DEFAULT 'veo-2.0-generate-001',

**********************************************************************
* Call Veo model
**********************************************************************
      call_model.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        prompt TYPE string,
      END OF ty_instance,

      tt_instances TYPE STANDARD TABLE OF ty_instance,


      BEGIN OF ty_parameters,
        sample_count TYPE i,
        storage_uri  TYPE string,
      END OF ty_parameters.


    DATA: lo_client     TYPE REF TO /goog/cl_aiplatform_v1,
          parameters    TYPE zveo_parameters,
          gs_input      TYPE /goog/cl_aiplatform_v1=>ty_1013,
          gs_instance   TYPE ty_instance,
          gt_instances  TYPE tt_instances,
          gs_parameters TYPE ty_parameters,
          gv_raw        TYPE string.


ENDCLASS.


CLASS zcl_veo_util IMPLEMENTATION.


  METHOD constructor.

    TRY.
        lo_client = NEW /goog/cl_aiplatform_v1( iv_key_name = 'VERTEX' ).

      CATCH /goog/cx_sdk INTO DATA(lcx_sdk).
      CATCH zcx_llm INTO DATA(lcx_llm).

    ENDTRY.
  ENDMETHOD.


  METHOD call_model.

* Call API method: aiplatform.projects.locations.publishers.models.predictLongRunning
    CALL METHOD lo_client->predict_long_running_models
      EXPORTING
        iv_p_projects_id   = parameters-project
        iv_p_locations_id  = parameters-location
        iv_p_publishers_id = parameters-publisher_id
        iv_p_models_id     = parameters-model_id
        is_input           = gs_input
      IMPORTING
        es_raw             = gv_raw
        es_output          = DATA(ls_output)
        ev_ret_code        = DATA(lv_ret_code)
        ev_err_text        = DATA(lv_err_text)
        es_err_resp        = DATA(ls_err_resp).


  ENDMETHOD.

  METHOD set_config.

* Configure Veo parameters and also global parameters.
    gs_parameters-storage_uri = gcs_uri.
    gs_parameters-sample_count = sample_count.
    GET REFERENCE OF gs_parameters INTO gs_input-parameters.


* Global parameters for class
    parameters-gcs_uri = gcs_uri.
    parameters-sample_count = sample_count.
    parameters-project = project.
    parameters-location = location.
    parameters-model_id = model_id.
    parameters-publisher_id = publisher_id.

  ENDMETHOD.


  METHOD set_prompt.

    gs_instance-prompt = prompt.
    GET REFERENCE OF gt_instances INTO gs_input-instances.

  ENDMETHOD.



ENDCLASS.
