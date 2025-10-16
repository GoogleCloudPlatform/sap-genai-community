CLASS zcl_llm_version_control_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC

**********************************************************************
* Only friends can call change methods or methods relating to changes.
**********************************************************************
  GLOBAL FRIENDS zcl_fiori_llm_util
                 zcl_llm_agents_util
                 zcl_llm_bot_handler
                 zcl_llm_copy_bot_util.

  PUBLIC SECTION.

    METHODS:
**********************************************************************
* Method to return active app version
**********************************************************************
      get_active_app_version IMPORTING app_id             TYPE zllm_app_id
                                       !business_function TYPE zllm_buss_function_name
                             RETURNING VALUE(version)     TYPE zllm_versions
                             RAISING   zcx_llm,

**********************************************************************
* Method to return active function version
**********************************************************************
      get_active_function_version IMPORTING app_id            TYPE zllm_app_id
                                            business_function TYPE zllm_buss_function_name
                                  RETURNING VALUE(version)    TYPE zllm_version
                                  RAISING   zcx_llm,

**********************************************************************
* Method to return all versions for an app
**********************************************************************
      get_all_app_versions IMPORTING app_id            TYPE zllm_app_id
                                     business_function TYPE zllm_buss_function_name
                           RETURNING VALUE(version)    TYPE zllm_app_versions_tt
                           RAISING   zcx_llm.
  PROTECTED SECTION.
  PRIVATE SECTION.

**********************************************************************
* Method to get the number range interval to be used for this AI app
**********************************************************************
    METHODS: get_number_range_interval IMPORTING app_id            TYPE zllm_app_id
                                                 business_function TYPE zllm_buss_function_name
                                                 type              TYPE zllm_config_type
                                       RETURNING VALUE(interval)   TYPE string
                                       RAISING   zcx_llm,

**********************************************************************
* Method to check if an existing BETA version exists for this AI app
**********************************************************************
      check_for_beta IMPORTING app_id            TYPE zllm_app_id
                               business_function TYPE zllm_buss_function_name
                     RETURNING VALUE(version)    TYPE zllm_version,

**********************************************************************
* Method to build prompting structures before moving to a new version
**********************************************************************
      build_prompts IMPORTING app_id            TYPE zllm_app_id
                              business_function TYPE zllm_buss_function_name
                              version           TYPE zllm_version
                    RAISING   zcx_llm,

**********************************************************************
* Method to build function call structures before moving to new version
**********************************************************************
      build_function_call  IMPORTING app_id            TYPE zllm_app_id
                                     business_function TYPE zllm_buss_function_name
                                     version           TYPE zllm_version
                           RAISING   zcx_llm,

**********************************************************************
* Build new version control record.
**********************************************************************
      build_version_control IMPORTING app_id            TYPE zllm_app_id
                                      business_function TYPE zllm_buss_function_name
                                      version           TYPE zllm_version
                            RAISING   zcx_llm,

**********************************************************************
* Method to get the next version number
**********************************************************************
      get_next_version IMPORTING app_id            TYPE zllm_app_id
                                 business_function TYPE zllm_buss_function_name
                       RETURNING VALUE(version)    TYPE string
                       RAISING   zcx_llm,

**********************************************************************
* Move prompt records to new version ID
**********************************************************************
      set_new_prompt_version IMPORTING version TYPE    zllm_version,

**********************************************************************
* Move function declaration records to new version ID
**********************************************************************
      set_new_func_declar_version IMPORTING version TYPE    zllm_version,

**********************************************************************
* Move func declar parame records to new version ID
**********************************************************************
      set_new_func_parame_version IMPORTING version TYPE    zllm_version,

**********************************************************************
* Method to create a new app version
**********************************************************************
      create_new_app_version IMPORTING app_id            TYPE zllm_app_id
                                       business_function TYPE zllm_buss_function_name
                             RETURNING VALUE(version)    TYPE zllm_version
                             RAISING   zcx_llm,

**********************************************************************
* Method to change active version.
**********************************************************************
      set_app_active_version IMPORTING app_id            TYPE zllm_app_id
                                       business_function TYPE zllm_buss_function_name
                                       version           TYPE zllm_version
                                       model_id          TYPE zllm_model_id OPTIONAL
                             RETURNING VALUE(status)     TYPE boolean
                             RAISING   zcx_llm,

**********************************************************************
* Method to check if version information is valid.
**********************************************************************
      is_valid_version IMPORTING app_id            TYPE zllm_app_id
                                 business_function TYPE zllm_buss_function_name
                                 version           TYPE zllm_version
                       RETURNING VALUE(valid)      TYPE boolean
                       RAISING   zcx_llm,

      set_user_version IMPORTING app_id            TYPE zllm_app_id
                                 business_function TYPE zllm_buss_function_name
                                 version           TYPE zllm_version
                       RETURNING VALUE(status)     TYPE boolean
                       RAISING   zcx_llm,
      delete_user_version IMPORTING app_id            TYPE zllm_app_id
                                    business_function TYPE zllm_buss_function_name
                                    version           TYPE zllm_version
                                    all               TYPE boolean OPTIONAL
                          RETURNING VALUE(status)     TYPE boolean
                          RAISING   zcx_llm,

**********************************************************************
* Method to set and delete
**********************************************************************
      set_user_model   IMPORTING app_id            TYPE zllm_app_id
                                 business_function TYPE zllm_buss_function_name
                                 version           TYPE zllm_version
                                 model_id          TYPE zllm_model_id
                       RETURNING VALUE(status)     TYPE boolean
                       RAISING   zcx_llm,
      delete_user_model IMPORTING app_id            TYPE zllm_app_id
                                  business_function TYPE zllm_buss_function_name
                                  version           TYPE zllm_version
                                  model_id          TYPE zllm_model_id
                        RETURNING VALUE(status)     TYPE boolean
                        RAISING   zcx_llm,

**********************************************************************
* Method to build agent map structure before moving to new version
**********************************************************************
      build_agent_map      IMPORTING app_id            TYPE zllm_app_id
                                     business_function TYPE zllm_buss_function_name
                                     version           TYPE zllm_version
                           RAISING   zcx_llm,

**********************************************************************
* Move agent map to new version ID
**********************************************************************
      set_new_agent_map_version IMPORTING version TYPE    zllm_version.



* Private data
**********************************************************************
    DATA: gt_prompts         TYPE TABLE OF zllm_prompts,
          gt_func_declar     TYPE TABLE OF zllm_func_declar,
          gt_func_parame     TYPE TABLE OF zllm_func_parame,
          gs_version_control TYPE zllm_version_ctr,
          gt_agent_map       TYPE STANDARD TABLE OF zllm_agent_map.

ENDCLASS.



CLASS zcl_llm_version_control_util IMPLEMENTATION.



  METHOD delete_user_version.

    DATA: ls_user_version TYPE zllm_usr_version.
* Select everyone from the user version table for this app ID and business function.

* If all is true, then delete this version for all users.
    IF all = abap_false.

      SELECT *
      FROM zllm_usr_version
      INTO TABLE @DATA(lt_user_version)
      WHERE business_function = @business_function
      AND app_id = @app_id
      AND version = @version
      AND usr = @sy-uname.


    ELSE.

      SELECT *
      FROM zllm_usr_version
      INTO TABLE @lt_user_version
      WHERE business_function = @business_function
      AND app_id = @app_id
      AND version = @version.

    ENDIF.

    DELETE zllm_usr_version FROM TABLE lt_user_version.


    IF sy-subrc <> 0.

      status = abap_false.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>failed_to_update_user_version
          msg1   = CONV string( sy-uname ).

    ELSE.
      COMMIT WORK AND WAIT.
      status = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD set_user_version.

    DATA: ls_user_version TYPE zllm_usr_version.

    ls_user_version-mandt = sy-mandt.
    ls_user_version-business_function = business_function.
    ls_user_version-app_id = app_id.
    ls_user_version-usr = sy-uname.
    ls_user_version-version = version.

    MODIFY zllm_usr_version FROM ls_user_version.


    IF sy-subrc <> 0.

      status = abap_false.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>failed_to_update_user_version
          msg1   = CONV string( sy-uname ).

    ELSE.
      COMMIT WORK AND WAIT.
      status = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD set_app_active_version.

* Check that the app id / business function and version are valid.
    IF me->is_valid_version( app_id = app_id
                             business_function = business_function
                             version = version ) = abap_true.

      TRY.
          DATA(lcl_llm_crud_util) = NEW zcl_llm_crud_control_util( app_id            = app_id
                                                                   business_function = business_function ).


          IF model_id IS INITIAL.
* Save as active version.
            lcl_llm_crud_util->save_app_active_version( version = version ).

          ELSE.

            lcl_llm_crud_util->save_model_per_version( version  = version
                                                       model_id = model_id ).

          ENDIF.

          me->delete_user_version( app_id            = app_id
                                   business_function = business_function
                                   version           = version
                                   all               = abap_true ).


        CATCH zcx_llm INTO DATA(lcx_llm).
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.
      ENDTRY.

    ELSE.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>invalid_version
          msg1   = CONV string( version ).
    ENDIF.


  ENDMETHOD.


  METHOD is_valid_version.

    TRY.

        DATA(versions) = me->get_all_app_versions( app_id            = app_id
                                                   business_function = business_function ).
* Select the version we are interested in
        DATA(valid_version) = versions[ version = version ].

        valid = abap_true.

      CATCH zcx_llm INTO DATA(zcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>failed_to_determine_version
            msg1   = CONV string( app_id )
            msg2   = CONV string( business_function ).


      CATCH cx_sy_itab_line_not_found.
* If the line was not found.
        valid = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD get_all_app_versions.

* If nothing found from the select, that is ok.
    SELECT version_type, version, version_active, dat, time, model_id
    FROM zllm_version_ctr
    INTO CORRESPONDING FIELDS OF TABLE @version
    WHERE app_id = @app_id
    AND business_function = @business_function.

  ENDMETHOD.


  METHOD check_for_beta.

* If nothing is found this is ok.
    SELECT SINGLE version
    FROM zllm_version_ctr
    INTO @version
    WHERE app_id = @app_id
    AND business_function = @business_function
    AND version_type = 'BETA'
    AND version_active = @abap_false.


  ENDMETHOD.


  METHOD get_active_app_version.


* Version type must be GLOBAL to get current active version for an app
    SELECT SINGLE version,
                  model_id
    FROM zllm_version_ctr
    INTO @DATA(ls_active_version)
    WHERE app_id = @app_id
    AND business_function = @business_function
    AND version_type = 'GLOBAL'
    AND version_active = @abap_true.


* Get any user versions.
    SELECT SINGLE version,
                  model_id
    FROM zllm_usr_version
    INTO @DATA(ls_user_version)
    WHERE app_id = @app_id
    AND business_function = @business_function
    AND usr = @sy-uname.



* Get user version
    version-active_version = ls_active_version-version.
    version-user_version = ls_user_version-version.
    version-beta_version = me->check_for_beta( app_id            = app_id
                                               business_function = business_function ).


* Get user model
    version-active_model = ls_active_version-model_id.
    version-user_model = ls_user_version-model_id.

*   If both active and beta versions don't exist for the app ID and business function,
*   raise an error
    IF version-active_version IS INITIAL AND
       version-beta_version IS INITIAL.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>no_active_app_version
          msg1   = CONV string( app_id )
          msg2   = CONV string( business_function ).

    ENDIF.

  ENDMETHOD.


  METHOD build_version_control.

    SELECT SINGLE *
    FROM zllm_version_ctr
    INTO @gs_version_control
    WHERE app_id = @app_id
    AND business_function = @business_function
    AND version = @version
    AND version_type = 'GLOBAL'.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>no_version_control_record
          msg1   = CONV string( app_id )
          msg2   = CONV string( business_function ).
    ENDIF.

  ENDMETHOD.


  METHOD get_active_function_version.

* Version type must be FUNCTION to get current active version of a function
    SELECT SINGLE version
    FROM zllm_version_ctr
    INTO @version
    WHERE app_id = @app_id
    AND business_function = @business_function
    AND version_type = 'FUNCTION'
    AND version_active = @abap_true.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>no_active_function_version
          msg1   = CONV string( app_id )
          msg2   = CONV string( business_function ).
    ENDIF.
  ENDMETHOD.


  METHOD create_new_app_version.

* Check if an existing BETA version exists.
    DATA(beta_version) = me->check_for_beta( app_id            = app_id
                                             business_function = business_function ).

    IF beta_version > 0.
* Existing Beta version exists.
* Do not create another version until Beta is moved to active status.
* Return instead with beta version

      version = beta_version.

    ELSE.

      TRY.

          DATA(active_version) = me->get_active_app_version( app_id            = app_id
                                                             business_function = business_function ).

*         Get the current active version.
          version = active_version-active_version.

*       If an exception is caught, it means that the there's no active version for the bot(App ID and business function combination)
*       This could mean that a new bot has been created
        CATCH zcx_llm INTO DATA(lcx_llm).

          version = '0'.

      ENDTRY.


      TRY.

* Build prompt structures using current active version.
          IF version <> 0.

            me->build_prompts( app_id            = app_id
                               business_function = business_function
                               version           = version ).

* Build function call structures using current active version.
            me->build_function_call( app_id            = app_id
                                     business_function = business_function
                                     version           = version ).

* Build agent map structure using current active version
            me->build_agent_map( app_id            = app_id
                                 business_function = business_function
                                 version           = version ).

* Build version control record.
            me->build_version_control( app_id            = app_id
                                       business_function = business_function
                                       version           = version ).

          ENDIF.



* Get the next version number in number range.
          DATA(next_version) = me->get_next_version( app_id            = app_id
                                                     business_function = business_function ).

          gs_version_control-app_id = app_id.
          gs_version_control-business_function = business_function.
          gs_version_control-version = CONV zllm_version( next_version ).
          gs_version_control-version_type = 'BETA'.
          gs_version_control-version_active = abap_false.
          gs_version_control-usr = sy-uname.
          gs_version_control-dat = sy-datum.
          gs_version_control-time = sy-uzeit.


* Move records to new version ID.
          me->set_new_func_declar_version( version = CONV zllm_version( next_version ) ).
          me->set_new_func_parame_version( version = CONV zllm_version( next_version ) ).
          me->set_new_prompt_version(  version = CONV zllm_version( next_version ) ).
          me->set_new_agent_map_version( version = CONV zllm_version( next_version ) ).

* Save new version.
          DATA(lcl_llm_crud_util) = NEW zcl_llm_crud_control_util( app_id            = app_id
                                                                   business_function = business_function ).

          DATA(save_status) = lcl_llm_crud_util->save_app_version( prompt       = me->gt_prompts
                                                                   func_declar  = me->gt_func_declar
                                                                   func_parame  = me->gt_func_parame
                                                                   version_ctrl = me->gs_version_control
                                                                   agent_map    = me->gt_agent_map ).

* Commit as LUW.
          IF save_status = abap_true.
* Done, version updated. Return new version to calling method.
            version = next_version.
          ENDIF.

        CATCH zcx_llm INTO lcx_llm.
* If we cannot find the current active version, we cannot move to a new version.
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.
      ENDTRY.
    ENDIF.


  ENDMETHOD.


  METHOD set_new_prompt_version.

    LOOP AT gt_prompts ASSIGNING FIELD-SYMBOL(<fs_prompts>).

* Update version
      <fs_prompts>-version = version.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_new_func_declar_version.


    LOOP AT gt_func_declar ASSIGNING FIELD-SYMBOL(<fs_func_declar>).

* Update version
      <fs_func_declar>-version = version.

    ENDLOOP.


  ENDMETHOD.


  METHOD set_new_func_parame_version.

    LOOP AT gt_func_parame ASSIGNING FIELD-SYMBOL(<fs_func_parame>).

* Update version
      <fs_func_parame>-version = version.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_next_version.

    TRY.
        DATA(interval_object) = me->get_number_range_interval( app_id            = app_id
                                                               business_function = business_function
                                                               type              = 'GLOBAL' ).

* Get next version ID
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = CONV char2( interval_object )
            object                  = 'ZLLM'
          IMPORTING
            number                  = version
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>version_interval_failed
              msg1   = CONV string( app_id )
              msg2   = CONV string( business_function ).
        ENDIF.


      CATCH zcx_llm INTO DATA(lcx_llm).
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2.
    ENDTRY.

  ENDMETHOD.


  METHOD build_prompts.

    SELECT *
    FROM zllm_prompts
    INTO TABLE @gt_prompts
    WHERE app_id = @app_id
    AND business_function = @business_function
    AND version = @version.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>no_prompt_record
          msg1   = CONV string( version )
          msg2   = CONV string( app_id )
          msg3   = CONV string( business_function ).
    ENDIF.

  ENDMETHOD.


  METHOD build_function_call.

* Get the function declarations.
    SELECT *
    FROM zllm_func_declar
    INTO TABLE @gt_func_declar
    WHERE app_id = @app_id
    AND business_function = @business_function
    AND version = @version.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>no_function_record
          msg1   = CONV string( version )
          msg2   = CONV string( app_id )
          msg3   = CONV string( business_function ).

    ENDIF.

* Get the parameters for function declarations.
    SELECT *
    FROM zllm_func_parame
    INTO TABLE @gt_func_parame
    WHERE app_id = @app_id
    AND business_function = @business_function
    AND version = @version.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>no_function_record
          msg1   = CONV string( version )
          msg2   = CONV string( app_id )
          msg3   = CONV string( business_function ).

    ENDIF.

  ENDMETHOD.


  METHOD get_number_range_interval.

    DATA(lcl_fiori_llm_util) = NEW zcl_fiori_llm_util(  ).

    TRY.
        interval = lcl_fiori_llm_util->get_config( app_id            = app_id
                                                   business_function = business_function
                                                   param             = 'VER_NUMBER_RANGE_INTERVAL'
                                                   type              = 'GLOBAL' ).
      CATCH zcx_llm INTO DATA(lcx_llm).
* Oops, no number range interval defined.
        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2.

    ENDTRY.
  ENDMETHOD.
  METHOD delete_user_model.

    DATA: ls_user_version TYPE zllm_usr_version.

    ls_user_version-mandt = sy-mandt.
    ls_user_version-business_function = business_function.
    ls_user_version-app_id = app_id.
    ls_user_version-usr = sy-uname.
    ls_user_version-version = version.
    ls_user_version-model_id = model_id.

    DELETE zllm_usr_version FROM ls_user_version.


    IF sy-subrc <> 0.

      status = abap_false.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>failed_to_update_user_version
          msg1   = CONV string( sy-uname ).

    ELSE.
      COMMIT WORK AND WAIT.
      status = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD set_user_model.

    DATA: ls_user_version TYPE zllm_usr_version.

    ls_user_version-mandt = sy-mandt.
    ls_user_version-business_function = business_function.
    ls_user_version-app_id = app_id.
    ls_user_version-usr = sy-uname.
    ls_user_version-version = version.
    ls_user_version-model_id = model_id.

    MODIFY zllm_usr_version FROM ls_user_version.


    IF sy-subrc <> 0.

      status = abap_false.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>failed_to_update_user_version
          msg1   = CONV string( sy-uname ).

    ELSE.
      COMMIT WORK AND WAIT.
      status = abap_true.
    ENDIF.

  ENDMETHOD.



  METHOD build_agent_map.

*   Get agent mappings on availability
    SELECT *
    FROM zllm_agent_map
    INTO TABLE @gt_agent_map
    WHERE app_id = @app_id
    AND business_function = @business_function
    AND version = @version.

*   Not raising an exception as not all agents need to necessarily have mappings
    IF sy-subrc <> 0.

    ENDIF.

  ENDMETHOD.

  METHOD set_new_agent_map_version.

    LOOP AT gt_agent_map ASSIGNING FIELD-SYMBOL(<fs_agent_map>).

*     Update version
      <fs_agent_map>-version = version.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
