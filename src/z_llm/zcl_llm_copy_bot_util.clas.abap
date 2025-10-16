CLASS zcl_llm_copy_bot_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: constructor IMPORTING from_app_id            TYPE zllm_app_id
                                   from_business_function TYPE zllm_buss_function_name
                                   to_app_id              TYPE zllm_app_id
                                   to_business_function   TYPE zllm_buss_function_name
                         RAISING   zcx_llm,
      copy_bot RAISING zcx_llm.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS: check_bot_config RETURNING VALUE(response) TYPE boolean,
      select_auths_roles,
      modify_auths_roles,
      save_auths_roles,

      select_config,
      modify_config,
      save_config,

      select_prompts,
      modify_prompts,
      save_prompts,

      select_func_declar,
      modify_func_declar,
      save_func_declar,

      select_bots,
      modify_bots,
      save_bots,

      select_version_ctr,
      modify_version_ctr,
      save_version_ctr,

      select_log,
      modify_log,
      save_log,

      select_func_parame,
      modify_func_parame,
      save_func_parame,

      select_func_index,
      modify_func_index,
      save_func_index.





    DATA: gv_from_app_id            TYPE zllm_app_id,
          gv_from_business_function TYPE zllm_buss_function_name,
          gv_to_app_id              TYPE zllm_app_id,
          gv_to_business_function   TYPE zllm_buss_function_name,
          gs_active_version         TYPE zllm_versions,
          gt_from_auths_roles       TYPE TABLE OF zllm_auths_roles,
          gt_to_auths_roles         TYPE TABLE OF zllm_auths_roles,
          gt_to_config              TYPE TABLE OF zllm_config,
          gt_from_config            TYPE TABLE OF zllm_config,
          gt_from_prompts           TYPE TABLE OF zllm_prompts,
          gt_to_prompts             TYPE TABLE OF zllm_prompts,
          gt_from_func_declar       TYPE TABLE OF zllm_func_declar,
          gt_to_func_declar         TYPE TABLE OF zllm_func_declar,
          gt_to_bots                TYPE TABLE OF zllm_bots,
          gt_from_bots              TYPE TABLE OF zllm_bots,
          gt_to_version_ctr         TYPE TABLE OF zllm_version_ctr,
          gt_from_version_ctr       TYPE TABLE OF zllm_version_ctr,
          gt_to_log                 TYPE TABLE OF zllm_log,
          gt_from_log               TYPE TABLE OF zllm_log,
          gt_to_func_parame         TYPE TABLE OF zllm_func_parame,
          gt_from_func_parame       TYPE TABLE OF zllm_func_parame,
          gt_to_func_index          TYPE TABLE OF zllm_func_index,
          gt_from_func_index        TYPE TABLE OF zllm_func_index.



ENDCLASS.



CLASS zcl_llm_copy_bot_util IMPLEMENTATION.


  METHOD constructor.

* assign basic attributes.
    gv_from_app_id = from_app_id.
    gv_from_business_function = from_business_function.

    gv_to_app_id = to_app_id.
    gv_to_business_function = to_business_function.

* Check the bot exists
    IF me->check_bot_config( ) = abap_false.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>bot_not_found.
    ENDIF.



* Get the current active version of the bot.
    DATA(lcl_active_version) = NEW zcl_llm_version_control_util( ).
    gs_active_version = lcl_active_version->get_active_app_version( app_id            = from_app_id
                                                                    business_function = from_business_function ).



  ENDMETHOD.


  METHOD check_bot_config.

    SELECT SINGLE *
    FROM zllm_bots
    INTO @DATA(ls_bots)
    WHERE app_id = @gv_from_app_id
    AND business_function = @gv_from_business_function.

* If a record was found, bot is good to begin copy process.
    IF sy-subrc = 0.
      response = abap_true.
    ELSE.
      response = abap_false.
    ENDIF.


  ENDMETHOD.

  METHOD copy_bot.

    TRY.

**********************************************************************
* Handle Auths and Roles
**********************************************************************
        me->select_auths_roles( ).
        me->modify_auths_roles(  ).
        me->save_auths_roles(  ).


**********************************************************************
* Handle config
**********************************************************************
        me->select_config( ).
        me->modify_config(  ).
        me->save_config(  ).


**********************************************************************
* Handle prompts
**********************************************************************
        me->select_prompts( ).
        me->modify_prompts(  ).
        me->save_prompts(  ).


**********************************************************************
* Handle function declarations
**********************************************************************
        me->select_func_declar( ).
        me->modify_func_declar(  ).
        me->save_func_declar(  ).


**********************************************************************
* Handle bots
**********************************************************************
        me->select_bots( ).
        me->modify_bots(  ).
        me->save_bots(  ).


**********************************************************************
* Handle version ctr
**********************************************************************
        me->select_version_ctr( ).
        me->modify_version_ctr(  ).
        me->save_version_ctr(  ).


**********************************************************************
* Handle log
**********************************************************************
        me->select_log( ).
        me->modify_log(  ).
        me->save_log(  ).


**********************************************************************
* Handle function parameter
**********************************************************************
        me->select_func_parame( ).
        me->modify_func_parame(  ).
        me->save_func_parame(  ).


**********************************************************************
* Handle function index
**********************************************************************
        me->select_func_index( ).
        me->modify_func_index(  ).
        me->save_func_index(  ).


**********************************************************************
* Commit work
**********************************************************************
        COMMIT WORK AND WAIT.

      CATCH cx_root INTO DATA(zcx_root).
* oops.
        ROLLBACK WORK.


    ENDTRY.
  ENDMETHOD.

  METHOD select_auths_roles.

    SELECT *
    FROM zllm_auths_roles
    INTO TABLE @me->gt_from_auths_roles
    WHERE app_id = @gv_from_app_id
    AND business_function = @gv_from_business_function.


  ENDMETHOD.


  METHOD modify_auths_roles.

* Assign new app id and business function to table structures.
    LOOP AT me->gt_from_auths_roles ASSIGNING FIELD-SYMBOL(<fs_from_auths_roles>).

      <fs_from_auths_roles>-app_id = gv_to_app_id.
      <fs_from_auths_roles>-business_function = gv_to_business_function.

      APPEND <fs_from_auths_roles> TO gt_to_auths_roles.


    ENDLOOP.

  ENDMETHOD.


  METHOD save_auths_roles.


    MODIFY zllm_auths_roles FROM TABLE gt_to_auths_roles.


  ENDMETHOD.

  METHOD select_prompts.

    SELECT *
    FROM zllm_prompts
    INTO TABLE @me->gt_from_prompts
    WHERE version = @gs_active_version-active_version
    AND app_id = @gv_from_app_id
    AND business_function = @gv_from_business_function.


  ENDMETHOD.


  METHOD modify_prompts.

* Assign new app id and business function to table structures.
    LOOP AT me->gt_from_prompts ASSIGNING FIELD-SYMBOL(<fs_from_prompts>).

      <fs_from_prompts>-app_id = gv_to_app_id.
      <fs_from_prompts>-business_function = gv_to_business_function.

      APPEND <fs_from_prompts> TO gt_to_prompts.


    ENDLOOP.

  ENDMETHOD.


  METHOD save_prompts.


    MODIFY zllm_prompts FROM TABLE gt_to_prompts.


  ENDMETHOD.


  METHOD select_config.

    SELECT *
    FROM zllm_config
    INTO TABLE @me->gt_from_config
    WHERE app_id = @gv_from_app_id
    AND buss_function = @gv_from_business_function.


  ENDMETHOD.


  METHOD modify_config.

* Assign new app id and business function to table structures.
    LOOP AT me->gt_from_config ASSIGNING FIELD-SYMBOL(<fs_from_config>).

      <fs_from_config>-app_id = gv_to_app_id.
      <fs_from_config>-buss_function = gv_to_business_function.

      APPEND <fs_from_config> TO gt_to_config.


    ENDLOOP.

  ENDMETHOD.


  METHOD save_config.


    MODIFY zllm_config FROM TABLE gt_to_config.


  ENDMETHOD.

  METHOD select_func_declar.

    SELECT *
    FROM zllm_func_declar
    INTO TABLE @me->gt_from_func_declar
    WHERE version = @gs_active_version-active_version
    AND app_id = @gv_from_app_id
    AND business_function = @gv_from_business_function.


  ENDMETHOD.


  METHOD modify_func_declar.

* Assign new app id and business function to table structures.
    LOOP AT me->gt_from_func_declar ASSIGNING FIELD-SYMBOL(<fs_from_func_declar>).

      <fs_from_func_declar>-app_id = gv_to_app_id.
      <fs_from_func_declar>-business_function = gv_to_business_function.

      APPEND <fs_from_func_declar> TO gt_to_func_declar.


    ENDLOOP.

  ENDMETHOD.


  METHOD save_func_declar.


    MODIFY zllm_func_declar FROM TABLE gt_to_func_declar.


  ENDMETHOD.

  METHOD select_bots.

    SELECT *
    FROM zllm_bots
    INTO TABLE @me->gt_from_bots
    WHERE app_id = @gv_from_app_id
    AND business_function = @gv_from_business_function.


  ENDMETHOD.


  METHOD modify_bots.

* Assign new app id and business function to table structures.
    LOOP AT me->gt_from_bots ASSIGNING FIELD-SYMBOL(<fs_from_bots>).

      <fs_from_bots>-app_id = gv_to_app_id.
      <fs_from_bots>-business_function = gv_to_business_function.

      APPEND <fs_from_bots> TO gt_to_bots.


    ENDLOOP.

  ENDMETHOD.


  METHOD save_bots.


    MODIFY zllm_bots FROM TABLE gt_to_bots.


  ENDMETHOD.

  METHOD select_version_ctr.

    SELECT *
    FROM zllm_version_ctr
    INTO TABLE @me->gt_from_version_ctr
    WHERE version = @gs_active_version-active_version
    AND app_id = @gv_from_app_id
    AND business_function = @gv_from_business_function.


  ENDMETHOD.


  METHOD modify_version_ctr.

* Assign new app id and business function to table structures.
    LOOP AT me->gt_from_version_ctr ASSIGNING FIELD-SYMBOL(<fs_from_version_ctr>).

      <fs_from_version_ctr>-app_id = gv_to_app_id.
      <fs_from_version_ctr>-business_function = gv_to_business_function.

      APPEND <fs_from_version_ctr> TO gt_to_version_ctr.


    ENDLOOP.

  ENDMETHOD.


  METHOD save_version_ctr.


    MODIFY zllm_version_ctr FROM TABLE gt_to_version_ctr.


  ENDMETHOD.


    METHOD select_log.

    SELECT *
    FROM zllm_log
    INTO TABLE @me->gt_from_log
    WHERE app_id = @gv_from_app_id
    AND buss_function = @gv_from_business_function.


  ENDMETHOD.


  METHOD modify_log.

* Assign new app id and business function to table structures.
    LOOP AT me->gt_from_log ASSIGNING FIELD-SYMBOL(<fs_from_log>).

      <fs_from_log>-app_id = gv_to_app_id.
      <fs_from_log>-buss_function = gv_to_business_function.

      APPEND <fs_from_log> TO gt_to_log.


    ENDLOOP.

  ENDMETHOD.


  METHOD save_log.


    MODIFY zllm_log FROM TABLE gt_to_log.


  ENDMETHOD.

  METHOD select_func_parame.

    SELECT *
    FROM zllm_func_parame
    INTO TABLE @me->gt_from_func_parame
    WHERE version = @gs_active_version-active_version
    AND app_id = @gv_from_app_id
    AND business_function = @gv_from_business_function.


  ENDMETHOD.


  METHOD modify_func_parame.

* Assign new app id and business function to table structures.
    LOOP AT me->gt_from_func_parame ASSIGNING FIELD-SYMBOL(<fs_from_func_parame>).

      <fs_from_func_parame>-app_id = gv_to_app_id.
      <fs_from_func_parame>-business_function = gv_to_business_function.

      APPEND <fs_from_func_parame> TO gt_to_func_parame.


    ENDLOOP.

  ENDMETHOD.


  METHOD save_func_parame.


    MODIFY zllm_func_parame FROM TABLE gt_to_func_parame.


  ENDMETHOD.

   METHOD select_func_index.

    SELECT *
    FROM zllm_func_index
    INTO TABLE @me->gt_from_func_index
    WHERE app_id = @gv_from_app_id
    AND business_function = @gv_from_business_function.


  ENDMETHOD.


  METHOD modify_func_index.

* Assign new app id and business function to table structures.
    LOOP AT me->gt_from_func_index ASSIGNING FIELD-SYMBOL(<fs_from_func_index>).

      <fs_from_func_index>-app_id = gv_to_app_id.
      <fs_from_func_index>-business_function = gv_to_business_function.

      APPEND <fs_from_func_index> TO gt_to_func_index.


    ENDLOOP.

  ENDMETHOD.


  METHOD save_func_index.


    MODIFY zllm_func_index FROM TABLE gt_to_func_index.


  ENDMETHOD.
ENDCLASS.
