*&---------------------------------------------------------------------*
*& Report zllm_migrate_version
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zllm_migrate_version.

PARAMETERS: p_app   TYPE zllm_app_id OBLIGATORY,
            p_bfunc TYPE zllm_buss_function_name OBLIGATORY,
            p_ver   TYPE zllm_version,
            p_beta  AS CHECKBOX, "Flag as beta version
            p_del   AS CHECKBOX, " Delete entries
            p_ctmpl AS CHECKBOX. " Generate config templates

PARAMETERS: p_delete AS CHECKBOX.

DATA: error TYPE boolean.




* Set this as the active version.
DATA: ls_version_control TYPE zllm_version_ctr.

ls_version_control-app_id = p_app.
ls_version_control-business_function = p_bfunc.
ls_version_control-dat = sy-datum.
ls_version_control-mandt = sy-mandt.
ls_version_control-time = sy-uzeit.
ls_version_control-version = p_ver.

IF p_beta = abap_true.
  ls_version_control-version_type = 'BETA'.
ELSE.

* Only none beta can be set as active
  ls_version_control-version_active = abap_true.
  ls_version_control-version_type = 'GLOBAL'.
ENDIF.




IF p_delete = abap_true.


* Update prompts table version numbers.
  SELECT * FROM zllm_prompts
  INTO TABLE @DATA(lt_prompts)
  WHERE app_id = @p_app
  AND business_function = @p_bfunc
  AND version = @p_ver.


  SELECT * FROM zllm_func_declar
  INTO TABLE @DATA(lt_func_declar)
  WHERE app_id = @p_app
  AND business_function = @p_bfunc
  AND version = @p_ver..


  SELECT * FROM zllm_func_parame
  INTO TABLE @DATA(lt_func_parame)
  WHERE app_id = @p_app
  AND business_function = @p_bfunc
  AND version = @p_ver.

  DELETE zllm_prompts FROM TABLE lt_prompts.
  DELETE zllm_func_declar FROM TABLE lt_func_declar.
  DELETE zllm_func_parame FROM TABLE lt_func_parame.
  DELETE zllm_version_ctr FROM ls_version_control.

  COMMIT WORK AND WAIT.


ELSEIF p_ctmpl IS NOT INITIAL.


  DATA: ls_config_template TYPE zllm_config_tepl,
        lt_config_template TYPE TABLE OF zllm_config_tepl.

  ls_config_template-mandt = sy-mandt.
  ls_config_template-app_id = p_app.
  ls_config_template-business_function = p_bfunc.
  ls_config_template-param = 'TEMPERATURE'.
  ls_config_template-value = '0.6'.
  ls_config_template-type = 'AGENT'.

  APPEND ls_config_template TO lt_config_template.

  ls_config_template-app_id = p_app.
  ls_config_template-business_function = p_bfunc.
  ls_config_template-param = 'NUMBER_RANGE_INTERVAL'.
  ls_config_template-value = '01'.
  ls_config_template-type = 'AGENT'.

  APPEND ls_config_template TO lt_config_template.

  ls_config_template-app_id = p_app.
  ls_config_template-business_function = p_bfunc.
  ls_config_template-param = 'TEMPERATURE'.
  ls_config_template-value = '0.3'.
  ls_config_template-type = 'GLOBAL'.

  APPEND ls_config_template TO lt_config_template.

  MODIFY zllm_config_tepl FROM TABLE lt_config_template.

ELSE.

* Update prompts table version numbers.
  SELECT * FROM zllm_prompts
  INTO TABLE @lt_prompts
  WHERE app_id = @p_app
  AND business_function = @p_bfunc.


  SELECT * FROM zllm_func_declar
  INTO TABLE @lt_func_declar
  WHERE app_id = @p_app
  AND business_function = @p_bfunc.


  SELECT * FROM zllm_func_parame
  INTO TABLE @lt_func_parame
  WHERE app_id = @p_app
  AND business_function = @p_bfunc.


* If another version exists, remove it from active state.
  SELECT SINGLE * FROM zllm_version_ctr
  INTO @DATA(ls_active_version)
  WHERE app_id = @p_app
  AND business_function = @p_bfunc
  AND version_active = @abap_true.

  IF sy-subrc = 0.
    ls_active_version-version_active = abap_false.
  ENDIF.

  LOOP AT lt_prompts ASSIGNING FIELD-SYMBOL(<fs_prompts>).

    <fs_prompts>-version = p_ver.

    IF p_del IS NOT INITIAL.
      <fs_prompts>-deletable = abap_true.
    ELSE.
      <fs_prompts>-deletable = abap_false.
    ENDIF.

  ENDLOOP.


  LOOP AT lt_func_declar ASSIGNING FIELD-SYMBOL(<fs_func_declar>).

    <fs_func_declar>-version = p_ver.

  ENDLOOP.


  LOOP AT lt_func_parame ASSIGNING FIELD-SYMBOL(<fs_func_parame>).

    <fs_func_parame>-version = p_ver.

  ENDLOOP.


  MODIFY zllm_prompts FROM TABLE lt_prompts.

  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    error = abap_false.
  ELSE.
    ROLLBACK WORK.
    error = abap_true.
  ENDIF.



  MODIFY zllm_func_declar FROM TABLE lt_func_declar.

  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    error = abap_false.
  ELSE.
    ROLLBACK WORK.
    error = abap_true.
  ENDIF.


  MODIFY zllm_func_parame FROM TABLE lt_func_parame.

  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    error = abap_false.
  ELSE.
    ROLLBACK WORK.
    error = abap_true.
  ENDIF.


  IF ls_active_version IS NOT INITIAL.
    MODIFY zllm_version_ctr FROM ls_active_version.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      error = abap_false.
    ELSE.
      ROLLBACK WORK.
      error = abap_true.
    ENDIF.

  ENDIF.


  MODIFY zllm_version_ctr FROM ls_version_control.

  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    error = abap_false.
  ELSE.
    ROLLBACK WORK.
    error = abap_true.
  ENDIF.

ENDIF.

IF error = abap_true.

  MESSAGE 'Prompt / Agent saving failed' TYPE 'E'.

ELSE.

  MESSAGE 'Prompt / Agent saving succeeded' TYPE 'S'.

ENDIF.
