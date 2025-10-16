CLASS zcl_llm_template_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: constructor IMPORTING app_id            TYPE zllm_app_id OPTIONAL
                                   business_function TYPE zllm_buss_function_name OPTIONAL
                         RAISING   zcx_llm,

**********************************************************************
* Get the application stack
**********************************************************************
      get_app_stack RETURNING VALUE(stackline) TYPE string,

**********************************************************************
* Get app and business function from memory location
**********************************************************************
      get_memory_id IMPORTING memory_id           TYPE char50
                    RETURNING VALUE(memory_value) TYPE char50,

**********************************************************************
* Get system instructions
**********************************************************************
      get_system_instructions RETURNING VALUE(response) TYPE string,


**********************************************************************
* Get parameters from a function call
* If the agent name is provided, we will lookup the requested
* params from the ZLLM_FUNC_PARAME table
* perform_llm_clean_up will use a simple model call to remove any
* special chars that might exists in the LLM response
* Specific param will get 1 param from the list provided.
**********************************************************************
      get_parameters IMPORTING agent_name           TYPE zllm_function_name OPTIONAL
                               perform_llm_clean_up TYPE boolean DEFAULT abap_false
                               parameters           TYPE /goog/t_function_parameters
                               specific_param       TYPE string OPTIONAL
                     EXPORTING param                TYPE zllm_fc_parameters
                     RETURNING VALUE(params)        TYPE zllm_fc_parameters_tt.


  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS: get_current_time RETURNING VALUE(response) TYPE string,
      get_current_date RETURNING VALUE(response) TYPE string,
      get_current_day  RETURNING VALUE(response) TYPE string,
      get_week_days    RETURNING VALUE(response) TYPE string,
      get_weekends     RETURNING VALUE(response) TYPE string,
      get_week_number  RETURNING VALUE(response) TYPE string.


    DATA: gv_app_id            TYPE zllm_app_id,
          gv_business_function TYPE zllm_buss_function_name,
          gv_version           TYPE zllm_version.

ENDCLASS.



CLASS zcl_llm_template_util IMPLEMENTATION.

  METHOD constructor.

    IF app_id IS NOT INITIAL AND
       business_function IS NOT INITIAL.

      me->gv_app_id = app_id.
      me->gv_business_function = business_function.

      TRY.
          DATA(lcl_llm_version_controller) = NEW zcl_llm_version_control_util(  ).
          DATA(versions) = lcl_llm_version_controller->get_active_app_version( app_id            = me->gv_app_id
                                                                               business_function = me->gv_business_function ).

* User version will always override current version.
          IF versions-user_version IS NOT INITIAL.
            me->gv_version = versions-user_version.
          ELSE.
            me->gv_version = versions-active_version.
          ENDIF.

        CATCH zcx_llm.

          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>invalid_app_id
              msg1   = CONV string( app_id ).

      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD get_system_instructions.

    response = me->get_current_time(  ).
    response = |{ response }\n{ me->get_current_date(  ) }|.
    response = |{ response }\n{ me->get_current_day(  ) }|.
    response = |{ response }\n{ me->get_week_days(  ) }|.
    response = |{ response }\n{ me->get_weekends(  ) }|.
    response = |{ response }\n{ me->get_week_number(  ) }|.

  ENDMETHOD.


  METHOD get_current_time.
    response = |<CURRENT_TIME>{ sy-uzeit }</CURRENT_TIME>|.
  ENDMETHOD.


  METHOD get_current_date.

    DATA: lv_external_date TYPE string.

    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        date_internal            = sy-datum
      IMPORTING
        date_external            = lv_external_date
      EXCEPTIONS
        date_internal_is_invalid = 1
        OTHERS                   = 2.

    response = |<CURRENT_DATE>{ lv_external_date }</CURRENT_DATE>|.

  ENDMETHOD.

  METHOD get_week_number.

    DATA: lv_week_number TYPE scal-week.

    CALL FUNCTION 'DATE_GET_WEEK'
      EXPORTING
        date         = sy-datum
      IMPORTING
        week         = lv_week_number
      EXCEPTIONS
        date_invalid = 1
        OTHERS       = 2.


    response = |<WEEK_NUMBER>{ lv_week_number+4(2) }</WEEK_NUMBER>|.

  ENDMETHOD.

  METHOD get_current_day.

    DATA: lv_day_number TYPE hrvsched-daynr,
          lv_day_name   TYPE hrvsched-daytxt.


    CALL FUNCTION 'HRIQ_GET_DATE_DAYNAME'
      EXPORTING
        langu               = sy-langu
        date                = sy-datum
      IMPORTING
        daynr               = lv_day_number
        daytxt              = lv_day_name
      EXCEPTIONS
        no_langu            = 1
        no_date             = 2
        no_daytxt_for_langu = 3
        invalid_date        = 4
        OTHERS              = 5.

    response = |<CURRENT_DAY> \n Day: { lv_day_name } \n Day number: { lv_day_number } \n </CURRENT_DAY>|.


  ENDMETHOD.

  METHOD get_week_days.
    response = |<WEEK_DAYS> \n * Monday \n * Tuesday \n * Wednesday \n * Thursday \n * Friday \n </WEEK_DAYS>|.
  ENDMETHOD.

  METHOD get_weekends.
    response = |<WEEKENDS> \n * Saturday \n * Sunday \n </WEEKENDS>|.
  ENDMETHOD.

  METHOD get_parameters.


    DATA: ls_params TYPE zllm_fc_parameters.

    IF specific_param IS NOT INITIAL.

      DATA(ls_single_param) = parameters[ parameter_name = specific_param ].

      param-checked = abap_false.
      param-param_name = ls_single_param-parameter_name.
      param-param_value = ls_single_param-parameter_value.


    ELSE.

      IF agent_name IS NOT INITIAL.

* Fetch only the requested parameters.
        SELECT parameter_name
        FROM zllm_func_parame
        INNER JOIN zllm_func_declar ON zllm_func_declar~function_parameters_id = zllm_func_parame~parameters_id
        WHERE zllm_func_declar~version = @me->gv_version
        AND zllm_func_declar~function_name = @agent_name
        INTO TABLE @DATA(lt_param_name).

      ENDIF.


      LOOP AT parameters ASSIGNING FIELD-SYMBOL(<fs_parameters>).

        IF lt_param_name IS NOT INITIAL.

          TRY.
* Check if the current parameter exists in the agent config.
              DATA(ls_param_name) = lt_param_name[ parameter_name = <fs_parameters>-parameter_name ].

* Assuming yes as no error.
              ls_params-checked = abap_false.
              ls_params-param_name = <fs_parameters>-parameter_name.
              ls_params-param_value = <fs_parameters>-parameter_value.

            CATCH cx_sy_itab_line_not_found.
          ENDTRY.


        ELSE.
          ls_params-checked = abap_false.
          ls_params-param_name = <fs_parameters>-parameter_name.
          ls_params-param_value = <fs_parameters>-parameter_value.

        ENDIF.

* Ensure we condense.
        CONDENSE ls_params-param_name NO-GAPS.
*        CONDENSE ls_params-param_value NO-GAPS.

        APPEND ls_params TO params.

      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD get_memory_id.

    DATA : lv_memory_id       TYPE char50.

    IMPORT p1 = memory_value FROM MEMORY ID memory_id.


  ENDMETHOD.

  METHOD get_app_stack.

* Get the function module name. Due to the nature of ABAP call stack using system functions doesnt work.
    DATA: lt_callstack TYPE abap_callstack,
          ls_callstack TYPE abap_callstack_line,
          lv_fname     TYPE rs38l_fnam.

* Get the ABAP stack.
    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level = 0
      IMPORTING
        callstack = lt_callstack.


    TRY.

        stackline = lt_callstack[ 2 ]-blockname.

      CATCH cx_sy_itab_line_not_found.
    ENDTRY.


  ENDMETHOD.

ENDCLASS.
