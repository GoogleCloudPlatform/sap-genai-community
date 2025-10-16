*&---------------------------------------------------------------------*
*& Report zrep_llm_edit_functions_dec
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrep_llm_edit_functions_dec.

DATA: lt_text                TYPE TABLE OF char255,
      lt_text2               TYPE TABLE OF char255,
      ok_code                TYPE sy-ucomm, " Holds the function code (e.g., for Save or Exit)
      go_textedit            TYPE REF TO cl_gui_textedit,  " Reference to the Text-Edit control
      go_custom_container    TYPE REF TO cl_gui_custom_container,  " Container for the control
      go_textedit2           TYPE REF TO cl_gui_textedit,  " Reference to the Text-Edit control
      go_custom_container2   TYPE REF TO cl_gui_custom_container,  " Container for the control
      go_custom_container3   TYPE REF TO cl_gui_custom_container,  " Container for the control
      go_alv_grid            TYPE REF TO cl_gui_alv_grid,
      lv_fd_in_string        TYPE string,
      ls_fd_tab              TYPE zllm_func_declar,
      lv_ans                 TYPE char1,
      lv_ans1                TYPE char1,
      lv_ans2                TYPE char1,
      ls_func_declar         TYPE zllm_func_declar,
      lt_func_param          TYPE TABLE OF zllm_func_parame,
      lt_func_param_ory      TYPE TABLE OF zllm_func_parame,
      ls_func_param          TYPE zllm_func_parame,
      lt_func_params_alv     TYPE TABLE OF zst_llm_func_parameters_alv,
      lv_paramdesc_in_string TYPE string,
      lv_parameter_name_inp  TYPE zllm_f_parameter_name,
      lv_parameter_type_inp  TYPE zllm_f_parameter_type,
      lv_parameter_mand_inp  TYPE zllm_is_required.




* Define a local class for event handling - to capture the mouse click
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: on_double_click FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
    " Read the data from the selected row
    TRY.
        DATA(ls_func_param_clicked) = lt_func_param[ e_row ].

        " Pre-populate data
        lv_parameter_name_inp = ls_func_param_clicked-parameter_name.
        lv_parameter_type_inp = ls_func_param_clicked-parameter_type.
        lv_parameter_mand_inp = ls_func_param_clicked-is_required.


        SPLIT ls_func_param_clicked-parameter_description AT cl_abap_char_utilities=>newline INTO TABLE lt_text2.
        " Load text into the text-edit control
        go_textedit2->set_text_as_r3table( EXPORTING table = lt_text2 ).

        "Refresh the screen
        LEAVE TO SCREEN 200.

      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

* Declare event handler object
DATA: go_event_handler TYPE REF TO lcl_event_handler.

SELECTION-SCREEN BEGIN OF BLOCK a0 WITH FRAME TITLE TEXT-000.
  PARAMETERS: p_crt RADIOBUTTON GROUP x1,
              p_upd RADIOBUTTON GROUP x1 DEFAULT 'X',
              p_del RADIOBUTTON GROUP x1,
              p_aid TYPE string DEFAULT 'CSHUB',
              p_bfu TYPE string DEFAULT 'EMAIL',
              p_fnm TYPE string.

  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN PUSHBUTTON 33(20) TEXT-001 USER-COMMAND button_click_fd. "Function declarations
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN PUSHBUTTON 33(20) TEXT-002 USER-COMMAND button_click_fp. " Function parameters
SELECTION-SCREEN END OF BLOCK a0.



AT SELECTION-SCREEN.

  IF sy-ucomm = 'BUTTON_CLICK_FD'.
    CLEAR ok_code.

    IF p_fnm IS INITIAL.
      MESSAGE TEXT-003 TYPE 'I'.
      EXIT.
    ENDIF.

    IF p_upd = abap_true OR p_del = abap_true.

      SELECT SINGLE *
      FROM zllm_func_declar
      INTO @ls_func_declar
      WHERE app_id = @p_aid
      AND business_function = @p_bfu
      AND function_name = @p_fnm.

    ENDIF.

    CALL SCREEN 100.

  ELSEIF sy-ucomm = 'BUTTON_CLICK_FP'.

    CLEAR ok_code.

    IF p_fnm IS INITIAL.
      MESSAGE TEXT-003 TYPE 'I'.
      EXIT.
    ENDIF.


    " If lt_func_param is not initial means that this is re-entry to the screen to correct the values.
    IF lt_func_param IS INITIAL OR p_fnm <> ls_func_declar-function_name.

      IF p_upd = abap_true OR p_del = abap_true.

        IF ls_func_declar IS INITIAL.

          SELECT SINGLE *
          FROM zllm_func_declar
          INTO @ls_func_declar
          WHERE app_id = @p_aid
          AND business_function = @p_bfu
          AND function_name = @p_fnm.

        ENDIF.

        SELECT zllm_func_parame~*
        FROM zllm_func_parame
        INNER JOIN zllm_func_declar
        ON zllm_func_declar~function_parameters_id = zllm_func_parame~parameters_id
        INTO TABLE @lt_func_param
        WHERE zllm_func_declar~app_id = @p_aid
        AND zllm_func_declar~business_function = @p_bfu
        AND zllm_func_declar~function_name = @p_fnm.

        lt_func_param_ory = lt_func_param.

      ENDIF.

    ENDIF.

    CALL SCREEN 200.

  ELSEIF sy-ucomm = 'ONLI'.

    IF p_fnm IS INITIAL.

      MESSAGE TEXT-003 TYPE 'I'.
      EXIT.

    ENDIF.


* If Create
    IF p_crt = abap_true.

      IF p_fnm IS INITIAL.
        MESSAGE TEXT-003 TYPE 'I'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Function declaration create alert'
          text_question         = |You are about to create the function declaration { p_fnm }. Are you sure?|
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = ''
        IMPORTING
          answer                = lv_ans1
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF lv_ans1 = 1.

        ls_fd_tab-mandt = sy-mandt.
        ls_fd_tab-app_id = p_aid.
        ls_fd_tab-business_function = p_bfu.
        ls_fd_tab-function_name = p_fnm.
        ls_fd_tab-function_description = lv_fd_in_string.

* Get next parameter ID
* Find the last function ID and assign next number.
        SELECT MAX( function_parameters_id )
          INTO @DATA(lv_highest_parameter_val)
          FROM zllm_func_declar.

        ls_fd_tab-function_parameters_id = lv_highest_parameter_val + 1.

        MODIFY zllm_func_declar FROM ls_fd_tab.

        IF sy-subrc <> 0.

          MESSAGE TEXT-005 TYPE 'I'.

        ELSE.

          IF lt_func_param IS NOT INITIAL.

            MODIFY zllm_func_parame FROM TABLE lt_func_param.

          ENDIF.

          IF sy-subrc <> 0.
            ROLLBACK WORK.
            MESSAGE TEXT-005 TYPE 'I'.
          ELSE.

            COMMIT WORK AND WAIT.

            CLEAR: lt_func_param, ls_func_declar.

            MESSAGE 'Saving successfull. Thank you!' TYPE 'I'.
          ENDIF.

        ENDIF.

      ELSEIF lv_ans1 = 2.

        MESSAGE TEXT-004 TYPE 'I'.
        EXIT.

      ENDIF.

* If update
    ELSEIF p_upd = abap_true.

* This will prevent from processing if the function name was changed after setting the parameters
      IF ls_func_declar-function_name <> p_fnm.
        CLEAR ls_func_declar.
      ENDIF.

      IF ls_func_declar IS INITIAL.

        MESSAGE 'Function declaration does not exist. Please use Create.' TYPE 'I'.

      ELSE.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Function declaration change alert'
            text_question         = |You are about to change the function declaration { p_fnm }. Are you sure?|
            text_button_1         = 'Yes'
            text_button_2         = 'No'
            default_button        = '2'
            display_cancel_button = ''
          IMPORTING
            answer                = lv_ans
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF lv_ans = 1.
          ls_fd_tab = ls_func_declar.
          ls_fd_tab-function_description = lv_fd_in_string.

          MODIFY zllm_func_declar FROM ls_fd_tab.

          IF sy-subrc <> 0.

            MESSAGE TEXT-005 TYPE 'I'.

          ELSE.

* Make sure to delete all old records and append a new ones
            DELETE FROM zllm_func_parame
            WHERE app_id = ls_fd_tab-app_id
            AND business_function = ls_fd_tab-business_function
            AND parameters_id = ls_fd_tab-function_parameters_id.

            IF lt_func_param IS NOT INITIAL.

              MODIFY zllm_func_parame FROM TABLE lt_func_param.

            ENDIF.

            IF sy-subrc <> 0.
              ROLLBACK WORK.
              MESSAGE TEXT-005 TYPE 'I'.
            ELSE.

              COMMIT WORK AND WAIT.

              CLEAR: lt_func_param, ls_func_declar.

              MESSAGE 'Saving successfull. Thank you!' TYPE 'I'.
            ENDIF.

          ENDIF.

        ELSEIF lv_ans = 2.

          MESSAGE TEXT-004 TYPE 'I'.
          EXIT.

        ENDIF.

      ENDIF.

* If delete
    ELSEIF p_del = abap_true.

      IF ls_fd_tab IS NOT INITIAL.

        MESSAGE 'Function declaration does not exist. Please check your function name' TYPE 'S' DISPLAY LIKE 'E'.

      ELSE.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Function declaration delete alert'
            text_question         = |You are about to delete the function declaration { p_fnm }. Are you sure?|
            text_button_1         = 'Yes'
            text_button_2         = 'No'
            default_button        = '2'
            display_cancel_button = ''
          IMPORTING
            answer                = lv_ans2
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF lv_ans2 = 1.
          ls_fd_tab-mandt = sy-mandt.
          ls_fd_tab-app_id = p_aid.
          ls_fd_tab-business_function = p_bfu.
          ls_fd_tab-function_name = p_fnm.

          DELETE zllm_func_declar FROM ls_fd_tab.

          IF sy-subrc <> 0.
            MESSAGE TEXT-005 TYPE 'I'.
          ELSE.
            MESSAGE 'Deleting successfull. Thank you!' TYPE 'I'.
          ENDIF.

        ELSEIF lv_ans2 = 2.

          MESSAGE TEXT-004 TYPE 'I'.
          EXIT.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.


* Screen 100 PBO (Process Before Output) Module
MODULE pbo_0100 OUTPUT.
  " Initialize the custom container and text-edit control only once
  IF go_custom_container IS INITIAL.

    CREATE OBJECT go_custom_container
      EXPORTING
        container_name = 'TEXT_CONTAINER'.  " Name of custom control in screen layout

    CREATE OBJECT go_textedit
      EXPORTING
        parent = go_custom_container.

    " Set word wrap properties for the text-edit control
    go_textedit->set_wordwrap_behavior( EXPORTING wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
                                                  wordwrap_position = 155 ). " Wrap at 1,000 characters
  ENDIF.

  IF lt_text IS INITIAL OR ls_func_declar-function_name <> p_fnm.
    SPLIT ls_func_declar-function_description AT cl_abap_char_utilities=>newline INTO TABLE lt_text.
  ENDIF.
  " Load any initial text into the text-edit control
  go_textedit->set_text_as_r3table( EXPORTING table = lt_text ).

ENDMODULE.



* Screen 100 PAI (Process After Input) Module
MODULE pai_0100 INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'SAVE'.
      " Retrieve text from the control into gv_text
      go_textedit->get_text_as_r3table( IMPORTING table = lt_text ).

      CLEAR: lv_fd_in_string.

* Capture the prompt into variable
      LOOP AT lt_text ASSIGNING FIELD-SYMBOL(<fs_text>).

        IF lv_fd_in_string IS INITIAL.
          lv_fd_in_string = |{ <fs_text> }|.
        ELSE.
          lv_fd_in_string = |{ lv_fd_in_string }\n{ <fs_text> }|.
        ENDIF.
      ENDLOOP.

      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'CANCEL'.

      "Clear the input and refresh the
      CLEAR: lt_text,lv_fd_in_string.

      go_textedit->set_text_as_r3table( EXPORTING table = lt_text ).

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.


* Screen 200 PBO (Process Before Output) Module
MODULE pbo_0200 OUTPUT.

  " Initialize the custom container and text-edit control only once
  IF go_custom_container2 IS INITIAL.

    CREATE OBJECT go_custom_container2
      EXPORTING
        container_name = 'PARAM_TEXT_CONTAINER'.  " Name of custom control in screen layout

    CREATE OBJECT go_textedit2
      EXPORTING
        parent = go_custom_container2.

    " Set word wrap properties for the text-edit control
    go_textedit2->set_wordwrap_behavior( EXPORTING wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
                                                   wordwrap_position = 155 ). " Wrap at 1,000 characters
  ENDIF.


  " Load any initial text into the text-edit control
  go_textedit2->set_text_as_r3table( EXPORTING table = lt_text2 ).

  " Initialize the custom container and text-edit control only once
  IF go_custom_container3 IS INITIAL.

    CREATE OBJECT go_custom_container3
      EXPORTING
        container_name = 'PARAMS'.  " Name of custom control in screen layout


    IF go_alv_grid IS INITIAL.

      CREATE OBJECT go_alv_grid
        EXPORTING
          i_parent = go_custom_container3.  " Assign the container to the ALV grid

    ENDIF.

    MOVE-CORRESPONDING lt_func_param TO lt_func_params_alv.

    go_alv_grid->set_table_for_first_display(
      EXPORTING
        i_structure_name = 'ZST_LLM_FUNC_PARAMETERS_ALV'
      CHANGING
        it_outtab        = lt_func_params_alv ).

    " Create event handler instance
    CREATE OBJECT go_event_handler.

    " Register the double-click event
    SET HANDLER go_event_handler->on_double_click FOR go_alv_grid.


  ELSE.

    MOVE-CORRESPONDING lt_func_param TO lt_func_params_alv.
    "Refresh the table
    go_alv_grid->refresh_table_display( ).

  ENDIF.


ENDMODULE.



* Screen 200 PAI (Process After Input) Module
MODULE pai_0200 INPUT.
  ok_code = sy-ucomm.

  CASE ok_code.

    WHEN 'ADDEDIT'.
      " Retrieve text from the control into gv_text
      go_textedit2->get_text_as_r3table( IMPORTING table = lt_text2 ).

* Capture the prompt into variable
      LOOP AT lt_text2 ASSIGNING FIELD-SYMBOL(<fs_text2>).

        IF lv_paramdesc_in_string IS INITIAL.
          lv_paramdesc_in_string = |{ <fs_text2> }|.
        ELSE.
          lv_paramdesc_in_string = |{ lv_paramdesc_in_string }\n{ <fs_text2> }|.
        ENDIF.
      ENDLOOP.

      "Check if this entry already exist
      TRY.
          DATA(ls_func_param_from_input) = lt_func_param[ app_id = p_aid
                                                          business_function = p_bfu
                                                          parameter_name = lv_parameter_name_inp ].

          "If yes - Change the existing entry
          ls_func_param_from_input-parameter_type = lv_parameter_type_inp.
          ls_func_param_from_input-is_required = lv_parameter_mand_inp.
          ls_func_param_from_input-parameter_description = lv_paramdesc_in_string.

          MODIFY lt_func_param
          FROM ls_func_param_from_input
          TRANSPORTING parameter_description
                       parameter_type
                       is_required
          WHERE app_id = ls_func_param_from_input-app_id
          AND parameter_type = ls_func_param_from_input-parameter_type
          AND parameters_id = ls_func_param_from_input-parameters_id
          AND business_function = ls_func_param_from_input-business_function.


        CATCH cx_sy_itab_line_not_found.

          "If not - Create a new entry
          ls_func_param-mandt = sy-mandt.
          ls_func_param-app_id = p_aid.
          ls_func_param-business_function = p_bfu.
          ls_func_param-parameter_name = lv_parameter_name_inp.
          ls_func_param-parameter_type = lv_parameter_type_inp.
          ls_func_param-parameter_description = lv_paramdesc_in_string.
          ls_func_param-is_required = lv_parameter_mand_inp.


* Parameters ID
          IF lt_func_param IS NOT INITIAL.
* use the parameter ID from the function declarations table
            TRY.
                ls_func_param-parameters_id = lt_func_param[ 1 ]-parameters_id.
              CATCH cx_sy_itab_line_not_found.
* this should never occur
            ENDTRY.

          ELSE.

            IF ls_func_declar-function_parameters_id IS NOT INITIAL.

              ls_func_param-parameters_id = ls_func_declar-function_parameters_id.

            ELSE.
* Find the last function ID and assign next number.
              SELECT MAX( function_parameters_id )
                INTO @DATA(lv_highest_parameter_value)
                FROM zllm_func_declar.

              ls_func_param-parameters_id = lv_highest_parameter_value + 1.

            ENDIF.

          ENDIF.

          APPEND ls_func_param TO lt_func_param.

      ENDTRY.

      CLEAR: lt_func_params_alv.
      MOVE-CORRESPONDING lt_func_param TO lt_func_params_alv.

      "Refresh the table
      go_alv_grid->refresh_table_display( ).

      "Clear the input fields
      CLEAR: lv_parameter_name_inp,
             lv_parameter_type_inp,
             lv_parameter_mand_inp,
             lv_paramdesc_in_string,
             lt_text2.

      "Clear the input box
      go_textedit2->set_text_as_r3table( EXPORTING table = lt_text2 ).

      "Refresh the screen
      LEAVE TO SCREEN 200.


    WHEN 'REMOVE'.

* Parameters ID
      IF lt_func_param IS NOT INITIAL.
* use the parameter ID from the function declarations table
        TRY.
            DATA(lv_param_id) = lt_func_param[ 1 ]-parameters_id.


            DELETE lt_func_param
            WHERE app_id = p_aid
            AND parameter_type = lv_parameter_type_inp
            AND business_function = p_bfu
            AND parameters_id = lv_param_id.

            CLEAR: lt_func_params_alv.
            MOVE-CORRESPONDING lt_func_param TO lt_func_params_alv.
            "Refresh the table
            go_alv_grid->refresh_table_display( ).


          CATCH cx_sy_itab_line_not_found.
* this should never occur
        ENDTRY.

      ENDIF.


    WHEN 'SAVE'.

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.

      CLEAR: lt_func_params_alv.
      "After hitting cancel. Revert the perameters list to original state
      lt_func_param = lt_func_param_ory.
      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.
