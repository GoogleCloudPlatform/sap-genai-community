*&---------------------------------------------------------------------*
*& Report zrep_llm_edit_prompts_tab
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrep_llm_edit_prompts_tab.

DATA: lt_text             TYPE TABLE OF char255,
      ok_code             TYPE sy-ucomm, " Holds the function code (e.g., for Save or Exit)
      go_textedit         TYPE REF TO cl_gui_textedit,  " Reference to the Text-Edit control
      go_custom_container TYPE REF TO cl_gui_custom_container,  " Container for the control
      lv_prompt_in_string TYPE string,
      ls_prompt_tab       TYPE zllm_prompts,
      lv_prompt_ans       TYPE char1,
      lv_prompt_ans1      TYPE char1,
      lv_prompt_ans2      TYPE char1,
      ls_prompt           TYPE zllm_prompts.

SELECTION-SCREEN BEGIN OF BLOCK a0 WITH FRAME TITLE TEXT-000.
  PARAMETERS: p_crt RADIOBUTTON GROUP x1,
              p_upd RADIOBUTTON GROUP x1 DEFAULT 'X',
              p_del RADIOBUTTON GROUP x1,
              p_aid TYPE string DEFAULT 'CSHUB',
              p_bfu TYPE string DEFAULT 'EMAIL',
              p_key TYPE string.

  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN PUSHBUTTON 33(15) TEXT-001 USER-COMMAND button_click.
SELECTION-SCREEN END OF BLOCK a0.


AT SELECTION-SCREEN.

  IF sy-ucomm = 'BUTTON_CLICK'.
    CLEAR ok_code.

    IF p_upd = abap_true OR p_del = abap_true.

      SELECT SINGLE *
      FROM zllm_prompts
      INTO @ls_prompt
      WHERE app_id = @p_aid
      AND business_function = @p_bfu
      AND prompt_key = @p_key.

    ENDIF.

    CALL SCREEN 100.

  ELSEIF sy-ucomm = 'ONLI'.

    IF p_key IS INITIAL.

      MESSAGE 'Prompt key can not be blank' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.

    ENDIF.


* If Create
    IF p_crt = abap_true.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Prompt create alert'
          text_question         = |You are about to create the prompt { p_key }. Are you sure?|
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = ''
        IMPORTING
          answer                = lv_prompt_ans1
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF lv_prompt_ans1 = 1.

        ls_prompt_tab-mandt = sy-mandt.
        ls_prompt_tab-app_id = p_aid.
        ls_prompt_tab-business_function = p_bfu.
        ls_prompt_tab-prompt_key = p_key.
        ls_prompt_tab-prompt_text = lv_prompt_in_string.

        MODIFY zllm_prompts FROM ls_prompt_tab.

        IF sy-subrc <> 0.
          MESSAGE 'Error saving the prompt' TYPE 'I'.
        ELSE.
          MESSAGE 'Saving successfull. Thank you!' TYPE 'I'.
        ENDIF.

      ELSEIF lv_prompt_ans1 = 2.

        MESSAGE 'Operation canceled' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.

      ENDIF.

* If update
    ELSEIF p_upd = abap_true.

      IF ls_prompt IS INITIAL.

        MESSAGE 'Prompt does not exist. Please use Create.' TYPE 'I'.
        EXIT.

      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Prompt change alert'
          text_question         = |You are about to change the prompt { p_key }. Are you sure?|
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = ''
        IMPORTING
          answer                = lv_prompt_ans
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF lv_prompt_ans = 1.
        ls_prompt_tab-mandt = sy-mandt.
        ls_prompt_tab-app_id = p_aid.
        ls_prompt_tab-business_function = p_bfu.
        ls_prompt_tab-prompt_key = p_key.
        ls_prompt_tab-prompt_text = lv_prompt_in_string.

        MODIFY zllm_prompts FROM ls_prompt_tab.

        IF sy-subrc <> 0.
          MESSAGE 'Error saving the prompt' TYPE 'I'.
        ELSE.
          MESSAGE 'Saving successfull. Thank you!' TYPE 'I'.
        ENDIF.

      ELSEIF lv_prompt_ans = 2.

        MESSAGE 'Operation canceled' TYPE 'I'.
        EXIT.

      ENDIF.

* If delete
    ELSEIF p_del = abap_true.

      IF ls_prompt IS INITIAL.

        MESSAGE 'Prompt does not exist. Please check your prompt key' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.

      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Prompt delete alert'
          text_question         = |You are about to delete the prompt { p_key }. Are you sure?|
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = ''
        IMPORTING
          answer                = lv_prompt_ans2
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF lv_prompt_ans2 = 1.
        ls_prompt_tab-mandt = sy-mandt.
        ls_prompt_tab-app_id = p_aid.
        ls_prompt_tab-business_function = p_bfu.
        ls_prompt_tab-prompt_key = p_key.

        DELETE zllm_prompts FROM ls_prompt_tab.

        IF sy-subrc <> 0.
          MESSAGE 'Error deleting the prompt' TYPE 'I'.
        ELSE.
          MESSAGE 'Deleting successfull. Thank you!' TYPE 'I'.
        ENDIF.

      ELSEIF lv_prompt_ans2 = 2.

        MESSAGE 'Operation canceled' TYPE 'I'.
        EXIT.

      ENDIF.

    ENDIF.

  ENDIF.


* Screen 100 PBO (Process Before Output) Module
MODULE pbo_0100 OUTPUT.
  " Initialize the custom container and text-edit control only once
  IF go_custom_container IS INITIAL.

    CREATE OBJECT go_custom_container
      EXPORTING
        container_name = 'CONTAINER'.  " Name of custom control in screen layout

    CREATE OBJECT go_textedit
      EXPORTING
        parent = go_custom_container.

    " Set word wrap properties for the text-edit control
    go_textedit->set_wordwrap_behavior( EXPORTING wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
                                                  wordwrap_position = 155 ). " Wrap at 1,000 characters
  ENDIF.

  CLEAR lt_text.
  SPLIT ls_prompt-prompt_text AT cl_abap_char_utilities=>newline INTO TABLE lt_text.
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

* Capture the prompt into variable
      LOOP AT lt_text ASSIGNING FIELD-SYMBOL(<fs_text>).

        IF lv_prompt_in_string IS INITIAL.
          lv_prompt_in_string = |{ <fs_text> }|.
        ELSE.
          lv_prompt_in_string = |{ lv_prompt_in_string }\n{ <fs_text> }|.
        ENDIF.
      ENDLOOP.

      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
