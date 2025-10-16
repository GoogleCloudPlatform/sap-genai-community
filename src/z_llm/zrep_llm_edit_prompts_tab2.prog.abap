*&---------------------------------------------------------------------*
*& Report zrep_llm_edit_prompts_tab2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrep_llm_edit_prompts_tab2.

TYPES: BEGIN OF ty_save_stat,
         status TYPE char1,
       END OF ty_save_stat.

DATA: lt_prompts              TYPE TABLE OF zllm_prompts,
      lt_edited_prompts       TYPE TABLE OF zllm_prompts,
      lt_edited_prompts_inv   TYPE TABLE OF zllm_prompts,
      ls_deleted_prompts      TYPE zllm_prompts,
      lt_deleted_prompts      TYPE TABLE OF zllm_prompts,
      lt_text                 TYPE TABLE OF char255, "Table to hold text from the custom container.
      ok_code                 TYPE sy-ucomm, " Holds the function code (e.g., for Save or Exit)
      go_textedit             TYPE REF TO cl_gui_textedit,  " Reference to the Text-Edit control to set text in custom container
      go_custom_container     TYPE REF TO cl_gui_custom_container,  " Container for the control of custom container
      go_custom_container2    TYPE REF TO cl_gui_custom_container,  " Container for the control of custom container
      go_alv_grid             TYPE REF TO cl_gui_alv_grid, " ALV grid class
      lt_prompts_alv          TYPE TABLE OF zst_llm_prompts_alv,
      lt_fieldcat             TYPE lvc_t_fcat,  " Field catalog internal table to set the length of alv column
      ls_fieldcat             TYPE lvc_s_fcat,  " Field catalog work area to set the length of alv column
      lv_prompt_key_inp       TYPE zllm_prompt_key,
      lv_prompttext_in_string TYPE string,
      ls_prompt_from_input    TYPE zllm_prompts,
      lt_save_stat            TYPE TABLE OF ty_save_stat.

FIELD-SYMBOLS <fs_stat> TYPE ty_save_stat.


** Define a local class for event handling - to capture the mouse click
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: on_double_click FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.
ENDCLASS.


CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
    " Read the data from the selected row
    TRY.
        DATA(ls_prompt_clicked) = lt_prompts[ e_row ].

        " Pre-populate data
        lv_prompt_key_inp = ls_prompt_clicked-prompt_key.


        SPLIT ls_prompt_clicked-prompt_text AT cl_abap_char_utilities=>newline INTO TABLE lt_text.
        " Load text into the text-edit control
        go_textedit->set_text_as_r3table( EXPORTING table = lt_text ).

        "Refresh the screen
        LEAVE TO SCREEN 100.

      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

* Declare event handler object
DATA: go_event_handler TYPE REF TO lcl_event_handler.


SELECTION-SCREEN BEGIN OF BLOCK a0 WITH FRAME TITLE TEXT-000.
  PARAMETERS: p_aid TYPE string DEFAULT 'CSHUB' OBLIGATORY,
              p_bfu TYPE string DEFAULT 'EMAIL' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK a0.



AT SELECTION-SCREEN.

  IF sy-ucomm = 'ONLI'.
    CLEAR ok_code.


* Select the prompts
    SELECT *
    FROM zllm_prompts
    INTO TABLE @lt_prompts
    WHERE app_id = @p_aid
    AND business_function = @p_bfu.

    IF sy-subrc = 0.

      CALL SCREEN 100.

    ELSE.

      MESSAGE TEXT-001 TYPE 'I'.

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

  " Load any initial text into the text-edit control
  go_textedit->set_text_as_r3table( EXPORTING table = lt_text ).





  " Initialize the custom container and text-edit control only once
  IF go_custom_container2 IS INITIAL.

    CREATE OBJECT go_custom_container2
      EXPORTING
        container_name = 'PROMPTS_CONTAINER'.  " Name of custom control in screen layout


    IF go_alv_grid IS INITIAL.

      CREATE OBJECT go_alv_grid
        EXPORTING
          i_parent = go_custom_container2.  " Assign the container to the ALV grid

    ENDIF.

    MOVE-CORRESPONDING lt_prompts TO lt_prompts_alv.

    " Define Field Catalog with custom widths
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'PROMPT_KEY'.         " Column name from the structure
    ls_fieldcat-outputlen = 25.                " Set width for PROMPT_KEY
    APPEND ls_fieldcat TO lt_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'PROMPT_TEXT'.         " Another column name from the structure
    ls_fieldcat-outputlen = 100.                " Set width for PROMPT_TEXT
    APPEND ls_fieldcat TO lt_fieldcat.


    go_alv_grid->set_table_for_first_display(
      EXPORTING
        i_structure_name = 'ZST_LLM_PROMPTS_ALV'
      CHANGING
        it_fieldcatalog  = lt_fieldcat     " Pass the field catalog with widths
        it_outtab        = lt_prompts_alv ).

    " Create event handler instance
    CREATE OBJECT go_event_handler.

    " Register the double-click event
    SET HANDLER go_event_handler->on_double_click FOR go_alv_grid.

  ELSE.

    MOVE-CORRESPONDING lt_prompts TO lt_prompts_alv.
    "Refresh the table
    go_alv_grid->refresh_table_display( ).

  ENDIF.


ENDMODULE.



* Screen 100 PAI (Process After Input) Module
MODULE pai_0100 INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'ADDEDIT'.

      IF lv_prompt_key_inp IS NOT INITIAL.

        " Retrieve text from the control into lt_text
        go_textedit->get_text_as_r3table( IMPORTING table = lt_text ).

* Capture the prompt into variable
        LOOP AT lt_text ASSIGNING FIELD-SYMBOL(<fs_text>).

          IF lv_prompttext_in_string IS INITIAL.
            lv_prompttext_in_string = |{ <fs_text> }|.
          ELSE.
            lv_prompttext_in_string  = |{ lv_prompttext_in_string }\n{ <fs_text> }|.
          ENDIF.
        ENDLOOP.

        "Check if this entry already exist
        TRY.
            ls_prompt_from_input = lt_prompts[ app_id = p_aid
                                               business_function = p_bfu
                                               prompt_key = lv_prompt_key_inp ].

            "If yes - Change the existing entry
            ls_prompt_from_input-prompt_text = lv_prompttext_in_string.


            MODIFY lt_prompts
            FROM ls_prompt_from_input
            TRANSPORTING prompt_text
            WHERE app_id = ls_prompt_from_input-app_id
            AND business_function = ls_prompt_from_input-business_function
            AND prompt_key = ls_prompt_from_input-prompt_key.


          CATCH cx_sy_itab_line_not_found.

            "If not - Create a new entry
            ls_prompt_from_input-mandt = sy-mandt.
            ls_prompt_from_input-app_id = p_aid.
            ls_prompt_from_input-business_function = p_bfu.
            ls_prompt_from_input-prompt_key = lv_prompt_key_inp.
            ls_prompt_from_input-prompt_text = lv_prompttext_in_string.

            APPEND ls_prompt_from_input TO lt_prompts.

        ENDTRY.

* Store the modified line in the separate table to process the save to DB later
        TRY.
* Check if already exist in the edited table. User might edit this few times in one session
            DATA(ls_edit_check) = lt_edited_prompts[ app_id = ls_prompt_from_input-app_id
                                                     business_function = ls_prompt_from_input-business_function
                                                     prompt_key = ls_prompt_from_input-prompt_key ].
            " If exists = Modify existing entry
            MODIFY lt_edited_prompts
            FROM ls_prompt_from_input
            TRANSPORTING prompt_text
            WHERE app_id = ls_prompt_from_input-app_id
            AND business_function = ls_prompt_from_input-business_function
            AND prompt_key = ls_prompt_from_input-prompt_key.

          CATCH cx_sy_itab_line_not_found.

            " Dont exist = Append a new entry
            APPEND ls_prompt_from_input TO lt_edited_prompts.

        ENDTRY.



        CLEAR: lt_prompts_alv.
        MOVE-CORRESPONDING lt_prompts TO lt_prompts_alv.
        "Refresh the table
        go_alv_grid->refresh_table_display( ).

        "Clear the input fields
        CLEAR: lv_prompt_key_inp,
               lv_prompttext_in_string,
               lt_text.

        "Clear the input box
        go_textedit->set_text_as_r3table( EXPORTING table = lt_text ).

        "Refresh the screen
        LEAVE TO SCREEN 100.

      ENDIF.

    WHEN 'REMOVE'.

      IF lv_prompt_key_inp IS NOT INITIAL.
* Parameters ID
        IF lt_prompts IS NOT INITIAL.

* Store the deleted line in the separate table to process the delete to DB later
          ls_deleted_prompts-app_id = p_aid.
          ls_deleted_prompts-business_function = p_bfu.
          ls_deleted_prompts-prompt_key = lv_prompt_key_inp.
          APPEND ls_deleted_prompts TO lt_deleted_prompts.


          DELETE lt_prompts
          WHERE app_id = ls_deleted_prompts-app_id
          AND business_function = ls_deleted_prompts-business_function
          AND prompt_key = ls_deleted_prompts-prompt_key.

          CLEAR: lt_prompts_alv,
                 lv_prompt_key_inp,
                 lv_prompttext_in_string,
                 ls_deleted_prompts,
                 lt_text.

          MOVE-CORRESPONDING lt_prompts TO lt_prompts_alv.
          "Refresh the table
          go_alv_grid->refresh_table_display( ).

          "Refresh the screen
          LEAVE TO SCREEN 100.

        ENDIF.

      ENDIF.

    WHEN 'SAVE'.

      IF lt_edited_prompts IS NOT INITIAL.

        MODIFY zllm_prompts FROM TABLE lt_edited_prompts.

        IF sy-subrc = 0.

          APPEND INITIAL LINE TO lt_save_stat ASSIGNING <fs_stat>.
          <fs_stat>-status = 'S'.

        ELSE.

          APPEND INITIAL LINE TO lt_save_stat ASSIGNING <fs_stat>.
          <fs_stat>-status = 'F'.

        ENDIF.

      ENDIF.

      IF lt_deleted_prompts IS NOT INITIAL.

        DELETE zllm_prompts FROM TABLE lt_deleted_prompts.

        IF sy-subrc = 0.

          APPEND INITIAL LINE TO lt_save_stat ASSIGNING <fs_stat>.
          <fs_stat>-status = 'S'.

        ELSE.

          APPEND INITIAL LINE TO lt_save_stat ASSIGNING <fs_stat>.
          <fs_stat>-status = 'F'.

        ENDIF.

      ENDIF.



* If nothing was edited
      IF lt_edited_prompts IS INITIAL AND lt_deleted_prompts IS INITIAL.

        MESSAGE TEXT-004 TYPE 'I'. "004 - Nothing edited

      ELSE.

* If any failure rollback work
        TRY.
            DATA(ls_fail_check) = lt_save_stat[ status = 'F' ].

            ROLLBACK WORK.
            MESSAGE TEXT-003 TYPE 'I'. "003 - Failed


          CATCH cx_sy_itab_line_not_found.


            COMMIT WORK AND WAIT.
            MESSAGE TEXT-002 TYPE 'I'. "002 - Success

        ENDTRY.

      ENDIF.


      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'CANCEL'.

      CLEAR: lt_prompts_alv,
             lv_prompt_key_inp,
             lv_prompttext_in_string,
             lt_text.

      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.
