*&---------------------------------------------------------------------*
*& Report zrep_llm_tr_read_source_code
*&---------------------------------------------------------------------*
*&
*& Description:
*& This program reads all objects from a given transport request or its
*& sub-tasks. It identifies ABAP source code based objects (Reports,
*& Classes, Methods, Function Groups, Function Modules) and prints their source code.
*& For DDIC objects (Tables, Structures, Views), it prints their definition.

REPORT zrep_llm_tr_read_source_code.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: e071.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: string_table TYPE TABLE OF string.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
PARAMETERS: p_trkorr TYPE e071-trkorr OBLIGATORY MEMORY ID trk.

*----------------------------------------------------------------------*
* DATA DECLARATIONS
*----------------------------------------------------------------------*
DATA: gt_objects    TYPE TABLE OF e071,
      gs_object     LIKE LINE OF gt_objects,
      gt_source     TYPE string_table,
      gv_line       TYPE string,
      gv_prog_name  TYPE progname,
      lv_fm_include TYPE progname.

*----------------------------------------------------------------------*
* LOCAL HELPER SUBROUTINE for reading includes
*----------------------------------------------------------------------*
FORM read_and_append_source
  USING
    iv_program_name TYPE progname
  CHANGING
    ct_source_table TYPE string_table.

  DATA: lt_temp_source TYPE string_table.

  READ REPORT iv_program_name INTO lt_temp_source STATE 'I'.
  IF sy-subrc <> 0.
    READ REPORT iv_program_name INTO lt_temp_source STATE 'A'.
  ENDIF.

  IF sy-subrc = 0.
    APPEND LINES OF lt_temp_source TO ct_source_table.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
* PROCESSING BLOCKS
*----------------------------------------------------------------------*

START-OF-SELECTION.

  " Select all objects from the specified transport request
  SELECT *
    FROM e071
    INTO TABLE @gt_objects
    WHERE trkorr = @p_trkorr.

  " If no objects are found directly, check for sub-tasks, as the main
  " request often acts only as a container.
  DATA: lt_tasks TYPE TABLE OF e070.
  SELECT * FROM e070
    INTO TABLE @lt_tasks
    WHERE strkorr = @p_trkorr.

  IF sy-subrc = 0.
    LOOP AT lt_tasks ASSIGNING FIELD-SYMBOL(<ls_task>).
      SELECT *
        FROM e071
        APPENDING TABLE @gt_objects
        WHERE trkorr = @<ls_task>-trkorr.
    ENDLOOP.
  ENDIF.

  IF gt_objects IS INITIAL.
    MESSAGE |No objects found for transport request { p_trkorr } or its tasks| TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  " Remove duplicates in case an object exists in multiple tasks.
  " The SORT must match the fields used in the DELETE ... COMPARING statement.
  SORT gt_objects BY object obj_name.
  DELETE ADJACENT DUPLICATES FROM gt_objects COMPARING object obj_name.

  " --- Identify full classes to prevent method duplication ---
  " If a CLAS object is in the transport, we'll print the whole class.
  " We can then skip the individual METH objects for that class.
  DATA: lt_full_classes TYPE SORTED TABLE OF seoclsname WITH UNIQUE KEY table_line.
  LOOP AT gt_objects INTO DATA(ls_object_check) WHERE object = 'CLAS'.
    DATA(lv_class_name_insert) = CONV seoclsname( ls_object_check-obj_name ).
    INSERT lv_class_name_insert INTO TABLE lt_full_classes.
  ENDLOOP.


  LOOP AT gt_objects INTO gs_object.
    " --- Prevent METH duplication if the entire CLAS is already being processed ---
    IF gs_object-object = 'METH'.
      DATA(lv_class_name_check) = gs_object-obj_name(30).
      READ TABLE lt_full_classes TRANSPORTING NO FIELDS WITH KEY table_line = lv_class_name_check.
      IF sy-subrc = 0.
        CONTINUE. " Skip this METH entry, as the full CLAS entry will handle it.
      ENDIF.
    ENDIF.

    " Clear source code table for each new object
    CLEAR: gt_source, gv_prog_name.

    " --- Print Header for the Object ---
    WRITE: / '================================================================================'.
    WRITE: / 'Object Type:', gs_object-object, ' | ', 'Object Name:', gs_object-obj_name.
    WRITE: / '================================================================================'.
    SKIP 1.

    CASE gs_object-object.

        " --- Program / Report ---
      WHEN 'PROG'.
        " Assign to a variable with the correct type (progname) to avoid a syntax error.
        gv_prog_name = gs_object-obj_name.
        PERFORM read_and_append_source
          USING    gv_prog_name
          CHANGING gt_source.

        IF gt_source IS INITIAL.
          WRITE: / 'Could not read source code for report:', gs_object-obj_name.
        ENDIF.

        " --- Entire Class ---
      WHEN 'CLAS'.
        " This logic reads the entire class definition and implementation.
        DATA: lo_cifref_c          TYPE REF TO if_oo_clif_incl_naming,
              lo_clsref_c          TYPE REF TO if_oo_class_incl_naming,
              ls_cifkey_c          TYPE seoclskey,
              lt_method_includes_c TYPE seop_methods_w_include,
              ls_method_include_c  LIKE LINE OF lt_method_includes_c.

        ls_cifkey_c-clsname = gs_object-obj_name.

        TRY.
            CALL METHOD cl_oo_include_naming=>get_instance_by_cifkey
              EXPORTING
                cifkey = ls_cifkey_c
              RECEIVING
                cifref = lo_cifref_c
              EXCEPTIONS
                OTHERS = 1.

            IF sy-subrc = 0 AND lo_cifref_c IS BOUND.
              lo_clsref_c ?= lo_cifref_c.

              IF lo_clsref_c IS BOUND.
                " --- Assemble the Class Definition ---
                "APPEND |CLASS { gs_object-obj_name } DEFINITION.| TO gt_source.
                PERFORM read_and_append_source USING lo_clsref_c->public_section CHANGING gt_source.
                PERFORM read_and_append_source USING lo_clsref_c->protected_section CHANGING gt_source.
                PERFORM read_and_append_source USING lo_clsref_c->private_section CHANGING gt_source.
                APPEND 'ENDCLASS.' TO gt_source.
                APPEND || TO gt_source. " Blank line for separation

                " --- Assemble the Class Implementation ---
                APPEND |CLASS { gs_object-obj_name } IMPLEMENTATION.| TO gt_source.
                lt_method_includes_c = lo_clsref_c->get_all_method_includes( ).
                LOOP AT lt_method_includes_c INTO ls_method_include_c.
                  PERFORM read_and_append_source USING ls_method_include_c-incname CHANGING gt_source.
                ENDLOOP.
                APPEND 'ENDCLASS.' TO gt_source.
              ENDIF.
            ENDIF.
          CATCH cx_root.
            WRITE: / 'An error occurred while reading class:', gs_object-obj_name.
        ENDTRY.

        IF gt_source IS INITIAL.
          WRITE: / 'Could not read any source code for class:', gs_object-obj_name.
        ENDIF.

        " --- Single Method ---
      WHEN 'METH'.
        " This logic reads only the specific method's implementation.
        DATA: lo_cifref_m         TYPE REF TO if_oo_clif_incl_naming,
              lo_clsref_m         TYPE REF TO if_oo_class_incl_naming,
              ls_cifkey_m         TYPE seoclskey,
              lv_method_prog_name TYPE progname,
              lv_class_name       TYPE seoclsname,
              lv_method_name      TYPE seomtdname.

        " The OBJ_NAME for METH is the class name padded to 30 chars, then the method name.
        lv_class_name  = gs_object-obj_name(30).
        lv_method_name = gs_object-obj_name+30.
        CONDENSE lv_method_name. " Remove any padding spaces

        ls_cifkey_m-clsname = lv_class_name.

        TRY.
            CALL METHOD cl_oo_include_naming=>get_instance_by_cifkey
              EXPORTING
                cifkey = ls_cifkey_m
              RECEIVING
                cifref = lo_cifref_m
              EXCEPTIONS
                OTHERS = 1.

            IF sy-subrc = 0 AND lo_cifref_m IS BOUND.
              lo_clsref_m ?= lo_cifref_m.

              IF lo_clsref_m IS BOUND.
                " Get the specific include for this one method using the correct method name
                lv_method_prog_name = lo_clsref_m->get_include_by_mtdname( lv_method_name ).
                IF lv_method_prog_name IS NOT INITIAL.
                  PERFORM read_and_append_source USING lv_method_prog_name CHANGING gt_source.
                ENDIF.
              ENDIF.
            ENDIF.
          CATCH cx_root.
            WRITE: / 'An error occurred while reading method:', gs_object-obj_name.
        ENDTRY.

        IF gt_source IS INITIAL.
          WRITE: / 'Could not read any source code for method:', gs_object-obj_name.
        ENDIF.

        " --- Function Group ---
      WHEN 'FUGR'.
        " This logic finds all function modules within a function group and prints them.
        DATA: lt_fms TYPE TABLE OF enlfdir-funcname.

        SELECT funcname
          FROM enlfdir
          INTO TABLE @lt_fms
          WHERE area = @gs_object-obj_name.

        IF sy-subrc = 0.
          LOOP AT lt_fms ASSIGNING FIELD-SYMBOL(<lv_fm_name>).
            DATA: lv_include_num_fugr TYPE tfdir-include,
                  lv_fm_include_fugr  TYPE progname.

            " Get the include number from table TFDIR for the current FM.
            SELECT SINGLE include
              FROM tfdir
              INTO @lv_include_num_fugr
              WHERE funcname = @<lv_fm_name>.

            IF sy-subrc = 0.
              " Construct the include program name.
              CONCATENATE 'L' gs_object-obj_name 'U' lv_include_num_fugr INTO lv_fm_include_fugr.

              APPEND |-------------------- Function Module: { <lv_fm_name> } --------------------| TO gt_source.
              PERFORM read_and_append_source
                USING    lv_fm_include_fugr
                CHANGING gt_source.
              APPEND || TO gt_source. " Blank line for separation
            ELSE.
              APPEND |Could not find source include for FM: { <lv_fm_name> }| TO gt_source.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF gt_source IS INITIAL.
          WRITE: / 'Could not find any function modules for function group:', gs_object-obj_name.
        ENDIF.

        " --- Function Module ---
      WHEN 'FUNC'.
        DATA: lv_area        TYPE rs38l_area,
              lv_include_num TYPE tfdir-include.

        " Get the function group (area) from table ENLFDIR.
        SELECT SINGLE area
          FROM enlfdir
          INTO @lv_area
          WHERE funcname = @gs_object-obj_name.

        IF sy-subrc = 0.
          " Get the include number from table TFDIR.
          SELECT SINGLE include
            FROM tfdir
            INTO @lv_include_num
            WHERE funcname = @gs_object-obj_name.

          IF sy-subrc = 0.
            " Construct the include program name directly.
            " Format: L<Function Group>U<Include Number>
            " Example: LSEUAU01 for function group SEUA, include 01.
            CONCATENATE 'L' lv_area 'U' lv_include_num INTO lv_fm_include.

            PERFORM read_and_append_source
              USING    lv_fm_include
              CHANGING gt_source.

            IF gt_source IS INITIAL.
              WRITE: / 'Could not read source for FM:', gs_object-obj_name, '(Program:', lv_fm_include, ')'.
            ENDIF.
          ELSE.
            WRITE: / 'Could not find include number in TFDIR for:', gs_object-obj_name.
          ENDIF.
        ELSE.
          WRITE: / 'Could not find function group in ENLFDIR for:', gs_object-obj_name.
        ENDIF.

        " --- Database Table, Structure, or View ---
      WHEN 'TABL' OR 'STRU' OR 'VIEW'.
        " This logic reads the object definition using Runtime Type Services (RTTS).
        DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
              lt_components  TYPE abap_component_tab.

        TRY.
            lo_structdescr ?= cl_abap_structdescr=>describe_by_name( gs_object-obj_name ).
            lt_components = lo_structdescr->get_components( ).

            IF lt_components IS NOT INITIAL.
              APPEND '-------------------- Definition --------------------' TO gt_source.
              LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_component>).
                " Get the type name (e.g., data element or built-in type).
                DATA(lv_type_name) = <ls_component>-type->get_relative_name( ).
                " For built-in types, the name can be prefixed with '\TYPE=', which we remove for cleaner output.
                REPLACE ALL OCCURRENCES OF REGEX '^\\TYPE=' IN lv_type_name WITH ''.

                DATA(lv_field_def) = |  { <ls_component>-name } TYPE { lv_type_name }.|.
                APPEND lv_field_def TO gt_source.
              ENDLOOP.
              APPEND '----------------------------------------------------' TO gt_source.
            ENDIF.
          CATCH cx_root. " Catching the root exception is safer and resolves the unknown type error.
            WRITE: / 'Could not retrieve definition for object:', gs_object-obj_name.
        ENDTRY.

        " --- Type Group ---
      WHEN 'TYPE'.
        WRITE: / 'Dictionary Object. Source code is not applicable.'.

        " --- Other object types ---
      WHEN OTHERS.
        WRITE: / 'This object type is not processed for source code reading.'.

    ENDCASE.

    " --- Print the Source Code if it was read successfully ---
    IF gt_source IS NOT INITIAL.
      LOOP AT gt_source INTO gv_line.
        WRITE: / gv_line.
      ENDLOOP.
    ENDIF.

    " Add spacing for the next object
    SKIP 2.

  ENDLOOP.
