CLASS zcl_llm_create_abap_agent_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.


    METHODS: constructor IMPORTING app_id            TYPE zllm_app_id
                                   business_function TYPE zllm_buss_function_name,
      create_abap_agent IMPORTING template_agent TYPE rs38l-name
                                  new_agent      TYPE rs38l-name
                                  function_group TYPE rs38l-area
                        RAISING   zcx_llm,

      assign_objects_to_transport IMPORTING transport           TYPE trkorr
                                  RAISING   zcx_llm,

      create_transport            IMPORTING type           TYPE trfunction
                                            description    TYPE as4text
                                            owner          TYPE sy-uname OPTIONAL
                                  EXPORTING request_header TYPE trwbo_request_header
                                            task_header    TYPE trwbo_request_headers
                                  RAISING   zcx_llm,


      add_to_transport_object_list IMPORTING object_name TYPE trobj_name
                                             object      TYPE trobjtype
                                             pgmid       TYPE string
                                             transport   type trkorr,


      activate_fm IMPORTING object_name TYPE e071-obj_name
                  RAISING   zcx_llm.



  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: gcl_log           TYPE REF TO zcl_llm_log,
          transport_objects TYPE TABLE OF rs_s_tr_object.

ENDCLASS.



CLASS zcl_llm_create_abap_agent_util IMPLEMENTATION.


  METHOD constructor.

* Set logging up
    me->gcl_log = NEW zcl_llm_log( app_id            = app_id
                                   business_function = business_function ).

  ENDMETHOD.


  METHOD create_transport.

    DATA: lt_users TYPE scts_users,
          ls_users TYPE scts_user.

    ls_users-user = sy-uname.
    ls_users-type = 'S'.

    APPEND ls_users TO lt_users.

    CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
      EXPORTING
        iv_type           = type
        iv_text           = description
        iv_owner          = sy-uname
        it_users          = lt_users
      IMPORTING
        es_request_header = request_header
        et_task_headers   = task_header
      EXCEPTIONS
        insert_failed     = 1
        enqueue_failed    = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>transport_request_failed
          msg1   = CONV string( description ).
    ENDIF.

  ENDMETHOD.

  METHOD activate_fm.

    DATA: lv_index_programs TYPE programt.


    CALL FUNCTION 'FUNC_OBJECT_ACTIVATE'
      EXPORTING
        object_name    = object_name
        author         = sy-uname
      IMPORTING
        index_programs = lv_index_programs
      EXCEPTIONS
        cancelled      = 1
        OTHERS         = 2.


    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>fm_activation_failed
          msg1   = CONV string( object_name ).

    ENDIF.


* Log that a new agent was created.
    gcl_log->write_general_log( object_type = 'ACTIVATED_FM'
                                new_value   = |FM: { object_name }| ).

  ENDMETHOD.


  METHOD add_to_transport_object_list.

    DATA: ls_object TYPE ko200.

    ls_object-pgmid = pgmid.
    ls_object-object = object.
    ls_object-obj_name = object_name.
    ls_object-objfunc = space.
    ls_object-trkorr = transport.
    ls_object-lockflag = abap_true.
    ls_object-lang = sy-langu.
    ls_object-author = sy-uname.
    ls_object-operation = 'I'.


    APPEND ls_object TO transport_objects.

  ENDMETHOD.


  METHOD assign_objects_to_transport.

    DATA: lv_e_msg     TYPE rs_t_msg,
          lv_e_subrc   TYPE sysubrc,
          lv_e_request TYPE trkorr.

    CALL FUNCTION 'RS_TR_WRITE_OBJECTS_TO_REQUEST'
      EXPORTING
        i_t_tr_object      = me->transport_objects
        i_request          = transport
        i_write_on_request = abap_true
      IMPORTING
        e_t_msg            = lv_e_msg
        e_subrc            = lv_e_subrc
        e_request          = lv_e_request.


    IF lv_e_msg is not initial.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>transport_request_failed
          msg1   = CONV string( transport ).

    ENDIF.

* Log that a new agent was created.
    gcl_log->write_general_log( object_type = 'TRANSPORT_CREATED'
                                new_value   = |Transport: { lv_e_request }| ).



  ENDMETHOD.


  METHOD create_abap_agent.

    DATA: lv_new_fm    TYPE rs38l-name,
          lv_new_group TYPE rs38l-area.


* Create new abap agent.
    CALL FUNCTION 'RS_FUNCTION_COPY'
      EXPORTING
        dark_flag     = abap_true
        new_name      = new_agent
        new_group     = function_group
        old_name      = template_agent
      IMPORTING
        new_group     = lv_new_group
        new_name      = lv_new_fm
      EXCEPTIONS
        error_message = 1
        cancelled     = 2
        OTHERS        = 3.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>abap_agent_create_failed
          msg1   = CONV string( new_agent ).

    ENDIF.


* Log that a new agent was created.
    gcl_log->write_general_log( object_type = 'ABAP_AGENT_CREATED'
                                new_value   = |FM: { lv_new_fm }. FG: { lv_new_group }| ).


  ENDMETHOD.


ENDCLASS.
