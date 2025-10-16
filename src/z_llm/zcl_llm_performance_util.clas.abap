CLASS zcl_llm_performance_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS: constructor IMPORTING app_id            TYPE zllm_app_id OPTIONAL
                                   business_function TYPE zllm_buss_function_name OPTIONAL
                                   type              TYPE string OPTIONAL
                                   prompt_key        TYPE zllm_prompt_key OPTIONAL
                                   uuid_key          TYPE guid16 OPTIONAL,
      start_perforamnce_trace IMPORTING type TYPE string OPTIONAL,
      end_performance_trace.


  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: app_id            TYPE zllm_app_id,
          business_function TYPE zllm_buss_function_name,
          type              TYPE string,
          prompt_key        TYPE zllm_prompt_key,
          uuid_key          TYPE guid16,
          start_time_stamp  TYPE timestampl,
          end_time_stamp    TYPE timestampl,
          gcl_log           TYPE REF TO zcl_llm_log.

    METHODS: performance EXPORTING ev_time_stamp TYPE timestampl,
      calc_performance RETURNING VALUE(count_s) TYPE tzntstmpl,
      write_performance_trace.

ENDCLASS.



CLASS zcl_llm_performance_util IMPLEMENTATION.

  METHOD constructor.

    me->app_id = app_id.
    me->business_function = business_function.
    me->type = type.
    me->prompt_key = prompt_key.
    me->uuid_key = uuid_key.

* Initialize logging
    gcl_log = NEW zcl_llm_log( app_id            = app_id
                               business_function = business_function ).

  ENDMETHOD.

  METHOD start_perforamnce_trace.

* If type is provided in the method, override the constructor type.
    me->type = type.

    performance( IMPORTING ev_time_stamp = start_time_stamp ).

  ENDMETHOD.

  METHOD end_performance_trace.

    performance( IMPORTING ev_time_stamp = end_time_stamp ).

    me->write_performance_trace(  ).

    CLEAR: start_time_stamp,
           end_time_stamp.

  ENDMETHOD.

  METHOD performance.

    GET TIME STAMP FIELD ev_time_stamp.

  ENDMETHOD.

  METHOD calc_performance.

    count_s = cl_abap_tstmp=>subtract( tstmp1 = end_time_stamp
                                       tstmp2 = start_time_stamp ).

  ENDMETHOD.

  METHOD write_performance_trace.

    gcl_log->write_general_log( uuid_key    = me->uuid_key
                                request     = me->type
                                object_type = 'PERFORMANCE'
                                object_key  = CONV zllm_log_object_key( me->calc_performance( ) ) ).


  ENDMETHOD.

ENDCLASS.
