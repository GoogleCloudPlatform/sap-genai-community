CLASS zcl_llm_security_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: constructor IMPORTING app_id            TYPE zllm_app_id
                                   business_function TYPE zllm_buss_function_name,

**********************************************************************
* Method to check for a run away function chain event
**********************************************************************
      check_runaway IMPORTING agent TYPE zllm_prompt_key.



  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_llm_security_util IMPLEMENTATION.


  METHOD constructor.

  ENDMETHOD.


  METHOD check_runaway.



  ENDMETHOD.

ENDCLASS.
