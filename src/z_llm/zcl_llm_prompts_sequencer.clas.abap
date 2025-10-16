CLASS zcl_llm_prompts_sequencer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

  METHODS :
            "! Receives and saves app id and business function
            constructor                 IMPORTING iv_app_id    TYPE zllm_app_id
                                                  iv_buss_func TYPE zllm_buss_function_name,

            opener.

  PROTECTED SECTION.

  PRIVATE SECTION.

  DATA : gv_app_id            TYPE zllm_app_id,
         gv_business_function TYPE zllm_buss_function_name.

  METHODS :
            "! <p class="shorttext synchronized" lang="en"></p>
            "! Method to fetch the relevant prompt keys and their sequences maintained for the app ID and business function
            "! @parameter rt_prompt_sequences | <p class="shorttext synchronized" lang="en"></p>
            fetch_prompt_sequence                RETURNING VALUE(rt_prompt_sequences) TYPE zllm_prompts_seq_tt.

ENDCLASS.



CLASS zcl_llm_prompts_sequencer IMPLEMENTATION.

  METHOD constructor.

    me->gv_app_id = iv_app_id.
    me->gv_business_function = iv_buss_func.

  ENDMETHOD.

  METHOD fetch_prompt_sequence.

    IF me->gv_app_id IS NOT INITIAL AND
       me->gv_business_function IS NOT INITIAL.

        "SELECT

    ENDIF.

  ENDMETHOD.

  METHOD opener.

  ENDMETHOD.

ENDCLASS.
