*&---------------------------------------------------------------------*
*& Report z_llm_copy_bot
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_llm_copy_bot.


PARAMETERS: p_tapp  TYPE zllm_app_id,
            p_tbuss TYPE zllm_buss_function_name,
            p_fapp  TYPE zllm_app_id,
            p_fbuss TYPE zllm_buss_function_name,
            p_inter TYPE string.


TRY.
    DATA(lcl_llm_copy_bot_util) = NEW zcl_llm_copy_bot_util( from_app_id            = p_fapp
                                                             from_business_function = p_fbuss
                                                             to_app_id              = p_tapp
                                                             to_business_function   = p_tbuss ).


    lcl_llm_copy_bot_util->copy_bot(  ).

  CATCH zcx_llm INTO DATA(lcx_llm).

ENDTRY.
