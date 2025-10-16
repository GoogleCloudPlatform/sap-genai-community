*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZLLM_PROMPTS_SEQ
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZLLM_PROMPTS_SEQ   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
