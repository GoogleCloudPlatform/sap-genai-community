*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLLM_PROMPTS_SEQ................................*
DATA:  BEGIN OF STATUS_ZLLM_PROMPTS_SEQ              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLLM_PROMPTS_SEQ              .
CONTROLS: TCTRL_ZLLM_PROMPTS_SEQ
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLLM_PROMPTS_SEQ              .
TABLES: ZLLM_PROMPTS_SEQ               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
