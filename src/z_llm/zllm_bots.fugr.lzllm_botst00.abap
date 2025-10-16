*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLLM_BOTS.......................................*
DATA:  BEGIN OF STATUS_ZLLM_BOTS                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLLM_BOTS                     .
CONTROLS: TCTRL_ZLLM_BOTS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLLM_BOTS                     .
TABLES: ZLLM_BOTS                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
