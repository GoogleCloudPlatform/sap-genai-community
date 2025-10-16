*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLLM_CONFIG.....................................*
DATA:  BEGIN OF STATUS_ZLLM_CONFIG                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLLM_CONFIG                   .
CONTROLS: TCTRL_ZLLM_CONFIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLLM_CONFIG                   .
TABLES: ZLLM_CONFIG                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
