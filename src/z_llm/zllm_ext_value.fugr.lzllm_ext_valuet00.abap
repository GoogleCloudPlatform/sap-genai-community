*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLLM_EXT_VALUE..................................*
DATA:  BEGIN OF STATUS_ZLLM_EXT_VALUE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLLM_EXT_VALUE                .
CONTROLS: TCTRL_ZLLM_EXT_VALUE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLLM_EXT_VALUE                .
TABLES: ZLLM_EXT_VALUE                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
