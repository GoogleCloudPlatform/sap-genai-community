*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLLM_SIGNATURE..................................*
DATA:  BEGIN OF STATUS_ZLLM_SIGNATURE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLLM_SIGNATURE                .
CONTROLS: TCTRL_ZLLM_SIGNATURE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLLM_SIGNATURE                .
TABLES: ZLLM_SIGNATURE                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
