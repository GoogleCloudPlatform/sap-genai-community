*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLLM_FUNC_INDEX.................................*
DATA:  BEGIN OF STATUS_ZLLM_FUNC_INDEX               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLLM_FUNC_INDEX               .
CONTROLS: TCTRL_ZLLM_FUNC_INDEX
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLLM_FUNC_INDEX               .
TABLES: ZLLM_FUNC_INDEX                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
