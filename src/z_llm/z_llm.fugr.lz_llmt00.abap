*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLLM_AUTHS......................................*
DATA:  BEGIN OF STATUS_ZLLM_AUTHS                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLLM_AUTHS                    .
CONTROLS: TCTRL_ZLLM_AUTHS
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZLLM_AUTHS_ROLES................................*
DATA:  BEGIN OF STATUS_ZLLM_AUTHS_ROLES              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLLM_AUTHS_ROLES              .
CONTROLS: TCTRL_ZLLM_AUTHS_ROLES
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZLLM_CLASS_CONTR................................*
DATA:  BEGIN OF STATUS_ZLLM_CLASS_CONTR              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLLM_CLASS_CONTR              .
CONTROLS: TCTRL_ZLLM_CLASS_CONTR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLLM_AUTHS                    .
TABLES: *ZLLM_AUTHS_ROLES              .
TABLES: *ZLLM_CLASS_CONTR              .
TABLES: ZLLM_AUTHS                     .
TABLES: ZLLM_AUTHS_ROLES               .
TABLES: ZLLM_CLASS_CONTR               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
