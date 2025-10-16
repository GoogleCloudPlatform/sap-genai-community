*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLLM_ART_TRENDIN................................*
DATA:  BEGIN OF STATUS_ZLLM_ART_TRENDIN              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLLM_ART_TRENDIN              .
CONTROLS: TCTRL_ZLLM_ART_TRENDIN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLLM_ART_TRENDIN              .
TABLES: ZLLM_ART_TRENDIN               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
