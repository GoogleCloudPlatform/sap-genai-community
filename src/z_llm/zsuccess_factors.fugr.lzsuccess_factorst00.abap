*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSF_DEFAULTS....................................*
DATA:  BEGIN OF STATUS_ZSF_DEFAULTS                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSF_DEFAULTS                  .
CONTROLS: TCTRL_ZSF_DEFAULTS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSF_DEFAULTS                  .
TABLES: ZSF_DEFAULTS                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
