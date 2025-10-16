*&---------------------------------------------------------------------*
*& Report zrep_update_llm_signature
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrep_update_llm_signature.


DATA : app_id        TYPE zllm_app_id,
       buss_function TYPE zllm_buss_function_name,
       signature     TYPE string.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: appid    LIKE app_id,
              bussfunc LIKE buss_function,
              sign     LIKE signature LOWER CASE.
SELECTION-SCREEN END OF BLOCK b1.

DATA : ls_signature TYPE zllm_signature.


ls_signature-mandt = sy-mandt.
ls_signature-app_id = appid.
ls_signature-business_function = bussfunc.
ls_signature-signature = sign.

IF ls_signature IS NOT INITIAL.

  MODIFY zllm_signature FROM ls_signature.

  IF sy-subrc = 0.

    COMMIT WORK AND WAIT.

    WRITE:/ |Update successful! Verify the entry in the table ZLLM_SIGNATURE.|.

  ELSE.

    WRITE:/ |Failed to update signature for App ID{ appid } and business function { bussfunc }|.

  ENDIF.

ENDIF.
