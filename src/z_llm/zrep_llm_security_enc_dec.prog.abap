*&---------------------------------------------------------------------*
*& Report zrep_llm_security_enc_dec
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrep_llm_security_enc_dec.

CLASS lcl_security DEFINITION
  FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS: encrypt IMPORTING prompt          TYPE string
                                     key             TYPE xstring
                           RETURNING VALUE(enc_text) TYPE string,

      decryption IMPORTING key                      TYPE xstring
                           enc_text                 TYPE string
                 RETURNING VALUE(decrypted_message) TYPE string.

    CLASS-DATA: gv_xstring TYPE xstring,
                gv_padding TYPE xstring.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_security IMPLEMENTATION.

  METHOD encrypt.

    DATA(lr_conv_sec) = cl_abap_conv_out_ce=>create( ).
    lr_conv_sec->write( data = prompt ).
    " encrypt using AES256
    gv_xstring = lr_conv_sec->get_buffer( ).
    gv_padding = '00000000000000000000000000000000'.

    cl_sec_sxml_writer=>encrypt_iv(
      EXPORTING
        plaintext  = gv_xstring
        key        = key
        iv         = gv_padding
        algorithm  = cl_sec_sxml_writer=>co_aes256_algorithm_pem
      IMPORTING
        ciphertext = DATA(lv_message) ).

    DATA: lr_conv    TYPE REF TO cl_abap_conv_in_ce,
          lr_xstring TYPE xstring,
          lr_string  TYPE string.
*Before sending encrypted information to external system, remove the extra
*16 bit padding from the xstring
    lr_xstring = lv_message+16.


    DATA: lt_data TYPE tsfixml, l_len TYPE i.
    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        input  = lr_xstring
      IMPORTING
        output = enc_text.
  ENDMETHOD.

  METHOD decryption.

    IF enc_text IS NOT INITIAL.
      CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
        EXPORTING
          input  = enc_text
*         UNESCAPE       = 'X'
        IMPORTING
          output = gv_xstring
*       EXCEPTIONS
*         FAILED = 1
*         OTHERS = 2
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.

    IF gv_xstring IS NOT INITIAL.
***** For CL_SEC_SXML_WRITER to work with external application we need to add 16 bit
***** extra padding before decryption
      gv_padding = '00000000000000000000000000000000'.

      CONCATENATE gv_padding(16) gv_xstring INTO gv_xstring IN BYTE MODE.
      TRY.
          cl_sec_sxml_writer=>decrypt(
            EXPORTING
              ciphertext = gv_xstring
              key        = key
              algorithm  = cl_sec_sxml_writer=>co_aes256_algorithm_pem
            IMPORTING
              plaintext  = DATA(lv_message_decrypted) ).
          " convert xstring to string for output
          cl_abap_conv_in_ce=>create( input = lv_message_decrypted )->read( IMPORTING data = decrypted_message ).
        CATCH cx_sec_sxml_encrypt_error INTO DATA(oref).
          " Handle decryption errors
          WRITE: / 'Decryption failed: ' && oref->get_longtext( ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA: public_key TYPE xstring.

**********************************************************************
* Method to generate public key
* This key gets shared to the calling program
**********************************************************************
  CALL METHOD cl_sec_sxml_writer=>generate_key
    EXPORTING
      algorithm = cl_sec_sxml_writer=>co_aes256_algorithm
    RECEIVING
      key       = public_key.




  DATA(enc_text) = lcl_security=>encrypt( key    = public_key
                                          prompt = 'Hello Codie' ).

*  CALL METHOD cl_sec_sxml_writer=>generate_key
*    EXPORTING
*      algorithm = cl_sec_sxml_writer=>co_aes256_algorithm
*    RECEIVING
*      key       = key.
*
*  WRITE key.

  DATA(decrypted_message) = lcl_security=>decryption( key      = public_key
                                                      enc_text = enc_text ). "Encoded text ('Hello Codie') in Base64 format - 'SGVsbG8gY29kaWU='

  WRITE decrypted_message.
