CLASS zcl_llm_conversation_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING app_id            TYPE zllm_app_id
                            business_function TYPE zllm_buss_function_name,
      addto_conversation IMPORTING conversation TYPE string
                                   type         TYPE zllm_conversation_type
                         RAISING   zcx_llm,
      fetch_conversation RETURNING VALUE(conversation) TYPE string.



  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: gv_app_id            TYPE zllm_app_id,
          gv_business_function TYPE zllm_buss_function_name.

ENDCLASS.



CLASS zcl_llm_conversation_util IMPLEMENTATION.

  METHOD constructor.

    gv_app_id = app_id.
    gv_business_function = business_function.

  ENDMETHOD.


  METHOD fetch_conversation.

    SELECT conversation
    FROM zllm_conv
    INTO TABLE @DATA(lt_conversation)
    WHERE user_name = @sy-uname
    AND app_id = @gv_app_id
    AND business_function = @gv_business_function.


    LOOP AT lt_conversation ASSIGNING FIELD-SYMBOL(<fs_conversation>).

      IF sy-tabix = 1.
        conversation = |{ <fs_conversation>-conversation }|.
      ELSE.
        conversation = |{ conversation }\n{ <fs_conversation>-conversation }|.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD addto_conversation.

* append to the existing conversation.
    CASE type.
      WHEN 'BOT'.
        DATA(lv_conversation_type) = 'BOT'.
        DATA(lv_direction) = 'RESPONSE'.
      WHEN 'HUMAN'.
        lv_conversation_type = sy-uname.
        lv_direction = 'QUESTION'.
    ENDCASE.


    DATA(converation) = |<{ lv_conversation_type }>\n| &
                        |<DATE-TIME>{ sy-datum } / { sy-uzeit } </DATE-TIME>\n| &
                        |<{ lv_direction }>{ conversation }</{ lv_direction }>| &
                        |</{ lv_conversation_type }>|.


    DATA(lcl_save_conv) = NEW zcl_llm_crud_control_util( app_id            = gv_app_id
                                                         business_function = gv_business_function ).


    TRY.

        lcl_save_conv->save_conversation( conversation = conversation ).

      CATCH zcx_llm INTO DATA(lcx_llm).

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = lcx_llm->if_t100_message~t100key
            msg1   = lcx_llm->msg1
            msg2   = lcx_llm->msg2
            msg3   = lcx_llm->msg3.

    ENDTRY.

  ENDMETHOD.


ENDCLASS.
