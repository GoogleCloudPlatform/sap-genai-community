CLASS zcl_llm_report_prompt_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_prompt,
             mandt             TYPE zllm_prompt_fav-mandt,
             app_id            TYPE zllm_prompt_fav-app_id,
             business_function TYPE zllm_prompt_fav-business_function,
             prompt_id         TYPE zllm_prompt_fav-prompt_id,
             user_id           TYPE zllm_prompt_fav-user_id,
             prompt            TYPE zllm_prompt_fav-prompt,
             deleted           TYPE zllm_prompt_fav-deleted,
           END OF ty_prompt,
           tt_prompt TYPE TABLE OF ty_prompt WITH EMPTY KEY.

    METHODS: constructor IMPORTING app_id            TYPE zllm_prompt_fav-app_id
                                   business_function TYPE zllm_prompt_fav-business_function
                                   prompt_id         TYPE zllm_prompt_fav-prompt_id OPTIONAL
                                   prompt            TYPE zllm_prompt_fav-prompt OPTIONAL
                                   deleted           TYPE zllm_prompt_fav-deleted OPTIONAL,

      save_prompt,

      get_prompt RETURNING VALUE(rt_prompt) TYPE tt_prompt.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: app_id            TYPE zllm_app_id,
          business_function TYPE zllm_buss_function_name,
          prompt_id         TYPE guid,
          prompt            TYPE zllm_prompt_text,
          deleted           TYPE zllm_prompt_deleted.

ENDCLASS.



CLASS zcl_llm_report_prompt_util IMPLEMENTATION.

  METHOD constructor.

    me->app_id = app_id.
    me->business_function = business_function.
    me->prompt_id = prompt_id.
    me->prompt = prompt.
    me->deleted = deleted.

  ENDMETHOD.

  METHOD get_prompt.

    SELECT FROM zllm_prompt_fav
       FIELDS mandt,
              app_id,
              business_function,
              prompt_id,
              user_id,
              prompt,
              deleted
       WHERE  app_id = @me->app_id
         AND  business_function = @me->business_function
         AND  user_id = @sy-uname
         AND  deleted = @abap_false
       INTO TABLE @rt_prompt.

  ENDMETHOD.

  METHOD save_prompt.

    IF me->prompt_id IS INITIAL.
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_16 = me->prompt_id.
    ENDIF.

    DATA(ls_prompt) = VALUE zllm_prompt_fav( mandt = sy-mandt
                                             app_id = me->app_id
                                             business_function = me->business_function
                                             prompt_id = me->prompt_id
                                             user_id = sy-uname
                                             prompt = me->prompt
                                             deleted = me->deleted ).



    MODIFY zllm_prompt_fav FROM ls_prompt.

  ENDMETHOD.

ENDCLASS.
