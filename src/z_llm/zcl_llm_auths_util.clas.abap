CLASS zcl_llm_auths_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_fiori_llm_util.

  PUBLIC SECTION.

    TYPES: tt_llm_auths TYPE STANDARD TABLE OF zllm_auths WITH EMPTY KEY.

    METHODS get_auth_roles IMPORTING app_id            TYPE zllm_app_id
                                     business_function TYPE zllm_buss_function_name
                           RETURNING VALUE(roles)      TYPE zllm_roles_auth_odata_tt.

    METHODS get_users_and_their_roles RETURNING VALUE(user_roles)  TYPE zllm_user_roles_odata_tt.

    METHODS get_auth_tabid RETURNING VALUE(auth_tabs)  TYPE zllm_auths_tabs_odata_tt.

    METHODS get_users_to_excl_from_report IMPORTING app_id            TYPE zllm_app_id
                                                    business_function TYPE zllm_buss_function_name
                                          RETURNING VALUE(rt_username_and_role) TYPE tt_llm_auths.


  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
      create_role IMPORTING role TYPE zllm_auths_roles
                  RAISING   zcx_llm,
      create_tabid IMPORTING tabs TYPE zllm_auths_tabs_odata
                   RAISING   zcx_llm,
      check_existing_role_config IMPORTING role          TYPE zllm_auths_roles
                                 RETURNING VALUE(exists) TYPE boolean,
      check_existing_roleid IMPORTING roleid        TYPE zllm_auth_role
                            RETURNING VALUE(exists) TYPE boolean,
      check_mandatory_fields IMPORTING role         TYPE zllm_auths_roles
                             RETURNING VALUE(valid) TYPE boolean
                             RAISING   zcx_llm,
      assign_role_to_user IMPORTING user              TYPE uname
                                    roleid            TYPE zllm_auth_role
                                    deleted           TYPE zllm_auth_deleted
                                    app_id            TYPE zllm_app_id
                                    business_function TYPE zllm_buss_function_name
                          RAISING   zcx_llm,
      check_existing_tabid IMPORTING tabs          TYPE zllm_auths_tabs_odata
                           RETURNING value(exists) TYPE boolean
                           RAISING   zcx_llm.

ENDCLASS.



CLASS ZCL_LLM_AUTHS_UTIL IMPLEMENTATION.


  METHOD get_auth_roles.

    SELECT roleid, role_desc, region, app_id, business_function, agent, tab_id, deleted, exclude_from_reporting
    FROM zllm_auths_roles
    INTO TABLE @roles
    WHERE app_id = @app_id
    AND business_function = @business_function
    AND deleted = @abap_false.


  ENDMETHOD.


    METHOD assign_role_to_user.

** First, check that the role being assigned exists in the config table.
*      DATA(exists) = me->check_existing_roleid( roleid = roleid ).
*
*      IF exists = abap_false.
*
*        RAISE EXCEPTION TYPE zcx_llm
*          EXPORTING
*            textid = zcx_llm=>invalid_role_config
*            msg1   = 'Role ID does not exist'.
*
*      ENDIF.

      TRY.
          DATA(lcl_crud_util) = NEW zcl_llm_crud_control_util( app_id            = app_id
                                                               business_function = business_function ).

          DATA(status) = lcl_crud_util->assign_role_to_user( roleid  = roleid
                                                             user    = user
                                                             deleted = deleted ).

        CATCH zcx_llm INTO DATA(lcx_llm).
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.
      ENDTRY.
    ENDMETHOD.


    METHOD create_role.

* Check for mandatory fields in structure.
      TRY.
          DATA(mandatory_fields) = me->check_mandatory_fields( role = role ).


* Check if the role already exists.
          DATA(exists) = me->check_existing_role_config( role = role ).


          IF exists = abap_true.
            RAISE EXCEPTION TYPE zcx_llm
              EXPORTING
                textid = zcx_llm=>role_already_exists
                msg1   = CONV string( role-app_id )
                msg2   = CONV string( role-business_function ).
          ENDIF.


          DATA(lcl_crud_util) = NEW zcl_llm_crud_control_util( app_id            = role-app_id
                                                               business_function = role-business_function ).

          DATA(status) = lcl_crud_util->create_authorisation_role( role = role ).


        CATCH zcx_llm INTO DATA(lcx_llm).
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.
      ENDTRY.
    ENDMETHOD.


    METHOD check_mandatory_fields.

      IF role-app_id IS INITIAL.

        valid = abap_false.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>invalid_role_config
            msg1   = 'app id is initial'.

      ELSEIF role-business_function IS INITIAL.

        valid = abap_false.

        RAISE EXCEPTION TYPE zcx_llm
          EXPORTING
            textid = zcx_llm=>invalid_role_config
            msg1   = 'business function is initial'.

      ELSEIF role-app_id IS NOT INITIAL AND
             role-business_function IS NOT INITIAL.

        SELECT SINGLE app_id, business_function
        FROM zllm_bots
        INTO @DATA(ls_bot)
        WHERE app_id = @role-app_id
          AND business_function = @role-business_function
          AND deleted = @abap_false.

        IF sy-subrc <> 0.

          valid = abap_false.

          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = zcx_llm=>invalid_role_config
              msg1   = 'bot configuration is missing'.
        ENDIF.


*      ELSEIF role-region IS INITIAL.
*
*        valid = abap_false.
*
*        RAISE EXCEPTION TYPE zcx_llm
*          EXPORTING
*            textid = zcx_llm=>invalid_role_config
*            msg1   = 'region is initial'.

      ELSE.
* Valid role config
        valid = abap_true.
      ENDIF.

    ENDMETHOD.


    METHOD check_existing_roleid.


      SELECT SINGLE *
      FROM zllm_auths_roles
      INTO @DATA(ls_roles)
      WHERE roleid = @roleid.


      IF sy-subrc <> 0.
        exists = abap_false.

      ELSE.
        exists = abap_true.
      ENDIF.

    ENDMETHOD.


    METHOD check_existing_role_config.

      SELECT SINGLE *
      FROM zllm_auths_roles
      INTO @DATA(ls_existing_role)
      WHERE roleid = @role-roleid
      AND app_id = @role-app_id
      AND business_function = @role-business_function.

* Nothing found so role cannot exist.
      IF sy-subrc <> 0.
        exists = abap_false.
        RETURN.
      ENDIF.


* We need to go through the tab seperately as it might be that the tab order is different, but the same tabs exists.
      SPLIT role-tab_id AT ',' INTO TABLE DATA(lt_existing_role_tab).
      SPLIT ls_existing_role-tab_id AT ',' INTO TABLE DATA(lt_new_role_tab).

      SORT lt_existing_role_tab DESCENDING.
      SORT lt_new_role_tab DESCENDING.
      DATA(lines) = lines( lt_new_role_tab ).
      DATA count TYPE i.

      LOOP AT lt_new_role_tab ASSIGNING FIELD-SYMBOL(<fs_new_role_tab>).
* Does this entry exist in the table?
        TRY.
            IF ls_existing_role-tab_id CA <fs_new_role_tab>.
              count = count + 1.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      ENDLOOP.

* If the count and lines match, then we have an identical role being re-created. Bit pointless.
      IF count = lines.
        exists = abap_true.
      ELSE.
        exists = abap_false.
      ENDIF.

    ENDMETHOD.


  METHOD get_users_and_their_roles.

  SELECT FROM zllm_auths
  FIELDS user_name,
         roleid,
         deleted
  WHERE deleted = @abap_false
  into TABLE @DATA(lt_user_roles).

  IF lt_user_roles IS NOT INITIAL.
  user_roles = VALUE zllm_user_roles_odata_tt( FOR ls_user_roles IN lt_user_roles
                                                                 ( app_id            = ' '
                                                                   business_function = ' '
                                                                   usr               = ls_user_roles-user_name
                                                                   roleid            = ls_user_roles-roleid
                                                                   deleted           = ls_user_roles-deleted ) ).

  ENDIF.

  ENDMETHOD.


  METHOD get_users_to_excl_from_report.
*** Method implementation to get list of usernames to be excluded from being counted in reports


*   Passing the App ID and business function, and the exclude flag as true, fetch the list of roles
*   that shouldn't be accounted for in reports, followed by fetching all the user names assigned to the role
    SELECT zllm_auths~mandt,
           zllm_auths~user_name,
           zllm_auths~roleid,
           zllm_auths~deleted
      FROM zllm_auths
      LEFT OUTER JOIN zllm_auths_roles ON zllm_auths_roles~roleid = zllm_auths~roleid
      INTO TABLE @rt_username_and_role
      WHERE zllm_auths_roles~app_id = @app_id
        AND zllm_auths_roles~business_function = @business_function
        AND zllm_auths_roles~exclude_from_reporting = @abap_true.

  ENDMETHOD.


  METHOD create_tabid.

      TRY.
         data: ls_tabs TYPE zllm_auths_tabs.
* Check if the tab id already exists.
          DATA(exists) = me->check_existing_tabid( tabs = tabs ).


          IF exists = abap_true.
          "Raise Tab id already exists
*            RAISE EXCEPTION TYPE zcx_llm
*              EXPORTING
*                textid = zcx_llm=>role_already_exists.
          ENDIF.

          ls_tabs-client     = sy-mandt.
          ls_tabs-tab_id     = tabs-tab_id.
          ls_tabs-tab        = tabs-tab.
          ls_tabs-permission = tabs-permission.
          ls_tabs-deleted    = tabs-deleted.

          DATA(lcl_crud_util) = NEW zcl_llm_crud_control_util( app_id            = tabs-app_id
                                                               business_function = tabs-business_function ).

          DATA(status) = lcl_crud_util->create_authorisation_tabid( tabs = ls_tabs ).


        CATCH zcx_llm INTO DATA(lcx_llm).
          RAISE EXCEPTION TYPE zcx_llm
            EXPORTING
              textid = lcx_llm->if_t100_message~t100key
              msg1   = lcx_llm->msg1
              msg2   = lcx_llm->msg2
              msg3   = lcx_llm->msg3.
      ENDTRY.

  ENDMETHOD.


  METHOD check_existing_tabid.

      SELECT SINGLE *
      FROM zllm_auths_tabs
      INTO @DATA(ls_existing_tabid)
      WHERE tab_id  = @tabs-tab_id
      AND   deleted = @abap_false.

* Nothing found so tab id cannot exist.
      IF sy-subrc <> 0.
        exists = abap_false.
        RETURN.
      ENDIF.

  ENDMETHOD.


  METHOD get_auth_tabid.

  SELECT FROM zllm_auths_tabs
  FIELDS tab_id,
         tab,
         permission,
         deleted
  WHERE deleted = @abap_false
  into TABLE @DATA(lt_auth_tabs).

  IF lt_auth_tabs IS NOT INITIAL.
  auth_tabs = VALUE zllm_auths_tabs_odata_tt( FOR ls_auth_tabs IN lt_auth_tabs
                                                                 ( app_id            = ' '
                                                                   business_function = ' '
                                                                   tab_id            = ls_auth_tabs-tab_id
                                                                   tab               = ls_auth_tabs-tab
                                                                   permission        = ls_auth_tabs-permission
                                                                   deleted           = ls_auth_tabs-deleted ) ).

  ENDIF.
  ENDMETHOD.
ENDCLASS.
