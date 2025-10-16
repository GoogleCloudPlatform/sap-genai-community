CLASS zcl_llm_roles_auth_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA: ls_roles_auth TYPE zllm_roles_auths.

    METHODS: get_roles_auth RETURNING VALUE(roles) TYPE zllm_roles_auths_tt
                            RAISING   zcx_llm.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_llm_roles_auth_util IMPLEMENTATION.

  METHOD get_roles_auth.

    DATA: tab_and_permission TYPE string.

* Check if the user has any role (Ex: Developer, CS Admin )
    SELECT SINGLE FROM zllm_auths
    FIELDS user_name, roleid, deleted
    WHERE user_name = @sy-uname
    INTO @DATA(user).
    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_llm
        EXPORTING
          textid = zcx_llm=>username_not_found.
    ELSE.

* Get the user and their respective roles
      SELECT FROM zllm_auths AS a
      INNER JOIN zllm_auths_roles AS b
      ON a~roleid = b~roleid
      FIELDS a~user_name, a~roleid, b~app_id, b~business_function, b~region, b~agent, b~tab_id, b~deleted, b~exclude_from_reporting
      WHERE a~user_name = @sy-uname
      AND   b~deleted = @abap_false
      INTO TABLE @DATA(auths_roles).

      IF sy-subrc EQ 0.

        SELECT FROM zllm_auths_tabs
        FIELDS *
        WHERE deleted = @abap_false
        INTO TABLE @DATA(lt_auths_tabs).
      ENDIF.

*        Build return structure
      LOOP AT auths_roles INTO DATA(ls_roles).
        CLEAR ls_roles_auth.
        ls_roles_auth-user_name = sy-uname.
        ls_roles_auth-role = ls_roles-roleid.
        ls_roles_auth-app_id = ls_roles-app_id.
        ls_roles_auth-business_function = ls_roles-business_function.
        ls_roles_auth-region = ls_roles-region.
        ls_roles_auth-agent = ls_roles-agent.
        ls_roles_auth-deleted = ls_roles-deleted.
        ls_roles_auth-exclude_from_reporting = ls_roles-exclude_from_reporting.

*              Separate the tabId values from commas
        SPLIT ls_roles-tab_id AT ',' INTO TABLE DATA(lt_tabid).

        LOOP AT lt_tabid ASSIGNING FIELD-SYMBOL(<fs_tabid>).
*               Fetch the relavant tab values using tabid
          DATA(auth_tabs) = VALUE zllm_auths_tabs( lt_auths_tabs[ tab_id = <fs_tabid> ] OPTIONAL ).

          IF auth_tabs IS NOT INITIAL.
*                  Separate the tab values from commas
            SPLIT auth_tabs-tab AT ',' INTO TABLE DATA(lt_tab).
*                       Build tab and permission using $$
            LOOP AT lt_tab ASSIGNING FIELD-SYMBOL(<fs_tab>).
              tab_and_permission = |{ tab_and_permission }| && |{ <fs_tab> } $$ { auth_tabs-permission } ,|.
              CONDENSE tab_and_permission NO-GAPS.
            ENDLOOP.
            CLEAR: auth_tabs,lt_tab.

          ENDIF.
        ENDLOOP.

        ls_roles_auth-tab = tab_and_permission.

        APPEND ls_roles_auth TO roles.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
