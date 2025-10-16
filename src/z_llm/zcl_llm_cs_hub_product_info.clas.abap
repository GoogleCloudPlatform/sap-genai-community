CLASS zcl_llm_cs_hub_product_info DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.


    TYPES : BEGIN OF ty_article_descriptions,
              article         TYPE matnr,
              webname         TYPE zsmt_web_name,
              sap_description TYPE maktx,
            END OF ty_article_descriptions,

            tt_article_descriptions TYPE STANDARD TABLE OF ty_article_descriptions WITH EMPTY KEY.

    METHODS : constructor,

              get_web_data                              IMPORTING it_article_range         TYPE ztt_article_range
                                                        RETURNING VALUE(response) TYPE string,

              get_article_measurements                  IMPORTING it_article_range         TYPE ztt_article_range
                                                        RETURNING VALUE(response) TYPE string,

              get_article_descriptions                  IMPORTING it_article_range       TYPE ztt_article_range
                                                                  it_country_range       TYPE ztt_country_range
                                                        RETURNING VALUE(rt_descriptions) TYPE tt_article_descriptions.                                          .



  PROTECTED SECTION.
  PRIVATE SECTION.



ENDCLASS.



CLASS zcl_llm_cs_hub_product_info IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.


  METHOD get_article_descriptions.

*   Fetch article web name and descriptions
*   ENSURE ARTICLE RANGE IS NOT EMPTY, as it will lead to fetching the whole table
*   However, if country range is not passed, it is okay, as fetching all relevant records might not impact performance
    IF it_article_range IS NOT INITIAL.

      DATA(lv_language) = 'E'.   "To change it to use country later

      SELECT ztb_descriptab2~matnr AS article,
             web_name2 AS webname,
             maktx AS sap_description
      FROM ztb_descriptab2
      INNER JOIN makt ON makt~matnr = ztb_descriptab2~matnr
                     AND spras = @lv_language
      WHERE ztb_descriptab2~matnr IN @it_article_range
        AND lang = @lv_language
       INTO TABLE @rt_descriptions.

    ENDIF.

  ENDMETHOD.

  METHOD get_article_measurements.
*** Method implementation to get measurements like dimensions, weight for the article range passed

    IF it_article_range IS NOT INITIAL.

      SELECT marm~matnr,
             marm~laeng,
             marm~breit,
             marm~hoehe,
             marm~meabm,
             marm~volum,
             marm~voleh,
             marm~brgew,
             marm~gewei
        FROM marm
        INTO TABLE @DATA(lt_marm)
        WHERE matnr IN @it_article_range
          AND meinh = 'EA'.

      LOOP AT lt_marm ASSIGNING FIELD-SYMBOL(<fs_marm>).

        response = response &&
                   |\n ARTICLE: <{ <fs_marm>-matnr ALPHA = OUT } - <DIMENSIONS>: \n\n| &&
                   | LENGTH: { <fs_marm>-laeng } \n BREADTH: { <fs_marm>-breit } \n  HEIGHT: { <fs_marm>-hoehe } \n| &&
                   | UNIT OF MEASUREMENT FOR L/B/H: { <fs_marm>-meabm } | &&
                   | VOLUME: { <fs_marm>-volum } \n UNIT FOR VOLUME:{ <fs_marm>-voleh } \n | &&
                   | GROSS WEIGHT: { <fs_marm>-brgew } \n UNIT FOR WEIGHT: { <fs_marm>-gewei }|.


      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD get_web_data.
*** Method implementation to get web data/data on website for the article range passed

    IF it_article_range IS NOT INITIAL.

      SELECT matnr,
             z_web_feature_1, z_web_feature_2, z_web_feature_3,
             z_web_feature_4, z_web_feature_5, z_web_feature_6,
             z_web_feature_7, z_web_feature_8, z_web_feature_9,
             z_web_feature_10
      FROM ztb_descriptab
      INTO TABLE @DATA(lt_article_web)
      WHERE matnr IN @it_article_range
        AND lang = 'E'.

      LOOP AT lt_article_web ASSIGNING FIELD-SYMBOL(<fs_article_web>).

        response = response &&
                   |\n ARTICLE: <{ <fs_article_web>-matnr ALPHA = OUT } - <PRODUCT DATA>: { <fs_article_web>-z_web_feature_1 } \n { <fs_article_web>-z_web_feature_2 } \n | &
                   |{ <fs_article_web>-z_web_feature_3 } \n { <fs_article_web>-z_web_feature_4 } \n { <fs_article_web>-z_web_feature_5 } \n| &
                   |{ <fs_article_web>-z_web_feature_6 } \n { <fs_article_web>-z_web_feature_7 } \n { <fs_article_web>-z_web_feature_8 } \n| &
                   |{ <fs_article_web>-z_web_feature_9 } \n { <fs_article_web>-z_web_feature_10 } \n|.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
