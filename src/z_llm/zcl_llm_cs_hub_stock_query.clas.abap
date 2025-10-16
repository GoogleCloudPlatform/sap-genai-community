CLASS zcl_llm_cs_hub_stock_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.


    TYPES : BEGIN OF ty_expected_date_text,
              article            TYPE matnr,
              country            TYPE zcountry,
              expected_date_text TYPE zexpecteddatetext,
              webname            TYPE zsmt_web_name2,
              sap_description    TYPE maktx,
              store              TYPE werks_d,
              store_name         type name1,
            END OF ty_expected_date_text,

            BEGIN OF ty_article_descriptions,
              article         TYPE matnr,
              webname         TYPE zsmt_web_name2,
              sap_description TYPE maktx,
            END OF ty_article_descriptions,

            BEGIN OF ty_stock,
            matnr   type matnr,
            labst   type labst,
            werks   type werks_d,
            END OF ty_stock,


            tt_expected_date_text   TYPE STANDARD TABLE OF ty_expected_date_text WITH EMPTY KEY,
            tt_article_descriptions TYPE STANDARD TABLE OF ty_article_descriptions WITH EMPTY KEY,
            tt_stock                TYPE STANDARD TABLE OF ty_stock WITH EMPTY KEY.

    METHODS : constructor,

      check_stock_availability                  IMPORTING it_article_range TYPE ztt_article_range
                                                          it_store_range   TYPE ztt_site_range
                                                RETURNING VALUE(rt_current_stock) TYPE tt_stock,

      get_expected_date_text                    IMPORTING it_article_range        TYPE ztt_article_range
                                                          it_country_range        TYPE ztt_country_range
                                                RETURNING VALUE(rt_expected_date) TYPE tt_expected_date_text,

      get_po_schedule_lines                     IMPORTING it_article_range        TYPE ztt_article_range
                                                          it_country_range        TYPE ztt_country_range
                                                RETURNING VALUE(rt_delivery_date) TYPE tt_expected_date_text,

      get_article_descriptions                  IMPORTING it_article_range       TYPE ztt_article_range
                                                          it_country_range       TYPE ztt_country_range
                                                RETURNING VALUE(rt_descriptions) TYPE tt_article_descriptions.                                          .



  PROTECTED SECTION.
  PRIVATE SECTION.



ENDCLASS.



CLASS zcl_llm_cs_hub_stock_query IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.



  METHOD check_stock_availability.
*** Method implementation to fetch current stock position
*** ENSURE THAT THIS LOGIC IS CONSISTENT WITH THE LOGIC IN CS HUB

    SELECT mard~matnr, mard~labst, mard~werks
    FROM mard
    INTO TABLE @rt_current_stock
    WHERE mard~matnr IN @it_article_range
    AND mard~werks IN @it_store_range
    AND lgort = '0001'.


    SELECT SINGLE value
    FROM zparameters
    INTO @DATA(reserve_stock)
    WHERE zprogram = 'CSHUB'
    AND param_name = 'RESERVE_STOCK'.


    LOOP AT rt_current_stock ASSIGNING FIELD-SYMBOL(<fs_article_stock>) WHERE labst > 0.

      SELECT SUM( vmeng )
        FROM vbbe
        INTO @DATA(lv_open_stock)
        WHERE matnr = @<fs_article_stock>-matnr
        AND werks = @<fs_article_stock>-werks
        AND vbtyp = 'J'
        AND lgort = '0001'.

      <fs_article_stock>-labst = <fs_article_stock>-labst - lv_open_stock.

      IF reserve_stock IS NOT INITIAL.
        <fs_article_stock>-labst = <fs_article_stock>-labst - reserve_stock.
      ENDIF.


      IF <fs_article_stock>-labst < 0.
        <fs_article_stock>-labst = 0.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_expected_date_text.

*   Fetch expected date text maintained for the article and country passed
*   ENSURE ARTICLE RANGE IS NOT EMPTY, as it will lead to fetching the whole table
*   However, if country range is not passed, it is okay, as fetching all relevant records might not impact performance
    IF it_article_range IS NOT INITIAL.

      SELECT zart_ctry_fields~matnr AS article,
             country,
             expect_date_text,
             web_name2 AS webname,
             maktx AS sap_description
      FROM zart_ctry_fields
      INNER JOIN ztb_descriptab2 ON zart_ctry_fields~matnr = ztb_descriptab2~matnr
                                AND lang = 'E'    "Change
      INNER JOIN makt ON makt~matnr = zart_ctry_fields~matnr
                     AND spras = 'E'               "NEEDS CHANGING
      WHERE zart_ctry_fields~matnr IN @it_article_range
        AND country IN @it_country_range
       INTO TABLE @rt_expected_date.

    ENDIF.

  ENDMETHOD.

  METHOD get_po_schedule_lines.

*   Look for PO delivery schedule lines for the article and country passed
*   ENSURE ARTICLE RANGE IS NOT EMPTY, as it will lead to fetching the whole table
*   However, if country range is not passed, it is okay, as fetching all relevant records might not impact performance as much
*   This SELECT is adjacent to AMDO logic for fetching scheduled lines
    IF it_article_range IS NOT INITIAL.

*     Selecting so many fields, to help debug and verify results
      SELECT ekpo~ebeln,
             ekpo~ebelp,
             ekpo~matnr,
             CASE ekpo~werks
               WHEN 'WE02'
                   THEN 'WE04'
                 WHEN 'WE05'
                   THEN 'WE04'
                 ELSE ekpo~werks
               END AS werks,
             ekko~lifnr,
             ekko~waers,
             ekko~verkf,
             zapp_site~location,
             ekpo~menge,
             ekpo~umrez,
             ekpo~umren,
             ekpo~pstyp,
             ekko~bsart,
             eket~ebeln AS sc_po,
             eket~ebelp AS sc_item,
             ekpo~matnr AS sc_art,
             eket~etenr,
             eket~eindt,
             eket~wemng,
             web_name2 AS webname,
             maktx AS sap_description,
             t001w~name1 AS store_name
          FROM ekpo
          INNER JOIN ekko      ON ekpo~ebeln = ekko~ebeln
          INNER JOIN zapp_site ON ekpo~werks = zapp_site~werks
          LEFT OUTER JOIN eket ON eket~ebeln = ekpo~ebeln
                              AND eket~ebelp = ekpo~ebelp
          INNER JOIN ztb_descriptab2 ON ekpo~matnr = ztb_descriptab2~matnr
                                    AND lang = 'E'    "Change
          INNER JOIN makt ON makt~matnr = ekpo~matnr
                         AND makt~spras = 'E'               "NEEDS CHANGING
          INNER JOIN t001w ON t001w~werks = ekpo~werks
          INTO TABLE @DATA(lt_scheduled_deliveries)
          WHERE ekpo~matnr IN @it_article_range
            AND zapp_site~location IN @it_country_range
            AND ekpo~loekz = @space
            AND ekpo~elikz = @space
            AND ekpo~bstyp = 'F'    "F : Purchase order
            AND eket~eindt >= @sy-datum  "To ensure expected delivery date is today or in the future
            AND ekko~bsart NOT IN ( 'ZICR', 'ZRPO', 'NB' )
          ORDER BY eindt ASCENDING.  "To get date that's closest from today


    ENDIF.

    LOOP AT lt_scheduled_deliveries ASSIGNING FIELD-SYMBOL(<fs_sch_del>).
      APPEND INITIAL LINE TO rt_delivery_date ASSIGNING FIELD-SYMBOL(<fs_del_date>).
      <fs_del_date>-article = <fs_sch_del>-matnr.
      <fs_del_date>-sap_description = <fs_sch_del>-sap_description.
      <fs_del_date>-webname = <fs_sch_del>-webname.
      <fs_del_date>-country = <fs_sch_del>-location.
      <fs_del_date>-expected_date_text = <fs_sch_del>-eindt.
      <fs_del_date>-store = <fs_sch_del>-werks.
      <fs_del_date>-store_name = <fs_sch_del>-store_name.
    ENDLOOP.

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

ENDCLASS.
