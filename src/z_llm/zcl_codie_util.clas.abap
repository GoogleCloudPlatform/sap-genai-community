CLASS zcl_codie_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_sites,
        werks     TYPE werks_d,
        location  TYPE zz_site_location,
        site_type TYPE zz_site_type,
      END OF ty_sites ,

      tt_sites TYPE TABLE OF ty_sites WITH EMPTY KEY,

      BEGIN OF ty_ekbe,
        ebeln TYPE ebeln,
        ebelp TYPE ebelp,
        matnr TYPE matnr,
        menge TYPE menge_d,
      END OF ty_ekbe ,

      tt_ekbe TYPE TABLE OF ty_ekbe WITH DEFAULT KEY,

      BEGIN OF ty_po_qty,
        article      TYPE matnr,
        region       TYPE zz_site_location,
        po_qty_in_ea TYPE bstmg,
      END OF ty_po_qty ,

      BEGIN OF ty_region_range,
        sign   TYPE ddsign,
        option TYPE ddoption,
        low    TYPE zregion,
        high   TYPE zregion,
      END OF ty_region_range,

      BEGIN OF ty_po_data,
        ebeln    TYPE ebeln,
        ebelp    TYPE ebelp,
        matnr    TYPE matnr,
        location TYPE zz_site_location,
        aedat    TYPE datum,
        eindt    TYPE eindt,
        menge    TYPE bstmg,
        umrez    TYPE umrez,
        umren    TYPE umren,
        bsart    TYPE bsart,
        lifnr    TYPE elifn,
        name1    TYPE name1_gp,
      END OF ty_po_data,

      tt_po_data TYPE TABLE OF ty_po_data,

      BEGIN OF ty_sales_org,
        vkorg       TYPE vkorg,
        country_key TYPE zregion,
        language    TYPE spras,
      END OF ty_sales_org,

      tt_sales_org TYPE TABLE OF ty_sales_org.

    METHODS: constructor IMPORTING VALUE(it_regions_range) TYPE ztt_region_range OPTIONAL,

      fetch_conf_po IMPORTING VALUE(it_articles)   TYPE ztt_matnr_range
                              VALUE(it_date_range) TYPE ztt_date_range
                    RETURNING VALUE(rt_return)     TYPE ztt_zcod_artpodeli ,

      fetch_sales_from_bw IMPORTING VALUE(it_articles)   TYPE ztt_matnr_range
                                    VALUE(it_date_range) TYPE ztt_date_range
                          RETURNING VALUE(rt_return)     TYPE ztt_codie_bw_sales,

      fetch_hist_stock_from_bw IMPORTING VALUE(it_articles)   TYPE ztt_matnr_range
                                         VALUE(it_date_range) TYPE ztt_date_range
                               RETURNING VALUE(rt_return)     TYPE ztt_codie_bw_hist_stock,

      fetch_price_history IMPORTING VALUE(it_articles) TYPE lmont_it_range_c22
                                    "VALUE(it_date_range) TYPE ztt_date_range
                          RETURNING VALUE(rt_return)   TYPE ztt_codie_price_history_per_ar,

      modify_zcod_artsalstock IMPORTING VALUE(it_sales) TYPE ztt_codie_bw_sales
                                        VALUE(it_stock) TYPE ztt_codie_bw_hist_stock,
      "VALUE(it_date_range) TYPE ztt_date_range,

      modify_zcod_artpodeli IMPORTING VALUE(it_po_data) TYPE ztt_zcod_artpodeli.


  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: gv_rfc_dest             TYPE rfcdest,
          gt_sites_range          TYPE ztt_werks_range,
          gt_regions_range        TYPE ztt_region_range,
          gt_sales_org_per_region TYPE tt_sales_org.

    METHODS: fetch_ekbe IMPORTING VALUE(it_po)   TYPE tt_po_data
                        RETURNING VALUE(rt_hist) TYPE tt_ekbe,

      fetch_sites,
      fetch_desc_and_webname IMPORTING VALUE(it_article_range) TYPE lmont_it_range_c22
                             RETURNING VALUE(rt_result)        TYPE ztt_codie_art_desc_webname,
      fetch_sales_org_per_region.



ENDCLASS.



CLASS zcl_codie_util IMPLEMENTATION.

  METHOD constructor.

*Get RFC destination
    gv_rfc_dest = NEW zcl_smyths_util( )->fetch_bw_destination( ).



* Fetch all active sites per region
* check if any region
    gt_regions_range = it_regions_range.

    IF it_regions_range IS INITIAL.

      APPEND INITIAL LINE TO gt_regions_range ASSIGNING FIELD-SYMBOL(<fs_region>).
      <fs_region>-sign = 'I'.
      <fs_region>-option = 'EQ'.
      <fs_region>-low = '*'.

    ENDIF.

    me->fetch_sites( ).

    me->fetch_sales_org_per_region( ).


  ENDMETHOD.

  METHOD fetch_conf_po.


    DATA: ls_region_range TYPE zst_region_range,
          lt_region_range TYPE ztt_region_range,
          lt_sites_range  TYPE ztt_werks_range,
          ls_articles     TYPE ty_po_qty,
          lt_po_data      TYPE tt_po_data,
          ls_return       LIKE LINE OF rt_return.

    SORT it_articles ASCENDING BY low.
    DELETE ADJACENT DUPLICATES FROM it_articles COMPARING low.



* Fetch POs
    SELECT ekpo~ebeln,
           ekpo~ebelp,
           ekpo~matnr,
           zapp_site~location,
           ekko~aedat,
           eket~eindt,
           ekpo~menge,
           ekpo~umrez,
           ekpo~umren,
           ekko~bsart,
           ekko~lifnr,
           lfa1~name1
      FROM ekpo
INNER JOIN zapp_site
        ON zapp_site~werks = ekpo~werks
INNER JOIN ekko
        ON ekpo~ebeln = ekko~ebeln
INNER JOIN eket
        ON eket~ebeln = ekko~ebeln
INNER JOIN lfa1
        ON lfa1~lifnr = ekko~lifnr
      INTO TABLE @lt_po_data
     WHERE ekpo~matnr IN @it_articles
       "AND ekpo~loekz = @space
       "AND ekpo~elikz = @space
       AND ekpo~pstyp = 0
       AND ekpo~bstyp = 'F'
       AND ekko~bsart NOT IN ( 'ZRPO', 'ZICR' , 'NB' )
       AND zapp_site~werks IN @gt_sites_range
       AND zapp_site~inactive = @space
       AND zapp_site~exclude_awr = @space
       AND zapp_site~exclude_stock = @space
       AND eket~eindt IN @it_date_range.



    SORT lt_po_data BY ebeln ebelp matnr.
    DELETE ADJACENT DUPLICATES FROM lt_po_data COMPARING ebeln ebelp matnr.



*    IF lt_po_data IS NOT INITIAL.
*      DATA(lt_po_history) = me->fetch_ekbe( EXPORTING it_po = lt_po_data ).
*    ENDIF.

    LOOP AT lt_po_data ASSIGNING FIELD-SYMBOL(<fs_po_data>).

*      TRY.
*          DATA(ls_ekbe) = lt_po_history[ ebeln = <fs_po_data>-ebeln
*                                         ebelp = <fs_po_data>-ebelp ].

*          <fs_po_data>-menge = <fs_po_data>-menge + ls_ekbe-menge.

*        CATCH cx_sy_itab_line_not_found.
*      ENDTRY.

      <fs_po_data>-menge = <fs_po_data>-menge * <fs_po_data>-umrez. " get qty in EA



* Assign return structure
      ls_return-po_number = <fs_po_data>-ebeln.
      ls_return-article = <fs_po_data>-matnr.
      ls_return-creation_date = <fs_po_data>-aedat.
      ls_return-delivery_date = <fs_po_data>-eindt.
      ls_return-meins = 'EA'. "Above logic will always covert to eaches
      ls_return-po_quantity = <fs_po_data>-menge.
      ls_return-region = <fs_po_data>-location.
      ls_return-vendor = <fs_po_data>-lifnr.
      ls_return-vendor_name = <fs_po_data>-name1.
      APPEND ls_return TO rt_return.
      CLEAR: ls_return. "ls_ekbe.

    ENDLOOP.


  ENDMETHOD.

  METHOD fetch_sales_from_bw.

    DATA: lt_sales    TYPE ztt_codie_bw_sales.

*************************************
* Get sales data
    CALL FUNCTION 'Z_CODIE_BW_SALES'
      DESTINATION gv_rfc_dest
      EXPORTING
        it_articles   = it_articles
        it_date_range = it_date_range
        it_regions    = gt_regions_range
      IMPORTING
        et_sales      = lt_sales.

    IF lt_sales IS NOT INITIAL.
      rt_return = lt_sales.
    ENDIF.

  ENDMETHOD.

  METHOD fetch_sites.


    SELECT 'I' AS sign,
           'EQ' AS option,
            werks AS low
     FROM zapp_site
     INTO TABLE @gt_sites_range
      WHERE location IN @gt_regions_range
        AND inactive EQ @abap_false
        AND exclude_stock EQ @abap_false
        AND exclude_awr EQ @abap_false.


  ENDMETHOD.

  METHOD fetch_ekbe.


    DATA: ls_ekbe TYPE ty_ekbe.

    SELECT ebeln,
           ebelp,
           zekkn,
           vgabe,
           gjahr,
           belnr,
           buzei,
           matnr,
           menge,
           shkzg
      FROM ekbe
INTO TABLE @DATA(lt_hist)
       FOR ALL ENTRIES IN @it_po
     WHERE matnr = @it_po-matnr
       AND ebeln = @it_po-ebeln
       AND ebelp = @it_po-ebelp
       AND bewtp = 'E'.

    LOOP AT lt_hist ASSIGNING FIELD-SYMBOL(<fs_hist>).

      ls_ekbe-matnr = <fs_hist>-matnr.
      ls_ekbe-ebeln = <fs_hist>-ebeln.
      ls_ekbe-ebelp = <fs_hist>-ebelp.

      IF <fs_hist>-shkzg = 'S'.

        ls_ekbe-menge = <fs_hist>-menge * -1.

      ELSEIF <fs_hist>-shkzg = 'H'.

        ls_ekbe-menge = <fs_hist>-menge.

      ENDIF.

      COLLECT ls_ekbe INTO rt_hist.
      CLEAR: ls_ekbe.

    ENDLOOP.

  ENDMETHOD.

  METHOD fetch_hist_stock_from_bw.

    DATA: lt_stock    TYPE ztt_codie_bw_hist_stock.

*************************************
* Get sales data
    CALL FUNCTION 'Z_BW_HISTORICAL_STOCK'
      DESTINATION gv_rfc_dest
      EXPORTING
        it_articles   = it_articles
        it_regions    = gt_regions_range
        it_sites      = gt_sites_range
        it_date_range = it_date_range
      IMPORTING
        rt_hist_stock = lt_stock.

    IF lt_stock IS NOT INITIAL.
      rt_return = lt_stock.
    ENDIF.

  ENDMETHOD.


  METHOD modify_zcod_artsalstock.

    DATA: ls_zcod_artsalstock TYPE zcod_artsalstock,
          lt_zcod_artsalstock TYPE ztt_zcod_artsalstock,
          lt_article_range    TYPE lmont_it_range_c22,
          lt_article_range2   TYPE lmont_it_range_c22,
          lt_art_desc_webname TYPE ztt_codie_art_desc_webname,
          ls_art_desc_webname TYPE zst_codie_art_desc_webname,
          lt_price_hist       TYPE ztt_codie_price_history_per_ar.
    "ls_price_hist       TYPE zst_codie_price_history_per_ar.


    IF it_sales IS NOT INITIAL.
*Create a range of articles from sales
      CALL FUNCTION 'LMON_RANGE_CREATE'
        EXPORTING
          iv_fieldname = 'ARTICLE'
        TABLES
          et_range     = lt_article_range
        CHANGING
          it_object    = it_sales.
    ENDIF.

    IF it_stock IS NOT INITIAL.
*Create a range of articles from stock
      CALL FUNCTION 'LMON_RANGE_CREATE'
        EXPORTING
          iv_fieldname = 'MATERIAL'
        TABLES
          et_range     = lt_article_range2
        CHANGING
          it_object    = it_stock.

    ENDIF.

* Add all together so we have the descriptions.
    APPEND LINES OF lt_article_range2 TO lt_article_range .
    SORT lt_article_range BY low ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_article_range COMPARING low.

*Fetch webname and desc
    lt_art_desc_webname = me->fetch_desc_and_webname( lt_article_range ).


*Fetch price per history
    lt_price_hist = me->fetch_price_history( it_articles   = lt_article_range ).

* Loop at all articles
    LOOP AT lt_article_range ASSIGNING FIELD-SYMBOL(<fs_art>).

      ls_zcod_artsalstock-client = 200.
      ls_zcod_artsalstock-article = <fs_art>-low.

      TRY.
          ls_art_desc_webname = lt_art_desc_webname[ article = <fs_art>-low ].
          ls_zcod_artsalstock-article_desc = ls_art_desc_webname-desc.
          ls_zcod_artsalstock-article_web_name = ls_art_desc_webname-web_name.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

* Loop at sales values. Potentially not all articles will have sales.
      LOOP AT it_sales ASSIGNING FIELD-SYMBOL(<fs_sales>) WHERE article = <fs_art>-low.

        ls_zcod_artsalstock-sales_quantity = <fs_sales>-sales_quantity.
        ls_zcod_artsalstock-stock_quantity = 0.
        ls_zcod_artsalstock-region = <fs_sales>-region.
        ls_zcod_artsalstock-dat = <fs_sales>-sales_date.
        ls_zcod_artsalstock-sales_net_val = <fs_sales>-sales_net_gbp.
        ls_zcod_artsalstock-sales_gros_val = <fs_sales>-sales_gros_gbp.
        APPEND ls_zcod_artsalstock TO lt_zcod_artsalstock.

      ENDLOOP.

* Loop at stock. Maybe some articles will not have stock because the BW table is not accurate yet
      LOOP AT it_stock ASSIGNING FIELD-SYMBOL(<fs_stock>) WHERE material = <fs_art>-low.

        TRY.
            ls_zcod_artsalstock = lt_zcod_artsalstock[ article = <fs_stock>-material
                                                       dat = <fs_stock>-calday
                                                       region = <fs_stock>-region ].


* If existing entry found. Just add stock
            ls_zcod_artsalstock-stock_quantity = <fs_stock>-stock.

            MODIFY lt_zcod_artsalstock
            FROM ls_zcod_artsalstock
            TRANSPORTING stock_quantity
            WHERE article = ls_zcod_artsalstock-article
            AND region = ls_zcod_artsalstock-region
            AND dat = ls_zcod_artsalstock-dat.

          CATCH cx_sy_itab_line_not_found.
* Entry with no sales. Populate entire structure
            ls_zcod_artsalstock-stock_quantity = <fs_stock>-stock.
            ls_zcod_artsalstock-sales_quantity = 0.
            ls_zcod_artsalstock-dat = <fs_stock>-calday.
            ls_zcod_artsalstock-region = <fs_stock>-region.
            APPEND ls_zcod_artsalstock TO lt_zcod_artsalstock.

        ENDTRY.
      ENDLOOP.

      CLEAR: ls_art_desc_webname.

    ENDLOOP.

* Add retail price
    SORT lt_zcod_artsalstock BY article dat DESCENDING.
    SORT lt_price_hist BY article start_date DESCENDING.
    LOOP AT lt_zcod_artsalstock ASSIGNING FIELD-SYMBOL(<fs_salst>).

      " Find the first matching record in the price history
      LOOP AT lt_price_hist ASSIGNING FIELD-SYMBOL(<fs_price>) WHERE article = <fs_salst>-article AND start_date <= <fs_salst>-dat AND end_date >= <fs_salst>-dat.
        "first match is found, assign the value and exit
        <fs_salst>-retail_price = <fs_price>-price.
        <fs_salst>-price_curr = <fs_price>-curr.
        EXIT.
      ENDLOOP.

    ENDLOOP.


    IF lt_zcod_artsalstock IS NOT INITIAL.
      MODIFY zcod_artsalstock FROM TABLE lt_zcod_artsalstock.
    ENDIF.



  ENDMETHOD.

  METHOD modify_zcod_artpodeli.

    DATA: ls_zcod_artpodeli   TYPE zcod_artpodeli,
          lt_zcod_artpodeli   TYPE ztt_zcod_artpodeli,
          lt_article_range    TYPE lmont_it_range_c22,
          lt_art_desc_webname TYPE ztt_codie_art_desc_webname,
          ls_art_desc_webname TYPE zst_codie_art_desc_webname.

*Create a range of articles
    CALL FUNCTION 'LMON_RANGE_CREATE'
      EXPORTING
        iv_fieldname = 'ARTICLE'
      TABLES
        et_range     = lt_article_range
      CHANGING
        it_object    = it_po_data.

*Fetch webname and desc
    lt_art_desc_webname = me->fetch_desc_and_webname( it_article_range = lt_article_range ).


    SORT it_po_data BY article delivery_date ASCENDING.

    LOOP AT it_po_data ASSIGNING FIELD-SYMBOL(<fs_podata>).

      ls_zcod_artpodeli = <fs_podata>.
      ls_zcod_artpodeli-client = 200.

      TRY.
          ls_art_desc_webname = lt_art_desc_webname[ article = <fs_podata>-article ].
          ls_zcod_artpodeli-article_desc = ls_art_desc_webname-desc.
          ls_zcod_artpodeli-article_web_name = ls_art_desc_webname-web_name.
        CATCH cx_sy_itab_line_not_found.
          CLEAR: ls_art_desc_webname.
      ENDTRY.



      APPEND ls_zcod_artpodeli TO lt_zcod_artpodeli.

    ENDLOOP.

    IF lt_zcod_artpodeli IS NOT INITIAL.
      MODIFY zcod_artpodeli FROM TABLE lt_zcod_artpodeli.
    ENDIF.

  ENDMETHOD.


  METHOD fetch_desc_and_webname.

    IF it_article_range IS NOT INITIAL.

      SELECT makt~matnr AS article,
             makt~maktx AS desc,
             ztb_descriptab2~web_name2 AS web_name
      FROM makt
      LEFT JOIN ztb_descriptab2
      ON ztb_descriptab2~matnr = makt~matnr
      AND ztb_descriptab2~lang = makt~spras
      INTO TABLE @rt_result
      WHERE makt~matnr IN @it_article_range
      AND makt~spras = 'E'.

    ENDIF.

    SORT rt_result BY article ASCENDING.

  ENDMETHOD.

  METHOD fetch_price_history.



    DATA: lt_dist_chan_range TYPE ztt_dist_chanl_range,
          lt_sales_org_range TYPE lmont_it_range_c22.


    CALL FUNCTION 'LMON_RANGE_CREATE'
      EXPORTING
        iv_fieldname = 'VKORG'
      TABLES
        et_range     = lt_sales_org_range
      CHANGING
        it_object    = gt_sales_org_per_region.


    APPEND INITIAL LINE TO lt_dist_chan_range ASSIGNING FIELD-SYMBOL(<fs_dist>).
    <fs_dist>-sign = 'I'.
    <fs_dist>-option = 'EQ'.
    <fs_dist>-low = 10.


    SELECT
      a~vkorg,
      a~vtweg,
      a~matnr,
      a~knumh,
      a~datab,
      a~datbi,
      p~kbetr,
      p~konwa,
      p~kpein,
      p~kmein
      FROM a073 AS a
      INNER JOIN konp AS p
      ON p~knumh = a~knumh
      AND p~kschl = a~kschl
      INTO TABLE @rt_return
      WHERE a~matnr IN @it_articles
        AND a~vkorg IN @lt_sales_org_range
        AND a~vtweg IN @lt_dist_chan_range
        AND a~vrkme = 'EA'
        AND a~kappl = 'V'       "'V' = Sales
        AND a~kschl = 'VKP0'   "Final Retail Price
        "AND ( a~datbi IN @it_date_range OR a~datab IN @it_date_range ) Always fetch all per article to make sure we have all even if the articles has no changes.
        ORDER BY a~datab DESCENDING.

    IF sy-subrc <> 0.

    ENDIF.

  ENDMETHOD.


  METHOD fetch_sales_org_per_region.

    SELECT
       zfiori_buss_data~vkorg,
       zfiori_buss_data~country_key,
       zfiori_buss_data~language
     FROM
      zfiori_buss_data
     INTO TABLE @gt_sales_org_per_region
     WHERE country_key IN @gt_regions_range
     AND app_id = 'AMDO'.

  ENDMETHOD.

ENDCLASS.
