class ZCL_SPDF_MERGED_PDF definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_PDF type XSTRING
      !IV_SIZE type I .
  methods SAVE_LOCAL
    importing
      !IV_FILENAME type STRING
      !IV_CODEPAGE type ABAP_ENCODING default SPACE .
  methods SAVE_IN_APPL_SERVER
    importing
      !IV_FILENAME type STRING
    raising
      ZCX_SPDF_EXCEPTION .
  methods GET_SIZE
    returning
      value(RV_SIZE) type I .
  methods TO_XSTRING
    returning
      value(RV_PDF) type XSTRING .
  methods TO_BINARY
    returning
      value(RT_BIN) type SOLIX_TAB .
  methods SHOW_IN_BROWSER .
  PROTECTED SECTION.
private section.

  data MV_PDF type XSTRING .
  data MV_SIZE type I .
ENDCLASS.



CLASS ZCL_SPDF_MERGED_PDF IMPLEMENTATION.


  METHOD constructor.
    mv_pdf = iv_pdf.
    mv_size = iv_size.
  ENDMETHOD.


  METHOD get_size.
    rv_size = mv_size.
  ENDMETHOD.


  METHOD save_in_appl_server.

    DATA(lt_binary) = to_binary( ).

    OPEN DATASET iv_filename FOR OUTPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      MESSAGE e006(zspool_pdf) WITH iv_filename INTO DATA(lv_message).

      RAISE EXCEPTION TYPE zcx_spdf_exception
        EXPORTING
          textid = VALUE #( msgid = 'ZSPOOL_PDF'
                            msgno = 006
                            attr1 = iv_filename ).

    ENDIF.
    LOOP AT lt_binary ASSIGNING FIELD-SYMBOL(<ls_binary>).
      TRANSFER <ls_binary> TO iv_filename LENGTH 255.
    ENDLOOP.
    CLOSE DATASET iv_filename.
  ENDMETHOD.


  METHOD save_local.

    DATA(lt_binary) = to_binary( ).
    DATA(lv_filesize) = get_size( ).
    cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize      = lv_filesize
                                                      filename          = iv_filename
                                                      filetype          = 'BIN'
                                                      confirm_overwrite = abap_true
                                                      codepage          = iv_codepage
                                             CHANGING data_tab          = lt_binary ).
  ENDMETHOD.


  METHOD show_in_browser.
    DATA: lo_html_container TYPE REF TO cl_gui_custom_container,
          lo_html_control   TYPE REF TO cl_gui_html_viewer,
          lt_data           TYPE STANDARD TABLE OF x255,
          lv_url            TYPE char255.

    CREATE OBJECT lo_html_container
      EXPORTING
        container_name = 'HTML'.

    CREATE OBJECT lo_html_control
      EXPORTING
        parent = lo_html_container.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = mv_pdf
      TABLES
        binary_tab = lt_data.

    lo_html_control->load_data( EXPORTING type         = 'application'
                                          subtype      = 'pdf'
                                IMPORTING assigned_url = lv_url
                                 CHANGING data_table   = lt_data ).

    lo_html_control->show_url_in_browser( url = lv_url ).
  ENDMETHOD.


  METHOD to_binary.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = mv_pdf
      TABLES
        binary_tab = rt_bin.
  ENDMETHOD.


  METHOD to_xstring.
    rv_pdf = mv_pdf.
  ENDMETHOD.
ENDCLASS.
