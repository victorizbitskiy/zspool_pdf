CLASS zcl_spdf_merged_pdf DEFINITION
  PUBLIC
  INHERITING FROM zcl_spdf_abstract_pdf
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_pdf  TYPE xstring
        !iv_size TYPE i .
    METHODS save_local
      IMPORTING
        !iv_filename TYPE string
        !iv_codepage TYPE abap_encoding DEFAULT space
      RAISING
        zcx_spdf_exception .
    METHODS save_in_appl_server
      IMPORTING
        !iv_filename TYPE string
      RAISING
        zcx_spdf_exception .
    METHODS get_size
      RETURNING
        VALUE(rv_size) TYPE i .
    METHODS to_xstring
      RETURNING
        VALUE(rv_pdf) TYPE xstring .
    METHODS to_binary
      RETURNING
        VALUE(rt_bin) TYPE solix_tab .
    METHODS show
      IMPORTING
        !iv_filename TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_pdf TYPE xstring .
    DATA mv_size TYPE i .
ENDCLASS.



CLASS ZCL_SPDF_MERGED_PDF IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
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

    check_filename( iv_filename ).
    DATA(lt_binary) = me->to_binary( ).
    DATA(lv_filesize) = get_size( ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize      = lv_filesize
        filename          = iv_filename
        filetype          = 'BIN'
        confirm_overwrite = abap_true
        codepage          = iv_codepage
      CHANGING
        data_tab          = lt_binary
      EXCEPTIONS
        OTHERS            = 0 ).

  ENDMETHOD.


  METHOD show.
    cl_gui_frontend_services=>execute( document = iv_filename ).
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
