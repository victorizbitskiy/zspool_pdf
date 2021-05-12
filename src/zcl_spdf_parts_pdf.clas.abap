CLASS zcl_spdf_parts_pdf DEFINITION
  PUBLIC
  INHERITING FROM zcl_spdf_abstract_pdf
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !it_pdf TYPE tfpcontent .
    METHODS save_local
      IMPORTING
        !iv_common_filename      TYPE string
        !iv_codepage             TYPE abap_encoding DEFAULT space
      RETURNING
        VALUE(ro_spdf_parts_pdf) TYPE REF TO zcl_spdf_parts_pdf
      RAISING
        zcx_spdf_exception .
    METHODS save_in_appl_server
      IMPORTING
        !iv_filename TYPE string
      RAISING
        zcx_spdf_exception .
    METHODS get_parts
      RETURNING
        VALUE(rt_parts) TYPE tfpcontent .
    METHODS show
      IMPORTING
        !iv_common_filename TYPE string OPTIONAL
      RAISING
        zcx_spdf_exception .
  PROTECTED SECTION.

    DATA mt_pdf TYPE tfpcontent .
    DATA mo_parts_filename_map TYPE REF TO cl_object_map .

    METHODS create_filename_part
      IMPORTING
        !iv_filename            TYPE string
        !iv_partnum             TYPE numc3
      RETURNING
        VALUE(rv_filename_part) TYPE string .
    METHODS process_show_parts .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SPDF_PARTS_PDF IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mt_pdf = it_pdf.
  ENDMETHOD.


  METHOD create_filename_part.
    rv_filename_part = iv_filename.
    REPLACE FIRST OCCURRENCE OF `.pdf` IN rv_filename_part WITH |_{ iv_partnum }|.
    rv_filename_part  = |{ rv_filename_part } `.pdf`|.
  ENDMETHOD.


  METHOD get_parts.
    rt_parts = mt_pdf.
  ENDMETHOD.


  METHOD process_show_parts.
    DATA: lo_value    TYPE REF TO zcl_spdf_map_value,
          ld_data     TYPE REF TO data,
          lv_filename TYPE string.

    FIELD-SYMBOLS: <la_data> TYPE any.

    DATA(lo_it) = mo_parts_filename_map->get_values_iterator( ).
    WHILE lo_it->has_next( ).
      lo_value ?= lo_it->get_next( ).
      ld_data = lo_value->get( ).
      ASSIGN ld_data->* TO <la_data>.
      lv_filename = <la_data>.
      cl_gui_frontend_services=>execute( document = lv_filename ).
    ENDWHILE.

  ENDMETHOD.


  METHOD save_in_appl_server.
    DATA: lv_partnum TYPE n LENGTH 3,
          lt_binary  TYPE solix_tab.

    LOOP AT mt_pdf ASSIGNING FIELD-SYMBOL(<ls_pdf>).
      lv_partnum = lv_partnum + 1.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = <ls_pdf>
        TABLES
          binary_tab = lt_binary.

      DATA(lv_filename_part) = create_filename_part( iv_filename = iv_filename
                                                     iv_partnum  = lv_partnum ).

      OPEN DATASET lv_filename_part FOR OUTPUT IN BINARY MODE.
      IF sy-subrc <> 0.
        MESSAGE e007(zspool_pdf) WITH iv_filename INTO DATA(lv_message).

        RAISE EXCEPTION TYPE zcx_spdf_exception
          EXPORTING
            textid = VALUE #( msgid = 'ZSPOOL_PDF'
                              msgno = 007
                              attr1 = iv_filename ).
      ENDIF.
      LOOP AT lt_binary ASSIGNING FIELD-SYMBOL(<ls_binary>).
        TRANSFER <ls_binary> TO lv_filename_part LENGTH 255.
      ENDLOOP.
      CLOSE DATASET iv_filename.

    ENDLOOP.
  ENDMETHOD.


  METHOD save_local.
    DATA: lv_partnum TYPE n LENGTH 3,
          lt_binary  TYPE solix_tab.

    mv_filename = iv_common_filename.
    check_filename( ).

    DATA(lv_pdf_no_of_parts) = lines( mt_pdf ).
    IF lv_pdf_no_of_parts = 1.
      DATA(lv_filename_part) = mv_filename.
    ENDIF.

    mo_parts_filename_map = NEW cl_object_map( ).

    LOOP AT mt_pdf ASSIGNING FIELD-SYMBOL(<ls_pdf>).
      lv_partnum = lv_partnum + 1.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = <ls_pdf>
        TABLES
          binary_tab = lt_binary.

      DATA(lv_filesize) = xstrlen( <ls_pdf> ).

      IF lv_pdf_no_of_parts > 1.
        lv_filename_part = create_filename_part( iv_filename = mv_filename
                                                 iv_partnum  = lv_partnum ).
      ENDIF.

      cl_gui_frontend_services=>gui_download(
        EXPORTING
          bin_filesize      = lv_filesize
          filename          = lv_filename_part
          filetype          = 'BIN'
          confirm_overwrite = abap_true
          codepage          = iv_codepage
        CHANGING
          data_tab          = lt_binary
        EXCEPTIONS
          OTHERS            = 99 ).
      IF sy-subrc = 0.
        mo_parts_filename_map->put( key = lv_partnum
                                    value = NEW zcl_spdf_map_value( mv_filename ) ).
      ENDIF.

    ENDLOOP.

    ro_spdf_parts_pdf = me.
  ENDMETHOD.


  METHOD show.
    DATA lv_temp_dir TYPE string.

    IF iv_common_filename IS SUPPLIED.
      DATA(lv_filename) = iv_common_filename.
      cl_gui_frontend_services=>execute( document = lv_filename ).
    ELSEIF mv_filename IS NOT INITIAL.
      lv_filename = mv_filename.
      cl_gui_frontend_services=>execute( document = lv_filename ).
    ELSE.
      cl_gui_frontend_services=>get_temp_directory( CHANGING temp_dir = lv_temp_dir ).
      cl_gui_cfw=>flush( ).
      lv_filename = |{ lv_temp_dir }{ get_separator( ) }temp.pdf|.
      save_local( lv_filename ).

      process_show_parts( ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
