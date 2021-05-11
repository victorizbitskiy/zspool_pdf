CLASS zcl_spdf_parts_pdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !it_pdf TYPE tfpcontent .
    METHODS save_local
      IMPORTING
        !iv_filename TYPE string
        !iv_codepage TYPE abap_encoding DEFAULT space .
    METHODS save_in_appl_server
      IMPORTING
        !iv_filename TYPE string
      RAISING
        zcx_spdf_exception .
    METHODS get_parts
      RETURNING
        VALUE(rt_parts) TYPE tfpcontent .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_pdf TYPE tfpcontent .

    METHODS create_filename_part
      IMPORTING
        !iv_filename            TYPE string
        !iv_partnum             TYPE numc3
      RETURNING
        VALUE(rv_filename_part) TYPE string .
ENDCLASS.



CLASS ZCL_SPDF_PARTS_PDF IMPLEMENTATION.


  METHOD constructor.
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

    LOOP AT mt_pdf ASSIGNING FIELD-SYMBOL(<ls_pdf>).
      lv_partnum = lv_partnum + 1.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = <ls_pdf>
        TABLES
          binary_tab = lt_binary.

      DATA(lv_filesize) = xstrlen( <ls_pdf> ).
      DATA(lv_filename_part) = create_filename_part( iv_filename = iv_filename
                                                     iv_partnum  = lv_partnum ).

      cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize      = lv_filesize
                                                        filename          = lv_filename_part
                                                        filetype          = 'BIN'
                                                        confirm_overwrite = abap_true
                                                        codepage          = iv_codepage
                                               CHANGING data_tab          = lt_binary ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
