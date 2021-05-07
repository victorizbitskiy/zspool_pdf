class ZCL_SPDF_PARTS_PDF definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IT_PDF type TFPCONTENT .
  methods SAVE_LOCAL
    importing
      !IV_FILENAME type STRING
      !IV_CODEPAGE type ABAP_ENCODING default SPACE .
  methods SAVE_IN_APPL_SERVER
    importing
      !IV_FILENAME type STRING
    raising
      ZCX_SPDF_EXCEPTION .
  methods GET_PARTS
    returning
      value(RT_PARTS) type TFPCONTENT .
  PROTECTED SECTION.
private section.

  data MT_PDF type TFPCONTENT .
  constants:
    gc_pdf_file_extension TYPE c LENGTH 4 value '.pdf' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_SPDF_PARTS_PDF IMPLEMENTATION.


  METHOD constructor.
    mt_pdf = it_pdf.
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

      DATA(lv_filename) = iv_filename.

      REPLACE FIRST OCCURRENCE OF gc_pdf_file_extension IN lv_filename WITH lv_partnum.
      CONCATENATE lv_filename gc_pdf_file_extension INTO lv_filename.

      OPEN DATASET lv_filename FOR OUTPUT IN BINARY MODE.
      IF sy-subrc <> 0.
        MESSAGE e007(zspool_pdf) WITH iv_filename INTO DATA(lv_message).

        RAISE EXCEPTION TYPE zcx_spdf_exception
          EXPORTING
            textid = VALUE #( msgid = 'ZSPOOL_PDF'
                              msgno = 007
                              attr1 = iv_filename ).
      ENDIF.
      LOOP AT lt_binary ASSIGNING FIELD-SYMBOL(<ls_binary>).
        TRANSFER <ls_binary> TO iv_filename LENGTH 255.
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
      DATA(lv_filename) = iv_filename.

      REPLACE FIRST OCCURRENCE OF gc_pdf_file_extension IN lv_filename WITH lv_partnum.
      CONCATENATE lv_filename gc_pdf_file_extension INTO lv_filename.

      cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize      = lv_filesize
                                                        filename          = lv_filename
                                                        filetype          = 'BIN'
                                                        confirm_overwrite = abap_true
                                                        codepage          = iv_codepage
                                               CHANGING data_tab          = lt_binary ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
