CLASS zcl_spdf_abstract_pdf DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
protected section.

  data MV_FILENAME type STRING .

  methods CHECK_FILENAME
    importing
      !IV_FILENAME type STRING
    raising
      ZCX_SPDF_EXCEPTION .
  methods CHECK_FILE_EXTENTION_IS_PDF
    importing
      !IV_FILENAME type STRING
    raising
      ZCX_SPDF_EXCEPTION .
  methods CHECK_FOLDER_EXIST
    importing
      !IV_FILENAME type STRING
    raising
      ZCX_SPDF_EXCEPTION .
private section.
ENDCLASS.



CLASS ZCL_SPDF_ABSTRACT_PDF IMPLEMENTATION.


  METHOD check_filename.

    check_file_extention_is_pdf( iv_filename ).
    check_folder_exist( iv_filename ).

  ENDMETHOD.


  METHOD check_file_extention_is_pdf.

    DATA(lv_filename_length) = strlen( iv_filename ).
    DATA(lv_ext_pos_start) = lv_filename_length - 4.

    IF iv_filename+lv_ext_pos_start(4) <> '.pdf'.
      MESSAGE e008(zspool_pdf) INTO DATA(lv_message).

      RAISE EXCEPTION TYPE zcx_spdf_exception
        EXPORTING
          textid = VALUE #( msgid = 'ZSPOOL_PDF'
                            msgno = 008 ).
    ENDIF.

  ENDMETHOD.


  METHOD check_folder_exist.
    DATA: lv_folder TYPE string.

    SPLIT iv_filename AT `\` INTO TABLE DATA(lt_filename).
    DELETE lt_filename INDEX lines( lt_filename ).

    LOOP AT lt_filename ASSIGNING FIELD-SYMBOL(<ls_filename>).
      lv_folder = |{ lv_folder }{ <ls_filename> }\\|.
    ENDLOOP.

    IF cl_gui_frontend_services=>directory_exist( lv_folder ) = abap_false.
      MESSAGE e009(zspool_pdf) INTO DATA(lv_message).

      RAISE EXCEPTION TYPE zcx_spdf_exception
        EXPORTING
          textid = VALUE #( msgid = 'ZSPOOL_PDF'
                            msgno = 009 ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
