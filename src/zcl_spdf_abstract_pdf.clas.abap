CLASS zcl_spdf_abstract_pdf DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    DATA mv_filename TYPE string .

    METHODS check_filename
      RAISING
        zcx_spdf_exception .
    METHODS check_file_extention_is_pdf
      RAISING
        zcx_spdf_exception .
    METHODS check_folder_exist
      RAISING
        zcx_spdf_exception .
    METHODS get_separator
      RETURNING
        VALUE(rv_separator) TYPE string .
    METHODS get_folder
      RETURNING
        VALUE(rv_folder) TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SPDF_ABSTRACT_PDF IMPLEMENTATION.


  METHOD check_filename.

    check_file_extention_is_pdf( ).
    check_folder_exist( ).

  ENDMETHOD.


  METHOD check_file_extention_is_pdf.

    DATA(lv_filename_length) = strlen( mv_filename ).
    DATA(lv_ext_pos_start) = lv_filename_length - 4.

    IF mv_filename+lv_ext_pos_start(4) <> '.pdf'.
      MESSAGE e008(zspool_pdf) INTO DATA(lv_message).

      RAISE EXCEPTION TYPE zcx_spdf_exception
        EXPORTING
          textid = VALUE #( msgid = 'ZSPOOL_PDF'
                            msgno = 008 ).
    ENDIF.

  ENDMETHOD.


  METHOD check_folder_exist.

    IF cl_gui_frontend_services=>directory_exist( get_folder( ) ) = abap_false.
      MESSAGE e009(zspool_pdf) INTO DATA(lv_message).

      RAISE EXCEPTION TYPE zcx_spdf_exception
        EXPORTING
          textid = VALUE #( msgid = 'ZSPOOL_PDF'
                            msgno = 009 ).
    ENDIF.

  ENDMETHOD.


  METHOD get_folder.

    DATA(lv_separator) = get_separator( ).

    SPLIT mv_filename AT lv_separator INTO TABLE DATA(lt_filename).
    DELETE lt_filename INDEX lines( lt_filename ).

    LOOP AT lt_filename ASSIGNING FIELD-SYMBOL(<ls_filename>).
      rv_folder = |{ rv_folder }{ <ls_filename> }\\|.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_separator.
    DATA(lv_platform) = cl_gui_frontend_services=>get_platform( ).

    IF lv_platform = cl_gui_frontend_services=>platform_linux OR lv_platform = cl_gui_frontend_services=>platform_macosx.
      rv_separator = `/`.
    ELSEIF lv_platform = cl_gui_frontend_services=>platform_windowsxp.
      rv_separator = `\`.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
