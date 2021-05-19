CLASS zcl_spdf_merged_pdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_pdf  TYPE xstring
        !iv_size TYPE i OPTIONAL .
    METHODS save_local
      IMPORTING
        !iv_filename              TYPE string
        !iv_codepage              TYPE abap_encoding DEFAULT space
      RETURNING
        VALUE(ro_spdf_merged_pdf) TYPE REF TO zcl_spdf_merged_pdf
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
        !iv_filename TYPE string OPTIONAL
      RAISING
        zcx_spdf_exception .
    METHODS send
      IMPORTING
        !iv_email          TYPE ad_smtpadr
        !iv_filename       TYPE string
        !iv_subject        TYPE so_obj_des
      RETURNING
        VALUE(ro_spdf_pdf) TYPE REF TO zcl_spdf_merged_pdf
      RAISING
        zcx_spdf_exception .
  PROTECTED SECTION.
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
    METHODS get_short_filename
      RETURNING
        VALUE(rv_short_filename) TYPE string .
  PRIVATE SECTION.

    DATA mv_pdf TYPE xstring .
    DATA mv_filename TYPE string .
    DATA mv_size TYPE i .
ENDCLASS.



CLASS ZCL_SPDF_MERGED_PDF IMPLEMENTATION.


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

    DATA(lv_folder) = get_folder( ).

    IF cl_gui_frontend_services=>directory_exist( lv_folder ) = abap_false.
      MESSAGE e009(zspool_pdf) INTO DATA(lv_message) WITH lv_folder.

      RAISE EXCEPTION TYPE zcx_spdf_exception
        EXPORTING
          textid = VALUE #( msgid = 'ZSPOOL_PDF'
                            msgno = 009
                            attr1 = lv_folder ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    mv_pdf = iv_pdf.

    IF iv_size IS SUPPLIED.
      mv_size = iv_size.
    ELSE.
      mv_size = xstrlen( iv_pdf ).
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

    IF lv_platform = cl_gui_frontend_services=>platform_linux
      OR lv_platform = cl_gui_frontend_services=>platform_macosx.
      rv_separator = `/`.
    ELSEIF lv_platform = cl_gui_frontend_services=>platform_windowsxp.
      rv_separator = `\`.
    ENDIF.

  ENDMETHOD.


  METHOD get_short_filename.

    DATA(lv_separator) = get_separator( ).
    SPLIT mv_filename AT lv_separator INTO TABLE DATA(lt_filename).
    rv_short_filename = lt_filename[ lines( lt_filename ) ].

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

    mv_filename = iv_filename.
    check_filename(  ).

    DATA(lt_binary) = to_binary( ).
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

    ro_spdf_merged_pdf = me.

  ENDMETHOD.


  METHOD send.
    DATA lt_binary  TYPE solix_tab.

    mv_filename = iv_filename.
    check_filename( ).

    TRY.
        DATA(lo_sender) = cl_sapuser_bcs=>create( sy-uname ).
        DATA(lo_email_body) = cl_document_bcs=>create_document( i_type = 'HTM'
                                                                i_subject = iv_subject ).
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer     = mv_pdf
          TABLES
            binary_tab = lt_binary.

        lo_email_body->add_attachment( i_attachment_type = 'PDF'
                                       i_attachment_subject = CONV sood-objdes( get_short_filename( ) )
                                       i_attachment_size = CONV so_obj_len( mv_size )
                                       i_att_content_hex = lt_binary ).

        DATA(lo_email) = cl_bcs=>create_persistent( ).
        lo_email->set_document( lo_email_body ).
        lo_email->set_sender( lo_sender ).
        DATA(lo_receiver) = cl_cam_address_bcs=>create_internet_address( iv_email ).
        lo_email->add_recipient( i_recipient = lo_receiver ).

        DATA(lv_ok) = lo_email->send( ).

        IF lv_ok = abap_true.
          COMMIT WORK.
        ENDIF.

      CATCH cx_bcs INTO DATA(lx_e).
        DATA(lv_e_text) = lx_e->get_text( ).
        MESSAGE e010(zspool_pdf) INTO DATA(lv_message) WITH lv_e_text.

        RAISE EXCEPTION TYPE zcx_spdf_exception
          EXPORTING
            textid = VALUE #( msgid = 'ZSPOOL_PDF'
                              msgno = 010
                              attr1 = lv_e_text ).
    ENDTRY.

    ro_spdf_pdf = me.
  ENDMETHOD.


  METHOD show.
    DATA lv_temp_dir TYPE string.

    IF iv_filename IS SUPPLIED.
      DATA(lv_filename) = iv_filename.
    ELSEIF mv_filename IS NOT INITIAL.
      lv_filename = mv_filename.
    ELSE.
      cl_gui_frontend_services=>get_temp_directory( CHANGING temp_dir = lv_temp_dir ).
      cl_gui_cfw=>flush( ).
      lv_filename = |{ lv_temp_dir }{ get_separator( ) }temp.pdf|.
      save_local( lv_filename ).
    ENDIF.

    cl_gui_frontend_services=>execute( document = lv_filename ).

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
