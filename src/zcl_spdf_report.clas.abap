class ZCL_SPDF_REPORT definition
  public
  final
  create public .

public section.

  constants:
    BEGIN OF gc_param_kind,
        p TYPE rsscr_kind VALUE 'P',
        s TYPE rsscr_kind VALUE 'S',
      END OF gc_param_kind .
  data MV_NAME type PROGNAME .
  data MV_VARIANT type VARIANT .

  methods CONSTRUCTOR
    importing
      !IV_NAME type PROGNAME
      !IV_VARIANT type VARIANT default SPACE
    raising
      ZCX_SPDF_EXCEPTION .
  methods ADD_PARAM
    importing
      !IV_NAME type CHAR8
      !I_DATA type ANY .
  methods SUBMIT_TO_SAP_SPOOL
    raising
      ZCX_SPDF_EXCEPTION .
  methods GET_MERGED_PDF
    returning
      value(RO_MERGED_PDF) type ref to ZCL_SPDF_MERGED_PDF
    raising
      CX_RSPO_SPOOLID_TO_PDF .
  methods GET_PARTS_PDF
    returning
      value(RO_PARTS_PDF) type ref to ZCL_SPDF_PARTS_PDF .
  methods BP_JOB_DELETE
    importing
      !IV_FORCEDMODE type SY-BATCH default SPACE
      !IV_COMMITMODE type BOOLE_D default 'X'
    raising
      ZCX_SPDF_EXCEPTION .
  PROTECTED SECTION.
private section.

  data MT_INITIAL_RSPARAMS type RSPARAMS_TT .
  data MT_RSPARAMS type RSPARAMS_TT .
  data MS_JOB_PARAMS type TBTCJOB .
  data MO_PARAMS_MAP type ref to CL_OBJECT_MAP .

  methods CHECK_REPORT_EXISTS
    importing
      !IV_NAME type PROGNAME
    raising
      ZCX_SPDF_EXCEPTION .
  methods CHECK_VARIANT_EXISTS
    importing
      !IV_NAME type PROGNAME
      !IV_VARIANT type VARIANT default SPACE
    raising
      ZCX_SPDF_EXCEPTION .
  methods FILL_RSPARAMS .
  methods SUBMIT_WITH_RSPARAMS
    raising
      ZCX_SPDF_EXCEPTION .
  methods GET_SPOOL_ID
    returning
      value(RV_SPOOL_ID) type RSPOID .
  methods READ_SPOOL_ID
    returning
      value(RV_SPOOL_ID) type RSPOID .
  methods READ_PARTS_PDF
    importing
      !IV_SPOOL_ID type RSPOID
    returning
      value(RT_PDF) type TFPCONTENT .
ENDCLASS.



CLASS ZCL_SPDF_REPORT IMPLEMENTATION.


  METHOD add_param.

    LOOP AT mt_initial_rsparams TRANSPORTING NO FIELDS WHERE selname = iv_name
                                                         AND kind = gc_param_kind-p
                                                          OR kind = gc_param_kind-s.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      mo_params_map->put( key = iv_name
                          value = NEW zcl_spdf_map_value( i_data ) ).
    ENDIF.

  ENDMETHOD.


  METHOD bp_job_delete.

    CALL FUNCTION 'BP_JOB_DELETE'
      EXPORTING
        jobcount                 = ms_job_params-jobcount
        jobname                  = ms_job_params-jobname
        forcedmode               = iv_forcedmode
        commitmode               = iv_commitmode
      EXCEPTIONS
        cant_delete_event_entry  = 1
        cant_delete_job          = 2
        cant_delete_joblog       = 3
        cant_delete_steps        = 4
        cant_delete_time_entry   = 5
        cant_derelease_successor = 6
        cant_enq_predecessor     = 7
        cant_enq_successor       = 8
        cant_enq_tbtco_entry     = 9
        cant_update_predecessor  = 10
        cant_update_successor    = 11
        commit_failed            = 12
        jobcount_missing         = 13
        jobname_missing          = 14
        job_does_not_exist       = 15
        job_is_already_running   = 16
        no_delete_authority      = 17
        OTHERS                   = 18.
    IF sy-subrc <> 0.
      MESSAGE e003(zspool_pdf) WITH ms_job_params-jobname ms_job_params-jobcount INTO DATA(lv_message).

      RAISE EXCEPTION TYPE zcx_spdf_exception
        EXPORTING
          textid = VALUE #( msgid = 'ZSPOOL_PDF'
                            msgno = 003
                            attr1 = ms_job_params-jobname
                            attr2 = ms_job_params-jobcount ).
    ENDIF.

  ENDMETHOD.


  METHOD check_report_exists.

    SELECT SINGLE name FROM trdir INTO @DATA(lv_name) WHERE name = @iv_name.
    IF sy-subrc <> 0.
      MESSAGE e001(zspool_pdf) WITH iv_name INTO DATA(lv_message).

      RAISE EXCEPTION TYPE zcx_spdf_exception
        EXPORTING
          textid = VALUE #( msgid = 'ZSPOOL_PDF'
                            msgno = 001
                            attr1 = iv_name ).
    ENDIF.
  ENDMETHOD.


  METHOD check_variant_exists.
    DATA: lv_return_code TYPE sy-subrc.

    CALL FUNCTION 'RS_VARIANT_EXISTS'
      EXPORTING
        report              = iv_name
        variant             = iv_variant
      IMPORTING
        r_c                 = lv_return_code
      EXCEPTIONS
        not_authorized      = 1
        no_report           = 2
        report_not_existent = 3
        report_not_supplied = 4
        OTHERS              = 5.
    IF sy-subrc = 0 AND lv_return_code IS INITIAL.
      MESSAGE e002(zspool_pdf) WITH iv_variant iv_name INTO DATA(lv_message).

      RAISE EXCEPTION TYPE zcx_spdf_exception
        EXPORTING
          textid = VALUE #( msgid = 'ZSPOOL_PDF'
                            msgno = 002
                            attr1 = iv_name ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.

    check_report_exists( iv_name ).

    IF iv_variant IS SUPPLIED.
      check_variant_exists( iv_name = iv_name
                            iv_variant = iv_variant ).
    ENDIF.

    mv_name = iv_name.
    mv_variant = iv_variant.

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = mv_name
      TABLES
        selection_table = mt_initial_rsparams.

    mo_params_map = NEW #( ).

  ENDMETHOD.


  METHOD fill_rsparams.
    DATA: ls_rsparams LIKE LINE OF mt_rsparams,
          lo_value    TYPE REF TO zcl_spdf_map_value,
          l_data      TYPE REF TO data.

    FIELD-SYMBOLS: <la_data> TYPE any,
                   <lt_data> TYPE ANY TABLE,
                   <ls_data> TYPE any,
                   <l_val>   TYPE any.

    LOOP AT mt_initial_rsparams ASSIGNING FIELD-SYMBOL(<ls_initial_rsparams>).
      CLEAR: l_data, ls_rsparams.

      CASE <ls_initial_rsparams>-kind.
        WHEN gc_param_kind-p.

          IF mo_params_map->contains_key( <ls_initial_rsparams>-selname ) = abap_true.

            lo_value ?= mo_params_map->get( <ls_initial_rsparams>-selname ).
            l_data = lo_value->get( ).
            ASSIGN l_data->* TO <la_data>.
            ls_rsparams = <ls_initial_rsparams>.
            ls_rsparams-sign = 'I'.
            ls_rsparams-option = 'EQ'.
            ls_rsparams-low = <la_data>.
            APPEND ls_rsparams TO mt_rsparams.

          ENDIF.

        WHEN gc_param_kind-s.

          IF mo_params_map->contains_key( <ls_initial_rsparams>-selname ) = abap_true.

            lo_value ?= mo_params_map->get( <ls_initial_rsparams>-selname ).
            l_data = lo_value->get( ).
            ASSIGN l_data->* TO <lt_data>.
            ls_rsparams = <ls_initial_rsparams>.

            LOOP AT <lt_data> ASSIGNING <ls_data>.
              UNASSIGN <l_val>.
              ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_data> TO <l_val>.
              IF <l_val> IS ASSIGNED.
                ls_rsparams-sign = <l_val>.
              ENDIF.

              UNASSIGN <l_val>.
              ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_data> TO <l_val>.
              IF <l_val> IS ASSIGNED.
                ls_rsparams-option = <l_val>.
              ENDIF.

              UNASSIGN <l_val>.
              ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_data> TO <l_val>.
              IF <l_val> IS ASSIGNED.
                ls_rsparams-low = <l_val>.
              ENDIF.

              UNASSIGN <l_val>.
              ASSIGN COMPONENT 'HIGH' OF STRUCTURE <ls_data> TO <l_val>.
              IF <l_val> IS ASSIGNED.
                ls_rsparams-high = <l_val>.
              ENDIF.
              APPEND ls_rsparams TO mt_rsparams.
            ENDLOOP.
            IF sy-subrc <> 0.
              APPEND <ls_initial_rsparams> TO mt_rsparams.
            ENDIF.

          ENDIF.

      ENDCASE.

    ENDLOOP.
  ENDMETHOD.


  METHOD get_merged_pdf.
    DATA: lv_pdf  TYPE xstring,
          lv_size TYPE i.

    cl_rspo_spoolid_to_pdf=>get_spool_pdf( EXPORTING iv_rqident = get_spool_id( )
                                           IMPORTING ev_pdf = lv_pdf
                                                     ev_size = lv_size ).
    ro_merged_pdf = NEW #( iv_pdf  = lv_pdf
                           iv_size = lv_size ).
  ENDMETHOD.


  METHOD get_parts_pdf.
    ro_parts_pdf = NEW #( read_parts_pdf( get_spool_id( ) ) ).
  ENDMETHOD.


  METHOD get_spool_id.
    CONSTANTS: lc_wait_seconds_max TYPE i VALUE 60,

               BEGIN OF lc_job_status,
                 f TYPE btcpstatus VALUE 'F',
                 a TYPE btcpstatus VALUE 'A',
               END OF lc_job_status.

    DATA: lv_wait_seconds_value TYPE i,
          lv_wait_delay         TYPE i VALUE 1,
          lv_job_status         TYPE btcpstatus.

    CLEAR: lv_wait_seconds_value.

    WHILE lv_job_status <> lc_job_status-f AND lv_wait_seconds_value < lc_wait_seconds_max.

*   Опрашиваем статус запланированных задач

      SELECT SINGLE status
        FROM tbtco
        INTO lv_job_status
        WHERE jobname = ms_job_params-jobname
          AND jobcount = ms_job_params-jobcount.

      CASE lv_job_status.
        WHEN lc_job_status-f. " F - Finished

          rv_spool_id = read_spool_id( ).

        WHEN lc_job_status-a.  " A = Aborted
*          TODO:
*          WRITE: / 'Ошибка при выполнении фонового задания',
*                 / 'Статус задания: A = Aborted'.
*          EXIT.
      ENDCASE.

      WAIT UP TO lv_wait_delay SECONDS.
      lv_wait_seconds_value  = lv_wait_seconds_value  + lv_wait_delay.

    ENDWHILE. " ждем завершения выполнения задачи
  ENDMETHOD.


  METHOD read_parts_pdf.
    DATA: lv_number_of_parts TYPE i,
          lv_partnum         TYPE adsnum,
          lv_pdf             TYPE fpcontent.

    CALL FUNCTION 'ADS_GET_NO_OF_PARTS'
      EXPORTING
        rqident         = iv_spool_id
      IMPORTING
        number_of_parts = lv_number_of_parts
      EXCEPTIONS
        no_such_job     = 1
        wrong_jobtype   = 2
        internal_error  = 3
        OTHERS          = 4.
    IF sy-subrc = 0.

      DO lv_number_of_parts TIMES.

        lv_partnum = lv_partnum + 1.

        CLEAR lv_pdf.
        CALL FUNCTION 'FPCOMP_CREATE_PDF_FROM_SPOOL'
          EXPORTING
            i_spoolid      = iv_spool_id
            i_partnum      = lv_partnum
          IMPORTING
            e_pdf          = lv_pdf
*           e_renderpagecount =
*           e_pdf_file     =
          EXCEPTIONS
            ads_error      = 1
            usage_error    = 2
            system_error   = 3
            internal_error = 4
            OTHERS         = 5.
        IF sy-subrc = 0.
          APPEND lv_pdf TO rt_pdf.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDMETHOD.


  METHOD read_spool_id.
    TYPES: BEGIN OF ty_range_tbtc_spoolid,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE btclistid,
             high   TYPE btclistid,
           END OF ty_range_tbtc_spoolid.

    CONSTANTS: lc_ads_doctype_adsp TYPE adsdoctype VALUE 'ADSP'.

    DATA: lt_tbtc_spoolid       TYPE STANDARD TABLE OF tbtc_spoolid,
          lt_range_tbtc_spoolid TYPE STANDARD TABLE OF ty_range_tbtc_spoolid,
          ls_range_tbtc_spoolid LIKE LINE OF lt_range_tbtc_spoolid,
          lv_rqident            TYPE rspoid.

    FIELD-SYMBOLS: <ls_tbtc_spoolid> LIKE LINE OF lt_tbtc_spoolid.

    SELECT spoolid
      FROM tbtc_spoolid
      INTO CORRESPONDING FIELDS OF TABLE lt_tbtc_spoolid
      WHERE jobname  = ms_job_params-jobname
        AND jobcount = ms_job_params-jobcount.

    IF sy-subrc = 0.

      ls_range_tbtc_spoolid-sign = 'I'.
      ls_range_tbtc_spoolid-option = 'EQ'.

      LOOP AT lt_tbtc_spoolid ASSIGNING <ls_tbtc_spoolid>.
        ls_range_tbtc_spoolid-low = <ls_tbtc_spoolid>-spoolid.
        APPEND ls_range_tbtc_spoolid TO lt_range_tbtc_spoolid.
      ENDLOOP.

      SELECT SINGLE rqident
        FROM tsp01
        INTO lv_rqident
        WHERE rqdoctype = lc_ads_doctype_adsp
          AND rqowner = sy-uname
          AND rqident IN lt_range_tbtc_spoolid.

      IF sy-subrc = 0.
        rv_spool_id = lv_rqident.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD submit_to_sap_spool.
    fill_rsparams( ).
    submit_with_rsparams( ).
  ENDMETHOD.


  METHOD submit_with_rsparams.
    DATA: ls_print_parameters TYPE pri_params.

    ms_job_params-jobname = mv_name.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = ms_job_params-jobname
      IMPORTING
        jobcount         = ms_job_params-jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc = 0.
      CALL FUNCTION 'GET_PRINT_PARAMETERS'
        EXPORTING
          destination            = 'PDF_'
          immediately            = abap_true
          no_dialog              = abap_true
        IMPORTING
          out_parameters         = ls_print_parameters
        EXCEPTIONS
          archive_info_not_found = 1
          invalid_print_params   = 2
          invalid_archive_params = 3
          OTHERS                 = 4.
      IF sy-subrc = 0.

        SUBMIT (mv_name)
          TO SAP-SPOOL
          WITH SELECTION-TABLE mt_rsparams
          SPOOL PARAMETERS ls_print_parameters
          WITHOUT SPOOL DYNPRO
          USER sy-uname VIA JOB ms_job_params-jobname NUMBER ms_job_params-jobcount
          USING SELECTION-SET mv_variant
          AND RETURN.

        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = ms_job_params-jobcount
            jobname              = ms_job_params-jobname
            strtimmed            = abap_true
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            invalid_target       = 8
            OTHERS               = 9.
        IF sy-subrc <> 0.
          MESSAGE e004(zspool_pdf) INTO DATA(lv_message).

          RAISE EXCEPTION TYPE zcx_spdf_exception
            EXPORTING
              textid = VALUE #( msgid = 'ZSPOOL_PDF'
                                msgno = 004 ).
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
