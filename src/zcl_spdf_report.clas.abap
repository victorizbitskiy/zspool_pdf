class ZCL_SPDF_REPORT definition
  public
  final
  create public .

public section.

  constants:
    BEGIN OF gc_param_kind,
        params TYPE rsscr_kind VALUE 'P',
        selopt TYPE rsscr_kind VALUE 'S',
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
      !IA_DATA type ANY .
  methods SUBMIT_TO_SAP_SPOOL
    returning
      value(RO_SPDF_REPORT) type ref to ZCL_SPDF_REPORT
    raising
      ZCX_SPDF_EXCEPTION .
  methods GET_MERGED_PDF
    importing
      !IV_RQDOCTYPE type RSPODOCTYP default 'ADSP'
      !IV_WAIT_SECONDS_MAX type I default 60
    returning
      value(RO_MERGED_PDF) type ref to ZCL_SPDF_MERGED_PDF
    raising
      CX_RSPO_SPOOLID_TO_PDF
      ZCX_SPDF_EXCEPTION .
  methods GET_PARTS_PDF
    importing
      !IV_RQDOCTYPE type RSPODOCTYP default 'ADSP'
      !IV_WAIT_SECONDS_MAX type I default 60
    returning
      value(RO_PARTS_PDF) type ref to ZCL_SPDF_PARTS_PDF
    raising
      ZCX_SPDF_EXCEPTION .
  methods BP_JOB_DELETE
    importing
      !IV_FORCEDMODE type SY-BATCH default SPACE
      !IV_COMMITMODE type BOOLE_D default 'X'
    raising
      ZCX_SPDF_EXCEPTION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_initial_rsparams TYPE rsparams_tt .
    DATA mt_rsparams TYPE rsparams_tt .
    DATA ms_job_params TYPE tbtcjob .
    DATA mo_params_map TYPE REF TO cl_object_map .

    METHODS check_report_exists
      IMPORTING
        !iv_name TYPE progname
      RAISING
        zcx_spdf_exception .
    METHODS check_variant_exists
      IMPORTING
        !iv_name    TYPE progname
        !iv_variant TYPE variant DEFAULT space
      RAISING
        zcx_spdf_exception .
    METHODS fill_rsparams .
    METHODS submit_with_rsparams
      RAISING
        zcx_spdf_exception .
    METHODS get_spool_id
      IMPORTING
        !iv_rqdoctype        TYPE rspodoctyp
        !iv_wait_seconds_max TYPE i OPTIONAL
      RETURNING
        VALUE(rv_spool_id)   TYPE rspoid
      RAISING
        zcx_spdf_exception .
    METHODS read_spool_id
      IMPORTING
        !iv_rqdoctype      TYPE rspodoctyp
      RETURNING
        VALUE(rv_spool_id) TYPE rspoid .
    METHODS read_parts_pdf
      IMPORTING
        !iv_spool_id  TYPE rspoid
      RETURNING
        VALUE(rt_pdf) TYPE tfpcontent .
ENDCLASS.



CLASS ZCL_SPDF_REPORT IMPLEMENTATION.


  METHOD add_param.

    LOOP AT mt_initial_rsparams TRANSPORTING NO FIELDS WHERE selname = iv_name
                                                         AND kind = gc_param_kind-params
                                                          OR kind = gc_param_kind-selopt.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      mo_params_map->put( key = iv_name
                          value = NEW zcl_spdf_map_value( ia_data ) ).
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
    IF sy-subrc <> 0 OR lv_return_code IS NOT INITIAL.
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
          ld_data     TYPE REF TO data.

    FIELD-SYMBOLS: <la_data> TYPE any,
                   <lt_data> TYPE ANY TABLE,
                   <ls_data> TYPE any,
                   <la_val>  TYPE any.

    LOOP AT mt_initial_rsparams ASSIGNING FIELD-SYMBOL(<ls_initial_rsparams>).
      CLEAR: ld_data, ls_rsparams.

      CASE <ls_initial_rsparams>-kind.
        WHEN gc_param_kind-params.

          IF mo_params_map->contains_key( <ls_initial_rsparams>-selname ) = abap_true.

            lo_value ?= mo_params_map->get( <ls_initial_rsparams>-selname ).
            ld_data = lo_value->get( ).
            ASSIGN ld_data->* TO <la_data>.
            ls_rsparams = <ls_initial_rsparams>.
            ls_rsparams-sign = 'I'.
            ls_rsparams-option = 'EQ'.
            ls_rsparams-low = <la_data>.
            APPEND ls_rsparams TO mt_rsparams.

          ENDIF.

        WHEN gc_param_kind-selopt.

          IF mo_params_map->contains_key( <ls_initial_rsparams>-selname ) = abap_true.

            lo_value ?= mo_params_map->get( <ls_initial_rsparams>-selname ).
            ld_data = lo_value->get( ).
            ASSIGN ld_data->* TO <lt_data>.
            ls_rsparams = <ls_initial_rsparams>.

            LOOP AT <lt_data> ASSIGNING <ls_data>.
              UNASSIGN <la_val>.
              ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_data> TO <la_val>.
              IF <la_val> IS ASSIGNED.
                ls_rsparams-sign = <la_val>.
              ENDIF.

              UNASSIGN <la_val>.
              ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_data> TO <la_val>.
              IF <la_val> IS ASSIGNED.
                ls_rsparams-option = <la_val>.
              ENDIF.

              UNASSIGN <la_val>.
              ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_data> TO <la_val>.
              IF <la_val> IS ASSIGNED.
                ls_rsparams-low = <la_val>.
              ENDIF.

              UNASSIGN <la_val>.
              ASSIGN COMPONENT 'HIGH' OF STRUCTURE <ls_data> TO <la_val>.
              IF <la_val> IS ASSIGNED.
                ls_rsparams-high = <la_val>.
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

    DATA(lv_spool_id) = get_spool_id( iv_rqdoctype = iv_rqdoctype
                                      iv_wait_seconds_max = iv_wait_seconds_max ).

    cl_rspo_spoolid_to_pdf=>get_spool_pdf( EXPORTING iv_rqident = lv_spool_id
                                           IMPORTING ev_pdf = lv_pdf
                                                     ev_size = lv_size ).
    ro_merged_pdf = NEW #( iv_pdf  = lv_pdf
                           iv_size = lv_size ).
  ENDMETHOD.


  METHOD get_parts_pdf.
    ro_parts_pdf = NEW #( read_parts_pdf( get_spool_id( iv_rqdoctype = iv_rqdoctype
                                                        iv_wait_seconds_max = iv_wait_seconds_max ) ) ).
  ENDMETHOD.


  METHOD get_spool_id.
    CONSTANTS: BEGIN OF lc_job_status,
                 finished TYPE btcpstatus VALUE 'F',
                 aborted  TYPE btcpstatus VALUE 'A',
               END OF lc_job_status.

    DATA: lv_wait_seconds_value TYPE i,
          lv_wait_delay         TYPE i VALUE 1,
          lv_job_status         TYPE btcpstatus.

    CLEAR: lv_wait_seconds_value.

    WHILE lv_job_status <> lc_job_status-finished AND lv_wait_seconds_value < iv_wait_seconds_max.

      SELECT SINGLE status
        FROM tbtco
        INTO @lv_job_status
        WHERE jobname = @ms_job_params-jobname
          AND jobcount = @ms_job_params-jobcount.

      CASE lv_job_status.
        WHEN lc_job_status-finished.

          rv_spool_id = read_spool_id( iv_rqdoctype ).

        WHEN lc_job_status-aborted.
          MESSAGE e005(zspool_pdf) INTO DATA(lv_message).

          RAISE EXCEPTION TYPE zcx_spdf_exception
            EXPORTING
              textid = VALUE #( msgid = 'ZSPOOL_PDF'
                                msgno = 005 ).
      ENDCASE.

      WAIT UP TO lv_wait_delay SECONDS.
      lv_wait_seconds_value  = lv_wait_seconds_value  + lv_wait_delay.

    ENDWHILE.
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

    DATA: lt_range_tbtc_spoolid TYPE STANDARD TABLE OF ty_range_tbtc_spoolid.

    SELECT 'I', 'EQ', spoolid
      FROM tbtc_spoolid
      INTO TABLE @lt_range_tbtc_spoolid
      WHERE jobname  = @ms_job_params-jobname
        AND jobcount = @ms_job_params-jobcount.

    IF sy-subrc = 0.

      SELECT SINGLE rqident
        FROM tsp01
        INTO @DATA(lv_rqident)
        WHERE rqdoctype = @iv_rqdoctype
          AND rqowner = @sy-uname
          AND rqident IN @lt_range_tbtc_spoolid.

      IF sy-subrc = 0.
        rv_spool_id = lv_rqident.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD submit_to_sap_spool.
    fill_rsparams( ).
    submit_with_rsparams( ).
    ro_spdf_report = me.
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
