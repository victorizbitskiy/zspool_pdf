*&---------------------------------------------------------------------*
*& Include          ZSPDF_MERGED_DEMO_CLI
*&---------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.
  METHOD main.
    DATA(lv_filename) = `C:\TEMP\spdf_merged_test.pdf`.

    TRY.
        DATA(lo_report) = NEW zcl_spdf_report( iv_name    = 'HRULNDFL'
                                               iv_variant = 'T1' ).

*        lo_report->add_param( iv_name = 'PNPPERNR' ia_data = so_pernr[] ).
*        lo_report->add_param( iv_name = 'P_YEAR' ia_data = p_year ).

        DATA(lv_pdf) = lo_report->submit_to_sap_spool( )->get_merged_pdf( )->to_xstring( ).
        lo_report->get_merged_pdf( )->save_local( lv_filename )->show( ).
        lo_report->bp_job_delete( ).

      CATCH zcx_spdf_exception
            cx_rspo_spoolid_to_pdf INTO DATA(lx_e).

        WRITE lx_e->get_text( ).
        RETURN.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
