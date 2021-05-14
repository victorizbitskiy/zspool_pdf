*&---------------------------------------------------------------------*
*& Include          ZSPDF_MERGED_DEMO_CLI
*&---------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.
  METHOD main.
    TYPES: ty_pernr TYPE n LENGTH 8.
    DATA lt_pernr TYPE RANGE OF ty_pernr.

*   This is an example of generating a 2-NDFL certificate (HCM module).
*   All PDF documents will be merged into one.

    DATA(lv_year) = '2021'.
    lt_pernr = VALUE #( sign = 'I' option = 'EQ' ( low = 00000001 ) ).
    DATA(lv_filename) = `C:\TEMP\spdf_merged_test.pdf`.

    TRY.
        DATA(lo_report) = NEW zcl_spdf_report( iv_name    = 'HRULNDFL'
                                               iv_variant = 'T1' ).

        lo_report->add_param( iv_name = 'PNPPERNR'
                              ia_data = lt_pernr ).

        lo_report->add_param( iv_name = 'P_YEAR'
                              ia_data = lv_year ).

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
