*&---------------------------------------------------------------------*
*& Include          ZSPDF_PARTS_DEMO_CLI
*&---------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.
  METHOD main.
    TYPES: ty_pernr TYPE n LENGTH 8.
    DATA lt_pernr TYPE RANGE OF ty_pernr.

*   This is an example of generating a 2-NDFL certificate (HCM module).

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

        lo_report->submit_to_sap_spool( ).

        DATA(lt_pdf) = lo_report->get_parts_pdf( )->get_parts( ).
        lo_report->get_parts_pdf( )->save_local( lv_filename )->show( ).
        lo_report->bp_job_delete( ).

      CATCH zcx_spdf_exception INTO DATA(lx_e).
        WRITE lx_e->get_text( ).
        RETURN.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
