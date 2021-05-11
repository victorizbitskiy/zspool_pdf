*&---------------------------------------------------------------------*
*& Include          ZSPDF_PARTS_DEMO_CLI
*&---------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.
  METHOD main.
    DATA(lv_filename) = `C:\TEMP\spdf_parts_test.pdf`.

    TRY.

        DATA(lo_report) = NEW zcl_spdf_report( iv_name    = 'HRULNDFL'
                                               iv_variant = 'T1' ).

*      lo_report->add_param( iv_name = 'PNPPERNR' i_data = so_pernr[] ).
*      lo_report->add_param( iv_name = 'P_YEAR' i_data = p_year ).

        lo_report->submit_to_sap_spool( ).
**
       DATA(lt_pdf) = lo_report->get_parts_pdf( )->get_parts( ).
       lo_report->get_parts_pdf( )->save_local( lv_filename ).
       lo_report->bp_job_delete( ).

      CATCH zcx_spdf_exception INTO DATA(lx_e).
        WRITE lx_e->get_text( ).
        RETURN.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
