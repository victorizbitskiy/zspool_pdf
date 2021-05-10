*&---------------------------------------------------------------------*
*& Include          ZSPDF_MERGED_DEMO_CLI
*&---------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.
  METHOD main.

    TRY.
        DATA(lo_report) = NEW zcl_spdf_report( iv_name    = 'RPCPAYRU_NDFL_PERS'
                                               iv_variant = 'CUS&SV').

*        lo_report->add_param( iv_name = 'PNPPERNR' ia_data = so_pernr[] ).
*        lo_report->add_param( iv_name = 'P_YEAR' ia_data = p_year ).

        lo_report->submit_to_sap_spool( ).

        DATA(lo_spdf_merged_pdf) = lo_report->get_merged_pdf( ).
        DATA(lv_pdf) = lo_spdf_merged_pdf->to_xstring( ).
        DATA(lt_bin) = lo_spdf_merged_pdf->to_binary( ).

        lo_spdf_merged_pdf->save_local( 'C:\TEMP\spdf_test.pdf' ).
*        lo_spdf_merged_pdf->save_in_appl_server( '' ).
        lo_spdf_merged_pdf->show_in_browser( ).

      CATCH zcx_spdf_exception
            cx_rspo_spoolid_to_pdf INTO DATA(lx_e).

        WRITE lx_e->get_text( ).
        RETURN.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
