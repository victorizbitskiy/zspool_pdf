*&---------------------------------------------------------------------*
*& Include          ZSPDF_MERGED_DEMO_CLI
*&---------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.
  METHOD main.
    DATA: lo_spdf_merged_pdf TYPE REF TO zcl_spdf_merged_pdf,
          lo_spdf_parts_pdf  TYPE REF TO zcl_spdf_parts_pdf,
          lt_pdf             TYPE tfpcontent,
          lv_pdf             TYPE xstring,
          lt_bin             TYPE solix_tab.

    TRY.
*        DATA(lo_report) = NEW zcl_spdf_report( iv_name    = 'RPCPAYRU_NDFL_PERS'
*                                               iv_variant = 'CUS&SV').

*      lo_report->add_param( iv_name = 'PNPPERNR' i_data = so_pernr[] ).
*      lo_report->add_param( iv_name = 'P_YEAR' i_data = p_year ).

*        lo_report->submit_to_sap_spool( ).
**
*        lo_spdf_parts_pdf = lo_report->get_parts_pdf( ).
*        lo_spdf_parts_pdf->save_local( 'C:\TEMP\spdf_test.pdf' ).
*        lt_pdf = lo_spdf_parts_pdf->get_parts( ).
*
*        lo_spdf_merged_pdf = lo_report->get_merged_pdf( ).
*        lv_pdf = lo_spdf_merged_pdf->to_xstring( ).
*        lt_bin = lo_spdf_merged_pdf->to_binary( ).
*        lo_spdf_merged_pdf->show_in_browser( ).
*        lo_spdf_merged_pdf->save_local( 'C:\TEMP\spdf_test.pdf' ).
*        lo_spdf_merged_pdf->save_in_appl_server( '' ).

      CATCH zcx_spdf_exception INTO DATA(lx_e).
        WRITE lx_e->get_text( ).
        RETURN.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
