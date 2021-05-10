*&---------------------------------------------------------------------*
*& Include          ZSPDF_PARTS_DEMO_CLI
*&---------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.
  METHOD main.
    TRY.
        DATA(lo_report) = NEW zcl_spdf_report( iv_name    = 'RPCPAYRU_NDFL_PERS'
                                               iv_variant = 'CUS&SV').

*      lo_report->add_param( iv_name = 'PNPPERNR' i_data = so_pernr[] ).
*      lo_report->add_param( iv_name = 'P_YEAR' i_data = p_year ).

        lo_report->submit_to_sap_spool( ).
**
        DATA(lo_spdf_parts_pdf) = lo_report->get_parts_pdf( ).
        lo_spdf_parts_pdf->save_local( 'C:\TEMP\spdf_test.pdf' ).
*        lo_spdf_parts_pdf->save_in_appl_server( '' ).
        DATA(lt_pdf) = lo_spdf_parts_pdf->get_parts( ).

      CATCH zcx_spdf_exception INTO DATA(lx_e).
        WRITE lx_e->get_text( ).
        RETURN.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
