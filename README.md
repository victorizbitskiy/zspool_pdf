<img src="https://github.com/victorizbitskiy/zspool_pdf/blob/main/logo/logo.svg" height="100px"/>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/victorizbitskiy/zspool_pdf/blob/main/LICENSE)
![ABAP 7.40+](https://img.shields.io/badge/ABAP-7.40sp08+-brightgreen)
![lint](https://github.com/victorizbitskiy/zspool_pdf/actions/workflows/main.yml/badge.svg)

**ATTENTION**: The project is still under development and subject to change.

## `SAP Spool PDF`

Submitting a report to the spool and receiving PDF.

# Table of contents
1. [What it is?](#what-it-is)
2. [What is this for?](#what-is-this-for)
3. [Installation](#installation)
4. [Using](#using)
5. [Diagrams](#diagrams)

## What it is?

`SAP Spool PDF` is a few classes to help you submit a report to spool and get PDF file easily.

## What is this for?

It is often necessary to get a PDF in *xstring* or *binary* format. To do this, you can send a report to the sap-spool (in the background), and then read PDF by the spool ID.  

The **cl_bp_abap_job** class exists in ABAP to run reports in the background. But this class only supports passing report parameters via variant (let me know if I'm wrong).
This is not always convenient, since a variant must either be created before the report is run, or it must be created at runtime.  

`SAP Spool PDF` allows you to run any report generating PDF in the background. Report parameters can be passed directly and/or via a variant.

## Installation

Installation is done with [abapGit](http://www.abapgit.org).

## Using

<details open>
<base target="_blank">
<summary>Example</summary>
   
```abap
    DATA(lv_filename) = `C:\TEMP\spdf_merged_test.pdf`.

    TRY.
        DATA(lo_report) = NEW zcl_spdf_report( iv_name    = 'HRULNDFL'
                                               iv_variant = 'TEST_MERGED' ).

        lo_report->add_param( iv_name = 'PNPPERNR' ia_data = so_pernr[] ).
        lo_report->add_param( iv_name = 'P_YEAR' ia_data = p_year ).
        lo_report->add_param( iv_name = 'P_MON' ia_data = p_mon ).

*       Cases:
*       1) Getting xstring
        DATA(lv_pdf) = lo_report->submit_to_sap_spool( )->get_merged_pdf( )->to_xstring( ).
        
*       2) Save and show
        lo_report->get_merged_pdf( )->save_local( lv_filename )->show( ).
        
        lo_report->bp_job_delete( ).

      CATCH zcx_spdf_exception
            cx_rspo_spoolid_to_pdf INTO DATA(lx_e).

        WRITE lx_e->get_text( ).
        RETURN.
    ENDTRY.
```
</details>

## Diagrams

