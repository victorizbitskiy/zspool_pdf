<img src="https://github.com/victorizbitskiy/zspool_pdf/blob/main/logo/logo.svg" height="100px"/>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/victorizbitskiy/zspool_pdf/blob/main/LICENSE)
![ABAP 7.40sp08+](https://img.shields.io/badge/ABAP-7.40sp08+-brightgreen)
[![Code Statistics](https://img.shields.io/badge/CodeStatistics-abaplint-blue)](https://abaplint.app/stats/victorizbitskiy/zspool_pdf)

Translations:
- [:ru: На русском языке](https://github.com/victorizbitskiy/zspool_pdf/tree/main/translations/ru) 

## `SAP Spool PDF`

Submitting a report to the spool and receiving PDF.

# Table of contents
1. [What it is?](#what-it-is)
2. [What is this for?](#what-is-this-for)
3. [Installation](#installation)
4. [Using](#using)
5. [Diagrams](#diagrams)
6. [How to contribute](#how-to-contribute)
7. [Got questions](#Got-questions)
8. [Logo](#logo)

## What it is?

`SAP Spool PDF` is a few classes to help you submit a report to spool and get PDF file easily.

## What is this for?

It is often necessary to get a PDF in *xstring* or *binary* format. To do this, you can send a report to the sap-spool (in the background), and then read PDF by the spool ID. This way of getting the file is more preferable, because there is no need to do the enhancement in standard code.

The **cl_bp_abap_job** class exists in ABAP to run reports in the background. But this class only supports passing report parameters via variant (let me know if I'm wrong).
This is not always convenient, since a variant must either be created before the report is run, or it must be created at runtime.  

`SAP Spool PDF` allows you to run any report generating PDF in the background. Report parameters can be passed directly and/or via a variant.  

The laconic API allows you to literally get a file in the form of *xstring* in one line, save, show, or send it.

## Installation

Installation is done with [abapGit](http://www.abapgit.org).

## Using

<details open>
<base target="_blank">
<summary>Example</summary>
   
```abap
   TYPES ty_pernr TYPE n LENGTH 8.
    DATA lt_pernr TYPE RANGE OF ty_pernr.

    " This is an example of generating a 2-NDFL certificate (HCM module).
    " All PDF documents will be merged into one.

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

        " Cases:
        " 1) Getting xstring
        DATA(lv_pdf) = lo_report->submit_to_sap_spool( )->get_merged_pdf( )->to_xstring( ).
        
        " 2) Save and show PDF
        lo_report->get_merged_pdf( )->save_local( lv_filename )->show( ).
   
        " 3) Send email
        lo_report->get_merged_pdf( )->send( iv_email    = 'test@test.com'
                                            iv_filename = lv_filename
                                            iv_subject  = 'This is an email with a PDF attachment' ).
      CATCH zcx_spdf_exception
            cx_rspo_spoolid_to_pdf INTO DATA(lx_e).
        WRITE lx_e->get_text( ).
    ENDTRY.
```
</details>

## Diagrams
<details open>
  <summary>UML Class Diagram</summary>
   <p><a target="_blank" rel="noopener noreferrer" href="https://github.com/victorizbitskiy/zspool_pdf/blob/main/docs/img/Class%20Diagram.svg"><img src="https://github.com/victorizbitskiy/zspool_pdf/blob/main/docs/img/Class%20Diagram.svg" alt="UML Class Diagram" style="max-width:100%;"></a></p>
</details>

## How to contribute
[This instruction](https://docs.abapgit.org/guide-contributing.html) will help you.
  
## Got questions?
If you have questions or general suggestions, don't hesitate to submit a new [(GitHub issue)](https://github.com/victorizbitskiy/zspool_pdf/issues/new).
  
## Logo
Project logo <a href="https://www.flaticon.com/ru/authors/pixel-buddha">designed by pixel-buddha</a>
