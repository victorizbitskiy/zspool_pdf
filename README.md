<img src="https://github.com/victorizbitskiy/zspool_pdf/blob/main/logo/logo.svg" height="100px"/>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/victorizbitskiy/zspool_pdf/blob/main/LICENSE)
![ABAP 7.40sp08+](https://img.shields.io/badge/ABAP-7.40sp08+-brightgreen)
[![Code Statistics](https://img.shields.io/badge/CodeStatistics-abaplint-blue)](https://abaplint.app/stats/victorizbitskiy/zspool_pdf)

Translations:
- [:ru: На русском языке](https://github.com/victorizbitskiy/zspool_pdf/tree/main/translations/ru) 

## `SAP Spool PDF`

Submitting a report to the spool and receiving PDF.

# Table of contents
1. [What is it?](#what-is-it)
2. [What is this for?](#what-is-this-for)
3. [Why not just use cl_bp_abap_job?](#why-not-just-use-cl_bp_abap_job?)
4. [Installation](#installation)
5. [Using](#using)
6. [Diagrams](#diagrams)
7. [How to contribute](#how-to-contribute)
8. [Got questions](#Got-questions)
9. [Logo](#logo)

## What is it?
`SAP Spool PDF` is a lightweight ABAP utility that simplifies generating PDFs from SAP reports. It provides a clean, object-oriented interface to submit a report to the spool and retrieve the output as a PDF file without dealing with low-level conversion logic.

## What is this for?
Often, you need a report’s output as a PDF in xstring or binary format. One clean and non-intrusive way to achieve this is by submitting the report to the SAP spool (typically in the background) and then converting the spool request into a PDF using its spool ID.
This approach avoids modifying standard SAP code—making it safer, upgrade-friendly, and easier to maintain.

## Why not just use cl_bp_abap_job?
SAP’s standard class **cl_bp_abap_job** can run reports in the background—but it only supports passing parameters via a variant. (Let me know if that’s changed!)
While variants work, they’re not always convenient: you either have to predefine one or create it dynamically at runtime, which adds complexity and overhead.

`SAP Spool PDF` gives you more flexibility. It lets you run any report in the background and generate a PDF from its output passing parameters directly, via a variant, or even both.

Thanks to its minimal and intuitive API, you can get the resulting PDF as an xstring in a single line ready to save, display, email, or process further.

## Installation

Install the package using [abapGit](http://www.abapgit.org). Just clone the repository into your SAP system, and you’re ready to go!

## Using

<details open>
<base target="_blank">
<summary>Example</summary>
   
```abap
   TYPES ty_pernr TYPE n LENGTH 8.
    DATA lt_pernr TYPE RANGE OF ty_pernr.

   " This example demonstrates how to generate 2-NDFL certificates (HCM module).
   " All individual PDF documents are automatically merged into a single file.

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
 will help you.
Contributions are welcome! Please follow the [official contribution guide](https://docs.abapgit.org/guide-contributing.html) from abapGit to get started.
  
## Got questions?
If you have any questions, feature ideas, or just want to share feedback - feel free to open a new [(GitHub issue)](https://github.com/victorizbitskiy/zspool_pdf/issues/new).
  
## Logo
Project logo <a href="https://www.flaticon.com/ru/authors/pixel-buddha">designed by pixel-buddha</a>
