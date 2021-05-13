<img src="https://github.com/victorizbitskiy/zspool_pdf/blob/main/logo/logo.svg" height="100px"/>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/victorizbitskiy/zspool_pdf/blob/main/LICENSE)
![ABAP 7.40+](https://img.shields.io/badge/ABAP-7.40sp08+-brightgreen)
![lint](https://github.com/victorizbitskiy/zspool_pdf/actions/workflows/main.yml/badge.svg)

**ВНИМАНИЕ**: Проект все еще разрабатывается и может изменяться

## `SAP Spool PDF`

Отправка отчета в спул и получение PDF.

# Оглавление
1. [Что это такое?](#что-это-такое)
2. [Зачем это нужно?](#зачем-это-нужно)
3. [Установка](#установка)
4. [Использование](#использование)

## Что это такое?

`SAP Spool PDF` это несколько классов, которые помогут вам отправить отчет в спул и легко получить файл PDF.

## Зачем это нужно?

Часто бывает необходимо получить PDF в формате xstring или binary. Для этого вы можете отправить отчет в спул(в фоновом режиме), а затем прочитать PDF по спул ID.

Для запуска отчетов в фоновом режиме в ABAP существует класс **cl_bp_abap_job**. Но этот класс поддерживает передачу параметров отчета только через вариант (сообщите мне, если я ошибаюсь).
Это не всегда удобно, так как вариант должен быть создан либо до запуска отчета, либо его приходится создавать во время выполнения.

SAP Spool PDF позволяет запускать любой отчет, генерирующий PDF, в фоновом режиме. Параметры отчета могут быть переданы непосредственно и/или через вариант.
Лаконичное API позволяет буквально одной строкой получить файл в виде xstring или binary, сохранить или открыть его.
Если PDF формируется как отдельные листы, то их можно автоматически объединить. 

## Installation

Installation is done with [abapGit](http://www.abapgit.org).

## Using

<details>
<base target="_blank">
<summary>Show code...</summary>
   
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

