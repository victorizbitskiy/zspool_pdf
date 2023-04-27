<img src="https://github.com/victorizbitskiy/zspool_pdf/blob/main/logo/logo.svg" height="100px"/>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/victorizbitskiy/zspool_pdf/blob/main/LICENSE)
![ABAP 7.40sp08+](https://img.shields.io/badge/ABAP-7.40sp08+-brightgreen)
[![Code Statistics](https://img.shields.io/badge/CodeStatistics-abaplint-blue)](https://abaplint.app/stats/victorizbitskiy/zspool_pdf)

Переводы:
- [:uk: In English](https://github.com/victorizbitskiy/zspool_pdf) 

## `SAP Spool PDF`

Отправка отчета в спул и получение PDF.

# Оглавление
1. [Что это такое?](#что-это-такое)
2. [Зачем это нужно?](#зачем-это-нужно)
3. [Установка](#установка)
4. [Использование](#использование)
5. [Диаграммы](#диаграммы)
6. [Как внести свой вклад](#Как-внести-свой-вклад)
7. [Остались вопросы?](#Остались-вопросы)
8. [Логотип](#Логотип)

## Что это такое?

`SAP Spool PDF` это несколько классов, которые помогут вам отправить отчет в спул и легко получить файл PDF.

## Зачем это нужно?

Часто бывает необходимо получить PDF в формате *xstring* или *binary*. Для этого вы можете отправить отчет в спул (в фоновом режиме), а затем прочитать PDF по спул ID.
Такой способ получения файла является более предпочтительным, т.к. нет необходимости делать расширение в стандартном коде.

Для запуска отчетов в фоновом режиме в ABAP существует класс **cl_bp_abap_job**. Но этот класс поддерживает передачу параметров отчета только через вариант (сообщите мне, если я ошибаюсь).
Это не всегда удобно, так как вариант должен быть создан либо до запуска отчета, либо приходится создавать его динамически, во время выполнения.

`SAP Spool PDF` позволяет запускать в фоновом режиме любой отчет генерирующий PDF. Параметры отчета могут быть переданы непосредственно и/или через вариант.  

Лаконичный API позволяет буквально одной строкой получить файл в виде *xstring*, сохранить, показать или отправить его на почту. 

## Установка

Установка выполняется с помощью [abapGit](http://www.abapgit.org).

## Использование

<details open>
<base target="_blank">
<summary>Пример</summary>
   
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

## Диаграммы
<details open>
  <summary>UML Class Diagram</summary>
   <p><a target="_blank" rel="noopener noreferrer" href="https://github.com/victorizbitskiy/zspool_pdf/blob/main/docs/img/Class%20Diagram.svg"><img src="https://github.com/victorizbitskiy/zspool_pdf/blob/main/docs/img/Class%20Diagram.svg" alt="UML Class Diagram" style="max-width:100%;"></a></p>
</details>

## Как внести свой вклад
[Эта инструкция](https://docs.abapgit.org/guide-contributing.html) поможет вам.

## Остались вопросы?
Если у вас есть вопросы или предложения не стесняйтесь отправлять их [(GitHub issue)](https://github.com/victorizbitskiy/zspool_pdf/issues/new).

## Логотип
Логотип проекта <a href="https://www.flaticon.com/ru/authors/pixel-buddha">designed by pixel-buddha</a>
