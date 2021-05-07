CLASS zcl_spdf_map_value DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !ia_value TYPE any .
    METHODS get
      RETURNING
        VALUE(rd_value) TYPE REF TO data .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA md_value TYPE REF TO data.
ENDCLASS.



CLASS ZCL_SPDF_MAP_VALUE IMPLEMENTATION.


  METHOD constructor.
    FIELD-SYMBOLS: <ld_value> TYPE any.

    CREATE DATA md_value LIKE ia_value.
    ASSIGN md_value->* TO <ld_value>.
    <ld_value> = ia_value.

  ENDMETHOD.


  METHOD get.
    rd_value = md_value.
  ENDMETHOD.
ENDCLASS.
