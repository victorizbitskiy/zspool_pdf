class ZCL_SPDF_MAP_VALUE definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !I_VALUE type ANY .
  methods GET
    returning
      value(R_VALUE) type ref to DATA .
protected section.
private section.

  data M_VALUE type ref to DATA .
ENDCLASS.



CLASS ZCL_SPDF_MAP_VALUE IMPLEMENTATION.


  METHOD constructor.
    FIELD-SYMBOLS: <m_value> TYPE any.

    CREATE DATA m_value LIKE i_value.
    ASSIGN m_value->* TO <m_value>.
    <m_value> = i_value.

  ENDMETHOD.


  METHOD get.
    r_value = m_value.
  ENDMETHOD.
ENDCLASS.
