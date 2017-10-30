class zcl_domain_values definition
  public
  final
  create public .

public section.
  types: tt_value type standard table of domvalue_l with empty key.

  methods constructor
    importing
      i_domain type domname
      i_lang type syst_langu default sy-langu.

  methods get
    importing i_domvalue type clike
    returning value(r_text) type ddtext.

  data: values type tt_value read-only.

protected section.
private section.
  types: begin of t_self,
    domvalue_l type domvalue_l,
    ddtext type val_text,
  end of t_self.

  types: tt_self type sorted table of t_self with unique key domvalue_l.

  data: self type tt_self.
ENDCLASS.



CLASS ZCL_DOMAIN_VALUES IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DOMAIN_VALUES->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DOMAIN                       TYPE        DOMNAME
* | [--->] I_LANG                         TYPE        SYST_LANGU (default =SY-LANGU)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method constructor.

    select domvalue_l ddtext
      into table self
      from dd07v
      where domname = i_domain
        and ddlanguage = i_lang.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DOMAIN_VALUES->GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DOMVALUE                     TYPE        DOMVALUE_L
* | [<-()] R_TEXT                         TYPE        DDTEXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get.
    r_text = value #( self[ key primary_key DOMVALUE_L = i_domvalue ]-ddtext optional ).
  endmethod.
ENDCLASS.
