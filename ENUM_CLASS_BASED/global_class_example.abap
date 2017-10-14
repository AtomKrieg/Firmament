class zcl_enum definition
  public
  create private
  final.

public section.

  class-data info type ref to zcl_enum read-only.
  class-data warning type ref to zcl_enum read-only.
  class-data error type ref to zcl_enum read-only.

  data string type string read-only.

  class-methods class_constructor.

ENDCLASS.



CLASS ZCL_ENUM IMPLEMENTATION.


  method class_constructor.
    data(lt_stack) = cl_abap_get_call_stack=>format_call_stack_with_struct(
      cl_abap_get_call_stack=>get_call_stack( ) ).

    data(ev) = lt_stack[ 1 ]-event.

    data(class_name) = segment( val = ev index = 1 sep = '=>' ).

    data(sdescr) = cast cl_abap_classdescr(
      cl_abap_classdescr=>describe_by_name( class_name ) ).

    data(attr_count) = 0.
    data(string_attr_name) = ``.

    loop at sdescr->attributes into data(ls_attr)
      where type_kind = cl_abap_typedescr=>typekind_string.

      attr_count = attr_count + 1.
      string_attr_name = ls_attr-name.
    endloop.

    loop at sdescr->attributes into ls_attr
      where is_class = abap_true
        and is_read_only = abap_true
        and visibility = cl_abap_classdescr=>public
        and type_kind = cl_abap_typedescr=>typekind_oref.

      attr_count = attr_count + 1.

      data: obj type ref to object.
      create object obj type (class_name).

      assign (class_name)=>(ls_attr-name) to field-symbol(<enum>).
      <enum> ?= obj.

      if not string_attr_name is initial.
        assign obj->(string_attr_name) to field-symbol(<string>).
        <string> = ls_attr-name.
      endif.
    endloop.
  endmethod.
ENDCLASS.
