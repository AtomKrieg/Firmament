report  zz_enum_domain.
*=====================================================================*
*                              TOP
*=====================================================================*
tables: dd07v.
type-pools: abap.
constants: true value abap_true, false value abap_false.
class lcl_app definition deferred.
class lcl_enum_domain definition deferred.

data: app type ref to lcl_app.

*----------------------------------------------------------------------*
*       CLASS lcl_app DEFINITION
*----------------------------------------------------------------------*
class lcl_app definition.
  public section.
    methods show.
    methods pbo.
    methods pai
      importing i_ucomm type syucomm.

  private section.
    types: tt_text type standard table of text72
    with non-unique default key.

    methods refresh_textedit
      importing i_domname type domname
      returning value(r_text) type tt_text.

    methods copy_to_clipboard.

    data: m_cont type ref to cl_gui_custom_container.
    data: m_textedit type ref to cl_gui_textedit.
    data: m_curdom type domname.
endclass.                    "lcl_app DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_enum_domain DEFINITION
*----------------------------------------------------------------------*
class lcl_enum_domain definition create private.
  public section.
    class-methods class_constructor.

    types: begin of t_value,
      name type fieldname,
      value type domvalue_l,
    end of t_value.

    types: tt_value type standard table of t_value
    with non-unique default key.

    types: tt_text type standard table of text72
    with non-unique default key.

    class-methods new
       importing i_domname type domname
       returning value(r_instance) type ref to lcl_enum_domain.

    data: mt_value type tt_value read-only.
    data: m_domname type domname read-only.
    data: m_rollname type rollname read-only.

    methods make.

    data: mt_code type tt_text read-only.

  private section.
    class-data: m_allowed_symbols type text100.
    class-data: m_prefix_for_numeric type c.
    class-data: mt_lang_order type range of spras.

    methods based_by_fixed.

    methods based_by_table
      importing i_tabname type tabname.

    methods add_codeline
      importing i_line type text72.

    methods transliterate.
    methods purge_names.
    methods add_prefix.
    methods generate.

    types: begin of t_texttableinfo,
        tabname type tabname,
        langfield type fieldname,
        keyfield type fieldname,
        keyrollname type rollname,
        textfield type fieldname,
        textrollname type fieldname,
      end of t_texttableinfo.

    methods get_text_table_info
      importing i_tabname type tabname
      returning value(r_texttableinfo) type t_texttableinfo.

    methods create_dynamic_table
      importing i_texttableinfo type t_texttableinfo
      returning value(r_dref) type ref to data.

    methods get_most_popular_rollname
      returning value(r_rollname) type rollname.
endclass.                    "lcl_enum_domain DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_app IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_app implementation.
  method show.
    call screen 100.
  endmethod.                    "show

  method pbo.
    set pf-status 'S100'.
    set titlebar 'T100'.

    check m_cont is initial.

    create object m_cont
      exporting
        container_name = 'TEXTEDIT'.

    create object m_textedit
      exporting
        parent        = m_cont
        wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position.

    m_textedit->set_readonly_mode( 1 ).
  endmethod.                    "pbo

  method pai.
    case i_ucomm.
      when 'BACK' or 'UP' or 'EXIT'. leave program.
      when 'COPY'. copy_to_clipboard( ).
      when others. refresh_textedit( dd07v-domname ).
    endcase.
  endmethod.                    "pai

  method refresh_textedit.
    data: lo type ref to lcl_enum_domain.
    lo = lcl_enum_domain=>new( dd07v-domname ).
    check lo is not initial.

    lo->make( ).
    m_textedit->set_text_as_r3table( lo->mt_code ).
  endmethod.                    "convert
  method copy_to_clipboard.
    data: lt_text type tt_text.
    data: rc type i.

    m_textedit->get_text_as_r3table( importing table = lt_text ).
    cl_gui_frontend_services=>clipboard_export( importing data = lt_text changing rc = rc ).
  endmethod.                    "copy_to_clipboard
endclass.                    "lcl_app IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_enum_domain IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_enum_domain implementation.
  method class_constructor.
    m_allowed_symbols = sy-abcde && to_lower( sy-abcde ) && '01234567890 _'.

    m_prefix_for_numeric = '_'.

    data: ls_lang_order like line of mt_lang_order value 'IEQ'.

    ls_lang_order-low = 'E'.
    append ls_lang_order to mt_lang_order.

    if sy-langu <> 'E'.
      ls_lang_order-low = sy-langu.
      append ls_lang_order to mt_lang_order.
    endif.
  endmethod.                    "class_constructor

  method new.
    data: ls_dd01l type dd01l.

    select single *
      from dd01l
      into ls_dd01l
      where domname = i_domname.

    if ls_dd01l-valexi <> true and ls_dd01l-entitytab is initial.
      return.
    endif.

    create object r_instance.
    r_instance->m_domname = i_domname.

    if ls_dd01l-valexi = true.
      r_instance->based_by_fixed( ).
    else.
      r_instance->based_by_table( ls_dd01l-entitytab ).
    endif.

  endmethod.                    "new

  method based_by_fixed.

    select single rollname
      from dd04l
      into m_rollname
      where rollname = m_domname
        and domname = m_domname.

    if sy-subrc <> 0.
      m_rollname = get_most_popular_rollname( ).
    endif.

    data: lt_dd07v type table of dd07v.
    data: ls_dd07v type dd07v.
    data: ls_lang_order like line of mt_lang_order.

    loop at mt_lang_order into ls_lang_order.

      select ddtext domvalue_l
        from dd07v
        into corresponding fields of table lt_dd07v
        where domname = m_domname
          and ddlanguage = ls_lang_order-low.

      if sy-subrc = 0.
        exit.
      endif.
    endloop.

    data: ls_value type t_value.

    loop at lt_dd07v into ls_dd07v.
      ls_value-name = ls_dd07v-ddtext.
      ls_value-value = ls_dd07v-domvalue_l.
      append ls_value to mt_value.
    endloop.
  endmethod.                    "based_by_fixed

  method based_by_table.
    data: l_fieldname type fieldname.

    case i_tabname.
      when 'T000'.
        l_fieldname = 'MANDT'.
        m_rollname = 'MANDT'.
      when others.
        select single fieldname rollname
          into (l_fieldname, m_rollname)
          from dd03l
          where tabname = i_tabname
            and keyflag = true
            and fieldname <> 'MANDT'.
    endcase.

    data: l_dref type ref to data.
    field-symbols: <tab> type standard table.
    field-symbols: <val> type any.
    data: ls_value type t_value.

    data: ls_texttableinfo type t_texttableinfo.

    ls_texttableinfo = get_text_table_info( i_tabname ).

    " no text table -> only values.
    if ls_texttableinfo is initial.
      create data l_dref type standard table of (m_rollname) with non-unique default key.
      assign l_dref->* to <tab>.

      select (l_fieldname)
        into table <tab>
        from (i_tabname).

      loop at <tab> assigning <val>.
        ls_value-name = <val>.
        ls_value-value = <val>.
        append ls_value to mt_value.
      endloop.

      return.
    endif.

    l_dref = create_dynamic_table( ls_texttableinfo ).
    assign l_dref->* to <tab>.

    data: ls_lang_order like line of mt_lang_order.
    data: fields_for_select type string.

    fields_for_select = ls_texttableinfo-keyfield && ` ` && ls_texttableinfo-textfield.

    if ls_texttableinfo-langfield is initial.
      select (fields_for_select)
          into table <tab>
          from (ls_texttableinfo-tabname).
    else.

      data: condition type string.
      loop at mt_lang_order into ls_lang_order.

        condition = |{ ls_texttableinfo-langfield } = '{ ls_lang_order-low }'|.

        select (fields_for_select)
          into table <tab>
          from (ls_texttableinfo-tabname)
          where (condition).

        if sy-subrc = 0.
          exit.
        endif.
      endloop.
    endif.

    field-symbols: <domname> type any.
    field-symbols: <domvalue> type any.

    if ls_texttableinfo-textfield is initial.
      loop at <tab> assigning <val>.
        assign component ls_texttableinfo-keyfield of structure <val> to <domname>.
        ls_value-name = <domname>.
        ls_value-value = <domname>.
        append ls_value to mt_value.
      endloop.
    else.
      loop at <tab> assigning <val>.
        assign component ls_texttableinfo-textfield of structure <val> to <domname>.
        assign component ls_texttableinfo-keyfield of structure <val> to <domvalue>.
        ls_value-name = <domname>.
        ls_value-value = <domvalue>.
        append ls_value to mt_value.
      endloop.
    endif.

  endmethod.                    "based_by_table

  method make.

    transliterate( ).
    purge_names( ).
    add_prefix( ).
    generate( ).

  endmethod.                    "make

  method add_codeline.
    append i_line to mt_code.
  endmethod.                    "add_codeline

  method transliterate.
    data: transliterator type ref to cl_icu_transformation.

    cl_icu_transformation=>create_instance_from_id(
      exporting im_id = 'Any-Latin'
      importing ex_trans = transliterator ).

    field-symbols: <value> type t_value.
    data: str type string.

    loop at mt_value assigning <value>.
      transliterator->transliterate(
        exporting im_text = <value>-name
        importing ex_text = str ).

      <value>-name = str.
    endloop.
  endmethod.                    "transliterate

  method purge_names.

    field-symbols: <value> type t_value.
    field-symbols: <name> type fieldname.
    data: idx type i.
    data: char type c.
    data: count type i value 0.
    data: result type fieldname.
    data: len type i.

    loop at mt_value assigning <value>.
      assign component 1 of structure <value> to <name>.

      count = 0.
      result = ''.
      len = strlen( <value>-name ).

      do len times.
        idx = sy-index - 1.
        char = <value>-name+idx(1).
        check char co m_allowed_symbols.
        translate char using ' _'.
        result+count(1) = char.
        count = count + 1.
      enddo.

      <value>-name = result.
    endloop.
  endmethod.                    "purge_name

  method add_prefix.
    field-symbols: <value> type t_value.
    data: need_prefix type boolean value false.

    loop at mt_value assigning <value>.
      check <value>-name(1) co '0123456789'.
      need_prefix = true.
      exit.
    endloop.

    check need_prefix = true.

    loop at mt_value assigning <value>.
      <value>-name = m_prefix_for_numeric && <value>-name.
    endloop.
  endmethod.                    "add_prefix

  method generate.
    field-symbols: <value> type t_value.

    add_codeline( |types: begin of t_{ to_lower( m_domname ) },| ).
    loop at mt_value assigning <value>.
      add_codeline( |  { <value>-name } type { to_lower( m_rollname ) } value '{ <value>-value }',| ).
    endloop.
    add_codeline( |end of t_{ to_lower( m_domname ) }.| ).
  endmethod.                    "generate

  method get_text_table_info.

    select single tabname fieldname
      from dd08l
      into (r_texttableinfo-tabname, r_texttableinfo-keyfield)
      where checktable = i_tabname
        and frkart = 'TEXT'
        and as4local = 'A'.

    check sy-subrc = 0.

    select single rollname
      from dd03l
      into (r_texttableinfo-keyrollname)
      where tabname = r_texttableinfo-tabname
        and fieldname = r_texttableinfo-keyfield
        and as4local = 'A'.

    select fieldname rollname
      from dd03l
      into (r_texttableinfo-textfield, r_texttableinfo-textrollname)
      where tabname = r_texttableinfo-tabname
        and as4local = 'A'
        and keyflag = false
      order by position.
      exit.
    endselect.

    select single fieldname
      from dd03l
      into r_texttableinfo-langfield
      where tabname = r_texttableinfo-tabname
        and as4local = 'A'
        and keyflag = true
        and checktable = 'T002'
        and not fieldname in (r_texttableinfo-keyfield, 'MANDT').

  endmethod.                    "get_text_table_info

  method create_dynamic_table.
    data: lt_comp type abap_component_tab.
    data: ls_comp type abap_componentdescr.
    data: edescr type ref to cl_abap_elemdescr.

    ls_comp-name = i_texttableinfo-keyfield.
    edescr ?= cl_abap_elemdescr=>describe_by_name( i_texttableinfo-keyrollname ).
    ls_comp-type = edescr.
    append ls_comp to lt_comp.

    if not i_texttableinfo-textfield is initial.
      ls_comp-name = i_texttableinfo-textfield.
      edescr ?= cl_abap_elemdescr=>describe_by_name( i_texttableinfo-textrollname ).
      ls_comp-type = edescr.
      append ls_comp to lt_comp.
    endif.

    data: sdescr type ref to cl_abap_structdescr.
    sdescr = cl_abap_structdescr=>create( lt_comp ).

    data: tdescr type ref to cl_abap_tabledescr.
    tdescr = cl_abap_tabledescr=>create( sdescr ).

    create data r_dref type handle tdescr.
  endmethod.                    "create_dynamic_table

  method get_most_popular_rollname.
    types: begin of t_rollname_count,
      rollname type rollname,
      count type i,
    end of t_rollname_count.

    types: tt_rollname_count type standard table of t_rollname_count
    with non-unique default key.

    data: lt_rollname type tt_rollname_count.
    data: ls_rollname type t_rollname_count.

    select rollname count( distinct rollname ) as count
      from dd04l
      into corresponding fields of table lt_rollname
      where domname = m_domname
        and as4local = 'A'
      group by rollname.

    read table lt_rollname into ls_rollname index lines( lt_rollname ).
    m_rollname = ls_rollname-rollname.
  endmethod.                    "get_most_popular_rollname
endclass.                    "lcl_enum_domain IMPLEMENTATION

*=====================================================================*
*                            PROGRAM
*=====================================================================*
start-of-selection.
  create object app.
  app->show( ).

*----------------------------------------------------------------------*
*  MODULE pbo OUTPUT
*----------------------------------------------------------------------*
module pbo output.
  app->pbo( ).
endmodule.                    "pbo OUTPUT

*----------------------------------------------------------------------*
*  MODULE pai INPUT
*----------------------------------------------------------------------*
module pai input.
  app->pai( sy-ucomm ).
endmodule.                    "pai INPUT