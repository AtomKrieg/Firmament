report  zz_find_text_table.

parameters: p_tname type tabname.

start-of-selection.
  perform find_text_table using p_tname.

*---------------------------------------------------------------------*
*      Form  find_text_table
*---------------------------------------------------------------------*
form find_text_table using i_tabname type tabname.

  data: ls_dd08l type dd08l.

  select single *
  from dd08l
  into ls_dd08l
  where checktable = i_tabname
    and frkart = 'TEXT'
    and as4local = 'A'.

  types: begin of t_tabinfo,
    keyflag type dd03l-keyflag,
    fieldname type dd03l-fieldname,
    rollname type dd03l-rollname,
    datatype  type dd03l-datatype,
    ddtext type dd04t-ddtext,
    end of t_tabinfo.

  types: tt_tabinfo type table of t_tabinfo.

  data: lt_tabinfo type tt_tabinfo.
  data: ls_tabinfo type t_tabinfo.

  select *
    from dd03l as l
    join dd04l as e
      on e~rollname = l~rollname
      and e~as4local = 'A'
    left join dd04t as t
      on  t~rollname = l~rollname
      and t~ddlanguage = sy-langu
      and t~as4local = 'A'
    into corresponding fields of table lt_tabinfo
    where l~tabname = ls_dd08l-tabname
    order by l~position.

  write: ls_dd08l-tabname color col_positive.
  write:
  /(4) 'KEY' color col_heading,
  (30) 'FIELDNAME' color col_heading,
  37(30) 'ROLLNAME' color col_heading,
  68(30) 'DATATYPE' color col_heading,
  99(30) 'DESCRIPTION' color col_heading.

  loop at lt_tabinfo into ls_tabinfo.
    write: /2(3) ls_tabinfo-keyflag.
    write: (30) ls_tabinfo-fieldname.
    write: 37(30) ls_tabinfo-rollname.
    write: 68(30) ls_tabinfo-datatype.
    write: 99(30) ls_tabinfo-ddtext.
  endloop.

endform.                    "find_text_table
