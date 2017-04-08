 **Class for converting domain values to abap code type definition.** 

Examples

**Domain:** mandt

>types: begin of t_mandt,

>  _000 type mandt value '000',  
>  _001 type mandt value '001',  
>  _066 type mandt value '066',  
>end of t_mandt.

**Domain:** koart
>types: begin of t_koart,
  Assets type koart value 'A',
  Customers type koart value 'D',
  Vendors type koart value 'K',
  Material type koart value 'M',
  GL_accounts type koart value 'S',
end of t_koart.

**Domain:** spras
>types: begin of t_spras,
  Serbian type spras value '0',
  Chinese type spras value '1',
  Thai type spras value '2',
  Korean type spras value '3',
  Romanian type spras value '4',
  Slovenian type spras value '5',
  Croatian type spras value '6',
  Malay type spras value '7',
  Ukrainian type spras value '8',
  Estonian type spras value '9',
  Arabic type spras value 'A',
  Hebrew type spras value 'B',
  Czech type spras value 'C',
  German type spras value 'D',
  English type spras value 'E',
  French type spras value 'F',
  Greek type spras value 'G',
  Hungarian type spras value 'H',
  Italian type spras value 'I',
  Japanese type spras value 'J',
  Danish type spras value 'K',
  Polish type spras value 'L',
  Chinese_trad type spras value 'M',
  Dutch type spras value 'N',
  Norwegian type spras value 'O',
  Portuguese type spras value 'P',
  Slovak type spras value 'Q',
  Russian type spras value 'R',
  Spanish type spras value 'S',
  Turkish type spras value 'T',
  Finnish type spras value 'U',
  Swedish type spras value 'V',
  Bulgarian type spras value 'W',
  Lithuanian type spras value 'X',
  Latvian type spras value 'Y',
  Customer_reserve type spras value 'Z',
  Afrikaans type spras value 'a',
  Icelandic type spras value 'b',
  Catalan type spras value 'c',
  Serbian_Latin type spras value 'd',
  Indonesian type spras value 'i',
end of t_spras.
