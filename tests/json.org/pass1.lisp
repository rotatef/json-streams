()
(:BEGIN-ARRAY
 "JSON Test Pattern pass1"
 :BEGIN-OBJECT "object with 1 member" :BEGIN-ARRAY  "array with 1 element" :END-ARRAY :END-OBJECT
 :BEGIN-OBJECT :END-OBJECT
 :BEGIN-ARRAY :END-ARRAY
 -42
 :TRUE
 :FALSE
 :NULL
 :BEGIN-OBJECT
 "integer" 1234567890
 "real" -9876.543210d0
 "e"  0.123456789d-12
 "E" 1.23456789d34
 "" 23456789012d66
 "zero" 0
 "one" 1
 "space" " "
 "quote" "\""
 "backslash" "\\"
 "controls" "
	"
 "slash" "/ & /"
 "alpha" "abcdefghijklmnopqrstuvwyz"
 "ALPHA" "ABCDEFGHIJKLMNOPQRSTUVWYZ"
 "digit" "0123456789"
 "0123456789" "digit"
 "special" "`1~!@#$%^&*()_+-={':[,]}|;.</>?"
 "hex" "ģ䕧覫췯ꯍ"
 "true" :TRUE
 "false" :FALSE
 "null" :NULL
 "array" :BEGIN-ARRAY :END-ARRAY
 "object" :BEGIN-OBJECT :END-OBJECT
 "address" "50 St. James Street"
 "url" "http://www.JSON.org/"
 "comment" "// /* <!-- --"
 "# -- --> */" " "
 " s p a c e d " :BEGIN-ARRAY 1 2 3
 4 5 6 7 :END-ARRAY "compact"
 :BEGIN-ARRAY
 1 2 3 4 5 6 7
 :END-ARRAY
 "jsontext" "{\"object with 1 member\":[\"array with 1 element\"]}"
 "quotes" "&#34; \" %22 0x22 034 &#x22;"
 "/\\\"쫾몾ꮘﳞ볚
	`1~!@#$%^&*()_+-=[]{}|;:',./<>?"
 "A key can be any string"
 :END-OBJECT
 0.5d0 98.6d0
 99.44d0
 1066
 10
 1
 0.1d0
 1 2 2
 "rosebud" :END-ARRAY
 :eof)
