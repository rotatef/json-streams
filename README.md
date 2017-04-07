# json-streams

json-streams is a Common Lisp library for reading and writing JSON
text.

Homepage: https://github.com/copyleft/json-streams

## Features

- Designed to used as a building block for more high level libraries.
- No dependencies.
- Easy to use streaming API.
- Validates input and output.
- Unambiguous mapping of datatypes between lisp and JSON.
- Streaming design, can proccess JSON text of unlimted size.
- Proper handling of Unicode.

## High level interface

### Streaming interface for output

Example

```common-lisp
(with-json-output (nil :key-encoder #'string-downcase :indent t)
  (with-json-object
    (json-output-member :first-name "John")
    (json-output-member :last-name "Smith")
    (with-json-member :is-alive (json-output-boolean t))
    (json-output-member :age 25)
    (with-json-member :address
      (json-output-alist '((:street-address . "21 2nd Street")
                           (:city . "New York")
                           (:state . "NY")
                           (:postal-code . "10021-3100"))))
    (with-json-member :phone-numbers
      (with-json-array
        (json-output-plist '(:type "home"
                             :number "212 555-1234"))
        (json-output-plist '(:type "office"
                             :number "646 555-4567"))
        (json-output-plist '(:type "mobile"
                             :number "123 456-7890"))))
    (json-output-member :children #())
    (with-json-member :spouse (json-output-null))))
```

```
Macro
JSON-STREAMS:WITH-JSON-OUTPUT (&optional target &rest options) &body body
```

The options are explained under `MAKE-JSON-OUTPUT-STREAM`. Target must
be a character stream or NIL. If target is NIL output is written to a
string that is returned, otherwise the values of body is returned.

The following functions and macros can only be used inside the body of `WITH-JSON-OUTPUT`:

- `WITH-JSON-ARRAY` - Use any of the other functions/macros in the body to output elements of the array.
- `WITH-JSON-OBJECT` - Use `WITH-JSON-MEMBER` or `JSON-OUTPUT-MEMBER` in the body.
- `WITH-JSON-MEMBER` - The body must contain one call to one output function/macros to output the value.
- `JSON-OUTPUT-MEMBER` - Outputs the member value using `JSON-OUTPUT-VALUE`.
- `JSON-OUTPUT-VALUE` - string, number, sequence or hash-table.
- `JSON-OUTPUT-BOOLEAN` - `NIL` outputs `false`, anything else outputs `true`.
- `JSON-OUTPUT-NULL` - Always outputs `null`. No arguments.
- `JSON-OUTPUT-ALIST` - Outputs an assoc list as a JSON object using `JSON-OUTPUT-MEMBER`.
- `JSON-OUTPUT-PLIST` - Outputs a propretry list as JSON object using `JSON-OUTPUT-MEMBER`.

### Parsing and writing using an unambigious s-expression notation

The high level interface uses the following datatype mapping:

JSON   | Common Lisp
-------|------------
true   | T
false  | NIL
null   | :NULL
string | string
number | integer, float or ratio
array  | (:ARRAY ... )
object | (:OBJECT (key . value) ... )

The options are explained under `MAKE-JSON-INPUT-STREAM` and
`MAKE-JSON-OUTPUT-STREAM`.

```
Function
JSON-STREAMS:JSON-PARSE source &rest options
=> json, position
```

Parses a single JSON text from source.  The second value is the
position where parsing stopped.

```
Function
JSON-STREAMS:JSON-PARSE-MULTIPLE source &rest options
=> jsons
```

Parses zero or more JSON texts from source.  Returns a list of JSON
values.

```
Function
JSON-STREAMS:JSON-STRINGIFY value &optional target &rest options
```

Stringifies value into JSON text.  If target is not provided or NIL,
returns a string with the JSON text.  Otherwise target must be a
character stream.

```
Function
JSON-STREAMS:JSON-STRINGIFY-MULTIPLE values &optional target &rest options
```

Stringifies multiple json values.


## The streaming interface

See parse.lisp and stringify.lisp for examples of how to use the
streaming interface.

```
Function
JSON-STREAMS:MAKE-JSON-INPUT-STREAM source &key start end close-stream multiple duplicate-key-check use-ratios max-exponent raw-strings
=> json-input-stream
```

- **source** - A character stream or a string. When source is a string,
  start and end are bounding index designators of string.
- **use-ratios** - Default false.  When false, numbers that can not be
  represented as integers are returned as double-floats.  Then true,
  these numbers are returned as ratios.
- **max-exponent** - Default 325.  The maximum (positive and negative)
  exponent accepted in numbers.  Setting to a high number opens up for a
  easy to use denial-of-service attack.
- **raw-strings** - Default false.  When true, strings are returned as
    `(:STRING numbers*)`, where numbers represent Unicode code points.
  For Unicode escape sequences in strings (\uXXXX), no UTF-16
  validation or decoding is done.
- **key-encoder** - Default `NIL`. A function taking one
  argument. This used by the higher lever functions to transform the
  keys of objects into strings.

```
Function
JSON-STREAMS:MAKE-JSON-OUTPUT-STREAM stream &key close-stream multiple duplicate-key-check indent escape-non-ascii
=> json-input-stream
```

- **stream** - A character stream.
- **indent** - Default false.  When true, the JSON text is broken into lines
  and indented.
- **escape-non-ascii** - Default false. When true, any character outside the
  ASCII character set will be escaped using the \uXXXX syntax.

### Common options

- **close-stream** - Default false.  When true, the source stream will be
  closed by JSON-CLOSE.
- **multiple** - Default false.  When true, only a single JSON text (object or
  array) is expected in the stream.  When T, zero or more JSON texts
  can be read from or written to stream.
- **duplicate-key-check** - Default true.  When true, an error will be
  signaled when duplicate keys are detected in an object.  Setting
  this to false avoids the added overhead of checking for duplicate keys.

```
Function
JSON-STREAMS:JSON-READ json-input-stream
=> token
```

Parses JSON text from the underlying stream and returns the next
token.

```
Function
JSON-STREAMS:JSON-WRITE token json-output-stream
=> token
```

Outputs JSON text to underlying stream based on the given token.

```
Function
JSON-STREAMS:JSON-CLOSE json-stream &key abort
```

Closes the json-stream.  Applications should always call `JSON-CLOSE`
when finished with a JSON stream.  This ensures that all syntax errors
are detected.  The value of abort is forwarded to `CL:CLOSE` of the
underlying stream (if close-stream was true) and JSON syntax checking
is suppressed.

```
Macro
JSON-STREAMS:WITH-OPEN-JSON-STREAM (var json-stream) &body
```

Binds var to json-stream, executes body, and ensures that JSON-CLOSE
is called on json-stream.

## Tokens

The possible tokens for input and output are

- `:BEGIN-OBJECT`
- `:END-OBJECT`
- `:BEGIN-ARRAY`
- `:END-ARRAY`
- `:NULL`
- `:FALSE`
- `:TRUE`
- A number
- A string or `(:STRING integer*)`
- `:EOF`

After a `:BEGIN-OBJECT` is returned, `JSON-READ` will alternate between
returning keys and values of the object, until `:END-OBJECT` is returned
instead of a key.  Keys are always returned as strings.  Values can be
anything, except `:END-OBJECT`, `:END-ARRAY` and `:EOF`.  Thus some kind of
recursive or stack based algorithm is neccearry.

### Handling of numbers

While JSON standard doesn't define any limits on the size of numbers,
many progamming languages does.  This library assumes the limits of
JavaScript.  In JavaScript there is only one number type, a 64-bit
IEEE 754 double precision float.  This has the following implications:

The syntax doesn't matter: 20, 20.0 or 2e1 are all the same number,
the integer 20.

The integer range is +/- (expt 10 53).  Numbers outside this range
can't be represented accurately.  This library will refuse to write an
integer outside this range.  If you need to output such large numbers,
convert them to double-float or string.

At input the handling of numbers outside the integer range depends on
syntax.  If the number contains a decimal point or an exponent it will
be returned as a double-float.  Otherwise an error will be signalled.
The option `:USE-RATIOS` disables this check.
