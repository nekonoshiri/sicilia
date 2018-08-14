tiny-proto-parser

# Language Specification

```
letter = "A" … "Z" | "a" … "z"
decimalDigit = "0" … "9"
octalDigit = "0" … "7"
hexDigit = "0" … "9" | "A" … "F" | "a" … "f"

ident = letter { letter | decimalDigit | "_" }
messageName = ident
fieldName = ident

intLit = decimalLit | octalLit | hexLit
decimalLit = ( "1" … "9" ) { decimalDigit }
octalLit = "0" { octalDigit }
hexLit = "0" ( "x" | "X" ) hexDigit { hexDigit }

type = "double" | "int32" | "string"
fieldNubmer = intLit

field = type fieldName "=" fieldNumber ";"

messageBody = "{" { field } "}"
message = "message" messageName messageBody

topLevelDef = message

proto = { topLevelDef }
```

# Language Specification with spaces

```
...

field = type spaces1 fieldName spaces "=" spaces fieldNumber spaces ";"

messageBody = "{" spaces { field spaces } "}"
message = "message" spaces1 messageName spaces messageBody

topLevelDef = message

proto = spaces { topLevelDef spaces }

space = ? space character ?
spaces = { space }
spaces1 = space { space }
```
