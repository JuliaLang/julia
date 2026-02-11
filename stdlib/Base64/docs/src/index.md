```@meta
EditURL = "https://github.com/JuliaLang/julia/blob/master/stdlib/Base64/docs/src/index.md"
```

# Base64

The Base64 standard library implements encoding and decoding of the [base64](https://en.wikipedia.org/wiki/Base64) format following the [RFC 4648](https://datatracker.ietf.org/doc/html/rfc4648) standard, with the following important notes:
- Non-alphabet characters are ignored when decoding (see [section 3.3 of RFC 4648](https://datatracker.ietf.org/doc/html/rfc4648#section-3.3)).
- When decoding, if the data has padding outside of its specified location in the last 0-2 characters of the last group of four alphabet characters (e.g. `"A==="`, `"AAAA="`, `"A=AA"`), the data is rejected with `ArgumentError: malformed base64 sequence`.
- Padding is optional when decoding (`"AA"` and `"AA=="` are equally valid), and is always inserted when encoding.
The following alphabet is used:

| Value | Encoding | Value | Encoding | Value | Encoding | Value   | Encoding |
| -----:|:-------- | -----:|:-------- | -----:|:-------- | -----:  |:-------- |
| 0     | A        | 17    | R        | 34    | i        | 51      | z        |
| 1     | B        | 18    | S        | 35    | j        | 52      | 0        |
| 2     | C        | 19    | T        | 36    | k        | 53      | 1        |
| 3     | D        | 20    | U        | 37    | l        | 54      | 2        |
| 4     | E        | 21    | V        | 38    | m        | 55      | 3        |
| 5     | F        | 22    | W        | 39    | n        | 56      | 4        |
| 6     | G        | 23    | X        | 40    | o        | 57      | 5        |
| 7     | H        | 24    | Y        | 41    | p        | 58      | 6        |
| 8     | I        | 25    | Z        | 42    | q        | 59      | 7        |
| 9     | J        | 26    | a        | 43    | r        | 60      | 8        |
| 10    | K        | 27    | b        | 44    | s        | 61      | 9        |
| 11    | L        | 28    | c        | 45    | t        | 62      | +        |
| 12    | M        | 29    | d        | 46    | u        | 63      | /        |
| 13    | N        | 30    | e        | 47    | v        | padding | =        |
| 14    | O        | 31    | f        | 48    | w        |
| 15    | P        | 32    | g        | 49    | x        |
| 16    | Q        | 33    | h        | 50    | y        |

## Encoding

```@docs
Base64.Base64EncodePipe
Base64.base64encode
Base64.stringmime
```

## Decoding
```@docs
Base64.Base64DecodePipe
Base64.base64decode
```
