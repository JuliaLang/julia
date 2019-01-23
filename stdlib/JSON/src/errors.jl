# The following errors may be thrown by the parser
const E_EXPECTED_EOF    = "Expected end of input"
const E_UNEXPECTED_EOF  = "Unexpected end of input"
const E_UNEXPECTED_CHAR = "Unexpected character"
const E_BAD_KEY         = "Invalid object key"
const E_BAD_ESCAPE      = "Invalid escape sequence"
const E_BAD_CONTROL     = "ASCII control character in string"
const E_LEADING_ZERO    = "Invalid leading zero in number"
const E_BAD_NUMBER      = "Invalid number"

export E_EXPECTED_EOF, E_UNEXPECTED_EOF, E_UNEXPECTED_CHAR, E_BAD_KEY,
       E_BAD_ESCAPE, E_BAD_CONTROL, E_LEADING_ZERO, E_BAD_NUMBER
