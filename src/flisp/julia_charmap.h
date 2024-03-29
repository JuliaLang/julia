/* Array of {original codepoint, replacement codepoint} normalizations
   to perform on Julia identifiers, to canonicalize characters that
   are both easily confused and easily inputted by accident.

   Important: when this table is updated, also update the corresponding table
              in base/strings/unicode.jl */
static const uint32_t charmap[][2] = {
    { 0x025B, 0x03B5 }, // latin small letter open e -> greek small letter epsilon
    { 0x00B5, 0x03BC }, // micro sign -> greek small letter mu
    { 0x00B7, 0x22C5 }, // middot char -> dot operator (#25098)
    { 0x0387, 0x22C5 }, // Greek interpunct -> dot operator (#25098)
    { 0x2212, 0x002D }, // minus -> hyphen-minus (#26193)
    { 0x210F, 0x0127 }, // hbar -> small letter h with stroke (#48870)
};
