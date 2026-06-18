module VersionedCustom

using CustomParser

const custom_parser_used = CUSTOM_PARSER_MARKER
const parser_ref = GlobalRef(CustomParser, :core_parser_hook)
const parser_installed =
    getglobal(@__MODULE__, Symbol("#_internal_julia_parse")).parser_ref == parser_ref

module Child
end

const child_parser_installed =
    getglobal(Child, Symbol("#_internal_julia_parse")).parser_ref == parser_ref

end
