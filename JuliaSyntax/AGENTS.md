# JuliaSyntax Agent Instructions

See [../AGENTS.md](../AGENTS.md) for general instructions.

## Parser changes

When modifying the parser, always verify that AST production comments (e.g., `# f.[x]  ==>  (error f x)`) accurately reflect the actual parser output.
