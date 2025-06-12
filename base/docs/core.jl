# This file is a part of Julia. License is MIT: https://julialang.org/license

module CoreDocs

import Core: @nospecialize, SimpleVector

# Temporary storage until the full docsystem is available
struct DocLinkedList
    args::Tuple
    next::DocLinkedList
    DocLinkedList() = new()
    DocLinkedList(args::Tuple, next::DocLinkedList) = new(args, next)
end

global DOCS = DocLinkedList()

function boot_setdoc(args...)
    global DOCS = DocLinkedList(args, DOCS)
    nothing
end

end
