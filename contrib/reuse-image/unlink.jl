# unlink.jl — reconstruct a relocatable ELF object (.o) from a shared library
# that was linked with --emit-relocs (-q).
#
# Prototype: x86-64 / aarch64 ELF64 little-endian only.
#
# Usage: julia unlink.jl input.so output.o [--prefix=str] [--drop-debug]
#
# Strategy:
#   * Keep all SHF_ALLOC sections except linker/loader-owned ones (dynamic
#     tables, PLT/GOT, crt-derived init/fini) — those are re-synthesized by the
#     next link, and --emit-relocs preserved the *original* relocations against
#     the real symbols, so re-resolution regenerates them.
#   * Convert virtual addresses back to section-relative offsets (symbol
#     values, relocation r_offset).
#   * Rebuild .symtab keeping: section symbols for kept sections, symbols
#     defined in kept sections, referenced UND/ABS symbols.
#   * Emit the retained .rela.* sections re-indexed against the new symtab.

module Unlink

# ---------------------------------------------------------------- ELF structs

const SHT_NULL = 0x0; const SHT_PROGBITS = 0x1; const SHT_SYMTAB = 0x2
const SHT_STRTAB = 0x3; const SHT_RELA = 0x4; const SHT_NOBITS = 0x8
const SHT_INIT_ARRAY = 0xe; const SHT_FINI_ARRAY = 0xf

const SHF_WRITE = 0x1; const SHF_ALLOC = 0x2; const SHF_EXECINSTR = 0x4
const SHF_MERGE = 0x10; const SHF_STRINGS = 0x20; const SHF_INFO_LINK = 0x40
const SHF_TLS = 0x400

const STT_NOTYPE = 0; const STT_OBJECT = 1; const STT_FUNC = 2
const STT_SECTION = 3; const STT_FILE = 4; const STT_TLS = 6; const STT_GNU_IFUNC = 10
const STB_LOCAL = 0; const STB_GLOBAL = 1; const STB_WEAK = 2

const SHN_UNDEF = 0x0000; const SHN_ABS = 0xfff1; const SHN_COMMON = 0xfff2

const ET_REL = 1; const ET_DYN = 3

struct Shdr
    name::UInt32; type::UInt32; flags::UInt64; addr::UInt64; offset::UInt64
    size::UInt64; link::UInt32; info::UInt32; addralign::UInt64; entsize::UInt64
end

struct Sym
    name::UInt32; info::UInt8; other::UInt8; shndx::UInt16; value::UInt64; size::UInt64
end

struct Rela
    offset::UInt64; info::UInt64; addend::Int64
end

rela_sym(r::Rela) = UInt32(r.info >> 32)
rela_type(r::Rela) = UInt32(r.info & 0xffffffff)
make_rela_info(sym::UInt32, typ::UInt32) = (UInt64(sym) << 32) | UInt64(typ)

sym_bind(s::Sym) = s.info >> 4
sym_type(s::Sym) = s.info & 0xf

function read_at(buf::Vector{UInt8}, ::Type{T}, off) where {T}
    return unsafe_load(Ptr{T}(pointer(buf, off + 1)))
end

struct ElfFile
    buf::Vector{UInt8}
    shdrs::Vector{Shdr}
    shnames::Vector{String}
    symtab::Vector{Sym}
    symnames::Vector{String}
    symtab_idx::Int
end

function parse_elf(path::String)
    buf = read(path)
    @assert buf[1:4] == UInt8[0x7f, 0x45, 0x4c, 0x46] "not an ELF file"
    @assert buf[5] == 2 "not ELF64"
    @assert buf[6] == 1 "not little-endian"
    e_shoff = read_at(buf, UInt64, 0x28)
    e_shentsize = read_at(buf, UInt16, 0x3a)
    e_shnum = Int(read_at(buf, UInt16, 0x3c))
    e_shstrndx = Int(read_at(buf, UInt16, 0x3e))
    @assert e_shentsize == 64
    shdrs = Shdr[]
    for i in 0:e_shnum-1
        off = e_shoff + i * 64
        push!(shdrs, Shdr(
            read_at(buf, UInt32, off + 0x00), read_at(buf, UInt32, off + 0x04),
            read_at(buf, UInt64, off + 0x08), read_at(buf, UInt64, off + 0x10),
            read_at(buf, UInt64, off + 0x18), read_at(buf, UInt64, off + 0x20),
            read_at(buf, UInt32, off + 0x28), read_at(buf, UInt32, off + 0x2c),
            read_at(buf, UInt64, off + 0x30), read_at(buf, UInt64, off + 0x38)))
    end
    shstr = shdrs[e_shstrndx + 1]
    getname(strtab::Shdr, nameoff) = begin
        start = Int(strtab.offset) + Int(nameoff) + 1
        stop = start
        while buf[stop] != 0x00; stop += 1; end
        String(buf[start:stop-1])
    end
    shnames = [getname(shstr, sh.name) for sh in shdrs]

    # symtab (static): required for --emit-relocs consumption
    symtab_idx = findfirst(i -> shdrs[i].type == SHT_SYMTAB, eachindex(shdrs))
    @assert symtab_idx !== nothing "no .symtab (stripped?); cannot unlink"
    symsh = shdrs[symtab_idx]
    strsh = shdrs[Int(symsh.link) + 1]
    nsyms = Int(symsh.size ÷ 24)
    symtab = Sym[]
    symnames = String[]
    for i in 0:nsyms-1
        off = symsh.offset + i * 24
        s = Sym(read_at(buf, UInt32, off + 0), read_at(buf, UInt8, off + 4),
                read_at(buf, UInt8, off + 5), read_at(buf, UInt16, off + 6),
                read_at(buf, UInt64, off + 8), read_at(buf, UInt64, off + 16))
        push!(symtab, s)
        push!(symnames, getname(strsh, s.name))
    end
    return ElfFile(buf, shdrs, shnames, symtab, symnames, symtab_idx)
end

function parse_relas(elf::ElfFile, secidx::Int)
    sh = elf.shdrs[secidx]
    n = Int(sh.size ÷ 24)
    relas = Vector{Rela}(undef, n)
    for i in 0:n-1
        off = sh.offset + i * 24
        relas[i+1] = Rela(read_at(elf.buf, UInt64, off + 0),
                          read_at(elf.buf, UInt64, off + 8),
                          read_at(elf.buf, Int64, off + 16))
    end
    return relas
end

# ------------------------------------------------------------- keep policy

# Sections owned by the dynamic linker or the next link's crt/synthesis.
const DROP_EXACT = Set([
    ".interp", ".hash", ".gnu.hash", ".dynsym", ".dynstr", ".dynamic",
    ".rela.dyn", ".rela.plt", ".plt", ".plt.got", ".plt.sec", ".iplt",
    ".got", ".got.plt", ".eh_frame_hdr",
    ".init", ".fini", ".init_array", ".fini_array",
    ".preinit_array", ".relro_padding",
    ".jlsysdata",       # serialized Julia data blob: dead when reusing code only
])
const DROP_PREFIX = [".note", ".gnu.version"]

# crt/linker-provided definitions that the next link will provide again.
# We keep their (dead) carcasses but demote them to local so they cannot
# collide with the fresh definitions.
const LOCALIZE = Set([
    "__TMC_END__", "__dso_handle", "_edata", "_end", "__bss_start",
    "_init", "_fini", "_IO_stdin_used",
])

# Linker-provided symbols that live in dropped (linker-owned) sections; the
# next link defines them again, so convert references into UND references.
const TO_UND = Set(["_GLOBAL_OFFSET_TABLE_", "_DYNAMIC"])

# Donor mode (--prefix given): per-image runtime slots that must bind to the
# NEW image's copies — drop the donor's definitions, reference by name.
const DONOR_TO_UND = Set([
    "jl_small_typeof", "jl_pgcstack_func_slot", "jl_pgcstack_key_slot", "jl_tls_offset",
])

function keep_section(name::String, sh::Shdr; drop_debug::Bool)
    (sh.flags & SHF_ALLOC) == 0 && return false
    (sh.flags & SHF_TLS) != 0 && error("TLS section $name unsupported")
    name in DROP_EXACT && return false
    any(p -> startswith(name, p), DROP_PREFIX) && return false
    return true
end

# -------------------------------------------------------------- .eh_frame
#
# The donor's .eh_frame is the *merged* stream: CIEs/FDEs from real inputs
# (whose pc-begin fields carry --emit-relocs relocations), plus
# linker/crt-synthesized entries (PLT FDEs, crtend's zero terminator) that have
# no relocations and must not survive into a relocatable object. Rebuild the
# section keeping CIEs and only reloc-bearing FDEs, remapping each FDE's CIE
# back-pointer and shifting contained relocation offsets.
function rebuild_eh_frame(data::Vector{UInt8}, relas::Vector{Rela})
    sorted = sort(relas; by = r -> r.offset)
    reloc_offsets = Set{UInt64}(r.offset for r in sorted)
    out = UInt8[]
    sizehint!(out, length(data))
    newoff = Dict{Int,Int}()        # old record offset -> new record offset
    keptrelas = Rela[]
    pos = 0
    ri = 1                          # cursor into sorted relas
    n = length(data)
    while pos < n
        len = read_at(data, UInt32, pos)
        len == 0 && break                       # crtend terminator
        len == 0xffffffff && error("extended-length .eh_frame record unsupported")
        reclen = 4 + Int(len)
        id = read_at(data, UInt32, pos + 4)
        iscie = id == 0
        keep = iscie || (UInt64(pos + 8) in reloc_offsets)
        if keep
            newpos = length(out)
            newoff[pos] = newpos
            append!(out, @view data[pos+1:pos+reclen])
            if !iscie
                # CIE pointer: distance from this field back to the CIE
                cie_old = (pos + 4) - Int(id)
                haskey(newoff, cie_old) || error(".eh_frame FDE references dropped/unseen CIE")
                cie_new_ptr = UInt32((newpos + 4) - newoff[cie_old])
                out[newpos+5:newpos+8] = reinterpret(UInt8, [cie_new_ptr])
            end
            while ri <= length(sorted) && sorted[ri].offset < pos + reclen
                r = sorted[ri]
                r.offset >= pos && push!(keptrelas, Rela(r.offset - pos + newpos, r.info, r.addend))
                ri += 1
            end
        else
            while ri <= length(sorted) && sorted[ri].offset < pos + reclen
                ri += 1
            end
        end
        pos += reclen
    end
    return out, keptrelas
end

# ------------------------------------------------------------------ output

mutable struct StrTab
    data::Vector{UInt8}
    index::Dict{String,UInt32}
end
StrTab() = StrTab(UInt8[0x00], Dict{String,UInt32}())
function intern!(t::StrTab, s::String)
    get!(t.index, s) do
        off = UInt32(length(t.data))
        append!(t.data, codeunits(s)); push!(t.data, 0x00)
        off
    end
end

const R_X86_64_64 = 0x1

function unlink(inpath::String, outpath::String; prefix::String="", drop_debug::Bool=true,
                sidecar::Union{String,Nothing}=nothing, bindfile::Union{String,Nothing}=nothing)
    elf = parse_elf(inpath)
    nsec = length(elf.shdrs)

    # 1. pick sections
    keep = falses(nsec)
    for i in 2:nsec
        keep[i] = keep_section(elf.shnames[i], elf.shdrs[i]; drop_debug)
    end
    # kept old idx -> new idx (1-based over output section list, 0 = NULL)
    newidx = zeros(Int, nsec)
    kept = Int[]
    for i in 2:nsec
        if keep[i]
            push!(kept, i)
            newidx[i] = length(kept)   # slot after NULL
        end
    end

    # 2. relas targeting kept sections (rela.sh_info = target section index)
    relas_for = Dict{Int,Vector{Rela}}()   # old target idx -> relas
    for i in 2:nsec
        sh = elf.shdrs[i]
        sh.type == SHT_RELA || continue
        (sh.flags & SHF_ALLOC) != 0 && continue        # .rela.dyn/.rela.plt
        tgt = Int(sh.info) + 1
        tgt <= nsec || continue
        keep[tgt] || continue
        relas_for[tgt] = parse_relas(elf, i)
    end

    # 3. symbols
    # referenced symbol set
    referenced = falses(length(elf.symtab))
    for (_, rl) in relas_for, r in rl
        s = rela_sym(r)
        s != 0 && (referenced[Int(s)+1] = true)
    end

    strtab = StrTab()
    newsyms = Sym[]                     # locals first, then globals
    newsymnames = String[]
    oldsym2new = zeros(UInt32, length(elf.symtab))
    secsym_of = zeros(UInt32, nsec)     # old section idx -> new sym idx

    push!(newsyms, Sym(0, 0, 0, 0, 0, 0)); push!(newsymnames, "")

    # section symbols for kept sections
    for oi in kept
        push!(newsyms, Sym(0, UInt8((STB_LOCAL << 4) | STT_SECTION), 0,
                           UInt16(newidx[oi]), 0, 0))
        push!(newsymnames, "")
        secsym_of[oi] = UInt32(length(newsyms) - 1)
    end

    problems = String[]
    locals = Tuple{Sym,String,Int}[]    # (sym, name, oldidx)
    globals = Tuple{Sym,String,Int}[]

    # Donor mode: names can repeat across the image's original shard objects
    # (local symbols). Disambiguate with the same rule the image builder uses:
    # any name occurring more than once (over FUNC/OBJECT/IFUNC/NOTYPE symtab
    # entries) gets ".sym<symtab-index>" appended.
    name_count = Dict{String,Int}()
    if !isempty(prefix)
        for (k, s) in enumerate(elf.symtab)
            t = sym_type(s)
            (t == STT_FUNC || t == STT_OBJECT || t == STT_GNU_IFUNC || t == STT_NOTYPE) || continue
            nm = elf.symnames[k]
            isempty(nm) || (name_count[nm] = get(name_count, nm, 0) + 1)
        end
    end
    uniquify(nm::String, symidx::Int) =
        (!isempty(prefix) && get(name_count, nm, 0) > 1) ? nm * ".sym" * string(symidx - 1) : nm
    for (i, s) in enumerate(elf.symtab)
        i == 1 && continue
        st = sym_type(s)
        if st == STT_SECTION
            # remapped via secsym_of on demand
            continue
        elseif st == STT_FILE
            continue
        end
        shndx = Int(s.shndx)
        newname = elf.symnames[i]
        if shndx == SHN_UNDEF || shndx == Int(SHN_ABS) || shndx == Int(SHN_COMMON)
            referenced[i] || continue
            ns = Sym(0, s.info, s.other, s.shndx, s.value, s.size)
            sym_bind(s) == STB_LOCAL ? push!(locals, (ns, newname, i)) :
                                       push!(globals, (ns, newname, i))
        elseif shndx + 1 <= nsec && keep[shndx + 1]
            sec = elf.shdrs[shndx + 1]
            val = s.value - sec.addr
            info = s.info
            other = s.other
            pname = newname
            if !isempty(prefix)
                # donor mode
                if newname in DONOR_TO_UND
                    # drop definition; bind to the consuming image's copy
                    referenced[i] || continue
                    ns = Sym(0, UInt8((STB_GLOBAL << 4) | STT_NOTYPE), 0, UInt16(SHN_UNDEF), 0, 0)
                    push!(globals, (ns, newname, i))
                    continue
                end
                isempty(newname) && continue
                # promote to global-hidden + prefix, so the consuming image can
                # reference this symbol across object files
                info = UInt8((STB_GLOBAL << 4) | sym_type(s))
                other = UInt8(2)   # STV_HIDDEN
                pname = prefix * uniquify(newname, i)
            elseif newname in LOCALIZE
                info = UInt8((STB_LOCAL << 4) | sym_type(s))
            end
            ns = Sym(0, info, other, UInt16(newidx[shndx + 1]), val, s.size)
            sym_bind(Sym(0, info, 0, 0, 0, 0)) == STB_LOCAL ? push!(locals, (ns, pname, i)) :
                                       push!(globals, (ns, pname, i))
        elseif newname in TO_UND
            referenced[i] || continue
            ns = Sym(0, UInt8((STB_GLOBAL << 4) | STT_NOTYPE), 0, UInt16(SHN_UNDEF), 0, 0)
            push!(globals, (ns, newname, i))
        else
            # defined in a dropped section: only a problem if relocs reference it
            if referenced[i]
                push!(problems, "reloc references symbol '$(elf.symnames[i])' in dropped section $(shndx+1 <= nsec ? elf.shnames[shndx+1] : string(shndx))")
            end
        end
    end
    for (ns, name, oldi) in locals
        push!(newsyms, Sym(intern!(strtab, name), ns.info, ns.other, ns.shndx, ns.value, ns.size))
        push!(newsymnames, name)
        oldsym2new[oldi] = UInt32(length(newsyms) - 1)
    end
    first_global = length(newsyms)      # sh_info for symtab
    for (ns, name, oldi) in globals
        push!(newsyms, Sym(intern!(strtab, name), ns.info, ns.other, ns.shndx, ns.value, ns.size))
        push!(newsymnames, name)
        oldsym2new[oldi] = UInt32(length(newsyms) - 1)
    end
    # map old STT_SECTION symbols to new section symbols
    for (i, s) in enumerate(elf.symtab)
        if sym_type(s) == STT_SECTION
            shndx = Int(s.shndx)
            if shndx + 1 <= nsec && keep[shndx + 1]
                oldsym2new[i] = secsym_of[shndx + 1]
            elseif referenced[i]
                push!(problems, "reloc references section symbol of dropped section '$(shndx+1 <= nsec ? elf.shnames[shndx+1] : string(shndx))'")
            end
        end
    end

    if !isempty(problems)
        for p in unique(problems)
            println(stderr, "UNLINK ERROR: ", p)
        end
        error("unresolvable references into dropped sections ($(length(problems)) relocs)")
    end

    # 4. rewrite relocations
    newrelas = Dict{Int,Vector{Rela}}()
    for (tgt, rl) in relas_for
        sec = elf.shdrs[tgt]
        out = Vector{Rela}(undef, length(rl))
        for (k, r) in enumerate(rl)
            oldsym = Int(rela_sym(r))
            ns = oldsym == 0 ? UInt32(0) : oldsym2new[oldsym + 1]
            if oldsym != 0 && ns == 0
                error("reloc in $(elf.shnames[tgt]) references dropped symbol '$(elf.symnames[oldsym+1])'")
            end
            out[k] = Rela(r.offset - sec.addr, make_rela_info(ns, rela_type(r)), r.addend)
        end
        newrelas[tgt] = out
    end

    # 4a. donor mode: apply LABEL / BIND requests from the image builder.
    # LABEL defines a synthesized symbol at a donor VA (used for data slots the
    # consuming image's tables must reference); BIND fills a slot with an ABS64
    # relocation against a target symbol (GOT-style slots whose load-time
    # patching has no meaning outside the donor's own image).
    # ABS64 relocations against the callee symbols (which the consuming link
    # resolves). Slot symbols were promoted+prefixed above.
    if !isempty(prefix) && bindfile !== nothing
        @assert read_at(elf.buf, UInt16, 0x12) == 62 "LABEL/BIND only implemented for x86-64"
        symindex = Dict{String,Int}()               # name -> new sym index (0-based)
        for (k, nm) in enumerate(newsymnames)
            isempty(nm) || get!(symindex, nm, k - 1)
        end
        sec_of_va = function (va::UInt64)
            for oi in kept
                sh = elf.shdrs[oi]
                sh.addr <= va < sh.addr + sh.size && return oi
            end
            error("VA 0x$(string(va, base=16)) not in a kept section")
        end
        nlabels = 0
        nbinds = 0
        # first pass: LABEL definitions (must exist before BINDs reference them)
        for line in eachline(bindfile)
            parts = split(line, '\t')
            length(parts) == 3 && parts[1] == "LABEL" || continue
            name, va = String(parts[2]), parse(UInt64, parts[3], base=16)
            startswith(name, prefix) || continue
            haskey(symindex, name) && error("LABEL $name already defined")
            oi = sec_of_va(va)
            push!(newsyms, Sym(intern!(strtab, name), UInt8((STB_GLOBAL << 4) | STT_OBJECT),
                               UInt8(2) #= hidden =#, UInt16(newidx[oi]),
                               va - elf.shdrs[oi].addr, 8))
            push!(newsymnames, name)
            symindex[name] = length(newsyms) - 1
            nlabels += 1
        end
        for line in eachline(bindfile)
            parts = split(line, '\t')
            length(parts) == 3 && parts[1] == "BIND" || continue
            slotname, target = String(parts[2]), String(parts[3])
            startswith(slotname, prefix) || continue
            si = get(symindex, slotname, nothing)
            si === nothing && (println(stderr, "BIND: unknown slot $slotname"); continue)
            slotsym = newsyms[si + 1]
            ti = get(symindex, target, nothing)
            if ti === nothing
                push!(newsyms, Sym(intern!(strtab, target), UInt8((STB_GLOBAL << 4) | STT_NOTYPE),
                                   0, UInt16(SHN_UNDEF), 0, 0))
                push!(newsymnames, target)
                ti = length(newsyms) - 1
                symindex[target] = ti
            end
            oldsec = kept[Int(slotsym.shndx)]
            push!(get!(newrelas, oldsec, Rela[]),
                  Rela(slotsym.value, make_rela_info(UInt32(ti), UInt32(R_X86_64_64)), 0))
            nbinds += 1
        end
        println("  applied $nlabels LABELs, $nbinds BIND slot fills")
    end

    # 4b. rebuild .eh_frame (drop synthesized FDEs / terminator, remap CIE ptrs)
    replaced_data = Dict{Int,Vector{UInt8}}()
    for oi in kept
        elf.shnames[oi] == ".eh_frame" || continue
        sh = elf.shdrs[oi]
        secdata = elf.buf[Int(sh.offset)+1:Int(sh.offset + sh.size)]
        newdata, keptrelas = rebuild_eh_frame(secdata, get(newrelas, oi, Rela[]))
        replaced_data[oi] = newdata
        newrelas[oi] = keptrelas
        isempty(keptrelas) && delete!(newrelas, oi)
    end

    # 5. write ET_REL object
    shstr = StrTab()
    io = IOBuffer()
    # placeholder ehdr, filled at the end
    write(io, zeros(UInt8, 64))

    # section table entries accumulated as (namestr, Shdr fields..., payload)
    out_shdrs = Any[]
    push!(out_shdrs, ("", Shdr(0, SHT_NULL, 0, 0, 0, 0, 0, 0, 0, 0), UInt8[]))

    alignto(io, a) = begin
        a = max(a, 1)
        while position(io) % a != 0; write(io, 0x00); end
    end

    # kept sections
    sec_file_off = Dict{Int,UInt64}()
    for oi in kept
        sh = elf.shdrs[oi]
        name = elf.shnames[oi]
        if sh.type == SHT_NOBITS && !isempty(prefix)
            # donor mode: materialize .bss as zero-filled PROGBITS so that BIND
            # relocations (ABS64 slot fills) can be applied to it
            alignto(io, Int(sh.addralign))
            off = UInt64(position(io))
            sec_file_off[oi] = off
            write(io, zeros(UInt8, Int(sh.size)))
            push!(out_shdrs, (name, Shdr(0, SHT_PROGBITS, sh.flags, 0, off, sh.size,
                                         0, 0, sh.addralign, sh.entsize), UInt8[]))
        elseif sh.type == SHT_NOBITS
            push!(out_shdrs, (name, Shdr(0, sh.type, sh.flags, 0, 0, sh.size,
                                         0, 0, sh.addralign, sh.entsize), UInt8[]))
        else
            alignto(io, Int(sh.addralign))
            off = UInt64(position(io))
            sec_file_off[oi] = off
            secdata = haskey(replaced_data, oi) ? replaced_data[oi] :
                      @view elf.buf[Int(sh.offset)+1:Int(sh.offset + sh.size)]
            write(io, secdata)
            push!(out_shdrs, (name, Shdr(0, sh.type, sh.flags, 0, off, UInt64(length(secdata)),
                                         0, 0, sh.addralign, sh.entsize), UInt8[]))
        end
    end

    # non-executable stack marker
    push!(out_shdrs, (".note.GNU-stack", Shdr(0, SHT_PROGBITS, 0, 0, UInt64(position(io)), 0, 0, 0, 1, 0), UInt8[]))

    # symtab index: 1(null) + nkept + 1(GNU-stack) + nrela   (0-based)
    nrela = length(newrelas)
    symtab_secidx = 1 + length(kept) + 1 + nrela

    # rela sections
    for oi in kept
        haskey(newrelas, oi) || continue
        rl = newrelas[oi]
        alignto(io, 8)
        off = UInt64(position(io))
        for r in rl
            write(io, r.offset); write(io, r.info); write(io, r.addend)
        end
        push!(out_shdrs, (".rela" * elf.shnames[oi],
                          Shdr(0, SHT_RELA, UInt64(SHF_INFO_LINK), 0, off, UInt64(24 * length(rl)),
                               UInt32(symtab_secidx), UInt32(newidx[oi]), 8, 24), UInt8[]))
    end

    # .symtab
    alignto(io, 8)
    symoff = UInt64(position(io))
    for s in newsyms
        write(io, s.name); write(io, s.info); write(io, s.other)
        write(io, s.shndx); write(io, s.value); write(io, s.size)
    end
    push!(out_shdrs, (".symtab", Shdr(0, SHT_SYMTAB, 0, 0, symoff, UInt64(24 * length(newsyms)),
                                      UInt32(symtab_secidx + 1), UInt32(first_global), 8, 24), UInt8[]))
    # .strtab
    stroff = UInt64(position(io))
    write(io, strtab.data)
    push!(out_shdrs, (".strtab", Shdr(0, SHT_STRTAB, 0, 0, stroff, UInt64(length(strtab.data)), 0, 0, 1, 0), UInt8[]))

    # .shstrtab
    for (name, _, _) in out_shdrs
        isempty(name) || intern!(shstr, name)
    end
    intern!(shstr, ".shstrtab")
    shstroff = UInt64(position(io))
    write(io, shstr.data)
    push!(out_shdrs, (".shstrtab", Shdr(0, SHT_STRTAB, 0, 0, shstroff, UInt64(length(shstr.data)), 0, 0, 1, 0), UInt8[]))

    # section headers
    alignto(io, 8)
    shoff = UInt64(position(io))
    for (name, sh, _) in out_shdrs
        nameoff = isempty(name) ? UInt32(0) : intern!(shstr, name)
        write(io, nameoff); write(io, sh.type); write(io, sh.flags); write(io, sh.addr)
        write(io, sh.offset); write(io, sh.size); write(io, sh.link); write(io, sh.info)
        write(io, sh.addralign); write(io, sh.entsize)
    end

    # ehdr
    data = take!(io)
    machine = read_at(elf.buf, UInt16, 0x12)
    hdr = IOBuffer()
    write(hdr, UInt8[0x7f, 0x45, 0x4c, 0x46, 2, 1, 1, 0], zeros(UInt8, 8))
    write(hdr, UInt16(ET_REL)); write(hdr, machine); write(hdr, UInt32(1))
    write(hdr, UInt64(0)); write(hdr, UInt64(0)); write(hdr, shoff)
    write(hdr, UInt32(0))                                   # e_flags
    write(hdr, UInt16(64)); write(hdr, UInt16(0)); write(hdr, UInt16(0)) # ehsize, phentsize, phnum
    write(hdr, UInt16(64)); write(hdr, UInt16(length(out_shdrs)))
    write(hdr, UInt16(length(out_shdrs) - 1))               # shstrndx = last
    hdrdata = take!(hdr)
    @assert length(hdrdata) == 64
    data[1:64] = hdrdata
    write(outpath, data)

    # sidecar: original VA -> emitted symbol name, for defined func/object syms,
    # so an image builder can reference donor code/slots by symbol.
    if sidecar !== nothing
        open(sidecar, "w") do sio
            for (ns, name, oldi) in locals
                olds = elf.symtab[oldi]
                t = sym_type(olds)
                (t == STT_FUNC || t == STT_OBJECT || t == STT_GNU_IFUNC) || continue
                println(sio, string(olds.value, base=16), '\t', string(olds.size, base=16), '\t',
                        t == STT_FUNC ? 'F' : 'O', '\t', name)
            end
            for (ns, name, oldi) in globals
                olds = elf.symtab[oldi]
                t = sym_type(olds)
                (t == STT_FUNC || t == STT_OBJECT || t == STT_GNU_IFUNC) || continue
                Int(olds.shndx) == SHN_UNDEF && continue
                println(sio, string(olds.value, base=16), '\t', string(olds.size, base=16), '\t',
                        t == STT_FUNC ? 'F' : 'O', '\t', name)
            end
        end
    end

    println("unlink: $(inpath) -> $(outpath)")
    println("  kept $(length(kept)) sections, $(sum(length, values(newrelas); init=0)) relocs in $(nrela) rela sections, $(length(newsyms)) symbols")
    return nothing
end

end # module

function (@main)(argv)
    prefix = ""
    sidecar = nothing
    bindfile = nothing
    args = String[]
    for a in argv
        if startswith(a, "--prefix=")
            prefix = a[10:end]
        elseif startswith(a, "--sidecar=")
            sidecar = a[11:end]
        elseif startswith(a, "--bind=")
            bindfile = a[8:end]
        else
            push!(args, a)
        end
    end
    Unlink.unlink(args[1], args[2]; prefix, sidecar, bindfile)
    return 0
end
