# This file is a part of Julia. License is MIT: https://julialang.org/license

using InteractiveUtils, Test

@testset "warntype highlighting" begin
    # Make sure that "expected" unions are highlighted with warning color instead of error color
    io = IOBuffer()
    f(x) = x > 1 ? "foo" : nothing
    c = Base.text_colors[Base.warn_color()]
    InteractiveUtils.highlighting[:warntype] = false
    code_warntype(IOContext(io, :color => true), f, Tuple{Int64})
    @test !occursin(c, String(take!(io)))
    InteractiveUtils.highlighting[:warntype] = true
    code_warntype(IOContext(io, :color => true), f, Tuple{Int64})
    @test occursin(c, String(take!(io)))
end

@testset "no colors" begin
    io = IOBuffer()
    code_llvm(io, sqrt, (Float32,))
    @test !occursin("\e", String(take!(io)))

    InteractiveUtils.print_llvm(io, "; comment")
    @test String(take!(io)) == "; comment\n"

    InteractiveUtils.highlighting[:llvm] = false
    code_llvm(IOContext(io, :color => true), abs, (Float64,))
    @test !occursin("\e", String(take!(io)))
    InteractiveUtils.highlighting[:llvm] = true
    code_llvm(IOContext(io, :color => true), abs, (Float64,))
    @test occursin("\e", String(take!(io)))

    code_native(io, sqrt, (Float32,))
    @test !occursin("\e", String(take!(io)))

    InteractiveUtils.print_native(io, "; comment", :x86)
    @test String(take!(io)) == "; comment\n"

    InteractiveUtils.print_native(io, "; comment", :unknown)
    @test String(take!(io)) == "; comment\n"

    InteractiveUtils.highlighting[:native] = false
    code_native(IOContext(io, :color => true), abs, (Float64,))
    @test !occursin("\e", String(take!(io)))
    InteractiveUtils.highlighting[:native] = true
    code_native(IOContext(io, :color => true), abs, (Float64,))
    @test occursin("\e", String(take!(io)))
end

function hilight_llvm(s)
    io = IOBuffer()
    InteractiveUtils.print_llvm(IOContext(io, :color=>true), s)
    r = String(take!(io))
    get(ENV, "JULIA_DEBUG", "") == "" && return r
    println(stdout, " input: ", s)
    println(stdout, "result: ", r)
    flush(stdout)
    r
end
function hilight_native(s, arch)
    io = IOBuffer()
    InteractiveUtils.print_native(IOContext(io, :color=>true), s, arch)
    r = String(take!(io))
    get(ENV, "JULIA_DEBUG", "") == "" && return r
    println(stdout, " input: ", s)
    println(stdout, "result: ", r)
    flush(stdout)
    r
end
hilight_x86(s) = hilight_native(s, :x86)
hilight_arm(s) = hilight_native(s, :arm)

function esc_code(s)
    io = IOBuffer()
    InteractiveUtils.printstyled_ll(IOContext(io, :color=>true), "!", s)
    split(String(take!(io)), "!")
end

const D, XD = esc_code(:default)
const C, XC = esc_code(:comment)
const L, XL = esc_code(:label)
const I, XI = esc_code(:instruction)
const T, XT = esc_code(:type)
const N, XN = esc_code(:number)
const B, XB = esc_code(:bracket)
const V, XV = esc_code(:variable)
const K, XK = esc_code(:keyword)
const F, XF = esc_code(:funcname)

const COM = D * "," * XD
const COL = D * ":" * XD
const EQU = D * "=" * XD
const P = B * "(" * XB
const XP = B * ")" * XB
const S = B * "[" * XB
const XS = B * "]" * XB
const U = B * "{" * XB
const XU = B * "}" * XB

@testset "LLVM IR" begin
    @testset "comment" begin
        @test hilight_llvm("; comment ; // # ") == "$(C); comment ; // # $(XC)\n"
    end
    @testset "lavel" begin
        @test hilight_llvm("top:") == "$(L)top:$(XL)\n"

        @test hilight_llvm("L7:\t\t; preds = %top") ==
            "$(L)L7:$(XL)\t\t$(C); preds = %top$(XC)\n"
    end
    @testset "define" begin
        @test hilight_llvm("define double @julia_func_1234(float) {") ==
            "$(K)define$(XK) $(T)double$(XT) " *
            "$(F)@julia_func_1234$(XF)$P$(T)float$(XT)$XP $U\n"

        @test hilight_llvm("}") == "$XU\n"
    end

    @testset "declare" begin
        @test hilight_llvm("declare i32 @jl_setjmp(i8*) #2") ==
            "$(K)declare$(XK) $(T)i32$(XT) " *
            "$(F)@jl_setjmp$(XF)$P$(T)i8$(XT)$(D)*$(XD)$XP $(D)#2$(XD)\n"
    end

    @testset "type" begin
        @test hilight_llvm("%jl_value_t = type opaque") ==
            "$(V)%jl_value_t$(XV) $EQU $(K)type$(XK) $(T)opaque$(XT)\n"
    end

    @testset "target" begin
        datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
        @test hilight_llvm("target datalayout = \"$datalayout\"") ==
            "$(K)target$(XK) $(K)datalayout$(XK) $EQU $(V)\"$datalayout\"$(XV)\n"
    end

    @testset "attributes" begin
        @test hilight_llvm(
            """attributes #1 = { uwtable "frame-pointer"="all" "thunk" }""") ==
            "$(K)attributes$(XK) $(D)#1$(XD) $EQU " *
            "$U $(K)uwtable$(XK) $(V)\"frame-pointer\"$(XV)$EQU" *
            "$(V)\"all\"$(XV) $(V)\"thunk\"$(XV) $XU\n"
    end

    @testset "terminator" begin
        @test hilight_llvm("  ret i8 %12") ==
            "  $(I)ret$(XI) $(T)i8$(XT) $(V)%12$(XV)\n"

        @test hilight_llvm("  br i1 %2, label %L6, label %L4") ==
            "  $(I)br$(XI) $(T)i1$(XT) $(V)%2$(XV)$COM " *
            "$(T)label$(XT) $(L)%L6$(XL)$COM $(T)label$(XT) $(L)%L4$(XL)\n"

        @test hilight_llvm("  br label %L5") ==
            "  $(I)br$(XI) $(T)label$(XT) $(L)%L5$(XL)\n"

        @test hilight_llvm("  unreachable") == "  $(I)unreachable$(XI)\n"
    end

    @testset "arithmetic" begin
        @test hilight_llvm("   %11 = add nuw nsw i64 %value_phi10, 1") ==
            "   $(V)%11$(XV) $EQU $(I)add$(XI) $(K)nuw$(XK) $(K)nsw$(XK) " *
            "$(T)i64$(XT) $(V)%value_phi10$(XV)$COM $(N)1$(XN)\n"

        @test hilight_llvm("   %13 = fadd double %12, -2.000000e+00") ==
            "   $(V)%13$(XV) $EQU $(I)fadd$(XI) " *
            "$(T)double$(XT) $(V)%12$(XV)$COM $(N)-2.000000e+00$(XN)\n"

        @test hilight_llvm("      %21 = fmul contract double %20, 0x0123456789ABCDEF") ==
            "      $(V)%21$(XV) $EQU $(I)fmul$(XI) $(K)contract$(XK) " *
            "$(T)double$(XT) $(V)%20$(XV)$COM $(N)0x0123456789ABCDEF$(XN)\n"
    end

    @testset "bitwise" begin
        @test hilight_llvm("   %31 = shl i64 %value_phi4, 52") ==
            "   $(V)%31$(XV) $EQU " *
            "$(I)shl$(XI) $(T)i64$(XT) $(V)%value_phi4$(XV)$COM $(N)52$(XN)\n"
    end

    @testset "aggregate" begin
        @test hilight_llvm("    %4 = extractvalue { i64, i1 } %1, 0") ==
            "    $(V)%4$(XV) $EQU $(I)extractvalue$(XI) " *
            "$U $(T)i64$(XT)$COM $(T)i1$(XT) $XU $(V)%1$(XV)$COM $(N)0$(XN)\n"
    end

    @testset "memory access" begin
        @test hilight_llvm("  %dims = alloca [1 x i64], align 8") ==
            "  $(V)%dims$(XV) $EQU $(I)alloca$(XI) " *
            "$S$(N)1$(XN) $(D)x$(XD) $(T)i64$(XT)$XS$COM $(K)align$(XK) $(N)8$(XN)\n"

        @test hilight_llvm("    %51 = load i32," *
                           " i32* inttoptr (i64 226995504 to i32*), align 16") ==
            "    $(V)%51$(XV) $EQU $(I)load$(XI) $(T)i32$(XT)$COM " *
            "$(T)i32$(XT)$(D)*$(XD) $(K)inttoptr$(XK) $P$(T)i64$(XT) $(N)226995504$(XN) " *
            "$(K)to$(XK) $(T)i32$(XT)$(D)*$(XD)$XP$COM $(K)align$(XK) $(N)16$(XN)\n"

        @test hilight_llvm("    %53 = load %jl_value_t addrspace(10)*, " *
                           "%jl_value_t addrspace(10)* addrspace(11)* %52, align 8") ==
            "    $(V)%53$(XV) $EQU $(I)load$(XI) $(V)%jl_value_t$(XV) " *
            "$(K)addrspace$(XK)$P$(N)10$(XN)$XP$(D)*$(XD)$COM " *
            "$(V)%jl_value_t$(XV) $(K)addrspace$(XK)$P$(N)10$(XN)$XP$(D)*$(XD) " *
            "$(K)addrspace$(XK)$P$(N)11$(XN)$XP$(D)*$(XD) " *
            "$(V)%52$(XV)$COM $(K)align$(XK) $(N)8$(XN)\n"

        @test hilight_llvm("    store i64 %61, i64 addrspace(11)* %60, align 8") ==
            "    $(I)store$(XI) $(T)i64$(XT) $(V)%61$(XV)$COM " *
            "$(T)i64$(XT) $(K)addrspace$(XK)$P$(N)11$(XN)$XP$(D)*$(XD) " *
            "$(V)%60$(XV)$COM $(K)align$(XK) $(N)8$(XN)\n"

        @test hilight_llvm("  store volatile %jl_value_t addrspace(10)** %62, " *
                           "%jl_value_t addrspace(10)*** %63, align 8") ==
            "  $(I)store$(XI) $(K)volatile$(XK) $(V)%jl_value_t$(XV) " *
            "$(K)addrspace$(XK)$P$(N)10$(XN)$XP$(D)**$(XD) $(V)%62$(XV)$COM " *
            "$(V)%jl_value_t$(XV) $(K)addrspace$(XK)$P$(N)10$(XN)$XP$(D)***$(XD) " *
            "$(V)%63$(XV)$COM $(K)align$(XK) $(N)8$(XN)\n"

        @test hilight_llvm("     %71 = getelementptr i8, i8* %70, i64 8") ==
            "     $(V)%71$(XV) $EQU $(I)getelementptr$(XI) $(T)i8$(XT)$COM " *
            "$(T)i8$(XT)$(D)*$(XD) $(V)%70$(XV)$COM $(T)i64$(XT) $(N)8$(XN)\n"
    end

    @testset "conversion" begin
        @test hilight_llvm("  %22 = zext i1 %21 to i8") ==
            "  $(V)%22$(XV) $EQU $(I)zext$(XI) $(T)i1$(XT) $(V)%21$(XV) " *
            "$(K)to$(XK) $(T)i8$(XT)\n"

        @test hilight_llvm("     %24 = sitofp i64 %23 to double") ==
            "     $(V)%24$(XV) $EQU $(I)sitofp$(XI) $(T)i64$(XT) $(V)%23$(XV) " *
            "$(K)to$(XK) $(T)double$(XT)\n"

        @test hilight_llvm("  %26 = ptrtoint i8* %25 to i64") ==
            "  $(V)%26$(XV) $EQU $(I)ptrtoint$(XI) $(T)i8$(XT)$(D)*$(XD) " *
            "$(V)%25$(XV) $(K)to$(XK) $(T)i64$(XT)\n"

        @test hilight_llvm("  %28 = bitcast %jl_value_t addrspace(10)* %27 " *
                           "to [2 x i16] addrspace(10)*") ==
            "  $(V)%28$(XV) $EQU $(I)bitcast$(XI) $(V)%jl_value_t$(XV) " *
            "$(K)addrspace$(XK)$P$(N)10$(XN)$XP$(D)*$(XD) $(V)%27$(XV) " *
            "$(K)to$(XK) $S$(N)2$(XN) $(D)x$(XD) $(T)i16$(XD)$XS " *
            "$(K)addrspace$(XK)$P$(N)10$(XN)$XP$(D)*$(XD)\n"
    end

    @testset "other" begin
        @test hilight_llvm("  %31 = icmp slt i64 %30, 0") ==
            "  $(V)%31$(XV) $EQU $(I)icmp$(XI) $(I)slt$(XI) " *
            "$(T)i64$(XT) $(V)%30$(XV)$COM $(N)0$(XN)\n"

        @test hilight_llvm("  %value_phi34 = phi double [ %33, %L50 ], [ %32, %L60 ]") ==
            "  $(V)%value_phi34$(XV) $EQU $(I)phi$(XI) $(T)double$(XT) " *
            "$S $(V)%33$(XV)$COM $(L)%L50$(XL) $XS$COM " *
            "$S $(V)%32$(XV)$COM $(L)%L60$(XL) $XS\n"

        @test hilight_llvm("   %.v = select i1 %35, i64 %36, i64 63") ==
            "   $(V)%.v$(XV) $EQU $(I)select$(XI) $(T)i1$(XT) $(V)%35$(XV)$COM " *
            "$(T)i64$(XT) $(V)%36$(XV)$COM $(T)i64$(XT) $(N)63$(XN)\n"

        @test hilight_llvm("   %38 = call i64 @llvm.cttz.i64(i64 %37, i1 false)") ==
            "   $(V)%38$(XV) $EQU $(I)call$(XI) $(T)i64$(XT) " *
            "$(F)@llvm.cttz.i64$(XF)$P$(T)i64$(XT) $(V)%37$(XV)$COM " *
            "$(T)i1$(XT) $(K)false$(XK)$XP\n"
    end
end

@testset "x86 ASM" begin
    @testset "comment" begin
        @test hilight_x86("; comment ; // # ") == "$(C); comment ; // # $(XC)\n"
    end
    @testset "label" begin
        @test hilight_x86("L123:") == "$(L)L123:$(XL)\n"
    end
    @testset "directive" begin
        @test hilight_x86("\t.text") == "\t$(D).text$(XD)\n"
    end

    @testset "0-operand" begin
        # AT&T
        @test hilight_x86("\tretq") == "\t$(I)retq$(XI)\n"

        # Intel
        @test hilight_x86("\tret") == "\t$(I)ret$(XI)\n"
    end
    @testset "1-operand" begin
        # AT&T
        @test hilight_x86("\tpopq\t%rax") == "\t$(I)popq$(XI)\t$(V)%rax$(XV)\n"

        @test hilight_x86("\tpushl\t\$4294967295\t# imm = 0xFFFFFFFF") ==
            "\t$(I)pushl$(XI)\t$(N)\$4294967295$(XN)\t$(C)# imm = 0xFFFFFFFF$(XC)\n"

        @test hilight_x86("\tja\tL234") == "\t$(I)ja$(XI)\t$(L)L234$(XL)\n"

        @test hilight_x86("\tnopw\t%cs:(%rax,%rax)") ==
            "\t$(I)nopw$(XI)\t$(V)%cs$(XV)$COL$P$(V)%rax$(XV)$COM$(V)%rax$(XV)$XP\n"

        # Intel
        @test hilight_x86("\tpop\trax") == "\t$(I)pop$(XI)\t$(V)rax$(XV)\n"

        @test hilight_x86("\tpush\t4294967295") ==
            "\t$(I)push$(XI)\t$(N)4294967295$(XN)\n"

        @test hilight_x86("\tja\tL234") == "\t$(I)ja$(XI)\t$(L)L234$(XL)\n"

        @test hilight_x86("\tnop\tword ptr cs:[rax + rax]") ==
            "\t$(I)nop$(XI)\t$(K)word$(XK) $(K)ptr$(XK) " *
            "$(V)cs$(XV)$COL$S$(V)rax$(XV) $(D)+$(XD) $(V)rax$(XV)$XS\n"
    end
    @testset "2-operand" begin
        # AT&T
        @test hilight_x86("\tshrq\t\$63, %rcx") ==
            "\t$(I)shrq$(XI)\t$(N)\$63$(XN)$COM $(V)%rcx$(XV)\n"

        @test hilight_x86("\tvmovsd\t(%rsi,%rdx,8), %xmm1\t# xmm1 = mem[0],zero") ==
            "\t$(I)vmovsd$(XI)\t$P$(V)%rsi$(XV)$COM$(V)%rdx$(XV)$COM$(N)8$(XN)$XP" *
            "$COM $(V)%xmm1$(XV)\t$(C)# xmm1 = mem[0],zero$(XC)\n"

        @test hilight_x86("\tmovabsq\t\$\"#string#338\", %rax") ==
            "\t$(I)movabsq$(XI)\t$(F)\$\"#string#338\"$(XF)$COM $(V)%rax$(XV)\n"

        # Intel
        @test hilight_x86("\tshr\trcx, 63") ==
            "\t$(I)shr$(XI)\t$(V)rcx$(XV)$COM $(N)63$(XN)\n"

        @test hilight_x86(
            "\tvmovsd\txmm1, dword ptr [rsi + 8*rdx]\t# xmm1 = mem[0],zero") ==
            "\t$(I)vmovsd$(XI)\t$(V)xmm1$(XV)$COM $(K)dword$(XK) $(K)ptr$(XK) " *
            "$S$(V)rsi$(XV) $(D)+$(XD) $(N)8$(XN)$(D)*$(XD)$(V)rdx$(XV)$XS" *
            "\t$(C)# xmm1 = mem[0],zero$(XC)\n"

        @test hilight_x86("\tmovabs\trax, offset \"#string#338\"") ==
            "\t$(I)movabs$(XI)\t$(V)rax$(XV)$COM " *
            "$(K)offset$(XK) $(F)\"#string#338\"$(XF)\n"
    end
    @testset "3-operand" begin
        # AT&T
        @test hilight_x86("\tvaddsd\t(%rax), %xmm0, %xmm0") ==
            "\t$(I)vaddsd$(XI)\t$P$(V)%rax$(XV)$XP$COM " *
            "$(V)%xmm0$(XV)$COM $(V)%xmm0$(XV)\n"

        # Intel
        @test hilight_x86("\tvaddsd\txmm0, xmm0, qword ptr [rax]") ==
            "\t$(I)vaddsd$(XI)\t$(V)xmm0$(XV)$COM $(V)xmm0$(XV)$COM " *
            "$(K)qword$(XK) $(K)ptr$(XK) $S$(V)rax$(XV)$XS\n"
    end
    @testset "4-operand" begin
        # AT&T
        @test hilight_x86("\tvroundsd\t\$4, %xmm1, %xmm1, %xmm1") ==
            "\t$(I)vroundsd$(XI)\t$(N)\$4$(XN)$COM " *
            "$(V)%xmm1$(XV)$COM $(V)%xmm1$(XV)$COM $(V)%xmm1$(XV)\n"

        # Intel
        @test hilight_x86("\tvroundsd\txmm1, xmm1, xmm1, 4") ==
            "\t$(I)vroundsd$(XI)\t" *
            "$(V)xmm1$(XV)$COM $(V)xmm1$(XV)$COM $(V)xmm1$(XV)$COM $(N)4$(XN)\n"
    end
    @testset "AVX-512" begin
        # AT&T
        @test hilight_x86("\tvmovaps\t(%eax), %zmm0") ==
            "\t$(I)vmovaps$(XI)\t$P$(V)%eax$(XV)$XP$COM $(V)%zmm0$(XV)\n"

        @test hilight_x86("\tvpaddd\t%zmm3, %zmm1, %zmm1 {%k1}") ==
            "\t$(I)vpaddd$(XI)\t$(V)%zmm3$(XV)$COM $(V)%zmm1$(XV)$COM " *
            "$(V)%zmm1$(XV) $U$(V)%k1$(XV)$XU\n"

        @test hilight_x86("\tvdivpd\t%zmm3, %zmm1, %zmm0 {%k1} {z}") ==
            "\t$(I)vdivpd$(XI)\t$(V)%zmm3$(XV)$COM $(V)%zmm1$(XV)$COM " *
            "$(V)%zmm0$(XV) $U$(V)%k1$(XV)$XU $U$(K)z$(XK)$XU\n"

        @test hilight_x86("\tvdivps\t(%ebx){1to16}, %zmm5, %zmm4") ==
            "\t$(I)vdivps$(XI)\t$P$(V)%ebx$(XV)$XP$U$(K)1to16$(XK)$XU$COM " *
            "$(V)%zmm5$(XV)$COM $(V)%zmm4$(XV)\n"

        @test hilight_x86("\tvcvtsd2si\t{rn-sae}, %xmm0, %eax") ==
            "\t$(I)vcvtsd2si$(XI)\t$U$(K)rn-sae$(XK)$XU$COM " *
            "$(V)%xmm0$(XV)$COM $(V)%eax$(XV)\n"

        # Intel
        @test hilight_x86("\tvmovaps\tzmm0, zmmword ptr [eax]") ==
            "\t$(I)vmovaps$(XI)\t$(V)zmm0$(XV)$COM " *
            "$(K)zmmword$(XK) $(K)ptr$(XK) $S$(V)eax$(XV)$XS\n"

        @test hilight_x86("\tvpaddd\tzmm1 {k1}, zmm1, zmm3") ==
            "\t$(I)vpaddd$(XI)\t$(V)zmm1$(XV) $U$(V)k1$(XV)$XU$COM " *
            "$(V)zmm1$(XV)$COM $(V)zmm3$(XV)\n"

        @test hilight_x86("\tvdivpd\tzmm0 {k1} {z}, zmm1, zmm3") ==
            "\t$(I)vdivpd$(XI)\t$(V)zmm0$(XV) $U$(V)k1$(XV)$XU $U$(K)z$(XK)$XU$COM " *
            "$(V)zmm1$(XV)$COM $(V)zmm3$(XV)\n"

        @test hilight_x86("\tvdivps\tzmm4, zmm5, dword ptr [ebx]{1to16}") ==
            "\t$(I)vdivps$(XI)\t$(V)zmm4$(XV)$COM $(V)zmm5$(XV)$COM " *
            "$(K)dword$(XK) $(K)ptr$(XK) $S$(V)ebx$(XV)$XS$U$(K)1to16$(XK)$XU\n"

        @test hilight_x86("\tvcvtsd2si\teax, xmm0$(XV), {rn-sae}") ==
            "\t$(I)vcvtsd2si$(XI)\t$(V)eax$(XV)$COM " *
            "$(V)xmm0$(XV)$COM $U$(K)rn-sae$(XK)$XU\n"
    end
end

@testset "ARM ASM" begin
    @testset "comment" begin
        @test hilight_arm("; comment ; // # ") == "$(C); comment ; // # $(XC)\n"
    end
    @testset "label" begin
        @test hilight_arm("L45:") == "$(L)L45:$(XL)\n"
    end
    @testset "directive" begin
        @test hilight_arm("\t.text") == "\t$(D).text$(XD)\n"
    end

    @testset "0-operand" begin
        @test hilight_arm("\tret") == "\t$(I)ret$(XI)\n"
    end
    @testset "1-operand" begin
        @test hilight_arm("\tbl\t0x12") == "\t$(I)bl$(XI)\t$(N)0x12$(XN)\n"

        @test hilight_arm("\tb\tL345") == "\t$(I)b$(XI)\t$(L)L345$(XL)\n"

        @test hilight_arm("\tb.gt\tL67") == "\t$(I)b.gt$(XI)\t$(L)L67$(XL)\n"

        @test hilight_arm("\tpop\t{r11, pc}") ==
            "\t$(I)pop$(XI)\t$U$(V)r11$(XV)$COM $(V)pc$(XV)$XU\n"
    end
    @testset "2-operand" begin
        @test hilight_arm("\tcmp\tx10, #2047\t// =2047") ==
            "\t$(I)cmp$(XI)\t$(V)x10$(XV)$COM $(N)#2047$(XN)\t$(C)// =2047$(XC)\n"

        @test hilight_arm("\tldr\td1, [x10]") ==
            "\t$(I)ldr$(XI)\t$(V)d1$(XV)$COM $S$(V)x10$(XV)$XS\n"

        @test hilight_arm("\tstr\tx30, [sp, #-16]!") ==
            "\t$(I)str$(XI)\t$(V)x30$(XV)$COM " *
            "$S$(V)sp$(XV)$COM $(N)#-16$(XN)$XS$(K)!$(XK)\n"

        @test hilight_arm("\tmov\tv0.16b, v1.16b") ==
            "\t$(I)mov$(XI)\t$(V)v0.16b$(XV)$COM $(V)v1.16b$(XV)\n"
    end
    @testset "3-operand" begin
        @test hilight_arm("\tfmul\td2, d0, d2") ==
            "\t$(I)fmul$(XI)\t$(V)d2$(XV)$COM $(V)d0$(XV)$COM $(V)d2$(XV)\n"

        @test hilight_arm("\tmovk\tx10, #65535, lsl #32") ==
            "\t$(I)movk$(XI)\t$(V)x10$COM $(N)#65535$(XN)$COM $(K)lsl$(XK) $(N)#32$(XN)\n"

        @test hilight_arm("\tcneg\tx8, x8, ge") ==
            "\t$(I)cneg$(XI)\t$(V)x8$(XV)$COM $(V)x8$(XV)$COM $(K)ge$(XK)\n"
    end
    @testset "4-operand" begin
        @test hilight_arm("\tadd\tx8, x9, x8, lsl #52") ==
            "\t$(I)add$(XI)\t$(V)x8$(XV)$COM $(V)x9$(XV)$COM $(V)x8$(XV)$COM " *
            "$(K)lsl$(XK) $(N)#52$(XN)\n"

        @test hilight_arm("\tfcsel\td1, d0, d1, eq") ==
            "\t$(I)fcsel$(XI)\t" *
            "$(V)d1$(XV)$COM $(V)d0$(XV)$COM $(V)d1$(XV)$COM $(K)eq$(XK)\n"
    end
    @testset "NEON" begin
        hilight_arm("\tvmul.f32\tq8, q9, q8") ==
            "\t$(I)vmul.f32$(XI)\t$(V)q8$(XV)$COM $(V)q9$(XV)$COM $(V)q8$(XV)\n"
        hilight_arm("\tvcvt.s32.f64\ts2, d20") ==
            "\t$(I)vcvt.s32.f64$(XI)\t$(V)s2$(XV)$COM $(V)d20$(XV)\n"
        hilight_arm("\tvld1.32\t{d18, d19}, [r1]") ==
            "\t$(I)vld1.32$(XI)\t$U$(V)d18$(XV)$COM $(V)d19$(XV)$XU$COM $S$(V)r1$(XV)$XS\n"
    end
    @testset "SVE" begin
        hilight_arm("\tld1d\tz1.d, p0/z, [x0, x4, lsl #3]") ==
            "\t$(I)ld1d$(XI)\t$(V)z1.d$(XV)$COM " *
            "$(V)p0$(XV)$(K)/z$(XK)$COM " *
            "$S$(V)x0$(XV)$COM $(V)x4$(XV)$COM $(K)lsl$(XK) $(N)#3$(XN)$XS\n"
        hilight_arm("\tb.first\tL123") == "\t$(I)b.first$(XI)\t$(L)L123$(XL)"
    end
end
