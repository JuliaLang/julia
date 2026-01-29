; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test that chains of dependent instructions are all sunk together

define i64 @test_sink_chain(i64 %a, i64 %b, i64 %c, i64 %bound) {
; CHECK-LABEL: @test_sink_chain
entry:
  ; All these computations form a chain and should be sunk together
  ; CHECK-NOT: %sum = add i64 %a, %b
  ; CHECK-NOT: %prod = mul i64 %sum, %c
  ; CHECK-NOT: %final = sub i64 %prod, 1
  %sum = add i64 %a, %b
  %prod = mul i64 %sum, %c
  %final = sub i64 %prod, 1

  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  ; CHECK: error:
  ; CHECK-DAG: %sum = add i64 %a, %b
  ; CHECK-DAG: %prod = mul i64 %sum, %c
  ; CHECK-DAG: %final = sub i64 %prod, 1
  call void @use(i64 %final)
  unreachable

ok:
  ret i64 %b
}

; Test partial chain - only part of the chain is used in error path

define i64 @test_sink_partial_chain(i64 %a, i64 %b, i64 %c, i64 %bound) {
; CHECK-LABEL: @test_sink_partial_chain
entry:
  ; %sum is used in both paths, so it stays
  ; CHECK: %sum = add i64 %a, %b
  %sum = add i64 %a, %b

  ; %prod is only used in error path, so it should be sunk
  ; CHECK-NOT: %prod = mul i64 %sum, %c
  %prod = mul i64 %sum, %c

  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  ; CHECK: error:
  ; CHECK: %prod = mul i64 %sum, %c
  call void @use(i64 %prod)
  unreachable

ok:
  ; %sum is used here too
  ret i64 %sum
}

; Test arithmetic + GEP + store chain - all sunk together

define i64 @test_sink_arith_gep_store_chain(i64 %a, i64 %b, i64 %bound) {
; CHECK-LABEL: @test_sink_arith_gep_store_chain
entry:
  %tuple = alloca [4 x i64], align 8
  ; Arithmetic and GEPs feeding stores should all sink together
  ; CHECK: entry:
  ; CHECK-NEXT: %tuple = alloca [4 x i64]
  ; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
  ; CHECK-NEXT: br i1 %cmp
  %sum = add i64 %a, %b
  %gep1 = getelementptr [4 x i64], ptr %tuple, i64 0, i64 0
  store i64 %sum, ptr %gep1, align 8
  %gep2 = getelementptr [4 x i64], ptr %tuple, i64 0, i64 1
  store i64 %b, ptr %gep2, align 8

  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  ; CHECK: error:
  ; CHECK-DAG: %sum = add i64 %a, %b
  ; CHECK-DAG: %gep1 = getelementptr
  ; CHECK-DAG: store i64 %sum, ptr %gep1
  ; CHECK-DAG: %gep2 = getelementptr
  ; CHECK-DAG: store i64 %b, ptr %gep2
  ; CHECK: call void @throw
  call void @throw(ptr %tuple)
  unreachable

ok:
  ret i64 %a
}

declare void @use(i64)
declare void @throw(ptr)
