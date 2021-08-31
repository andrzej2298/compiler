target triple = "x86_64-pc-linux-gnu"

%_Array = type { i32, i8* }
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @_concatenate_strings(i8*, i8*)
declare void @_initial_bookkeeping()
declare void @_final_bookkeeping()
declare %_Array* @_allocate_flat_array(i32, i32)
declare i8* @_allocate_object(i32)
declare i32 @_array_length(%_Array*)
declare void @_initialize_string_array(%_Array*, i8*)

define i32 @main() {
  call void @_initial_bookkeeping()
  %main_function_result = call i32 @_main()
  call void @_final_bookkeeping()
  ret i32 %main_function_result
}

@_s3 = private unnamed_addr constant [9 x i8] c"/* world\00"
@_s1 = private unnamed_addr constant [2 x i8] c"=\00"
@_s0 = private unnamed_addr constant [1 x i8] c"\00"
@_s2 = private unnamed_addr constant [9 x i8] c"hello */\00"
define i32 @_main () {
  %_return_value = alloca i32
  %_r0 = call i32 @fac(i32 10)
  call void @printInt(i32 %_r0)
  %_r1 = call i32 @rfac(i32 10)
  call void @printInt(i32 %_r1)
  %_r2 = call i32 @mfac(i32 10)
  call void @printInt(i32 %_r2)
  %_r3 = call i32 @ifac(i32 10)
  call void @printInt(i32 %_r3)
  ; CDecl
  %r$0 = alloca i8*
  %_r4 = bitcast [1 x i8]* @_s0 to i8*
  store i8* %_r4, i8** %r$0
  ; CDecl
  %n$1 = alloca i32
  store i32 10, i32* %n$1
  ; CDecl
  %r$2 = alloca i32
  store i32 1, i32* %r$2
  ; CWhile
  br label %L0
L0:
  %_r6 = load i32, i32* %n$1
  %_r5 = icmp sgt i32 %_r6, 0
  br i1 %_r5 , label %L1 , label %L2
L1:
  ; CAss
  %_r7 = load i32, i32* %r$2
  %_r8 = load i32, i32* %n$1
  %_r9 = mul i32 %_r7, %_r8
  store i32 %_r9, i32* %r$2
  ; CDecr
  ; CAss
  %_r10 = load i32, i32* %n$1
  %_r11 = sub i32 %_r10, 1
  store i32 %_r11, i32* %n$1
  br label %L0
L2:
  %_r12 = load i32, i32* %r$2
  call void @printInt(i32 %_r12)
  %_r13 = bitcast [2 x i8]* @_s1 to i8*
  %_r14 = call i8* @repStr(i8* %_r13, i32 60)
  call void @printString(i8* %_r14)
  %_r15 = bitcast [9 x i8]* @_s2 to i8*
  call void @printString(i8* %_r15)
  %_r16 = bitcast [9 x i8]* @_s3 to i8*
  call void @printString(i8* %_r16)
  ; CRet
  store i32 0, i32* %_return_value
  br label %ReturnLabel
  br label %ReturnLabel
ReturnLabel:
  %_r17 = load i32, i32* %_return_value
  ret i32 %_r17
}

define i32 @fac (i32 %_r0) {
  %_return_value = alloca i32
  %a$0 = alloca i32
  store i32 %_r0, i32* %a$0
  ; CDecl
  %r$1 = alloca i32
  store i32 0, i32* %r$1
  ; CDecl
  %n$2 = alloca i32
  store i32 0, i32* %n$2
  ; CAss
  store i32 1, i32* %r$1
  ; CAss
  %_r1 = load i32, i32* %a$0
  store i32 %_r1, i32* %n$2
  ; CWhile
  br label %L0
L0:
  %_r3 = load i32, i32* %n$2
  %_r2 = icmp sgt i32 %_r3, 0
  br i1 %_r2 , label %L1 , label %L2
L1:
  ; CAss
  %_r4 = load i32, i32* %r$1
  %_r5 = load i32, i32* %n$2
  %_r6 = mul i32 %_r4, %_r5
  store i32 %_r6, i32* %r$1
  ; CAss
  %_r7 = load i32, i32* %n$2
  %_r8 = sub i32 %_r7, 1
  store i32 %_r8, i32* %n$2
  br label %L0
L2:
  ; CRet
  %_r9 = load i32, i32* %r$1
  store i32 %_r9, i32* %_return_value
  br label %ReturnLabel
  br label %ReturnLabel
ReturnLabel:
  %_r10 = load i32, i32* %_return_value
  ret i32 %_r10
}

define i32 @rfac (i32 %_r0) {
  %_return_value = alloca i32
  %n$0 = alloca i32
  store i32 %_r0, i32* %n$0
  ; CCondElse
  %_r2 = load i32, i32* %n$0
  %_r1 = icmp eq i32 %_r2, 0
  br i1 %_r1 , label %L0 , label %L1
L0:
  ; CRet
  store i32 1, i32* %_return_value
  br label %ReturnLabel
  br label %L2
L1:
  ; CRet
  %_r3 = load i32, i32* %n$0
  %_r4 = load i32, i32* %n$0
  %_r5 = sub i32 %_r4, 1
  %_r6 = call i32 @rfac(i32 %_r5)
  %_r7 = mul i32 %_r3, %_r6
  store i32 %_r7, i32* %_return_value
  br label %ReturnLabel
  br label %L2
L2:
  br label %ReturnLabel
ReturnLabel:
  %_r8 = load i32, i32* %_return_value
  ret i32 %_r8
}

define i32 @mfac (i32 %_r0) {
  %_return_value = alloca i32
  %n$0 = alloca i32
  store i32 %_r0, i32* %n$0
  ; CCondElse
  %_r2 = load i32, i32* %n$0
  %_r1 = icmp eq i32 %_r2, 0
  br i1 %_r1 , label %L0 , label %L1
L0:
  ; CRet
  store i32 1, i32* %_return_value
  br label %ReturnLabel
  br label %L2
L1:
  ; CRet
  %_r3 = load i32, i32* %n$0
  %_r4 = load i32, i32* %n$0
  %_r5 = sub i32 %_r4, 1
  %_r6 = call i32 @nfac(i32 %_r5)
  %_r7 = mul i32 %_r3, %_r6
  store i32 %_r7, i32* %_return_value
  br label %ReturnLabel
  br label %L2
L2:
  br label %ReturnLabel
ReturnLabel:
  %_r8 = load i32, i32* %_return_value
  ret i32 %_r8
}

define i32 @nfac (i32 %_r0) {
  %_return_value = alloca i32
  %n$0 = alloca i32
  store i32 %_r0, i32* %n$0
  ; CCondElse
  %_r2 = load i32, i32* %n$0
  %_r1 = icmp ne i32 %_r2, 0
  br i1 %_r1 , label %L0 , label %L1
L0:
  ; CRet
  %_r3 = load i32, i32* %n$0
  %_r4 = sub i32 %_r3, 1
  %_r5 = call i32 @mfac(i32 %_r4)
  %_r6 = load i32, i32* %n$0
  %_r7 = mul i32 %_r5, %_r6
  store i32 %_r7, i32* %_return_value
  br label %ReturnLabel
  br label %L2
L1:
  ; CRet
  store i32 1, i32* %_return_value
  br label %ReturnLabel
  br label %L2
L2:
  br label %ReturnLabel
ReturnLabel:
  %_r8 = load i32, i32* %_return_value
  ret i32 %_r8
}

define i32 @ifac (i32 %_r0) {
  %_return_value = alloca i32
  %n$0 = alloca i32
  store i32 %_r0, i32* %n$0
  ; CRet
  %_r1 = load i32, i32* %n$0
  %_r2 = call i32 @ifac2f(i32 1, i32 %_r1)
  store i32 %_r2, i32* %_return_value
  br label %ReturnLabel
  br label %ReturnLabel
ReturnLabel:
  %_r3 = load i32, i32* %_return_value
  ret i32 %_r3
}

define i32 @ifac2f (i32 %_r0, i32 %_r1) {
  %_return_value = alloca i32
  %l$1 = alloca i32
  store i32 %_r0, i32* %l$1
  %h$0 = alloca i32
  store i32 %_r1, i32* %h$0
  ; CCond
  %_r3 = load i32, i32* %l$1
  %_r4 = load i32, i32* %h$0
  %_r2 = icmp eq i32 %_r3, %_r4
  br i1 %_r2 , label %L0 , label %L1
L0:
  ; CRet
  %_r5 = load i32, i32* %l$1
  store i32 %_r5, i32* %_return_value
  br label %ReturnLabel
  br label %L1
L1:
  ; CCond
  %_r7 = load i32, i32* %l$1
  %_r8 = load i32, i32* %h$0
  %_r6 = icmp sgt i32 %_r7, %_r8
  br i1 %_r6 , label %L2 , label %L3
L2:
  ; CRet
  store i32 1, i32* %_return_value
  br label %ReturnLabel
  br label %L3
L3:
  ; CDecl
  %m$2 = alloca i32
  store i32 0, i32* %m$2
  ; CAss
  %_r9 = load i32, i32* %l$1
  %_r10 = load i32, i32* %h$0
  %_r11 = add i32 %_r9, %_r10
  %_r12 = sdiv i32 %_r11, 2
  store i32 %_r12, i32* %m$2
  ; CRet
  %_r13 = load i32, i32* %l$1
  %_r14 = load i32, i32* %m$2
  %_r15 = call i32 @ifac2f(i32 %_r13, i32 %_r14)
  %_r16 = load i32, i32* %m$2
  %_r17 = add i32 %_r16, 1
  %_r18 = load i32, i32* %h$0
  %_r19 = call i32 @ifac2f(i32 %_r17, i32 %_r18)
  %_r20 = mul i32 %_r15, %_r19
  store i32 %_r20, i32* %_return_value
  br label %ReturnLabel
  br label %ReturnLabel
ReturnLabel:
  %_r21 = load i32, i32* %_return_value
  ret i32 %_r21
}

define i8* @repStr (i8* %_r0, i32 %_r1) {
  %_return_value = alloca i8*
  %s$1 = alloca i8*
  store i8* %_r0, i8** %s$1
  %n$0 = alloca i32
  store i32 %_r1, i32* %n$0
  ; CDecl
  %r$2 = alloca i8*
  %_r2 = bitcast [1 x i8]* @_s0 to i8*
  store i8* %_r2, i8** %r$2
  ; CDecl
  %i$3 = alloca i32
  store i32 0, i32* %i$3
  ; CWhile
  br label %L0
L0:
  %_r4 = load i32, i32* %i$3
  %_r5 = load i32, i32* %n$0
  %_r3 = icmp slt i32 %_r4, %_r5
  br i1 %_r3 , label %L1 , label %L2
L1:
  ; CAss
  %_r6 = load i8*, i8** %r$2
  %_r7 = load i8*, i8** %s$1
  %_r8 = call i8* @_concatenate_strings(i8* %_r6, i8* %_r7)
  store i8* %_r8, i8** %r$2
  ; CIncr
  ; CAss
  %_r9 = load i32, i32* %i$3
  %_r10 = add i32 %_r9, 1
  store i32 %_r10, i32* %i$3
  br label %L0
L2:
  ; CRet
  %_r11 = load i8*, i8** %r$2
  store i8* %_r11, i8** %_return_value
  br label %ReturnLabel
  br label %ReturnLabel
ReturnLabel:
  %_r12 = load i8*, i8** %_return_value
  ret i8* %_r12
}

