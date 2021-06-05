; ModuleID = 'basic'


 


%wasmc.tbl = type [2 x i64]


@wasmc.tbl =    global %wasmc.tbl [i64 0, i64 0]


@wasmc.min_mem_size =    constant i64 64000


@wasmc.linear_mem =    global i64 0


declare external ccc  void @free(i8*)    


declare external ccc  float @llvm.ceil.f32(float)    


declare external ccc  i32 @llvm.ctlz.i32(i32, i1)    


declare external ccc  i32 @llvm.ctpop.i32(i32)    


declare external ccc  i32 @llvm.cttz.i32(i32, i1)    


declare external ccc  float @llvm.fabs.f32(float)    


declare external ccc  float @llvm.floor.f32(float)    


declare external ccc  float @llvm.sqrt.f32(float)    


declare external ccc  float @llvm.trunc.f32(float)    


declare external ccc  i8* @malloc(i64)    


define external ccc  i32 @func0(i32  %arg0, i32  %arg1, i32  %arg2, i32  %arg3, i32  %arg4)    {
block0:
  %local0 = alloca i32 
  store  i32 %arg0, i32* %local0 
  %local1 = alloca i32 
  store  i32 %arg1, i32* %local1 
  %local2 = alloca i32 
  store  i32 %arg2, i32* %local2 
  %local3 = alloca i32 
  store  i32 %arg3, i32* %local3 
  %local4 = alloca i32 
  store  i32 %arg4, i32* %local4 
  %tmp0 = load  i32, i32* %local0 
  %tmp1 = load  i32, i32* %local1 
  %tmp2 = add   i32 %tmp0, %tmp1 
  %tmp3 = load  i32, i32* %local2 
  %tmp4 = sub   i32 %tmp2, %tmp3 
  %tmp5 = load  i32, i32* %local3 
  %tmp6 = mul   i32 %tmp4, %tmp5 
  %tmp7 = load  i32, i32* %local4 
  %tmp8 = udiv  i32 %tmp6, %tmp7 
  ret i32 %tmp8 
}


define external ccc  i32 @func1(i32  %arg0, i32  %arg1)    {
block0:
  %local0 = alloca i32 
  store  i32 %arg0, i32* %local0 
  %local1 = alloca i32 
  store  i32 %arg1, i32* %local1 
  %tmp0 = load  i32, i32* %local0 
  %tmp1 = load  i32, i32* %local1 
  %tmp2 = and i32 %tmp0, %tmp1 
  %tmp3 = load  i32, i32* %local0 
  %tmp4 = load  i32, i32* %local1 
  %tmp5 = or i32 %tmp3, %tmp4 
  %tmp6 = add   i32 %tmp2, %tmp5 
  %tmp7 = load  i32, i32* %local0 
  %tmp8 = load  i32, i32* %local0 
  %tmp9 = and i32 %tmp7, %tmp8 
  %tmp10 = add   i32 %tmp6, %tmp9 
  %tmp11 = load  i32, i32* %local1 
  %tmp12 = load  i32, i32* %local1 
  %tmp13 = or i32 %tmp11, %tmp12 
  %tmp14 = add   i32 %tmp10, %tmp13 
  %tmp15 = load  i32, i32* %local0 
  %tmp16 = load  i32, i32* %local1 
  %tmp17 = xor i32 %tmp15, %tmp16 
  %tmp18 = add   i32 %tmp14, %tmp17 
  %tmp19 = load  i32, i32* %local0 
  %tmp20 = load  i32, i32* %local0 
  %tmp21 = xor i32 %tmp19, %tmp20 
  %tmp22 = add   i32 %tmp18, %tmp21 
  %tmp23 = load  i32, i32* %local0 
  %tmp24 = load  i32, i32* %local0 
  %tmp25 = shl   i32 %tmp23, %tmp24 
  %tmp26 = add   i32 %tmp22, %tmp25 
  ret i32 %tmp26 
}


define external ccc  float @func2(float  %arg0)    {
block0:
  %local0 = alloca float 
  store  float %arg0, float* %local0 
  %tmp0 = load  float, float* %local0 
  %tmp1 =  call ccc  float  @llvm.fabs.f32(float  %tmp0)  
  ret float %tmp1 
}


define external ccc  float @func3(float  %arg0)    {
block0:
  %local0 = alloca float 
  store  float %arg0, float* %local0 
  %tmp0 = load  float, float* %local0 
  %tmp1 = fmul float %tmp0, -1.000000e0 
  ret float %tmp1 
}


define external ccc  float @func4(float  %arg0)    {
block0:
  %local0 = alloca float 
  store  float %arg0, float* %local0 
  %tmp0 = load  float, float* %local0 
  %tmp1 =  call ccc  float  @llvm.sqrt.f32(float  %tmp0)  
  ret float %tmp1 
}


define external ccc  float @func5(float  %arg0, float  %arg1, float  %arg2)    {
block0:
  %local0 = alloca float 
  store  float %arg0, float* %local0 
  %local1 = alloca float 
  store  float %arg1, float* %local1 
  %local2 = alloca float 
  store  float %arg2, float* %local2 
  %tmp0 = load  float, float* %local0 
  %tmp1 =  call ccc  float  @llvm.fabs.f32(float  %tmp0)  
  %tmp2 = load  float, float* %local1 
  %tmp3 = fmul float %tmp2, -1.000000e0 
  %tmp4 = fadd float %tmp1, %tmp3 
  %tmp5 = load  float, float* %local2 
  %tmp6 =  call ccc  float  @llvm.sqrt.f32(float  %tmp5)  
  %tmp7 = fadd float %tmp4, %tmp6 
  ret float %tmp7 
}


define external ccc  float @func6(float  %arg0)    {
block0:
  %local0 = alloca float 
  store  float %arg0, float* %local0 
  %tmp0 = load  float, float* %local0 
  %tmp1 =  call ccc  float  @llvm.ceil.f32(float  %tmp0)  
  ret float %tmp1 
}


define external ccc  float @func7(float  %arg0)    {
block0:
  %local0 = alloca float 
  store  float %arg0, float* %local0 
  %tmp0 = load  float, float* %local0 
  %tmp1 =  call ccc  float  @llvm.floor.f32(float  %tmp0)  
  ret float %tmp1 
}


define external ccc  float @func8(float  %arg0)    {
block0:
  %local0 = alloca float 
  store  float %arg0, float* %local0 
  %tmp0 = load  float, float* %local0 
  %tmp1 =  call ccc  float  @llvm.trunc.f32(float  %tmp0)  
  ret float %tmp1 
}


define external ccc  float @func9(float  %arg0)    {
block0:
  %local0 = alloca float 
  store  float %arg0, float* %local0 
  %tmp0 = load  float, float* %local0 
  %tmp1 =  call ccc  float  @llvm.floor.f32(float  %tmp0)  
  ret float %tmp1 
}


define external ccc  float @func10(float  %arg0, float  %arg1, float  %arg2, float  %arg3)    {
block0:
  %local0 = alloca float 
  store  float %arg0, float* %local0 
  %local1 = alloca float 
  store  float %arg1, float* %local1 
  %local2 = alloca float 
  store  float %arg2, float* %local2 
  %local3 = alloca float 
  store  float %arg3, float* %local3 
  %tmp0 = load  float, float* %local0 
  %tmp1 =  call ccc  float  @llvm.ceil.f32(float  %tmp0)  
  %tmp2 = load  float, float* %local1 
  %tmp3 =  call ccc  float  @llvm.floor.f32(float  %tmp2)  
  %tmp4 = fadd float %tmp1, %tmp3 
  %tmp5 = load  float, float* %local2 
  %tmp6 =  call ccc  float  @llvm.trunc.f32(float  %tmp5)  
  %tmp7 = fadd float %tmp4, %tmp6 
  %tmp8 = load  float, float* %local3 
  %tmp9 =  call ccc  float  @llvm.ceil.f32(float  %tmp8)  
  %tmp10 = fadd float %tmp7, %tmp9 
  ret float %tmp10 
}


define external ccc  i32 @func11()    {
block0:
  ret i32 666 
}


define external ccc  i32 @func12()    {
block0:
  %tmp0 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 0 
  %tmp1 = load  i64, i64* %tmp0 
  %tmp2 = inttoptr i64 %tmp1 to i32 ()* 
  %tmp3 =  call ccc  i32  %tmp2()  
  %tmp4 = add   i32 %tmp3, 999 
  ret i32 %tmp4 
}


define external ccc  i32 @func13(i32  %arg0)    {
block0:
  %local0 = alloca i32 
  store  i32 %arg0, i32* %local0 
  %tmp0 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 0 
  %tmp1 = load  i64, i64* %tmp0 
  %tmp2 = inttoptr i64 %tmp1 to i32 ()* 
  %tmp3 =  call ccc  i32  %tmp2()  
  %tmp4 = load  i32, i32* %local0 
  %tmp5 = add   i32 %tmp3, %tmp4 
  ret i32 %tmp5 
}


define external ccc  i32 @func14()    {
block0:
  %tmp0 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 1 
  %tmp1 = load  i64, i64* %tmp0 
  %tmp2 = inttoptr i64 %tmp1 to i32 ()* 
  %tmp3 =  call ccc  i32  %tmp2()  
  ret i32 %tmp3 
}


define external ccc  i64 @func15(i64  %arg0)    {
block0:
  %local0 = alloca i64 
  store  i64 %arg0, i64* %local0 
  %local1 = alloca i64 
  %local2 = alloca i64 
  store  i64 0, i64* %local1 
  %tmp0 = load  i64, i64* %local0 
  %tmp1 = icmp eq i64 %tmp0, 0 
  %tmp2 = zext i1 %tmp1 to i32  
  %tmp3 = icmp ne i32 %tmp2, 0 
  %tmp4 = zext i1 %tmp3 to i32  
  %tmp5 = trunc i32 %tmp4 to i1  
  br i1 %tmp5, label %block1, label %block2 
block1:
  store  i64 42, i64* %local2 
  br label %block5 
block2:
  %tmp6 = load  i64, i64* %local0 
  %tmp7 = icmp eq i64 %tmp6, 1 
  %tmp8 = zext i1 %tmp7 to i32  
  %tmp9 = icmp ne i32 %tmp8, 0 
  %tmp10 = zext i1 %tmp9 to i32  
  %tmp11 = trunc i32 %tmp10 to i1  
  br i1 %tmp11, label %block3, label %block4 
block3:
  store  i64 99, i64* %local2 
  br label %block5 
block4:
  store  i64 7, i64* %local2 
  br label %block5 
block5:
  %tmp12 = load  i64, i64* %local2 
  ret i64 %tmp12 
}


define external ccc  i32 @func16()    {
block0:
  %tmp0 = load  i64, i64* @wasmc.linear_mem 
  %tmp1 = inttoptr i64 %tmp0 to i8* 
  %tmp2 = getelementptr inbounds i8, i8* %tmp1, i32 0 
  %tmp3 = bitcast i8* %tmp2 to i32* 
  store  i32 300, i32* %tmp3 
  ret i32 47 
}


define external ccc  i32 @func17()    {
block0:
  %tmp0 =  call ccc  i32  @func16()  
  %tmp1 = load  i64, i64* @wasmc.linear_mem 
  %tmp2 = inttoptr i64 %tmp1 to i8* 
  %tmp3 = getelementptr inbounds i8, i8* %tmp2, i32 0 
  %tmp4 = bitcast i8* %tmp3 to i32* 
  %tmp5 = load  i32, i32* %tmp4 
  %tmp6 = add   i32 %tmp0, %tmp5 
  ret i32 %tmp6 
}


define external ccc  i32 @func18()    {
block0:
  %tmp0 =  call ccc  i32  @llvm.ctpop.i32(i32  6)  
  %tmp1 =  call ccc  i32  @llvm.cttz.i32(i32  16, i1  0)  
  %tmp2 = add   i32 %tmp0, %tmp1 
  %tmp3 =  call ccc  i32  @llvm.ctlz.i32(i32  32, i1  0)  
  %tmp4 = add   i32 %tmp2, %tmp3 
  ret i32 %tmp4 
}


define external ccc  i64 @func19(i64  %arg0)    {
block0:
  %local0 = alloca i64 
  store  i64 %arg0, i64* %local0 
  %local1 = alloca i64 
  %local2 = alloca i64 
  store  i64 0, i64* %local1 
  store  i64 0, i64* %local2 
  %tmp0 = load  i64, i64* %local1 
  %tmp1 = load  i64, i64* %local0 
  %tmp2 = icmp sge i64 %tmp0, %tmp1 
  %tmp3 = zext i1 %tmp2 to i32  
  %tmp4 = icmp ne i32 %tmp3, 0 
  %tmp5 = zext i1 %tmp4 to i32  
  %tmp6 = trunc i32 %tmp5 to i1  
  br i1 %tmp6, label %block4, label %block1 
block1:
  br label %block2 
block2:
  %tmp7 = load  i64, i64* %local1 
  %tmp8 = add   i64 %tmp7, 1 
  store  i64 %tmp8, i64* %local1 
  %tmp9 = load  i64, i64* %local2 
  %tmp10 = load  i64, i64* %local1 
  %tmp11 = add   i64 %tmp9, %tmp10 
  store  i64 %tmp11, i64* %local2 
  %tmp12 = load  i64, i64* %local0 
  %tmp13 = load  i64, i64* %local1 
  %tmp14 = icmp eq i64 %tmp12, %tmp13 
  %tmp15 = zext i1 %tmp14 to i32  
  %tmp16 = icmp ne i32 %tmp15, 0 
  %tmp17 = zext i1 %tmp16 to i32  
  %tmp18 = trunc i32 %tmp17 to i1  
  br i1 %tmp18, label %block4, label %block3 
block3:
  br label %block2 
block4:
  %tmp19 = load  i64, i64* %local2 
  ret i64 %tmp19 
}


define external ccc  void @_main()    {
block0:
  %tmp0 =  call ccc  i8*  @malloc(i64  64000)  
  %tmp1 = ptrtoint i8* %tmp0 to i64 
  store  i64 %tmp1, i64* @wasmc.linear_mem 
  %tmp2 = ptrtoint i32 ()* @func11 to i64 
  %tmp3 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 0 
  store  i64 %tmp2, i64* %tmp3 
  %tmp4 = ptrtoint i32 ()* @func12 to i64 
  %tmp5 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 1 
  store  i64 %tmp4, i64* %tmp5 
  %tmp6 = load  i64, i64* @wasmc.linear_mem 
  %tmp7 = inttoptr i64 %tmp6 to i8* 
   call ccc  void  @free(i8*  %tmp7)  
  ret void 
}