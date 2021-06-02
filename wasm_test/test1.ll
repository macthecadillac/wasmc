; ModuleID = 'basic'


 


%wasmc.tbl = type [2 x i64]


@wasmc.tbl =    global %wasmc.tbl [i64 0, i64 0]


@wasmc.min_mem_size =    constant i64 64000


@wasmc.linear_mem =    global i64 0


declare external ccc  void @free(i8*)    


declare external ccc  float @llvm.ceil.f32(float)    


declare external ccc  double @llvm.ceil.f64(double)    


declare external ccc  float @llvm.copysign.f32(float, float)    


declare external ccc  double @llvm.copysign.f64(double, double)    


declare external ccc  i32 @llvm.ctlz.i32(i32, i1)    


declare external ccc  i64 @llvm.ctlz.i64(i64, i1)    


declare external ccc  i32 @llvm.ctpop.i32(i32)    


declare external ccc  i64 @llvm.ctpop.i64(i64)    


declare external ccc  i32 @llvm.cttz.i32(i32, i1)    


declare external ccc  i64 @llvm.cttz.i64(i64, i1)    


declare external ccc  float @llvm.fabs.f32(float)    


declare external ccc  double @llvm.fabs.f64(double)    


declare external ccc  float @llvm.floor.f32(float)    


declare external ccc  double @llvm.floor.f64(double)    


declare external ccc  float @llvm.maxnum.f32(float, float)    


declare external ccc  double @llvm.maxnum.f64(double, double)    


declare external ccc  float @llvm.minnum.f32(float, float)    


declare external ccc  double @llvm.minnum.f64(double, double)    


declare external ccc  float @llvm.roundeven.f32(float)    


declare external ccc  double @llvm.roundeven.f64(double)    


declare external ccc  float @llvm.sqrt.f32(float)    


declare external ccc  double @llvm.sqrt.f64(double)    


declare external ccc  float @llvm.trunc.f32(float)    


declare external ccc  double @llvm.trunc.f64(double)    


declare external ccc  i8* @malloc(i64)    


define external ccc  i32 @func0(i32  %local0, i32  %local1, i32  %local2, i32  %local3, i32  %local4)    {
block0:
  %tmp0 = add   i32 %local0, %local1 
  %tmp1 = sub   i32 %tmp0, %local2 
  %tmp2 = mul   i32 %tmp1, %local3 
  %tmp3 = udiv  i32 %tmp2, %local4 
  ret i32 %tmp3 
}


define external ccc  i32 @func1(i32  %local0, i32  %local1)    {
block0:
  %tmp0 = and i32 %local0, %local1 
  %tmp1 = or i32 %local0, %local1 
  %tmp2 = add   i32 %tmp0, %tmp1 
  %tmp3 = and i32 %local0, %local0 
  %tmp4 = add   i32 %tmp2, %tmp3 
  %tmp5 = or i32 %local1, %local1 
  %tmp6 = add   i32 %tmp4, %tmp5 
  %tmp7 = xor i32 %local0, %local1 
  %tmp8 = add   i32 %tmp6, %tmp7 
  %tmp9 = xor i32 %local0, %local0 
  %tmp10 = add   i32 %tmp8, %tmp9 
  %tmp11 = shl   i32 %local0, %local0 
  %tmp12 = add   i32 %tmp10, %tmp11 
  ret i32 %tmp12 
}


define external ccc  float @func2(float  %local0)    {
block0:
  %tmp0 =  call ccc  float  @llvm.fabs.f32(float  %local0)  
  ret float %tmp0 
}


define external ccc  float @func3(float  %local0)    {
block0:
  %tmp0 = fmul float %local0, -1.000000e0 
  ret float %tmp0 
}


define external ccc  float @func4(float  %local0)    {
block0:
  %tmp0 =  call ccc  float  @llvm.sqrt.f32(float  %local0)  
  ret float %tmp0 
}


define external ccc  float @func5(float  %local0, float  %local1, float  %local2)    {
block0:
  %tmp0 =  call ccc  float  @llvm.fabs.f32(float  %local0)  
  %tmp1 = fmul float %local1, -1.000000e0 
  %tmp2 = fadd float %tmp0, %tmp1 
  %tmp3 =  call ccc  float  @llvm.sqrt.f32(float  %local2)  
  %tmp4 = fadd float %tmp2, %tmp3 
  ret float %tmp4 
}


define external ccc  float @func6(float  %local0)    {
block0:
  %tmp0 =  call ccc  float  @llvm.ceil.f32(float  %local0)  
  ret float %tmp0 
}


define external ccc  float @func7(float  %local0)    {
block0:
  %tmp0 =  call ccc  float  @llvm.floor.f32(float  %local0)  
  ret float %tmp0 
}


define external ccc  float @func8(float  %local0)    {
block0:
  %tmp0 =  call ccc  float  @llvm.trunc.f32(float  %local0)  
  ret float %tmp0 
}


define external ccc  float @func9(float  %local0)    {
block0:
  %tmp0 =  call ccc  float  @llvm.floor.f32(float  %local0)  
  ret float %tmp0 
}


define external ccc  float @func10(float  %local0, float  %local1, float  %local2, float  %local3)    {
block0:
  %tmp0 =  call ccc  float  @llvm.ceil.f32(float  %local0)  
  %tmp1 =  call ccc  float  @llvm.floor.f32(float  %local1)  
  %tmp2 = fadd float %tmp0, %tmp1 
  %tmp3 =  call ccc  float  @llvm.trunc.f32(float  %local2)  
  %tmp4 = fadd float %tmp2, %tmp3 
  %tmp5 =  call ccc  float  @llvm.ceil.f32(float  %local3)  
  %tmp6 = fadd float %tmp4, %tmp5 
  ret float %tmp6 
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


define external ccc  i32 @func13(i32  %local0)    {
block0:
  %tmp0 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 0 
  %tmp1 = load  i64, i64* %tmp0 
  %tmp2 = inttoptr i64 %tmp1 to i32 ()* 
  %tmp3 =  call ccc  i32  %tmp2()  
  %tmp4 = add   i32 %tmp3, %local0 
  ret i32 %tmp4 
}


define external ccc  i32 @func14()    {
block0:
  %tmp0 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 1 
  %tmp1 = load  i64, i64* %tmp0 
  %tmp2 = inttoptr i64 %tmp1 to i32 ()* 
  %tmp3 =  call ccc  i32  %tmp2()  
  ret i32 %tmp3 
}


define external ccc  i64 @func15(i64  %local0)    {
block0:
  %tmp0 = icmp eq i64 %local0, 0 
  %tmp1 = zext i1 %tmp0 to i32  
  %tmp2 = icmp ne i32 %tmp1, 0 
  %tmp3 = zext i1 %tmp2 to i32  
  %tmp4 = trunc i32 %tmp3 to i1  
  br i1 %tmp4, label %block1, label %block2 
block1:
  br label %block5 
block2:
  %tmp5 = icmp eq i64 %local0, 1 
  %tmp6 = zext i1 %tmp5 to i32  
  %tmp7 = icmp ne i32 %tmp6, 0 
  %tmp8 = zext i1 %tmp7 to i32  
  %tmp9 = trunc i32 %tmp8 to i1  
  br i1 %tmp9, label %block3, label %block4 
block3:
  br label %block5 
block4:
  br label %block5 
block5:
  %tmp10 = phi i64 [42, %block1], [99, %block3], [7, %block4] 
  ret i64 %tmp10 
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
  ret void 
}