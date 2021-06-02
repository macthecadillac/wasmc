; ModuleID = 'basic'


 


%wasmc.tbl = type [18 x i64]


@wasmc.tbl =    global %wasmc.tbl [i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0]


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


define external ccc  i32 @func0()    {
block0:
  ret i32 1 
}


define external ccc  i32 @func1()    {
block0:
  ret i32 0 
}


define external ccc  i32 @func2(i32  %local0, i32  %local1)    {
block0:
  %tmp0 = mul   i32 200, %local1 
  %tmp1 = mul   i32 4, %local0 
  %tmp2 = add   i32 %tmp0, %tmp1 
  ret i32 %tmp2 
}


define external ccc  void @func3(i32  %local0, i32  %local1, i32  %local2)    {
block0:
  %tmp0 =  call ccc  i32  @func2(i32  %local0, i32  %local1)  
  %tmp1 = load  i64, i64* @wasmc.linear_mem 
  %tmp2 = inttoptr i64 %tmp1 to i8* 
  %tmp3 = getelementptr inbounds i8, i8* %tmp2, i32 %tmp0 
  ; reverted it back to the original
  ; but i'm not sure what's causing errors
  ; %tmp4 = bitcast i8* %tmp3 to i64* 
  ; store  i32 %local2, i64* %tmp4 
  %tmp4 = bitcast i8* %tmp3 to i32* 
  store  i32 %local2, i32* %tmp4 
  ret void 
}


define external ccc  i32 @func4(i32  %local0, i32  %local1)    {
block0:
  %tmp0 =  call ccc  i32  @func6(i32  0, i32  50, i32  %local0)  
  %tmp1 =  call ccc  i32  @func6(i32  0, i32  50, i32  %local1)  
  %tmp2 = and i32 %tmp0, %tmp1 
  br label %block1 
block1:
  %tmp3 = icmp ne i32 %tmp2, 0 
  %tmp4 = zext i1 %tmp3 to i32  
  %tmp5 = trunc i32 %tmp4 to i1  
  br i1 %tmp5, label %block2, label %block3 
block2:
  %tmp6 =  call ccc  i32  @func2(i32  %local0, i32  %local1)  
  %tmp7 = load  i64, i64* @wasmc.linear_mem 
  %tmp8 = inttoptr i64 %tmp7 to i8* 
  %tmp9 = getelementptr inbounds i8, i8* %tmp8, i32 %tmp6 
  %tmp10 = bitcast i8* %tmp9 to i8* 
  %tmp11 = load  i8, i8* %tmp10 
  %tmp12 = zext i8 %tmp11 to i32  
  br label %block4 
block3:
  br label %block4 
block4:
  %tmp13 = phi i32 [%tmp12, %block2], [0, %block3] 
  ret i32 %tmp13 
}


define external ccc  i32 @func5(i32  %local0, i32  %local1)    {
block0:
  %tmp0 = add   i32 %local0, 1 
  %tmp1 =  call ccc  i32  @func7(i32  %tmp0, i32  %local1)  
  %tmp2 = add   i32 0, %tmp1 
  %tmp3 = add   i32 %local0, 4294967295 
  %tmp4 =  call ccc  i32  @func7(i32  %tmp3, i32  %local1)  
  %tmp5 = add   i32 %tmp2, %tmp4 
  %tmp6 = add   i32 %local1, 4294967295 
  %tmp7 =  call ccc  i32  @func7(i32  %local0, i32  %tmp6)  
  %tmp8 = add   i32 %tmp5, %tmp7 
  %tmp9 = add   i32 %local0, 4294967295 
  %tmp10 = add   i32 %local1, 4294967295 
  %tmp11 =  call ccc  i32  @func7(i32  %tmp9, i32  %tmp10)  
  %tmp12 = add   i32 %tmp8, %tmp11 
  %tmp13 = add   i32 %local0, 1 
  %tmp14 = add   i32 %local1, 4294967295 
  %tmp15 =  call ccc  i32  @func7(i32  %tmp13, i32  %tmp14)  
  %tmp16 = add   i32 %tmp12, %tmp15 
  %tmp17 = add   i32 %local1, 1 
  %tmp18 =  call ccc  i32  @func7(i32  %local0, i32  %tmp17)  
  %tmp19 = add   i32 %tmp16, %tmp18 
  %tmp20 = add   i32 %local0, 4294967295 
  %tmp21 = add   i32 %local1, 1 
  %tmp22 =  call ccc  i32  @func7(i32  %tmp20, i32  %tmp21)  
  %tmp23 = add   i32 %tmp19, %tmp22 
  %tmp24 = add   i32 %local0, 1 
  %tmp25 = add   i32 %local1, 1 
  %tmp26 =  call ccc  i32  @func7(i32  %tmp24, i32  %tmp25)  
  %tmp27 = add   i32 %tmp23, %tmp26 
  ret i32 %tmp27 
}


define external ccc  i32 @func6(i32  %local0, i32  %local1, i32  %local2)    {
block0:
  %tmp0 = icmp sge i32 %local2, %local0 
  %tmp1 = zext i1 %tmp0 to i32  
  %tmp2 = icmp slt i32 %local2, %local1 
  %tmp3 = zext i1 %tmp2 to i32  
  %tmp4 = and i32 %tmp1, %tmp3 
  ret i32 %tmp4 
}


define external ccc  i32 @func7(i32  %local0, i32  %local1)    {
block0:
  %tmp0 =  call ccc  i32  @func4(i32  %local0, i32  %local1)  
  %tmp1 = and i32 %tmp0, 1 
  ret i32 %tmp1 
}


define external ccc  void @func8(i32  %local0, i32  %local1, i32  %local2)    {
block0:
  %tmp0 =  call ccc  i32  @func7(i32  %local0, i32  %local1)  
  %tmp1 = shl   i32 %local2, 1 
  %tmp2 = or i32 %tmp0, %tmp1 
   call ccc  void  @func3(i32  %local0, i32  %local1, i32  %tmp2)  
  ret void 
}


define external ccc  void @func9(i32  %local0, i32  %local1)    {
block0:
  %tmp0 =  call ccc  i32  @func7(i32  %local0, i32  %local1)  
  %tmp1 = mul   i32 9, %tmp0 
  %tmp2 =  call ccc  i32  @func5(i32  %local0, i32  %local1)  
  %tmp3 = or i32 %tmp1, %tmp2 
  %tmp4 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 %tmp3 
  %tmp5 = load  i64, i64* %tmp4 
  %tmp6 = inttoptr i64 %tmp5 to i32 ()* 
  %tmp7 =  call ccc  i32  %tmp6()  
   call ccc  void  @func8(i32  %local0, i32  %local1, i32  %tmp7)  
  ret void 
}


define external ccc  i32 @func10(i32  %local0)    {
block0:
  %tmp0 = add   i32 %local0, 1 
  ret i32 %tmp0 
}


define external ccc  void @func11()    {
block0:
  %local0 = alloca i32, align 4
  %local1 = alloca i32, align 4
  store i32 0, i32* %local0, align 4
  store i32 0, i32* %local1, align 4
  ; %local0 = call ccc i32 @func1()
  ; %local1 = call ccc i32 @func1()
  br label %block2
block1:
  store i32 0, i32* %local0, align 4
  br label %block2
block2:
  %sim0 = load i32, i32* %local0, align 4
  %simy = load i32, i32* %local1, align 4
  ; %sim1 = call ccc i32 @func10(i32 %sim0)
  %sim1 = add i32 %sim0, 1
  ; %sim2 =  call ccc  i32  @func10(i32 %local0)  
  store i32 %sim1, i32* %local0, align 4

  call ccc  void  @func9(i32  %sim0, i32  %simy)  

  %tmp1 = icmp eq i32 %sim1, 50 
  ; %tmp2 = zext i1 %tmp1 to i32  
  ; %tmp3 = icmp ne i32 %tmp2, 0 
  ; %tmp4 = zext i1 %tmp3 to i32  
  ; %tmp5 = trunc i32 %tmp4 to i1  
  ; br i1 %tmp5, label %block4, label %block2 
  br i1 %tmp1, label %block4, label %block2 
block3:
  br label %block2
block4:
  %sim3 = load i32, i32* %local1, align 4
  ; %sim4 = call ccc i32 @func10(i32 %sim3)
  %sim4 = add i32 %sim3, 1
  ; %local1 =  call ccc  i32  @func10(i32  %local1)  
  store i32 %sim4, i32* %local1, align 4

  %tmp7 = icmp eq i32 %sim4, 50 
  %tmp8 = zext i1 %tmp7 to i32  
  %tmp9 = icmp ne i32 %tmp8, 0 
  %tmp10 = zext i1 %tmp9 to i32  
  %tmp11 = trunc i32 %tmp10 to i1  
  br i1 %tmp11, label %block6, label %block1
block5:
  br label %block2
block6:
  ret void 
}


define external ccc  void @func12()    {
block0:
; sim making it work ig
  %local0 = alloca i32, align 4
  %local1 = alloca i32, align 4
  store i32 0, i32* %local0, align 4
  store i32 0, i32* %local1, align 4
  br label %block2
block1:
  store i32 0, i32* %local0, align 4
  br label %block2
block2:
  ; %local0 =  call ccc  i32  @func10(i32  0)  
  %sim0 = load i32, i32* %local0, align 4
  %simy = load i32, i32* %local1, align 4
  ; %sim1 = call ccc i32 @func10(i32 %sim0)
  %sim1 = add i32 %sim0, 1
  ; %sim2 =  call ccc  i32  @func10(i32 %local0)  
  store i32 %sim1, i32* %local0, align 4

  %tmp0 =  call ccc  i32  @func4(i32  %sim0, i32  %simy)  
  %tmp1 = lshr  i32 %tmp0, 1 
   call ccc  void  @func3(i32  %sim0, i32  %simy, i32  %tmp1)  

  %tmp3 = icmp eq i32 %sim1, 50
  ; %tmp4 = zext i1 %tmp3 to i32  
  ; %tmp5 = icmp ne i32 %tmp4, 0 
  ; %tmp6 = zext i1 %tmp5 to i32  
  ; %tmp7 = trunc i32 %tmp6 to i1  
  ; br i1 %tmp7, label %block4, label %block2
  br i1 %tmp3, label %block4, label %block2
block3:
  br label %block2
block4:
  ; %local1 =  call ccc  i32  @func10(i32  0)  
  %sim3 = load i32, i32* %local1, align 4
  %sim4 = add i32 %sim3, 1
  store i32 %sim4, i32* %local1, align 4
  
  %tmp9 = icmp eq i32 %sim4, 50
  ; %tmp10 = zext i1 %tmp9 to i32  
  ; %tmp11 = icmp ne i32 %tmp10, 0 
  ; %tmp12 = zext i1 %tmp11 to i32  
  ; %tmp13 = trunc i32 %tmp12 to i1  
  ; br i1 %tmp13, label %block6, label %block1
  br i1 %tmp9, label %block6, label %block1
block5:
  br label %block2
block6:
  ret void 
}


define external ccc  void @func13()    {
block0:
   call ccc  void  @func11()  
   call ccc  void  @func12()  
  ret void 
}


define external ccc  void @_main()    {
block0:
  %tmp0 =  call ccc  i8*  @malloc(i64  64000)  
  %tmp1 = ptrtoint i8* %tmp0 to i64 
  store  i64 %tmp1, i64* @wasmc.linear_mem 
  %tmp2 = ptrtoint i32 ()* @func1 to i64 
  %tmp3 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 0 
  store  i64 %tmp2, i64* %tmp3 
  %tmp4 = ptrtoint i32 ()* @func1 to i64 
  %tmp5 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 1 
  store  i64 %tmp4, i64* %tmp5 
  %tmp6 = ptrtoint i32 ()* @func1 to i64 
  %tmp7 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 2 
  store  i64 %tmp6, i64* %tmp7 
  %tmp8 = ptrtoint i32 ()* @func0 to i64 
  %tmp9 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 3 
  store  i64 %tmp8, i64* %tmp9 
  %tmp10 = ptrtoint i32 ()* @func1 to i64 
  %tmp11 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 4 
  store  i64 %tmp10, i64* %tmp11 
  %tmp12 = ptrtoint i32 ()* @func1 to i64 
  %tmp13 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 5 
  store  i64 %tmp12, i64* %tmp13 
  %tmp14 = ptrtoint i32 ()* @func1 to i64 
  %tmp15 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 6 
  store  i64 %tmp14, i64* %tmp15 
  %tmp16 = ptrtoint i32 ()* @func1 to i64 
  %tmp17 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 7 
  store  i64 %tmp16, i64* %tmp17 
  %tmp18 = ptrtoint i32 ()* @func1 to i64 
  %tmp19 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 8 
  store  i64 %tmp18, i64* %tmp19 
  %tmp20 = ptrtoint i32 ()* @func1 to i64 
  %tmp21 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 9 
  store  i64 %tmp20, i64* %tmp21 
  %tmp22 = ptrtoint i32 ()* @func1 to i64 
  %tmp23 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 10 
  store  i64 %tmp22, i64* %tmp23 
  %tmp24 = ptrtoint i32 ()* @func0 to i64 
  %tmp25 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 11 
  store  i64 %tmp24, i64* %tmp25 
  %tmp26 = ptrtoint i32 ()* @func0 to i64 
  %tmp27 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 12 
  store  i64 %tmp26, i64* %tmp27 
  %tmp28 = ptrtoint i32 ()* @func1 to i64 
  %tmp29 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 13 
  store  i64 %tmp28, i64* %tmp29 
  %tmp30 = ptrtoint i32 ()* @func1 to i64 
  %tmp31 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 14 
  store  i64 %tmp30, i64* %tmp31 
  %tmp32 = ptrtoint i32 ()* @func1 to i64 
  %tmp33 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 15 
  store  i64 %tmp32, i64* %tmp33 
  %tmp34 = ptrtoint i32 ()* @func1 to i64 
  %tmp35 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 16 
  store  i64 %tmp34, i64* %tmp35 
  %tmp36 = ptrtoint i32 ()* @func1 to i64 
  %tmp37 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 17 
  store  i64 %tmp36, i64* %tmp37 
  ret void 
}