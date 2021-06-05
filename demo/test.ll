; ModuleID = 'basic'


 


%wasmc.tbl = type [18 x i64]


@wasmc.tbl =    global %wasmc.tbl [i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0]


@wasmc.min_mem_size =    constant i64 64000


@wasmc.linear_mem =    global i64 0


declare external ccc  void @free(i8*)    


declare external ccc  i8* @malloc(i64)    


define external ccc  i32 @func0()    {
block0:
  ret i32 1 
}


define external ccc  i32 @func1()    {
block0:
  ret i32 0 
}


define external ccc  i32 @func2(i32  %arg0, i32  %arg1)    {
block0:
  %local0 = alloca i32 
  store  i32 %arg0, i32* %local0 
  %local1 = alloca i32 
  store  i32 %arg1, i32* %local1 
  %tmp0 = load  i32, i32* %local1 
  %tmp1 = mul   i32 200, %tmp0 
  %tmp2 = load  i32, i32* %local0 
  %tmp3 = mul   i32 4, %tmp2 
  %tmp4 = add   i32 %tmp1, %tmp3 
  ret i32 %tmp4 
}


define external ccc  void @func3(i32  %arg0, i32  %arg1, i32  %arg2)    {
block0:
  %local0 = alloca i32 
  store  i32 %arg0, i32* %local0 
  %local1 = alloca i32 
  store  i32 %arg1, i32* %local1 
  %local2 = alloca i32 
  store  i32 %arg2, i32* %local2 
  %tmp0 = load  i32, i32* %local0 
  %tmp1 = load  i32, i32* %local1 
  %tmp2 =  call ccc  i32  @func2(i32  %tmp0, i32  %tmp1)  
  %tmp3 = load  i32, i32* %local2 
  %tmp4 = load  i64, i64* @wasmc.linear_mem 
  %tmp5 = inttoptr i64 %tmp4 to i8* 
  %tmp6 = getelementptr inbounds i8, i8* %tmp5, i32 %tmp2 
  %tmp7 = bitcast i8* %tmp6 to i32* 
  store  i32 %tmp3, i32* %tmp7 
  ret void 
}


define external ccc  i32 @func4(i32  %arg0, i32  %arg1)    {
block0:
  %local0 = alloca i32 
  store  i32 %arg0, i32* %local0 
  %local1 = alloca i32 
  store  i32 %arg1, i32* %local1 
  %tmp0 = load  i32, i32* %local0 
  %tmp1 =  call ccc  i32  @func6(i32  0, i32  50, i32  %tmp0)  
  %tmp2 = load  i32, i32* %local1 
  %tmp3 =  call ccc  i32  @func6(i32  0, i32  50, i32  %tmp2)  
  %tmp4 = and i32 %tmp1, %tmp3 
  br label %block1 
block1:
  %tmp5 = icmp ne i32 %tmp4, 0 
  %tmp6 = zext i1 %tmp5 to i32  
  %tmp7 = trunc i32 %tmp6 to i1  
  br i1 %tmp7, label %block2, label %block3 
block2:
  %tmp8 = load  i32, i32* %local0 
  %tmp9 = load  i32, i32* %local1 
  %tmp10 =  call ccc  i32  @func2(i32  %tmp8, i32  %tmp9)  
  %tmp11 = load  i64, i64* @wasmc.linear_mem 
  %tmp12 = inttoptr i64 %tmp11 to i8* 
  %tmp13 = getelementptr inbounds i8, i8* %tmp12, i32 %tmp10 
  %tmp14 = bitcast i8* %tmp13 to i8* 
  %tmp15 = load  i8, i8* %tmp14 
  %tmp16 = zext i8 %tmp15 to i32  
  br label %block4 
block3:
  br label %block4 
block4:
  %tmp17 = phi i32 [%tmp16, %block2], [0, %block3] 
  ret i32 %tmp17 
}


define external ccc  i32 @func5(i32  %arg0, i32  %arg1)    {
block0:
  %local0 = alloca i32 
  store  i32 %arg0, i32* %local0 
  %local1 = alloca i32 
  store  i32 %arg1, i32* %local1 
  %tmp0 = load  i32, i32* %local0 
  %tmp1 = add   i32 %tmp0, 1 
  %tmp2 = load  i32, i32* %local1 
  %tmp3 =  call ccc  i32  @func7(i32  %tmp1, i32  %tmp2)  
  %tmp4 = add   i32 0, %tmp3 
  %tmp5 = load  i32, i32* %local0 
  %tmp6 = add   i32 %tmp5, 4294967295 
  %tmp7 = load  i32, i32* %local1 
  %tmp8 =  call ccc  i32  @func7(i32  %tmp6, i32  %tmp7)  
  %tmp9 = add   i32 %tmp4, %tmp8 
  %tmp10 = load  i32, i32* %local0 
  %tmp11 = load  i32, i32* %local1 
  %tmp12 = add   i32 %tmp11, 4294967295 
  %tmp13 =  call ccc  i32  @func7(i32  %tmp10, i32  %tmp12)  
  %tmp14 = add   i32 %tmp9, %tmp13 
  %tmp15 = load  i32, i32* %local0 
  %tmp16 = add   i32 %tmp15, 4294967295 
  %tmp17 = load  i32, i32* %local1 
  %tmp18 = add   i32 %tmp17, 4294967295 
  %tmp19 =  call ccc  i32  @func7(i32  %tmp16, i32  %tmp18)  
  %tmp20 = add   i32 %tmp14, %tmp19 
  %tmp21 = load  i32, i32* %local0 
  %tmp22 = add   i32 %tmp21, 1 
  %tmp23 = load  i32, i32* %local1 
  %tmp24 = add   i32 %tmp23, 4294967295 
  %tmp25 =  call ccc  i32  @func7(i32  %tmp22, i32  %tmp24)  
  %tmp26 = add   i32 %tmp20, %tmp25 
  %tmp27 = load  i32, i32* %local0 
  %tmp28 = load  i32, i32* %local1 
  %tmp29 = add   i32 %tmp28, 1 
  %tmp30 =  call ccc  i32  @func7(i32  %tmp27, i32  %tmp29)  
  %tmp31 = add   i32 %tmp26, %tmp30 
  %tmp32 = load  i32, i32* %local0 
  %tmp33 = add   i32 %tmp32, 4294967295 
  %tmp34 = load  i32, i32* %local1 
  %tmp35 = add   i32 %tmp34, 1 
  %tmp36 =  call ccc  i32  @func7(i32  %tmp33, i32  %tmp35)  
  %tmp37 = add   i32 %tmp31, %tmp36 
  %tmp38 = load  i32, i32* %local0 
  %tmp39 = add   i32 %tmp38, 1 
  %tmp40 = load  i32, i32* %local1 
  %tmp41 = add   i32 %tmp40, 1 
  %tmp42 =  call ccc  i32  @func7(i32  %tmp39, i32  %tmp41)  
  %tmp43 = add   i32 %tmp37, %tmp42 
  ret i32 %tmp43 
}


define external ccc  i32 @func6(i32  %arg0, i32  %arg1, i32  %arg2)    {
block0:
  %local0 = alloca i32 
  store  i32 %arg0, i32* %local0 
  %local1 = alloca i32 
  store  i32 %arg1, i32* %local1 
  %local2 = alloca i32 
  store  i32 %arg2, i32* %local2 
  %tmp0 = load  i32, i32* %local2 
  %tmp1 = load  i32, i32* %local0 
  %tmp2 = icmp sge i32 %tmp0, %tmp1 
  %tmp3 = zext i1 %tmp2 to i32  
  %tmp4 = load  i32, i32* %local2 
  %tmp5 = load  i32, i32* %local1 
  %tmp6 = icmp slt i32 %tmp4, %tmp5 
  %tmp7 = zext i1 %tmp6 to i32  
  %tmp8 = and i32 %tmp3, %tmp7 
  ret i32 %tmp8 
}


define external ccc  i32 @func7(i32  %arg0, i32  %arg1)    {
block0:
  %local0 = alloca i32 
  store  i32 %arg0, i32* %local0 
  %local1 = alloca i32 
  store  i32 %arg1, i32* %local1 
  %tmp0 = load  i32, i32* %local0 
  %tmp1 = load  i32, i32* %local1 
  %tmp2 =  call ccc  i32  @func4(i32  %tmp0, i32  %tmp1)  
  %tmp3 = and i32 %tmp2, 1 
  ret i32 %tmp3 
}


define external ccc  void @func8(i32  %arg0, i32  %arg1, i32  %arg2)    {
block0:
  %local0 = alloca i32 
  store  i32 %arg0, i32* %local0 
  %local1 = alloca i32 
  store  i32 %arg1, i32* %local1 
  %local2 = alloca i32 
  store  i32 %arg2, i32* %local2 
  %tmp0 = load  i32, i32* %local0 
  %tmp1 = load  i32, i32* %local1 
  %tmp2 = load  i32, i32* %local0 
  %tmp3 = load  i32, i32* %local1 
  %tmp4 =  call ccc  i32  @func7(i32  %tmp2, i32  %tmp3)  
  %tmp5 = load  i32, i32* %local2 
  %tmp6 = shl   i32 %tmp5, 1 
  %tmp7 = or i32 %tmp4, %tmp6 
   call ccc  void  @func3(i32  %tmp0, i32  %tmp1, i32  %tmp7)  
  ret void 
}


define external ccc  void @func9(i32  %arg0, i32  %arg1)    {
block0:
  %local0 = alloca i32 
  store  i32 %arg0, i32* %local0 
  %local1 = alloca i32 
  store  i32 %arg1, i32* %local1 
  %tmp0 = load  i32, i32* %local0 
  %tmp1 = load  i32, i32* %local1 
  %tmp2 = load  i32, i32* %local0 
  %tmp3 = load  i32, i32* %local1 
  %tmp4 =  call ccc  i32  @func7(i32  %tmp2, i32  %tmp3)  
  %tmp5 = mul   i32 9, %tmp4 
  %tmp6 = load  i32, i32* %local0 
  %tmp7 = load  i32, i32* %local1 
  %tmp8 =  call ccc  i32  @func5(i32  %tmp6, i32  %tmp7)  
  %tmp9 = or i32 %tmp5, %tmp8 
  %tmp10 = getelementptr inbounds %wasmc.tbl, %wasmc.tbl* @wasmc.tbl, i32 0, i32 %tmp9 
  %tmp11 = load  i64, i64* %tmp10 
  %tmp12 = inttoptr i64 %tmp11 to i32 ()* 
  %tmp13 =  call ccc  i32  %tmp12()  
   call ccc  void  @func8(i32  %tmp0, i32  %tmp1, i32  %tmp13)  
  ret void 
}


define external ccc  i32 @func10(i32  %arg0)    {
block0:
  %local0 = alloca i32 
  store  i32 %arg0, i32* %local0 
  %tmp0 = load  i32, i32* %local0 
  %tmp1 = add   i32 %tmp0, 1 
  ret i32 %tmp1 
}


define external ccc  void @func11()    {
block0:
  %local0 = alloca i32 
  %local1 = alloca i32 
  store  i32 0, i32* %local1 
  br label %block1 
block1:
  store  i32 0, i32* %local0 
  br label %block2 
block2:
  %tmp0 = load  i32, i32* %local0 
  %tmp1 = load  i32, i32* %local1 
   call ccc  void  @func9(i32  %tmp0, i32  %tmp1)  
  %tmp2 = load  i32, i32* %local0 
  %tmp3 =  call ccc  i32  @func10(i32  %tmp2)  
  store  i32 %tmp3, i32* %local0 
  %tmp4 = load  i32, i32* %local0 
  %tmp5 = icmp eq i32 %tmp4, 50 
  %tmp6 = zext i1 %tmp5 to i32  
  %tmp7 = icmp ne i32 %tmp6, 0 
  %tmp8 = zext i1 %tmp7 to i32  
  %tmp9 = trunc i32 %tmp8 to i1  
  br i1 %tmp9, label %block4, label %block3 
block3:
  br label %block2 
block4:
  %tmp10 = load  i32, i32* %local1 
  %tmp11 =  call ccc  i32  @func10(i32  %tmp10)  
  store  i32 %tmp11, i32* %local1 
  %tmp12 = load  i32, i32* %local1 
  %tmp13 = icmp eq i32 %tmp12, 50 
  %tmp14 = zext i1 %tmp13 to i32  
  %tmp15 = icmp ne i32 %tmp14, 0 
  %tmp16 = zext i1 %tmp15 to i32  
  %tmp17 = trunc i32 %tmp16 to i1  
  br i1 %tmp17, label %block6, label %block5 
block5:
  br label %block1 
block6:
  ret void 
}


define external ccc  void @func12()    {
block0:
  %local0 = alloca i32 
  %local1 = alloca i32 
  store  i32 0, i32* %local1 
  br label %block1 
block1:
  store  i32 0, i32* %local0 
  br label %block2 
block2:
  %tmp0 = load  i32, i32* %local0 
  %tmp1 = load  i32, i32* %local1 
  %tmp2 = load  i32, i32* %local0 
  %tmp3 = load  i32, i32* %local1 
  %tmp4 =  call ccc  i32  @func4(i32  %tmp2, i32  %tmp3)  
  %tmp5 = lshr  i32 %tmp4, 1 
   call ccc  void  @func3(i32  %tmp0, i32  %tmp1, i32  %tmp5)  
  %tmp6 = load  i32, i32* %local0 
  %tmp7 =  call ccc  i32  @func10(i32  %tmp6)  
  store  i32 %tmp7, i32* %local0 
  %tmp8 = load  i32, i32* %local0 
  %tmp9 = icmp eq i32 %tmp8, 50 
  %tmp10 = zext i1 %tmp9 to i32  
  %tmp11 = icmp ne i32 %tmp10, 0 
  %tmp12 = zext i1 %tmp11 to i32  
  %tmp13 = trunc i32 %tmp12 to i1  
  br i1 %tmp13, label %block4, label %block3 
block3:
  br label %block2 
block4:
  %tmp14 = load  i32, i32* %local1 
  %tmp15 =  call ccc  i32  @func10(i32  %tmp14)  
  store  i32 %tmp15, i32* %local1 
  %tmp16 = load  i32, i32* %local1 
  %tmp17 = icmp eq i32 %tmp16, 50 
  %tmp18 = zext i1 %tmp17 to i32  
  %tmp19 = icmp ne i32 %tmp18, 0 
  %tmp20 = zext i1 %tmp19 to i32  
  %tmp21 = trunc i32 %tmp20 to i1  
  br i1 %tmp21, label %block6, label %block5 
block5:
  br label %block1 
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
  %tmp38 = load  i64, i64* @wasmc.linear_mem 
  %tmp39 = inttoptr i64 %tmp38 to i8* 
   call ccc  void  @free(i8*  %tmp39)  
  ret void 
}