; ModuleID = 'basic'


 


define external ccc  i32 @main()    {
block0:
  %ident1 = add   i32 2, 3 
  %ident2 = add   i32 1, 4 
  %ident3 = add   i32 %ident2, %ident1 
  ret i32 %ident3 
}