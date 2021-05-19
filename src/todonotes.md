### Tables and Elems
Tables are composed of elements

Below is how the table in the Game of Life is parsed.
```
tables = [Table (TableType (Limit 18 Nothing) FuncRef)], 
elems  = [ElemSegment {tableIndex = 0, offset = [I32Const 0], funcIndexes = [1,1,1,0,1,1,1,1,1,1,1,0,0,1,1,1,1,1]}], 
```

It is used in line 223, `call_indirect` where it pops a number off the stack, n, and calls the nth function in the table.

A table is empty and initialized with elements.

> elem     ::= {type reftype, init vec(expr), mode elemmode}

I don't really get this?

> elemmode ::= passive 

A passive element segment’s elements can be copied to a table using the tableinit instruction.

> elemmode ::= active {table tableidx, offset expr}  

An active element segment copies its elements into a table during instantiation, as specified by a table index and a constant expression defining an offset into that table 

> elemmode ::= declarative 

A declarative element segment is not available at runtime but merely serves to forward-declare references that are formed in code with instructions like 𝗋𝖾𝖿.𝖿𝗎𝗇𝖼