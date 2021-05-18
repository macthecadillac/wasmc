(module
  (; (import "console" "log" (func $log (param i32) (param i32))) ;)

  (memory $mem 1)

  (table 18 funcref)
  (elem (i32.const 0)
    ;; for cells that are currently dead
    $dead
    $dead
    $dead
    $alive
    $dead
    $dead
    $dead
    $dead
    $dead
    ;; for cells that are currently alive
    $dead
    $dead
    $alive
    $alive
    $dead
    $dead
    $dead
    $dead
    $dead
  )

  (func $alive (result i32)
    i32.const 1
  )

  (func $dead (result i32)
    i32.const 0
  )


  (func $offsetFromCoordinate (param $x i32) (param $y i32) (result i32)
    (i32.add
      (i32.mul
        (i32.const 200)
        (local.get $y))
      (i32.mul
        (i32.const 4)
        (local.get $x))
    )
  )

  (func $setCell (param $x i32) (param $y i32) (param $value i32)
    (i32.store
      (call $offsetFromCoordinate
        (local.get $x)
        (local.get $y)
      )
      (local.get $value)
    )
  )

  (func $getCell (param $x i32) (param $y i32) (result i32)
    (if (result i32)
      (block (result i32)
        (i32.and
          (call $inRange
            (i32.const 0)
            (i32.const 50)
            (local.get $x)
          )
          (call $inRange
            (i32.const 0)
            (i32.const 50)
            (local.get $y)
          )
        )
      )
      (then
        (i32.load8_u
          (call $offsetFromCoordinate
            (local.get $x)
            (local.get $y))
        )
      )
      (else
        (i32.const 0)
      )
    )    
  )

  (func $liveNeighbourCount (param $x i32) (param $y i32) (result i32)
    i32.const 0

    ;; add the cell value from x + 1, y
    (call $isCellAlive
      (i32.add
        (local.get $x)
        (i32.const 1)
      )    
      (local.get $y)
    )
    i32.add

    ;; add the cell value from x - 1, y
    (call $isCellAlive
      (i32.add
        (local.get $x)
        (i32.const -1)
      )    
      (local.get $y)
    )
    i32.add

    ;; add the cell value from x, y - 1
    (call $isCellAlive
      (local.get $x)
      (i32.add
        (local.get $y)
        (i32.const -1)
      )
    )
    i32.add

    ;; add the cell value from x - 1, y - 1
    (call $isCellAlive
      (i32.add
        (local.get $x)
        (i32.const -1)
      ) 
      (i32.add
        (local.get $y)
        (i32.const -1)
      )
    )
    i32.add

    ;; add the cell value from x + 1, y - 1
    (call $isCellAlive
      (i32.add
        (local.get $x)
        (i32.const 1)
      ) 
      (i32.add
        (local.get $y)
        (i32.const -1)
      )
    )
    i32.add

    ;; add the cell value from x, y + 1
    (call $isCellAlive
      (local.get $x)
      (i32.add
        (local.get $y)
        (i32.const 1)
      )
    )
    i32.add

    ;; add the cell value from x - 1, y + 1
    (call $isCellAlive
      (i32.add
        (local.get $x)
        (i32.const -1)
      ) 
      (i32.add
        (local.get $y)
        (i32.const 1)
      )
    )
    i32.add

    ;; add the cell value from x + 1, y + 1
    (call $isCellAlive
      (i32.add
        (local.get $x)
        (i32.const 1)
      ) 
      (i32.add
        (local.get $y)
        (i32.const 1)
      )
    )
    i32.add
  )

  (func $inRange (param $low i32) (param $high i32) (param $value i32) (result i32)
    (i32.and
      (i32.ge_s (local.get $value) (local.get $low))
      (i32.lt_s (local.get $value) (local.get $high))
    )  
  )

  (func $isCellAlive (param $x i32) (param $y i32) (result i32)
    (i32.and
      (call $getCell
        (local.get $x)
        (local.get $y)
      )
      (i32.const 1)
    )
  )

  (func $setCellStateForNextGeneration (param $x i32) (param $y i32) (param $value i32)
    (call $setCell
      (local.get $x)
      (local.get $y)
      (i32.or
        (call $isCellAlive
          (local.get $x)
          (local.get $y)
        )
        (i32.shl
          (local.get $value)
          (i32.const 1)
        )
      )
    )
  )

  (func $evolveCellToNextGeneration (param $x i32) (param $y i32)
    (call $setCellStateForNextGeneration
      (local.get $x)
      (local.get $y)
      (call_indirect (result i32)
        (i32.or
          (i32.mul
            (i32.const 9)
            (call $isCellAlive
              (local.get $x)
              (local.get $y)
            )
          )
          (call $liveNeighbourCount
            (local.get $x)
            (local.get $y)
          )
        )
      )
    )
  )

  (func $increment (param $value i32) (result i32)
    (i32.add 
      (local.get $value)
      (i32.const 1)
    )
  )

  (func $evolveAllCells
    (local $x i32)
    (local $y i32)

    (local.set $y (i32.const 0))
    
    (block 
      (loop 

        (local.set $x (i32.const 0))

        (block 
          (loop 
            (call $evolveCellToNextGeneration
              (local.get $x)
              (local.get $y)
            )
            (local.set $x (call $increment (local.get $x)))
            (br_if 1 (i32.eq (local.get $x) (i32.const 50)))
            (br 0)
          )
        )
        
        (local.set $y (call $increment (local.get $y)))
        (br_if 1 (i32.eq (local.get $y) (i32.const 50)))
        (br 0)
      )
    )
  )

  (func $promoteNextGeneration
    (local $x i32)
    (local $y i32)

    (local.set $y (i32.const 0))
    
    (block 
      (loop 

        (local.set $x (i32.const 0))

        (block 
          (loop
            (call $setCell
              (local.get $x)
              (local.get $y)
              (i32.shr_u
                (call $getCell
                  (local.get $x)
                  (local.get $y)
                )
                (i32.const 1)
              )
            )

            (local.set $x (call $increment (local.get $x)))
            (br_if 1 (i32.eq (local.get $x) (i32.const 50)))
            (br 0)
          )
        )
        
        (local.set $y (call $increment (local.get $y)))
        (br_if 1 (i32.eq (local.get $y) (i32.const 50)))
        (br 0)
      )
    )
  )

  (func $tick
    (call $evolveAllCells)
    (call $promoteNextGeneration)
  )

  (export "tick" (func $tick))
  (export "promoteNextGeneration" (func $promoteNextGeneration))
  (export "evolveAllCells" (func $evolveAllCells))
  (export "evolveCellToNextGeneration" (func $evolveCellToNextGeneration))
  (export "setCellStateForNextGeneration" (func $setCellStateForNextGeneration))
  (export "isCellAlive" (func $isCellAlive))
  (export "inRange" (func $inRange))
  (export "offsetFromCoordinate" (func $offsetFromCoordinate))
  (export "liveNeighbourCount" (func $liveNeighbourCount))
  (export "getCell" (func $getCell))
  (export "setCell" (func $setCell))
  (export "memory" (memory $mem))
)
