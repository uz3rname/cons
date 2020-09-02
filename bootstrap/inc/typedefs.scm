(define type-defs
  '((throw
      (casts-to *)
      (casts-from *))
    (void
      (casts-from *))
    (null
      (casts-to (|:| list *) (|:| array *)))
    (bool)
    (byte
      (class number)
      (casts-to int real char))
    (int
      (class number)
      (casts-to byte real (|:| ptr *))
      (casts-expl-to char int16 int32))
    (int16
      (class number)
      (casts-to int32 int byte)
      (casts-expl-to char))
    (int32
      (class number)
      (casts-to int16 int byte)
      (casts-expl-to char))
    (real
      (class number)
      (casts-to byte int))
    (char
      (casts-expl-to int byte))
    (ptr
      (args a)
      (casts-to (|:| ptr *))  
      (casts-from (|:| ptr *))
      (casts-expl-to int))))

