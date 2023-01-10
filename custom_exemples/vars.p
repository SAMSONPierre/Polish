COMMENT exemple 1 de vars
READ n
x := 0
y := 1
IF n = 0
  PRINT 0
ELSE
  n := - n 1
  WHILE n > 0
    z := + x y
    x := y
    y := z
    n := - n 1
  PRINT z
  PRINT y
PRINT n
COMMENT n est initialisé au début donc pas de pb
COMMENT mais z est initialisé dans le while donc 
COMMENT ne peut pas être appelé en dehors