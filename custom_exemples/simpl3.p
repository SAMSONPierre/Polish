READ n
x := * 100 0
y := * 1 1
COMMENT l'expression est simplifié et le bloc if supprimé'
IF * 10 1 = 0
  PRINT 0
ELSE
  n := - n 1
  WHILE n > 0
    z := + x y
    x := y
    y := z
    n := - n 1
  PRINT y