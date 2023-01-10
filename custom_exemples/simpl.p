READ n
COMMENT 100 * 0 est simplifié en 0
x := * 100 0
COMMENT 1 * 1 est simplifié en 1
y := * 1 1
IF n = 0
  PRINT 0
ELSE
  n := - n 1
  WHILE n > 0
    z := + x y
    x := y
    y := z
    n := - n 1
  PRINT y