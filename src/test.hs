test :: (IO a, b) -> b
test (x, y) = seq x y