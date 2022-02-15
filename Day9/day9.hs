funHelper1 :: [Integer] -> [Integer]
funHelper1 = map (\x -> if x > 10 then x `div` 2 else x) 

funHelper2 :: [Integer] -> [Integer]
funHelper2 [] = []
funHelper2 (x:xs)= if even x then x : funHelper2 xs else funHelper2 xs

funHelper3 :: [Integer] -> [Integer]
funHelper3 = filter (>8)

mainFun :: [Integer] -> [Integer]
mainFun = funHelper3 . funHelper2 . funHelper1