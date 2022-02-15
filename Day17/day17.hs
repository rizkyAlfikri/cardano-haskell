import Data.Monoid

anotherMap :: [Integer ]
anotherMap = map (+1) [1,2,3]

cetak :: Show a => a -> IO()
cetak xx = print xx

anotherFM :: IO()
anotherFM = foldMap cetak [1,2,3]

-- Monoid Law
-- mempty <> x = x
-- x <> mempty = x
-- x <> (y <> z) == (x <> y) <> z
-- "1" <> "2" = "12"
-- <> persamaan simbol dari 'mappend'