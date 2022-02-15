f1:: a -> a -> a
f1 x y = x

f2 :: a -> a -> a
f2 x y = y

hanyaNum :: Num a => a -> a -> a
hanyaNum x y = x + y

-- TypeClass
-- Define Class
class CekIsi a where
        meth :: a -> String -- meth is method

-- Define Instance Class
instance CekIsi Int where
    meth x = "Valuenya Int"

instance CekIsi Bool where
    meth x = "Valuenya Bool"

-- Example
x :: String 
x  = meth(10 :: Int)

y :: String 
y = meth(True  :: Bool)