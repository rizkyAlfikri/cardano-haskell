import Data.Semigroup (option)
-- Enum
data Color = Red
            | Black
            | Green
            | Blue
            | Pink
            | Yellow
            deriving Show


lukesLightSaberColor :: Color
lukesLightSaberColor = Yellow

lightOfColor :: [Color]
lightOfColor = [Blue, Green, Black]

warnaLampu :: Color -> Bool 
warnaLampu Red = True 
warnaLampu Green = True 
warnaLampu Yellow = True 
warnaLampu _ = False

type Warna = (Color, String)
favoriteColor :: Color
favoriteColor = Red

whoFavoriteColor :: Warna
whoFavoriteColor = (Green, "Hijau")

-- Contoh lain penggunaan enum yang mirip Maybe
data FailableDouble = Failure | Ok Double  deriving Show

opps :: FailableDouble
opps = Failure

notOpps:: FailableDouble
notOpps = Ok 3.5

-- Record syntax
data Menu = Menu { menuName :: String , menuPrice:: Int} deriving Show 

addOrder :: String -> Int -> Int -> Menu
addOrder menuName price total = Menu {menuName = menuName, menuPrice = totalPrice}
    where totalPrice = price * total


-- IO
main:: IO()
main = do
    putStrLn "Silahkan masukan Pesanan ?"
    namaMenu <- getLine 
    print namaMenu
    putStrLn "\nJumlah yang ingin dibeli"
    jmlMenu <- getLine
    putStrLn "\nHarga Satuan"
    hargaSatuan <- getLine 
    let pesanan = addOrder namaMenu (read jmlMenu) (read hargaSatuan)
    putStrLn "\n===Pesanan Anda Adalah===\n"
    print pesanan
    putStrLn "\n===Sedang diproses===\n"
    putStrLn "\n===Sedang selesai===\n"
    putStrLn "\n===Apakah Ingin memesan lagi===\n"
    putStrLn "\nMasukan 1 untuk Ya dan 2 untuk Tidak"
    main

nasiGoreng = Menu {menuName = "Nasi Goreng", menuPrice = 20000}
ayamGoreng = Menu {menuName = "Ayam Goreng", menuPrice = 30000}
rendang = Menu {menuName = "Rendang", menuPrice = 50000}
bebekGoreng = Menu {menuName = "Bebek Goreng", menuPrice = 5000}
iceTea = Menu {menuName = "Ice Tea", menuPrice = 10000}


menuList :: [Menu]
menuList = [nasiGoreng, ayamGoreng, rendang, bebekGoreng, iceTea]

-- inputJumlah:: Int -> IO()

     

chooseMenu :: IO()
chooseMenu = do
    putStrLn "=== Menu ===="
    putStrLn "1. Nasi Goreng | Rp.20.0000"
    putStrLn "2. Ayam Goreng | Rp.30.0000"
    putStrLn "3. Rendang | Rp.50.0000"
    putStrLn "4. Bebek Goreng | Rp.5.0000"
    putStrLn "5. Ice Tea | Rp.10.0000"
    putStrLn "Silahkan pilih menu dengan mengetik nomor menu yang tertera"
    optionMenu <- getLine
    let order = read optionMenu :: Int
    let resultMenu = menuList !! order - 1
    let word = "Silahkan masukan jumlah pesanan " ++ menuName resultMenu ++ " | 1 X " ++ menuPrice resultMenu
    putStrLn word



checkMenu:: String   -> IO()
checkMenu "1" = chooseMenu
checkMenu "2" = do 
    putStrLn "Terima kasih. Sampai jumpa"
    return ()
checkMenu x = do 
          putStrLn "Kata kuncu yang anda masukan salah, Silahkan coba lagi"
          option <- getLine  
          checkMenu option

    

mainMenu:: IO ()
mainMenu = do 
      putStrLn "Selamat Datang Resto Palugada"
      putStrLn "1. Menu"
      putStrLn "2. Exit"
      option <- getChar  
      checkMenu option