data Bit = NULL | EINS deriving Show
type Bits = [Bit]

integer2Bits :: Integer -> Bits
integer2Bits 0 = [NULL]
integer2Bits 1 = [EINS]
integer2Bits x
    | x `mod` 2 == 0 = integer2Bits (x `div` 2) ++ [NULL]
    | x `mod` 2 == 1 = integer2Bits (x `div` 2) ++ [EINS]

bits2String :: Bits -> String
bits2String [NULL] = "0"
bits2String [EINS] = "1"
bits2String (NULL:bits) = bits2String bits ++ "0"
bits2String (EINS:bits) = bits2String bits ++ "1"


string2Bits :: String -> Bits
string2Bits "0" = [NULL]
string2Bits "1" = [EINS]
string2Bits y 
    | last y == '0' = string2Bits (init y) ++ [NULL]
    | otherwise = string2Bits (init y) ++ [EINS]

integer2binString :: Integer -> String
integer2binString x = bits2String (integer2Bits x)

bits2Integer :: Bits -> Integer
bits2Integer [NULL] = 0
bits2Integer [EINS] = 1
bits2Integer (NULL:bits) = 2 * bits2Integer bits
bits2Integer (EINS:bits) = 2 * bits2Integer bits + 1

bitString2Integer :: String -> Integer
bitString2Integer y = bits2Integer (string2Bits y)