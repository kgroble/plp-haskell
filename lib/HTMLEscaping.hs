module HTMLEscaping ( getString
                   , convertString )
where

data UnsafeString = Unsafe String

convertString :: UnsafeString -> String
convertString (Unsafe s) = s

getString :: String -> UnsafeString
getString s = Unsafe s
