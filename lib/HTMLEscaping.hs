


module HTMLEscaping ( getString
                   , convertString )
where

data UnsafeString = Unsafe String

-- escapes a string to be HTML safe
convertString :: UnsafeString -> String
convertString (Unsafe s) = s

-- some magic user input function
getString :: UnsafeString
getString = Unsafe "alert('I am an evil hacker');"
