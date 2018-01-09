-- printJson.hs

module PrintJson where

type JSON = Value

data Value = Str String
           | Number Float
           | Object Obj
           | Boolean Bool
           | Array [Value]
           | Null

newtype Obj = Obj [ObjMember]

data ObjMember = ObjMember String Value

instance Show ObjMember where
  show (ObjMember x y) = show x ++ ": " ++ show y

instance Show Obj where
 show x = "{\n" ++ show' x ++ "\n}"
    where
      show' (Obj [])     = ""
      show' (Obj [y])    = "  " ++ show y
      show' (Obj (y:ys)) = "  " ++ show y ++ ", \n" ++ show' (Obj ys)

instance Show Value where
  show (Str x)         = show x
  show (Number x)      = show x
  show (Object x)      = show x
  show (Boolean True)  = "true"
  show (Boolean False) = "false"
  show (Array x)       = show x
  show Null            = "null"

json :: JSON
json = let m1 = ObjMember "a" Null
           m2 = ObjMember "b" (Boolean True)
           m3 = ObjMember "c" (Boolean False)
           m4 = ObjMember "d" (Str "abc")
           m5 = ObjMember "e" (Number 5)
           m6 = ObjMember "f" (Array [Boolean True, Number 6.55])
           obj = Obj [m1, m2, m3, m4, m5, m6]
           in Object obj

{-

> json
{
  "a": null,
  "b": true,
  "c": false,
  "d": "abc",
  "e": 5.0,
  "f": [true,6.55]
}

-}

-- type AAA = Int
-- instance Show AAA where
--  show x = "1"

-- type ObjMember = (String, Value)   -- why i cant implement show on them?
-- issues : (Number 5) shows as "5.0"
