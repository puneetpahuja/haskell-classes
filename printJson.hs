-- printJson.hs

module PrintJson where

type JSON = Value

data Value = Str String
           | Number Float
           | Object Obj
           | Boolean Bool
           | Array (List Value)
           | Null

data List a = EmptyList | ConsList a (List a)

data Obj = EmptyObj | Obj (List ObjMember)

data ObjMember = ObjMember String Value

instance (Show a) => Show (List a) where
  show x = "[" ++ show' x ++ "]"
    where
      show' EmptyList               = ""
      show' (ConsList y EmptyList)  = show y
      show' (ConsList y ys)         = show y ++ ", " ++ show' ys

instance Show ObjMember where
  show (ObjMember x y) = show x ++ ": " ++ show y ++ "\n"

instance Show Obj where 
 show x = "{" ++ show' x ++ "}"
    where
      show' EmptyObj     = ""
      show' (Obj y) = show y

instance Show Value where
  show (Str x) = show x
  show (Number x) = show x
  show (Object x) = show x
  show (Boolean x) = show x
  show (Array x) = show x
  show Null = "Null"

json :: JSON
json = let m1 = ObjMember "a" Null
           m2 = ObjMember "b" (Boolean True)
           m3 = ObjMember "c" (Boolean False)
           m4 = ObjMember "d" (Str "abc")
           m5 = ObjMember "e" (Number 5)
           m6 = ObjMember "f" (Array (ConsList (Boolean True) (ConsList (Number 6.55) EmptyList)))
           obj = Obj (ConsList m1 (ConsList m2 (ConsList m3 (ConsList m4 (ConsList m5 (ConsList m6 EmptyList))))))
           in Object obj



-- type AAA = Int
-- instance Show AAA where
--  show x = "1"

-- type ObjMember = (String, Value)   -- why i cant implement show on them?
-- issues : (Number 5) shows as "5.0"



{-

 json
{["a": Null
, "b": True
, "c": False
, "d": "abc"
, "e": 5.0
, "f": [True, 6.55]
]}

-}


