import Control.Applicative

data Maybe' a = Just' a | Nothing'

instance Alternative Maybe' where
  empty = Nothing'
  Nothing' <|> m2 = m2
  m1 <|> _ = m1

data Either' a = Left' a | Right' a

instance Alternative Parser where
  empty = Parser $ \input -> (Nothing, input)
