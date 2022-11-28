module Eval where

import Syntax

astrec :: (AST -> AST) -> AST -> AST
astrec f (Appl a b) = Appl (f a) (f b)
astrec f (Seq a b) = Seq (f a) (f b)
astrec f (Fn a b) = Fn a (f b)
astrec f (Def a b) = Def a (f b)
astrec f x = x

subref :: String -> AST -> AST -> AST
subref s t (Ref ss) = if s == ss then t else Ref ss
subref s t (Fn v x) = if v == s then Fn v x else Fn v (subref s t x)
subref s t x = astrec (subref s t) x

ctx :: AST -> AST -> AST
ctx (Def a b) = subref a b
ctx (Seq a b) = (ctx a) . (ctx b)
ctx x = Seq (undef x)

beta :: AST -> AST
beta (Appl a b) = case beta a of
    Fn s t -> beta $ subref s (beta b) t
    x -> Appl x b
beta x = astrec beta x

undef :: AST -> AST
undef (Seq a b) = ctx a (undef b)
undef x = astrec undef x

flatten :: AST -> Either String String
flatten (Seq a b) = (++) <$> flatten a <*> flatten b
flatten (Lit t) = Right $ t
flatten x = Left $ "Unable to flatten " ++ show x

eval = flatten . beta . undef
