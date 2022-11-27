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

type Ctx = AST -> AST

ctx :: AST -> Ctx
ctx (Def a b) = subref a b
ctx x = Seq x

beta :: AST -> AST
beta (Appl a b) = case beta a of
    Fn s t -> subref s b t
    x -> Appl x b
beta x = astrec beta x

undef :: AST -> AST
undef (Seq a b) = ctx a b
undef x = astrec undef x

flatten :: AST -> Maybe String
flatten (Seq a b) = (++) <$> flatten a <*> flatten b
flatten (Lit t) = Just $ t
flatten x = Nothing

eval = flatten . beta . undef
