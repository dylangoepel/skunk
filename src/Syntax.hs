module Syntax where

import Data.Bifunctor (first, second)

data Parser a = P { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f = P . fmap (fmap (first f)) . parse

instance Applicative Parser where
    pure x = P $ Just . (,) x
    (P p) <*> (P q) = P $ \s ->
        do (f, t) <- p s
           (x, tt) <- q t
           return (f x, tt)

instance Monad Parser where
    (P p) >>= f = P $ \s ->
        do (x, t) <- p s
           parse (f x) t

fparse :: Parser a -> String -> IO a
fparse p s = case parse p s of
    Nothing -> fail $ "Unable to parse '" ++ take 20 s ++ "'"
    Just (r, t) -> if t == "" then return r else fail $ "Unable to parse '" ++ take 20 t ++ "'"

-- some general parsers and parser combinators
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = P $ \s -> case parse p s of
    Nothing -> parse q s
    x -> x

rep p = fmap (:) p <*> (rep p <|> return [])

token :: String -> Parser String
token t = P $ \s -> if take (length t) s == t
                    then Just (t, drop (length t) s)
                    else Nothing

predp :: (Char -> Bool) -> Parser String
predp p = P $ \s -> case takeWhile p s of
    "" -> Nothing
    x -> Just (x, drop (length x) s)

wsp = predp (`elem` "\t\n ") <|> return ""

symbol = token "\\" *> predp (`elem` ['a'..'z'])
litp = predp $ not . (`elem` "\\{}")

data AST = Lit String | Ref String | Appl AST AST | Seq AST AST | Fn String AST | Def String AST
    deriving (Show, Eq)

applp = foldl1 Appl <$> (rep $ (fmap Ref symbol) <|> (token "{" *> wsp *> astp <* wsp <* token "}") <|> (token "[" *> fmap Lit (predp (/= ']')) <* token "]"))

fnp = fmap Fn symbol <* token "." <* wsp <*> astp

defp = token "\\def" *> fmap Def symbol <*> (fmap Ref symbol <|> (token "{" *> wsp  *> astp <* wsp <* token "}")) <* wsp

astp = foldl1 Seq <$> rep (defp <|> fnp <|> applp <|> fmap Lit litp)

filep = astp <|> return (Lit "")
