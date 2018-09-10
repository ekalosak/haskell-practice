-- string to and from tree

module Pr67 (tostr) where
import Tree(Tree(Branch, Empty), test_tree, leaf)

-- tostr :: Show a => Tree a -> [Char]
-- tostr Empty = []
-- tostr (Branch x Empty Empty) = (show x)
-- tostr (Branch x lc Empty) =
--     (show x) ++ "(" ++ (tostr lc) ++ ",)"
-- tostr (Branch x Empty rc) =
--     (show x) ++ "(," ++ (tostr rc) ++ ")"
-- tostr (Branch x lc rc) =
--     (show x) ++ "(" ++ (tostr lc) ++ "," ++ (tostr rc) ++ ")"

tostr :: Tree Char -> [Char]
tostr Empty = []
tostr (Branch x Empty Empty) = [x]
tostr (Branch x lc Empty) =
    x:("(" ++ (tostr lc) ++ ",)")
tostr (Branch x Empty rc) =
    x:("(," ++ (tostr rc) ++ ")")
tostr (Branch x lc rc) =
    x:("(" ++ (tostr lc) ++ "," ++ (tostr rc) ++ ")")

totree :: [Char] -> Tree Char
totree (x:[]) = leaf x
totree (x:xs) = let (lpar, rpar) = parsplit xs in
    Branch x (totree $ lpar) (totree $ rpar)
    -- Branch x Empty Empty

parsplit :: [Char] -> ([Char], [Char])
-- TODO
parsplit str =
    if head str == '(' && last str == ')'
    then ("asdf", "qwer")
    else error $ "Parse error: <" ++ str ++ ">"

test_string = "a(b(c,d),e(f,))"
