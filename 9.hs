-- contigHead("aaabbbaaa") -> "aaa"
-- chop("aaabbbaaa") -> "aaa", "bbbaaa"
-- first((a, b)) -> a
-- pack("aaabbbaaa") -> "aaa", "bbb", "aaa"

contigHead :: [Char] -> [Char]
contigHead str
    | length str == 0 = ""
    | otherwise =
        if length ts == 0 then [h]
        else
            if h == head ts
            then h : contigHead ts
            else [h]
    where h : ts = str

chop :: [Char] -> ([Char], [Char])
chop str
    | length str == 0 = ("", "")
    | length str == 1 = (str, "")
    | otherwise = (hs, ts)
    where
        hs = contigHead str
        ts = drop (length hs) str

pack :: [Char] -> [[Char]]
pack str
    | length str == 0 = []
    | otherwise = [hs] ++ pack ts
    where (hs, ts) = chop str
