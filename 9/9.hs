-- contigHead("aaabbbaaa") -> "aaa"
-- chop("aaabbbaaa") -> "aaa", "bbbaaa"
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
    | otherwise = ("foo", "bar")

pack :: [Char] -> [[Char]]
pack str
    | length str == 0 = [""]
    | otherwise = [str]
