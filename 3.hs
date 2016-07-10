elementAt :: ([a],Int) -> a
-- elementAt (l,i) = last(take i l)
elementAt (l,i) = head(drop (i-1) l)
