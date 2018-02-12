
module Units.Helper
( len
)
where

len :: (Integral n) => [a] -> n
len [] = 0
len (a:as) = 1 + len as
