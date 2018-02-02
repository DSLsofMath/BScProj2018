
module Helper
( nStycken
, len
)
where

nStycken :: (Integral n) => n -> a -> [a]
nStycken n = take (fromIntegral n) . repeat

len :: (Integral n) => [a] -> n
len [] = 0
len (a:as) = 1 + len as
