module Parser where

type Parser a = [a] -> Maybe (a, [a], [a])

_divList :: (a -> Bool) -> [a] -> Parser a
_divList p (x : xs1) xs2 -- xs2 には初期値 [] を入れる
    | p x       = Just (x, xs2, xs1)
    | otherwise = _divList p xs1 (xs2 ++ [x])
_divList _ [] _ = Nothing

divList :: (a -> Bool) -> Parser a
divList p xs = _divList p xs []

divListBy :: Eq a => a -> Parser a
divListBy x xs = divList (== x) xs

_divListInto :: Eq a => a -> [a] -> [[a]] -> [[a]]
_divListInto x xs res = -- res は初期値 []
    case divListBy x xs of
        Nothing -> (res ++ [xs])
        Just (_, ys, zs) ->
            _divListInto x zs (res ++ [ys])

divListInto :: Eq a => a -> [a] -> [[a]]
divListInto x xs = _divListInto x xs []
