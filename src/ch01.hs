module Main where

-- challenge 1: the identity function, works for any type `a`
identity :: a -> a
identity x = x

-- challenge 2: the composition function, takes two functions, returns a new one: "g after f"
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose g' f' = \x -> g' (f' x)

-- an infix version that reads more like math: g <.> f
infixr 9 <.>
(<.>) :: (b -> c) -> (a -> b) -> (a -> c)
(<.>) = compose

-- challenge 3: test that composition respects identity (plus associativity)
-- agreeOn xs p q: p and q give the same output on every x in xs
agreeOn :: Eq b => [a] -> (a -> b) -> (a -> b) -> Bool
agreeOn xs p q = all (\x -> p x == q x) xs

-- some examples
ints :: [Int]
ints = [-3 .. 10]

strs :: [String]
strs = ["scientists", "reveal", "the", "hi"]

f :: Int -> Int
f = (* 10)

g :: Int -> String
g = show

h :: String -> Int
h = length

-- same type as identity on Int, but not identity so should fail the law
incr :: Int -> Int
incr = (+ 1)

testChallenge3 :: [(String, Bool, Bool)]          -- (name, result, expected)
testChallenge3 =
  [ ("f <.> identity == f",                       agreeOn ints (f <.> identity) f,                  True)
  , ("identity <.> f == f",                       agreeOn ints (identity <.> f) f,                  True)
  , ("g <.> identity == g  (Int side)",           agreeOn ints (g <.> identity) g,                  True)
  , ("identity <.> g == g  (String side)",        agreeOn ints (identity <.> g) g,                  True)
  , ("identity <.> reverse == reverse",           agreeOn strs (identity <.> reverse) reverse,      True)
  , ("assoc: h <.> (g <.> f) == (h <.> g) <.> f", agreeOn ints (h <.> (g <.> f)) ((h <.> g) <.> f), True)
  , ("incr: f <.> incr == f ?",                   agreeOn ints (f <.> incr) f,                     False)
  ]

main :: IO ()
main = do
  putStrLn "challenge 3: composition vs identity (plus associativity)"
  mapM_ (\(name, got, want) ->
           putStrLn ((if got == want then "  ok   " else "  FAIL ") ++ name ++ "  ->  " ++ show got))
        testChallenge3
