module Main where
import Base
import Map
import System

data Charser a = Charser { getCharser :: String -> Either String (a, String) }
instance Functor Charser where fmap f (Charser x) = Charser $ fmap (first f) . x
instance Applicative Charser where
  pure a = Charser \s -> Right (a, s)
  f <*> x = Charser \s -> do
    (fun, t) <- getCharser f s
    (arg, u) <- getCharser x t
    pure (fun arg, u)
instance Monad Charser where
  Charser f >>= g = Charser $ (good =<<) . f
    where good (r, t) = getCharser (g r) t
  return = pure
instance Alternative Charser where
  empty = Charser \_ -> Left ""
  (<|>) x y = Charser \s -> either (const $ getCharser y s) Right $ getCharser x s

bad = Charser . const . Left

next = Charser \case
  [] -> Left "got EOF"
  h:t -> Right (h, t)

sat f = Charser \case
  h:t | f h -> Right (h, t)
  _ -> Left "unsat"
char c = sat (c ==)
whitespace = many $ sat isSpace
eof = Charser \case
  [] -> Right ((), "")
  _ -> Left "want EOF"

digit = sat \c -> '0' <= c && c <= '9'

varuint :: Charser Int
varuint = unleb 1 0

unleb m acc = do
  d <- ord <$> next
  if d > 127 then unleb (m * 128) $ (d - 128) * m + acc else pure $ d*m + acc

leb n | n < 128 = [n]
      | otherwise = 128 + mod n 128 : leb (div n 128)

bigmapNew :: Map String (Map String String)
bigmapNew = singleton "demo" $ fromList
  [ ("helloworld", "import System\nmain = putStrLn \"Hello, World!\"\n")
  , ("edigits", [r|import Base
import System
-- Digits of e. See http://miranda.org.uk/examples.
mkdigit n | n <= 9 = chr (n + ord '0')
norm c (d:e:x)
  | e `mod` c + 10 <= c = d + e  `div` c : e' `mod` c : x'
  | otherwise           = d + e' `div` c : e' `mod` c : x'
  where (e':x') = norm (c+1) (e:x)
convert x = mkdigit h:convert t
  where (h:t) = norm 2 (0:map (10*) x)
edigits = "2." ++ convert (repeat 1)
main = putStr $ take 1024 edigits
|])
  , ("primes", [r|import Base
import System
primes = sieve [2..]
sieve (p:x) = p : sieve [n | n <- x, n `mod` p /= 0]
main = print $ take 100 $ primes
|])
  , ("queens", [r|-- Eight queens puzzle. See http://miranda.org.uk/examples.
import Base
import System
safe q b = and[not $ q==p || abs(q-p)==i|(p,i) <- zip b [1..]]
queens sz = go sz where
  go 0 = [[]]
  go n = [q:b | b <- go (n - 1), q <- [1..sz], safe q b]
main = print $ queens 8
|])
  , ("hexmaze", [r|-- https://fivethirtyeight.com/features/can-you-escape-this-enchanted-maze/
import Base
import Map
import System
maze = fromList $ concat $ zipWith row [0..]
  [ "."
  , "IF"
  , " BLUE"
  , "Z ASKS"
  , "AMY EE"
  , "DANCES"
  , " QUEEN"
  , "   Z O"
  , "     O"
  ]
  where
  row r s = concat $ zipWith (cell r) [0..] s
  cell r c x | x /= ' '  = [((r, c), x)]
             | otherwise = []
dirs = [(1, 0), (0, 0-1), (0-1, 0-1), (0-1, 0), (0, 1), (1, 1)]
turn f x = take 2 $ tail $ dropWhile (/= x) $ cycle $ f dirs
data Hex = Hex (Int, Int) (Int, Int) String
step (Hex (x, y) (xd, yd) path) =
  [Hex pos' (xd', yd') (c:path) | (xd', yd') <- next (xd, yd),
    let pos' = (x + xd', y + yd'), member pos' maze]
  where
  c = maze!(x, y)
  next = turn $ if elem c "AEIOUY" then id else reverse

bfs moves = case asum $ won <$> moves of
  Nothing -> bfs $ step =<< moves
  Just soln -> reverse soln
  where
  won (Hex pos _ path)
    | maze!pos == '.' && elem 'M' path = Just path
    | otherwise = Nothing

main = putStrLn $ bfs [Hex (5, 0) (1, 1) ""]
|])
  , ("douady", [r|-- Based on https://sametwice.com/4_line_mandelbrot.
import Base
import System
prec :: Int
prec = 16384
infixl 7 #
x # y = x * y `div` prec
sqAdd (x, y) (a, b) = (a#a - b#b + x, 2*(a#b) + y)
norm (x, y) = x#x + y#y
douady p = null . dropWhile (\z -> norm z < 4*prec) . take 30 $ iterate (sqAdd p) (0, 0)
main = putStr $ unlines
  [[if douady (616*x - 2*prec, 1502*y - 18022)
    then '*' else ' ' | x <- [0..79]] | y <- [0..23]]
|])
  , ("enigma", [r|-- https://crypto.stanford.edu/~blynn/haskell/enigma.html
import Base
import System
wI   = ("EKMFLGDQVZNTOWYHXUSPAIBRCJ", "Q")
wII  = ("AJDKSIRUXBLHWTMCQGZNPYFVOE", "E")
wIII = ("BDFHJLCPRTXVZNYEIWGAKMUSQO", "V")
wIV  = ("ESOVPZJAYQUIRHXLNFTGKDCMWB", "J")
wV   = ("VZBRGITYUPSDNHLXAWMJQOFECK", "Z")
ukwA = "EJMZALYXVBWFCRQUONTSPIKHGD"
ukwB = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
ukwC = "FVPJIAOYEDRZXWGCTKUQSBNMHL"

abc = ['A'..'Z']
abc2 = abc ++ abc
sub p x   = maybe x id $ lookup x $ zip abc p
unsub p x = maybe x id $ lookup x $ zip p abc
shift k   = sub   $ dropWhile (/= k) $ abc2
unshift k = unsub $ dropWhile (/= k) $ abc2
conjugateSub p k = unshift k . sub p . shift k
rotorSubs gs = zipWith conjugateSub (fst <$> rotors) gs
rotors = [wI, wII, wIII]
zap gs = unsub p . sub ukwB . sub p where
  p = foldr1 (.) (rotorSubs gs) <$> abc
turn gs@[_, g2, g3] = zipWith (bool id $ shift 'B') bs gs where
  [_, n2, n3] = snd <$> rotors
  bs = [g2 `elem` n2, g2 `elem` n2 || g3 `elem` n3, True]
enigma grundstellung = zipWith zap $ tail $ iterate turn grundstellung
main = interact $ enigma "AAA"
|])
  ]
bigmap = unsafePerformIO $ newIORef bigmapNew

str = varuint >>= (`replicateM` next)

getCharserEof p s = fst <$> getCharser (p <* eof) s

foreign export ccall "canister_query lscap" lscap
lscap = print . keys =<< readIORef bigmap

foreign export ccall "canister_query ls" ls
ls = do
  s <- getContents
  case getCharserEof str s of
    Left e -> putStr e
    Right cap -> do
      m <- readIORef bigmap
      let files = maybe [] keys $ mlookup cap m
      mapM (putChar . chr) $ leb $ length files
      let
        lenc s = do
          mapM (putChar . chr) $ leb $ length s
          putStr s
      mapM_ lenc files

writeArgs = (\a b c -> (a,b,c)) <$> str <*> str <*> str

foreign export ccall "canister_update save" save
save = do
  s <- getContents
  case getCharserEof writeArgs s of
    Left e -> putStr e
    Right (a,b,c) -> do
      m <- readIORef bigmap
      let
        mm = maybe mempty id $ mlookup a m
      writeIORef bigmap $ insert a (insert b c mm) m

foreign export ccall "canister_query cat" cat
cat = do
  s <- getContents
  case getCharserEof ((,) <$> str <*> str) s of
    Left e -> putStr e
    Right (cap,file) -> do
      m <- readIORef bigmap
      case mlookup cap m of
        Nothing -> pure ()
        Just mm -> putStr $ maybe "" id $ mlookup file mm
