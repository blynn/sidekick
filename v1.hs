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

varuint :: Charser Int
varuint = unleb 1 0

unleb m acc = do
  d <- ord <$> next
  if d > 127 then unleb (m * 128) $ (d - 128) * m + acc else pure $ d*m + acc

leb n | n < 128 = (chr n:)
      | otherwise = (chr (128 + mod n 128):) . leb (div n 128)

bigmapNew = singleton "demo" $ fromList
  [ ("hello world", "main = putStrLn \"Hello, World!\"\n")
  , ("candid explainer", [r|-- https://fxa77-fiaaa-aaaae-aaana-cai.raw.ic0.app/explain
-- Example input: "4449444C016b0180017502007100DEADBEEF03466F6F"
data Tree a = Node {rootLabel :: a, subForest :: [Tree a]} deriving Show

primTypes =
  [ (0x7f,"null")
  , (0x7e,"bool")
  , (0x7d,"nat")
  , (0x7c,"int")
  , (0x7b,"nat8")
  , (0x7a,"nat16")
  , (0x79,"nat32")
  , (0x78,"nat64")
  , (0x77,"int8")
  , (0x76,"int16")
  , (0x75,"int32")
  , (0x74,"int64")
  , (0x73,"float32")
  , (0x72,"float64")
  , (0x71,"text")
  , (0x70,"reserved")
  , (0x6f,"empty")
  ]

-- Parser combinators.
data Lexer a = Lexer (String -> Either String (a, String))
instance Functor Lexer where fmap f (Lexer x) = Lexer $ fmap (first f) . x
instance Applicative Lexer where
  pure x = Lexer \inp -> Right (x, inp)
  f <*> x = Lexer \inp -> case lexer f inp of
    Left e -> Left e
    Right (fun, t) -> case lexer x t of
      Left e -> Left e
      Right (arg, u) -> Right (fun arg, u)
instance Monad Lexer where
  return = pure
  x >>= f = Lexer \inp -> case lexer x inp of
    Left e -> Left e
    Right (a, t) -> lexer (f a) t
instance Alternative Lexer where
  empty = Lexer \_ -> Left ""
  (<|>) x y = Lexer \inp -> either (const $ lexer y inp) Right $ lexer x inp

lexer (Lexer f) inp = f inp
sat f = Lexer \s -> case s of
  [] -> Left "EOF"
  h:t -> if f h then Right (h, t) else Left "unsat"
char c = sat (c ==)
anyChar = sat (const True)
bad s = Lexer $ const $ Left s

-- Parser.
leb128 = leb128With 0 1 id
leb128With n b f = do
  c <- anyChar
  let d = ord c
  if d <= 127
    then pure (n + b*d, f [c])
    else leb128With (n + b*(d - 128)) (b*128) (f . (c:))

sleb128 = leb128  -- TODO: Fix this!

magic = mapM id $ char <$> "DIDL"

typeStar = do
  (n, s) <- leb128
  Node s <$> replicateM n typeLone

typeLone = do
  c <- anyChar
  let t = ord c
  if t <= 127 then
    case t of
      0x6e -> Node [c] . (:[]) <$> typeLone
      0x6d -> Node [c] . (:[]) <$> typeLone
      0x6c -> Node [c] . (:[]) <$> fieldStar
      0x6b -> Node [c] . (:[]) <$> fieldStar
      0x6a -> do
        ins <- typeStar
        outs <- typeStar
        anns <- do
          (n, s) <- leb128
          as <- replicateM n anyChar
          pure $ Node s [Node as []]
        pure $ Node [c] [ins, outs, anns]
      _ -> pure $ Node [c] []
    else bad "want type opcode <= 127"

fieldStar = do
  (n, s) <- leb128
  Node s <$> replicateM n fieldLone

fieldLone = do
  (n, s) <- leb128
  Node s . (Node (show n) []:) . (:[]) <$> typeLone

extractInt s = go 0 s where
  go n "" = n
  go n (h:t)
    | '0' <= h && h <= '9' = go (10*n + ord h - ord '0') t
    | True = go n t

value ts t = case getType t ts of
  Left e -> bad e
  Right (Node [c] kids) -> case ord c of
    0x7f -> pure $ Node "" []
    0x7e -> flip Node [] <$> replicateM 1 anyChar
    0x7d -> do
      (n, s) <- leb128
      pure $ Node s [Node (show n) []]
    0x7c -> do
      (n, s) <- sleb128
      pure $ Node s [Node (show n) []]
    0x71 -> do
      (n, s) <- leb128
      txt <- replicateM n anyChar
      pure $ Node s [Node txt []]
    0x70 -> pure $ Node "" []
    0x6e -> do
      c <- anyChar
      case ord c of
        0 -> pure $ Node [c] []
        1 -> Node [c] . (:[]) <$> value ts (head kids)
        _ -> bad "invalid opt"
    0x6d -> do
      (n, s) <- leb128
      Node s <$> replicateM n (value ts $ head kids)
    0x6c -> Node "" <$> mapM (value ts . (!!1) . subForest) (subForest $ head kids)
    0x6b -> do
      (n, s) <- leb128
      let vTypes = subForest (head kids)
      if n > length vTypes then bad $ "bad index: " ++ show n else
        Node s . (Node (show n) []:) . (:[]) <$> value ts (subForest (vTypes!!n)!!1)
    _ -> case lookup (ord c) primTypes of
      Just name -> let b = extractInt name `div` 8 in if b > 0
        then flip Node [] <$> replicateM b anyChar
        else bad $ "TODO: " ++ name
      _ -> bad $ "TODO: " ++ xxd [c]

getType t@(Node [c] kids) ts = if i < 0x68
  then if i < length ts then Right $ ts!!i else Left $ "bad index: " ++ show i
  else Right t
  where i = ord c

-- Explainer.
hexit n | n < 10 = chr $ n + ord '0'
        | True = chr $ n + ord 'a' - 10
hex2 n = hexit <$> [div n 16, mod n 16]
xxd ns = intercalate " " $ hex2 . ord <$> ns

data Line = Line { margin :: Int, lhs :: String, rhs :: String }
instance Show Line where
  show (Line m l r) = concat [replicate m ' ', xxd l, if null l then "" else ": ", r, "\n"]
indent = map \line -> line { margin = margin line + 2 }
bytes >-< desc = Line 0 bytes desc

explainStar idxF msgF kidder (Node s kids) = s >-< msgF (show $ length kids)
  : indent (zeroIndex idxF $ kidder <$> kids)

zeroIndex idxF xss = concat $ zipWith go [0..] xss where
  go n (line1:rest) = line1 { rhs = idxF (show n) ++ rhs line1 } : rest

explainType (Node [c] kids) = case lookup t primTypes of
  Nothing -> case t of
    0x6e -> [c] >-< "opt" : indent (explainType kid)
    0x6d -> [c] >-< "vec" : indent (explainType kid)
    0x6c -> (c:s) >-< ("record of size " ++ show (length grandkids))
      : indent (concat $ explainField <$> grandkids)
    0x6b -> (c:s) >-< ("variant of size " ++ show (length grandkids))
      : indent (concat $ explainField <$> grandkids)
    0x6a -> [c] >-< "function" : case kids of
      [Node insCount ins, Node outsCount outs, Node annsCount [Node anns []]] -> indent $ concat
        [ insCount >-< concat [show $ length ins, " inputs"] : (concatMap explainType ins)
        , outsCount >-< concat [show $ length outs, " outputs"] : (concatMap explainType outs)
        , [annsCount >-< concat [show $ length anns, " annotations: ", show $ ord <$> anns]]
        ]
    _ -> [[c] >-< ("type #" ++ show t)]
  Just name -> [[c] >-< name]
  where
  t = ord c
  kid@(Node s grandkids) = head kids

explainField (Node hash [Node n [], ty]) = hash >-< ("field with hash " ++ n)
  : indent (explainType ty)

chain lexFun s f = case lexer lexFun s of
  Left e -> ["" >-< ("ERROR: " ++ e)]
  Right (x, rest) -> f x rest

explainCandid s = chain magic s \x rest -> x >-< "magic header" : explainTypeList rest

explainTypeList s = chain typeStar s \typeList@(Node _ ts) rest ->
  explainStar (\idx -> concat ["[type #", idx, "] "]) ("type table of size "++) explainType typeList
  ++ explainArgList ts rest

explainArgList ts s = chain typeStar s \argList@(Node _ as) rest ->
  explainStar (\idx -> concat ["[arg #", idx, "] "]) ("arg count of "++) explainType argList
  ++ explainValues ts as rest

explainValues ts as s = chain (mapM (value ts) as) s \vs rest ->
  concat (zipWith (:) argHds (zipWith (explainValue ts) as vs))
  ++ if rest /= "" then [rest >-< "TRAILING BYTES"] else []
  where argHds = map (\i -> "" >-< (concat ["arg #", show i, ":"])) [0..]

explainValue ts a (Node v kids) = case getType a ts of
  Left e -> error "unreachable"
  Right (Node [c] tKids) -> case ord c of
    0x7f -> ["" >-< "(null)"]
    0x71 -> let [Node txt []] = kids in v >-< ("utf8 text of length " ++ show (length txt)) : indent [txt >-< show txt]
    0x70 -> ["" >-< "(reserved)"]
    0x6e -> case ord $ head v of
      0 -> [v >-< "Nothing"]
      1 -> v >-< "Just" : indent (explainValue ts (head tKids) (head kids))
    0x6d -> v >-< ("vec of length " ++ show (length kids)) : indent (concatMap (explainValue ts $ head tKids) kids)
    0x6c -> "" >-< "record:" : indent (concat $ zipWith (explainValue ts) ((!!1) . subForest <$> subForest (head tKids)) kids)
    0x6b -> v >-< ("variant field " ++ show n) : indent (explainValue ts (subForest (subForest (head tKids)!!n)!!1) $ kids!!1) where n = extractInt $ rootLabel $ head kids
    _ -> case lookup (ord c) primTypes of
      Just name -> [v >-< name]
      _ -> [[c] >-< "TODO"]

unhexit c
  | '0' <= c && c <= '9' = Right $ ord c - ord '0'
  | 'a' <= c && c <= 'f' = Right $ ord c - ord 'a' + 10
  | 'A' <= c && c <= 'F' = Right $ ord c - ord 'A' + 10
  | True = Left "bad hex digit"
unxxd "" = Right ""
unxxd (c:d:rest) = do
  h1 <- unhexit c
  h0 <- unhexit d
  (:) (chr $ h1*16 + h0) <$> unxxd rest
unxxd s = Left $ "odd byte " ++ s

pleaseExplain = either ((:[]) . ("" >-<) . ("Error: "++)) explainCandid . unxxd . filter (not . (`elem` " \n"))

foreign export ccall "canister_query go" main  -- Build a canister out of this!
main = interact $ concatMap show . pleaseExplain
|])
  , ("candid field hash", [r|-- Candid record field hash
c2w :: Char -> Word
c2w = fromIntegral . ord
main = interact \s -> show $ sum $ zipWith (*) (c2w <$> reverse s) $ iterate (223*) 1
|])
  , ("edigits", [r|-- Digits of e. See http://miranda.org.uk/examples.
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
  , ("primes", [r|primes = sieve [2..]
sieve (p:x) = p : sieve [n | n <- x, n `mod` p /= 0]
main = print $ take 100 $ primes
|])
  , ("queens", [r|-- Eight queens puzzle. See http://miranda.org.uk/examples.
safe q b = and[not $ q==p || abs(q-p)==i|(p,i) <- zip b [1..]]
queens sz = go sz where
  go 0 = [[]]
  go n = [q:b | b <- go (n - 1), q <- [1..sz], safe q b]
main = print $ queens 8
|])
  , ("hex maze", [r|-- https://fivethirtyeight.com/features/can-you-escape-this-enchanted-maze/
import Map
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
-- Example input: ATTACK AT DAWN
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

foreign export ccall "canister_query go" main
main = interact $ enigma "AAA"
|])
  , ("persist map", [r|import Map
bigmap :: IORef (Map String String)
bigmap = unsafePerformIO $ newIORef mempty

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

next = Charser \case
  [] -> Left "got EOF"
  h:t -> Right (h, t)
eof = Charser \case
  [] -> Right ((), "")
  _ -> Left "want EOF"

varuint :: Charser Int
varuint = unleb 1 0

unleb m acc = do
  d <- ord <$> next
  if d > 127 then unleb (m * 128) $ (d - 128) * m + acc else pure $ d*m + acc

leb n | n < 128 = (chr n:)
      | otherwise = (chr (128 + mod n 128):) . leb (div n 128)

lenc s = putStr $ leb (length s) s

str = varuint >>= (`replicateM` next)

getCharserEof p s = fst <$> getCharser (p <* eof) s

foreign export ccall "canister_query ls" ls
ls = do
  ks <- keys <$> readIORef bigmap
  putStr $ leb (length ks) ""
  mapM_ lenc ks

foreign export ccall "canister_update save" save
save = do
  s <- getContents
  case getCharserEof ((,) <$> str <*> str) s of
    Left e -> putStr e
    Right (k, v) -> do
      m <- readIORef bigmap
      writeIORef bigmap $ insert k v m

foreign export ccall "canister_query cat" cat
cat = do
  s <- getContents
  case getCharserEof str s of
    Left e -> putStr e
    Right k -> do
      m <- readIORef bigmap
      putStr $ maybe "" id $ mlookup k m
|])
  , ("webpage", [r|-- When run here, prints a Candid-encoded HTTP response.
-- When compiled as a canister, serves content on `...raw.ic0.app`.
html = "Hello, World!"
leb128 n | n <= 127  = (chr n:)
         | otherwise = (chr $ 128 + r:) . leb128 q
         where (q, r) = divMod n 128

candid = concat
  [ "DIDL\x03\x6c\x03"
  , "\xa2\xf5\xed\x88\x04\x01"
  , "\xc6\xa4\xa1\x98\x06\x02"
  , "\x9a\xa1\xb2\xf9\x0c\x7a"
  , "\x6d\x7b"
  , "\x6d\x6f"
  , "\x01\x00"
  , leb128 (length html) html
  , "\x00\xc8\x00"
  ]

foreign export ccall "canister_query http_request" binary
binary = putStr candid

xxd "" = id
xxd (h:t) = (hexDigit q:) . (hexDigit r:) . xxd t
  where (q, r) = divMod (ord h) 16
hexDigit n | n < 10    = chr $ n + ord '0'
           | otherwise = chr $ n - 10 + ord 'a'

main = putStr $ xxd candid ""
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
      putStr $ leb (length files) ""
      let lenc s = putStr $ leb (length s) s
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
      unless (a == "demo" && (head b /= '{' || last b /= '}'))
        $ writeIORef bigmap $ insert a (insert b c mm) m

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
