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
bigmapNew = mempty
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
