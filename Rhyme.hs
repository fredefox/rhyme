{-# Language UnicodeSyntax, BangPatterns, StandaloneDeriving,
  LambdaCase, GeneralizedNewtypeDeriving, NamedWildCards,
  ExplicitForAll, TypeApplications, TemplateHaskell, OverloadedStrings
#-}
module Rhyme (main) where

import Prelude hiding (Word)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Category ((>>>), (<<<))
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.List
import System.Console.Haskeline
import Data.Function
import Text.Printf
import Data.FileEmbed
import Data.Text (Text)
import qualified Data.Text as Text
import Data.String.Conversions (convertString)
import Text.PrettyPrint.ANSI.Leijen

prettyPrint ∷ Pretty a ⇒ a → IO ()
prettyPrint = putDoc . pretty

-- newtype Phoneme = Phoneme String
type Phoneme = Text

phoneme ∷ Text → Phoneme
phoneme = id

newtype Word = Word Text

instance Pretty Word where
  pretty (Word w) = text $ Text.unpack w

deriving instance Eq Word
deriving instance Ord Word

{-# NOINLINE dict #-}
dict ∷ Map Word [Phoneme]
dict
  = Map.fromList
  . catMaybes
  . map (go . Text.words)
  . Text.lines
  $! dictData
  where
  go ∷ [Text] → Maybe (Word, [Phoneme])
  go [] = Nothing
  go (x:xs) = Just $ (Word x, map phoneme xs)
  dictData ∷ Text
  dictData = convertString $(embedFile "cmudict-0.7b")

caseInsensitive ∷ Text → Word
caseInsensitive = Word . Text.map toUpper

findRhymes ∷ Word → IO ()
findRhymes w = case Map.lookup w dict of
  Nothing → printf "Word not found\n"
  Just a
    → report w
    $ take 10
    $ filter (\(_, b) → b /= 0)
    $ reverse
    $ sortBy (compare `on` snd)
    $ map (valuation a)
    $ Map.toList dict

putDocLn ∷ Doc → IO ()
putDocLn doc = putDoc doc >> putStr "\n"

report ∷ Word → [(Word, Int)] → IO ()
report w mp = putDocLn $ hang 2 $ vsep
  $  [pretty w <+> "rhymes with:"]
  <> map (\(Word w, d) → text (Text.unpack w) <+> parens (int d)) mp
  
valuation ∷ [Phoneme] → (Word, [Phoneme]) → (Word, Int)
valuation ps0 (w1, ps1) = (w1, rhymes ps0 ps1)

app ∷ Text → IO ()
app = Text.words >>> map caseInsensitive >>> mapM_ findRhymes

main :: IO ()
main =
  runInputT defaultSettings $ forever $ do
    m <- getInputLine "§ "
    case m of
      Nothing → pure ()
      Just a → liftIO $ app (Text.pack a)

type List = []

-- | @'rhymes' xs ys@ determines if @xs@ rhymes with @ys@, by matching
-- phonemes from both.
rhymes ∷ List Phoneme → List Phoneme → Int
rhymes xs ys = go (reverse xs) (reverse ys)
  where
    go ∷ List Phoneme → List Phoneme → Int
    go xsR ysR = case (xsR, ysR) of
      ([]    , [])    → 0 -- True
      ([]    , _:_)   → 0 -- False
      (_:_   , [])    → 0 -- False
      (x:xss ,y:yss ) → -- x == y && go xss yss
        if x == y
        then succ (go xss yss)
        else 0
