module FrequencyTests (
        freqInfo,
        FreqInfo (..)
    ) where
import qualified Data.MultiSet as S
import qualified Data.Text.Lazy as T
import Data.Char (isLetter)
import Data.Function (on)

freqInfo :: T.Text -> FreqInfo
freqInfo = process' . map (T.filter isLetter) . T.words
    where
        process' :: [T.Text] -> FreqInfo
        process' = mconcat . map wordInfo

wordInfo :: T.Text -> FreqInfo
wordInfo =
    FreqInfo
        <$> letterFreqs
        <*> biFreqs
        <*> triFreqs
        <*> (fromIntegral . T.length)

letterFreqs :: T.Text -> S.MultiSet Char
letterFreqs = T.foldl' (flip S.insert) S.empty

biFreqs :: T.Text -> S.MultiSet (Char, Char)
biFreqs word
    | T.length word < 2 = S.empty
    | otherwise = S.fromList $ T.zip word (T.tail word)

triFreqs :: T.Text -> S.MultiSet (Char, Char, Char)
triFreqs word
    | T.length word < 3 = S.empty
    | otherwise =
        S.fromList $ zip3
            (T.unpack word)
            (T.unpack $ T.tail word)
            (T.unpack . T.tail $ T.tail word)

data FreqInfo = FreqInfo {
    letter :: S.MultiSet Char,
    bigram :: S.MultiSet (Char, Char),
    trigram :: S.MultiSet (Char, Char, Char),
    len :: Int
}

instance Monoid FreqInfo where
    mempty = FreqInfo S.empty S.empty S.empty 0
    f1 `mappend` f2 =
        FreqInfo {
            letter = (S.union `on` letter) f1 f2,
            bigram = (S.union `on` bigram) f1 f2,
            trigram = (S.union `on` trigram) f1 f2,
            len = len f1 + len f2
        }
