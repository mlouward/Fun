module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

readNucleotide :: Char -> Nucleotide
readNucleotide x =
    case x of
        'A' -> A
        'C' -> C
        'G' -> G
        'T' -> T

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts [] = Left "Empty DNA strand"
nucleotideCounts xs = Right (fromList [(x,c) | x <- [C, G, T, A],
                                       let nuc = map readNucleotide xs,
                                       let c = (length.filter (==x)) nuc])
