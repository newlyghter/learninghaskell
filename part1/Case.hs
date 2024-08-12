module Case where

describe :: Integer -> String
describe n = case n of 0 -> "zero"
                       1 -> "one"
                       2 -> "an even prime"
                       n -> "the number " ++ show n

parseCountry :: String -> Maybe String
parseCountry "FI" = Just "Finland"
parseCountry "SE" = Just "Sweden"
parseCountry _    = Nothing

flyTo :: String -> String
flyTo countryCode = case parseCountry countryCode of Just country -> "You're flying to " ++ country
                                                     Nothing -> "You're not flying anywhere"
flyTo' :: String -> String
flyTo' countryCode = handleResult (parseCountry countryCode)
  where handleResult (Just country) = "You're flying to " ++ country
        handleResult Nothing        = "You're not flying anywhere."

-- given a sentence, decide whether it is a statement, question or exclamation
sentenceType :: String -> String
sentenceType sentence = case last sentence of '.' -> "statement"
                                              '?' -> "question"
                                              '!' -> "exclamation"
                                              _   -> "not a sentence"

-- same function, helper function instead of case-of
sentenceType' sentence = classify (last sentence)
  where classify '.' = "statement"
        classify '?' = "question"
        classify '!' = "exclamation"
        classify _   = "not a sentence"
