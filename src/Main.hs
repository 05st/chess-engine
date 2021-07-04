module Main where

import Data.List
import Data.Char (isSpace)

data Tag = Event String | Site String | Date String | Round Int | White String | Black String | Result String deriving (Show)

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

filterComments :: String -> String
filterComments = takeWhile (/= ';')

parseTag :: String -> [Tag]
parseTag line = return tag
    where inputs = words . init . tail $ line
          tagType = head inputs
          field = init . tail . intercalate " " . tail $ inputs
          tag = case tagType of "Event" -> Event field
                                "Site" -> Site field
                                "Date" -> Date field
                                "Round" -> Round (read field)
                                "White" -> White field
                                "Black" -> Black field
                                "Result" -> Result field
                                _ -> error "Invalid tag type"

main :: IO ()
main = do
    contents <- readFile =<< getLine

    let processedInput = filter (not . null) . map (trim . filterComments) . lines $ contents
    let tags = concatMap parseTag . takeWhile ((== '[') . head) $ processedInput

    let move_lines = words . intercalate " " . dropWhile ((== '[') . head) $ processedInput

    print $ move_lines
    putStrLn $ "Tags: " ++ (show tags)

