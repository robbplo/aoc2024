{-# LANGUAGE OverloadedStrings #-}

module Day4
  ( part1,
    part2,
  )
where

import Data.Char (isDigit, isHexDigit)
import Data.Either (fromRight)
import Data.Ix (Ix (inRange))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Read
import Data.Void
import Text.Megaparsec


data Passport = Passport
  { birthYear :: Maybe Int,
    issueYear :: Maybe Int,
    expirationYear :: Maybe Int,
    height :: Maybe Int,
    hairColor :: Maybe String,
    eyeColor :: Maybe String,
    passportID :: Maybe Int,
    countryID :: Maybe Int
  }

part1 :: String -> Int
part1 input = length $ filter valid pairs
  where
    pairs = split $ T.pack input

split :: T.Text -> [M.Map T.Text T.Text]
split str = pairs
  where
    words = map T.words $ T.splitOn "\n\n" str
    split word = (T.take 3 word, T.drop 4 word)
    toMap words = M.fromList $ map split words
    pairs = map toMap words

valid :: M.Map T.Text T.Text -> Bool
valid passport
  | size == 8 = True
  | size == 7 && M.notMember "cid" passport = True
  | otherwise = False
  where
    size = M.size passport

valid2 passport = valid $ M.filterWithKey validField passport

part2 :: String -> Int
part2 input = length $ filter valid2 pairs
  where
    pairs = split $ T.pack input

validField :: T.Text -> T.Text -> Bool
validField "byr" value = inRange (1920, 2002) $ int value
validField "iyr" value = inRange (2010, 2020) $ int value
validField "eyr" value = inRange (2020, 2030) $ int value
validField "hgt" value
  | measurement == "cm" && inRange (150, 193) num = True
  | measurement == "in" && inRange (59, 76) num = True
  | otherwise = False
  where
    (num, measurement) = intstr value
validField "hcl" value = head str == '#' && length (filter isHexDigit (tail str)) == 6
  where
    str = T.unpack value
validField "ecl" value = value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validField "pid" value = length (filter isDigit (T.unpack value)) == 9
validField _ _ = True

int value = fst $ intstr value

intstr value = fromRight (0, "") (decimal value)
