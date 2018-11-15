
{-# LANGUAGE
    LambdaCase
  , OverloadedStrings
#-}


module Main
  where


import System.Environment
  ( getArgs
  )

import Data.Aeson
  ( Value (..)
  , ToJSON
  , object
  , (.=)
  , toJSON
  )

import Data.Aeson.Encode.Pretty
  ( encodePretty'
  , defConfig
  , confIndent
  , Indent (..)
  )

import Data.ByteString.Lazy
  ( ByteString
  )

import qualified Data.ByteString.Lazy.Char8 as BS
  ( putStrLn
  )

import Text.HTML.TagSoup
  ( Tag (..)
  , fromTagText
  , innerText
  , partitions
  , parseTags
  , (~==)
  , (~/=)
  )


type STag   = Tag String

type Result = [(STag, [(STag, [(String, String)])])]


pick :: (a -> Bool) -> ([a] -> [b]) -> [a] -> [b]
pick s f = concatMap f . partitions s


pickString :: String -> ([STag] -> [a]) -> [STag] -> [a]
pickString x = pick (~== x)


prettifyJson :: ToJSON a => a -> ByteString
prettifyJson = encodePretty' (defConfig { confIndent = Spaces 1 })


getDays :: [STag] -> [(STag, [(STag, [(String, String)])])]
getDays = pickString "<div class='day'>" $
  \ case
    _ : _ : _ : x : xs  -> [(x, getChannels xs)]
    _                   -> []


getChannels :: [STag] -> [(STag, [(String, String)])]
getChannels = pickString "<h3>" $
  \ case
    _ : x : xs -> [(x, getPrograms xs)]
    _          -> []


getPrograms :: [STag] -> [(String, String)]
getPrograms = pickString "<div>" $
  \ ts ->
    let time_and_name =
          map (takeWhile (~/= ("</span>" :: String))) $
            partitions (~== ("<span>" :: String)) ts
    in
    case time_and_name of
      [t, n] -> [(innerText n, innerText t)]
      _      -> []


programmingJson (name, time) =
  object
    [ "name"     .= toJSON name
    , "timeslot" .= toJSON time
    ]


channelProgrammingJson (channel, programming) =
  object
    [ "channel"     .= fromTagText channel
    , "programming" .= toJSON (map programmingJson programming)
    ]


daysProgrammingJson days =
  let object_pattern = \ day channels ->
        object
          [ "day"      .= fromTagText day
          , "channels" .= toJSON (map channelProgrammingJson channels)
          ]
  in
  toJSON [ object_pattern day channels | (day, channels) <- days ]


main :: IO ()
main =
  do
    cmdArgs <- getArgs
    case cmdArgs of
      []     ->
        do
          BS.putStrLn "!!! supply a file path"
          return ()
      fp : _ ->
        let input = if (fp == "-") then getContents else readFile fp
        in
        do
          rs <- (getDays . parseTags) <$> input
          BS.putStrLn $ prettifyJson $ daysProgrammingJson rs

