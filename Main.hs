{-# LANGUAGE TupleSections #-}

import Control.Monad (forM, forM_)
import Data.List (intercalate, isPrefixOf)
import System.Process (readCreateProcess, shell)
import Text.Printf (printf)


numberOfRows :: Int
numberOfRows = 100000


row_to_json :: Int -> String
row_to_json numberOfKeys
  = unlines
    [ "select row_to_json(x.*) from"
    , "\t(select"
    , intercalate ",\n" [
        printf "\t\tvalue as key_%i" i | i <- [1..numberOfKeys]
      ]
    , printf
      "\tfrom generate_series(1, %i) y(value)) x" numberOfRows
    ]


json_build_object :: Int -> String
json_build_object numberOfKeys
  = unlines
    [ "select json_build_object("
    , intercalate ",\n" [
        printf "\t'key_%i', value" i | i <- [1..numberOfKeys]
      ]
    , "\t)"
    , printf
      "from generate_series(1, %i) x(value)" numberOfRows
    ]


json_object :: Int -> String
json_object numberOfKeys
  = unlines
    [ "select json_object(array["
    , intercalate ",\n" [
        printf "\t'key_%i', value::text" i | i <- [1..numberOfKeys]
      ]
    , "\t])"
    , printf
      "from generate_series(1, %i) x(value)" numberOfRows
    ]


json_object_agg :: Int -> String
json_object_agg numberOfKeys
  = unlines
    [ "select ("
    , "\tselect json_object_agg(key, val) from ("
    , "\t\tvalues"
    , intercalate ",\n" [
        printf "\t\t\t('key_%i', value)" i | i <- [1..numberOfKeys]
      ]
    , "\t) y(key, val))"
    , printf
      "from generate_series(1, %i) x(value)" numberOfRows
    ]


bench :: String -> IO Double
bench query = do
  res <- readCreateProcess
    -- Adding `--protocol=prepared` does not change results significantly.
    (shell "pgbench --no-vacuum --protocol=prepared --time=20 --file=-")
    query

  return
    $ head
    $ map (fst . head . reads . drop (length "tps = "))
    $ filter ("tps = " `isPrefixOf`)
    $ lines res


replace :: String -> String -> String -> String
replace from to str@(s:str')
  | isPrefixOf from str = to ++ drop (length from) str
  | otherwise = s : replace from to str'
replace _ _ [] = []


main :: IO ()
main = do
  let to_jsonb = replace "json" "jsonb"
  let variants =
        [ ("row_to_json",        row_to_json)
        , ("json_build_object",  json_build_object)
        , ("json_object",        json_object)
        , ("json_object_agg",    json_object_agg)
        , ("jsonb_build_object", to_jsonb . json_build_object)
        , ("jsonb_object",       to_jsonb . json_object)
        , ("jsonb_object_agg",   to_jsonb . json_object_agg)
        ]

  forM_ variants $ \(name, fn) ->
    forM_ [2, 25, 50] $ \numberOfKeys -> do
      res <- bench $ fn numberOfKeys
      print (name, numberOfKeys, res)
