module Text.IndentedString.QQ
  ( indented
  , deindent
  , deindentLines
  ) where

import Data.Char (isSpace)

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH

-- | A QuasiQuoter that produces a string literal with the quasiquoted contents,
-- but without the initial indentation. Indentation level of all lines (in spaces)
-- is decreased by the level of minimally-indented line. Tabs are not supported,
-- they don't count as indentation.
--
-- Example:
-- 
-- @
-- [indented|
--   foo
--     bar
--   baz
-- |]
-- @
--
-- will produce the string `"foo\n  bar\nbaz"`.
indented :: TH.QuasiQuoter
indented = TH.QuasiQuoter
  { TH.quoteExp = pure . TH.LitE . TH.StringL . deindent
  , TH.quotePat = pure . TH.LitP . TH.StringL . deindent
  , TH.quoteType = pure . TH.LitT . TH.StrTyLit . deindent
  , TH.quoteDec = error "indented: Declarations not supported"
  }

deindent :: String -> String
deindent = unlines . deindentLines . dropInitialBlankLine . dropLastBlankLine . lines

dropLastBlankLine :: [String] -> [String]
dropLastBlankLine = reverse . dropInitialBlankLine . reverse

dropInitialBlankLine :: [String] -> [String]
dropInitialBlankLine (x : xs)
  | all isSpace x = xs
  | otherwise     = x : xs

deindentLines :: [String] -> [String]
deindentLines lines =
  let maxPrefix = minimum $ map prefixLength $ filter (not . null) lines
  in map (drop maxPrefix) lines

prefixLength :: String -> Int
prefixLength = length . takeWhile (==' ')
