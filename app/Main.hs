module Main where

import Text.Pandoc.JSON
import Debug.Trace
import Data.Maybe
import Data.Functor((<&>))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Text.Pandoc.Builder (Inlines, HasMeta (deleteMeta))
import Text.Pandoc.Walk (Walkable(walk))

main :: IO ()
main = toJSONFilter meta

meta :: Pandoc -> IO Pandoc
meta p@(Pandoc meta bs) =
  let
    citeIds = citeKeyMapping meta
  in do
    traceIO $ show $ M.lookup (T.pack "references") (unMeta meta)
    -- traceIO $ show citeIds
    return
      $ deleteMeta (T.pack "references")
      $ walk (adjustCitation citeIds) p

adjustCitation :: CiteKeyMapping -> Inline -> Inline
adjustCitation m (Cite cs is) =
  let findById c = M.lookup (CiteId . citationId $ c) m
      update c = case findById c of
        Just (CitationKey key) -> c { citationId = key }
        Nothing  -> c
  in Cite (update <$> cs) is
adjustCitation _ x = x

newtype CiteId = CiteId T.Text
  deriving (Eq, Ord, Show)

newtype CitationKey = CitationKey T.Text
  deriving (Eq, Ord, Show)

type CiteKeyMapping = M.Map CiteId CitationKey

citeKeyMapping :: Meta -> CiteKeyMapping
citeKeyMapping m =
  let
    map  = unMeta m
  in
    case M.lookup (T.pack "references") map of
      Just (MetaList refs) ->
        M.fromList $ refIds `mapMaybe` refs
      Nothing -> M.empty

refIds :: MetaValue -> Maybe (CiteId, CitationKey)
refIds (MetaMap m) = do
  pure (,)
  <*> (CiteId <$> lookupAsText (T.pack "id") m)
  <*> (CitationKey <$> lookupAsText (T.pack "citation-key") m)
refIds _ = Nothing

lookupAsText:: (Ord k) => k -> M.Map k MetaValue -> Maybe T.Text
lookupAsText k m = do
  v <- M.lookup k m
  case v of
    MetaString s -> return s
    MetaInlines is -> toText is
    _ -> Nothing

toText :: [Inline] -> Maybe T.Text
toText [] = mempty
toText ts = pure $ foldMap str ts
  where
    str (Str t) = t
    str Space   = T.pack " "
    str _       = T.empty