{-# LANGUAGE FlexibleInstances #-}
module MusicTypes where

import Data.Trie       
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Char (toLower)

-- Think About: Add synonyms for artists, labels, genres. For example "UK Hardcore"
-- could be a synonym for "Happy Hardcore". "DnB", "Drum and Bass", "Drum 'n' Bass", etc.   
       
----------------------
-- Type Definitions --
----------------------       

data Track = Track { artists  :: [String]
                   , title    :: String
                   , location :: FilePath
                   , bpm      :: Int   -- to simplify things. Set to whatever if no bpm info available.
                   , duration :: Int   -- in seconds
                   , cues     :: [Int]
                   , genre    :: String }       

emptyTrack :: FilePath -> Track
emptyTrack path = Track { artists = []
                   , title = []
                   , location = path
                   , bpm = 0
                   , duration = 0
                   , cues = []
                   , genre = [] }
     
data Release = Release { tracks :: [(String,[String])] -- track name and artists in album order
                       , name   :: String
                       , year   :: Int
                       , label  :: String }

data Crate = Crate { trackIndex   :: Trie Track
                   , releaseIndex :: Trie Release
                   , labelIndex   :: Trie String
                   , artistIndex  :: Trie String
                   , genreIndex   :: Trie String
                   , releaseLookupTable :: Trie [Release] }

emptyCrate = buildCrate [] []
     
-----------------------------     
-- Call This After Parsing --
-----------------------------   
     
buildCrate :: [Track] -> [Release] -> Crate
buildCrate tcks releases = Crate { trackIndex = fromList $ zip trackKeys tcks      -- might need unique keys for tracks with same name.
                                 , releaseIndex = fromList $ zip releaseKeys releases -- might need unique keys for releases with same name.
                                 , labelIndex = stringTrie (map label releases)
                                 , artistIndex = stringTrie (concatMap artists tcks)
                                 , genreIndex = stringTrie (map genre tcks)
                                 , releaseLookupTable = fromList $ map (\t -> ((toKey t),(trackReleases t))) tcks }
                                   -- the last one is for track -> release lookup.
  where
    trackReleases :: Track -> [Release]
    trackReleases t = filter (\r -> toKey t `elem` (map (uncurry trackKey) (tracks r))) releases
    trackKeys = map (toKey . title) tcks
    releaseKeys = map (toKey . name) releases
    stringTrie strings = fromList $ zip (map toKey strings) strings


------------------------  
-- For Building Tries --
------------------------     
  
class Keyable a where
  toKey :: a -> ByteString

instance Keyable String where
  toKey = pack . (map toLower)

instance Keyable Track where -- we do the "--artists" thing to get unique keys for tracks with the same name by different artists.
  toKey t = trackKey (title t) (artists t)

instance Keyable Release where -- as with tracks, we want unique keys for releases with the same name on different labels
  toKey r = pack $ (map toLower (name r)) ++ "--" ++ (label r)

trackKey :: String -> [String] -> ByteString
trackKey title artists = pack $ (map toLower title) ++ "--" ++ (concat artists)

----------------------------         
-- For Looking For Things --
----------------------------            
         
matchTitle :: Crate -> String -> [Track]
matchTitle c x = map snd $ toList (submap (toKey x) (trackIndex c))

matchArtist :: Crate -> String -> [String]
matchArtist c x = map snd $ toList (submap (toKey x) (artistIndex c))

matchGenre :: Crate -> String -> [String]
matchGenre c x = map snd $ toList (submap (toKey x) (genreIndex c))

matchRelease :: Crate -> String -> [Release]
matchRelease c x = map snd $ toList (submap (toKey x) (releaseIndex c))

matchLabel :: Crate -> String -> [String]
matchLabel c x = map snd $ toList (submap (toKey x) (labelIndex c))           
           
type TrackFilter = Track -> Bool

instance Monoid TrackFilter where
  mempty = const True
  mappend a b = (\x -> (a x) && (b x))

filterArtist :: String -> TrackFilter
filterArtist a = (\t -> (map toLower a) `elem` (map (map toLower) (artists t)))

filterGenre :: String -> TrackFilter
filterGenre g = (\t -> (map toLower g) == (map toLower (genre t)))

filterBPM :: Int -> Int -> TrackFilter
filterBPM high low = (\t -> ((bpm t) <= high) && ((bpm t) >= low))

type ReleaseFilter = Release -> Bool

filterLabel :: String -> ReleaseFilter
filterLabel l = (\r -> (map toLower l) == (map toLower (label r)))

filterYear :: Int -> Int -> ReleaseFilter
filterYear oldest newest = (\r -> ((year r) <= newest) && ((year r) >= oldest))

findReleases :: Crate -> Track -> [Release]
findReleases c t = case Data.Trie.lookup (toKey t) (releaseLookupTable c) of
                     Nothing -> []
                     Just rs -> rs
  
findTracks :: Crate -> Release -> [Track]        
findTracks c r = map (\(Just x) -> x) $ filter something $ 
                   map ((\x -> Data.Trie.lookup x (trackIndex c)) . (uncurry trackKey))
                     (tracks r)
  where
    something :: Maybe a -> Bool
    something x = case x of
                   Nothing -> False
                   _       -> True

-----------------------
-- For Adding Things --  
-----------------------
        
addTrack :: Crate -> Track -> Crate -- also have to regenerate the artist, genre indices if new ones added.
addTrack c t = c { trackIndex = insert (toKey t) t (trackIndex c)
                 , artistIndex = foldr (\a x -> insert (toKey a) a x) (artistIndex c) (artists t)
                 , genreIndex = foldr (\a x -> insert (toKey a) a x) (artistIndex c) (artists t)}


            

addRelease :: Crate -> Release -> Crate
addRelease c r = c { releaseIndex = insert (toKey r) r (releaseIndex c)
                   , labelIndex = insert (toKey (label r)) (label r) (labelIndex c)
                   , releaseLookupTable = foldr nodef (releaseLookupTable c) (tracks r) }
  where
    nodef :: (String,[String]) -> Trie [Release] -> Trie [Release]
    nodef tinfo table = case Data.Trie.lookup ((uncurry trackKey) tinfo) (releaseLookupTable c) of
                          Nothing -> insert ((uncurry trackKey) tinfo) [r] table    -- if only release for this track
                          Just rs -> insert ((uncurry trackKey) tinfo) (r:rs) table -- if other releases exist, add this one
            

      

                     
     
        

