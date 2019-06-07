import Test.Hspec
import Lib 
  ( _1
  , _2
  , set
  , view
  , over
  , (<%~)
  , (<<%~)
  )
import FileSystem
  ( FS (..)
  , ls
  , cd
  , file
  , replaceFileExtension
  , deleteDirectory
  , lsRecursive
  )
import Data.Char (toUpper)
import Lens.Micro ((^?), (^..))
import Control.Applicative (liftA, liftA2)

main :: IO ()
main = hspec $ do spec

spec :: Spec
spec = do
        let testDir1 = Dir "kek" [Dir "lol" [], Dir "lel" [File "zaz", File "zaz.kek"], File "mda", File "mdamda.mda"] 
        describe "Set" (do
          it "Set left 1" $ 
            set _1 3 (-1 :: Int, 5 :: Int) `shouldBe` (3, 5)
          it "Set left 2" $ 
            set _1 "lol" ("asd", "fgh") `shouldBe` ("lol", "fgh")
          it "Set right 1" $ 
            set _2 3 (-1 :: Int, 5 :: Int) `shouldBe` (-1, 3)
          it "Set right 2" $ 
            set _2 "lol" ("asd", "fgh") `shouldBe` ("asd", "lol"))
        describe "View" (do
          it "View left 1" $ 
            view _1 (-1 :: Int, 5 :: Int) `shouldBe` -1
          it "View left 2" $ 
            view _1 ("asd", "fgh") `shouldBe` "asd"
          it "View right 1" $ 
            view _2 (-1 :: Int, 5 :: Int) `shouldBe` 5
          it "View right 2" $ 
            view _2 ("asd", "fgh") `shouldBe` "fgh")
        describe "Over" (do
          it "Over left 1" $ 
            over _1 (+ 3) (-1 :: Int, 5 :: Int) `shouldBe` (2, 5)
          it "Over left 2" $ 
            over _1 (++ "kek") ("asd", "fgh") `shouldBe` ("asdkek", "fgh")
          it "Over right 1" $ 
            over _2 (+ 3) (-1 :: Int, 5 :: Int) `shouldBe` (-1, 8)
          it "Over right 2" $ 
            over _2 (++ "kek") ("asd", "fgh") `shouldBe` ("asd", "fghkek"))
        describe "<%~ and <<%~" (do
          it "<%~ 1" $ 
            (<%~) _1 (\_ -> "zzz") (-1 :: Int, 5 :: Int) `shouldBe` ("zzz", ("zzz", 5))
          it "<%~ 2" $ 
            (<%~) _2 (\x -> map toUpper x) ("asd", "fgh") `shouldBe` ("FGH", ("asd", "FGH"))
          it "<<%~ 1" $ 
            (<<%~) _1 (\_ -> "zzz") (-1 :: Int, 5 :: Int) `shouldBe` (-1, ("zzz", 5))
          it "<<%~ 2" $ 
            (<<%~) _2 (\x -> map toUpper x) ("asd", "fgh") `shouldBe` ("fgh", ("asd", "FGH")))
        describe "ls, cd, file" (do
          it "ls" $ 
            testDir1 ^.. ls `shouldBe` ["lol", "lel", "mda", "mdamda.mda"]
          it "ls with cd in empty directory" $ 
            testDir1 ^.. cd "wtf" . ls `shouldBe` []
          it "ls with cd in non-empty directory" $ 
            testDir1 ^.. cd "lel" . ls `shouldBe` ["zaz", "zaz.kek"]
          it "existing file" $ 
            testDir1 ^? cd "lel" . file "zaz" `shouldBe` Just "zaz"
          it "not an existing file" $ 
            testDir1 ^? cd "lel" . file "zaaz" `shouldBe` Nothing)
        describe "replaceFileExtension" (do
          it "replaceFileExtension 1" $ 
            replaceFileExtension "lol" testDir1 `shouldBe` Dir "kek" [Dir "lol" [], Dir "lel" [File "zaz", File "zaz.kek"], File "mda.lol", File "mdamda.lol"] 
          it "replaceFileExtension 2" $ 
            liftA2 replaceFileExtension (Just "lol") (testDir1 ^? cd "lel") `shouldBe` Just (Dir "lel" [File "zaz.lol", File "zaz.lol"]))
        describe "deleteDirectory" (do
          it "Deleting an empty directory" $ 
            deleteDirectory "lol" testDir1 `shouldBe` Dir "kek" [Dir "lel" [File "zaz", File "zaz.kek"], File "mda", File "mdamda.mda"] 
          it "Trying to delete an non-empty directory" $ 
            deleteDirectory "lel" testDir1 `shouldBe` Dir "kek" [Dir "lol" [], Dir "lel" [File "zaz", File "zaz.kek"], File "mda", File "mdamda.mda"] 
          it "Trying to delete a file" $ 
            liftA2 deleteDirectory (Just "zaz") (testDir1 ^? cd "lel") `shouldBe` Just (Dir "lel" [File "zaz", File "zaz.kek"]))
        describe "lsRecursive" (do
          it "lsRecursive 1" $ 
            lsRecursive testDir1 `shouldBe` ["mda", "mdamda.mda", "zaz", "zaz.kek"]
          it "lsRecursive 2" $ 
            liftA lsRecursive (testDir1 ^? cd "lel") `shouldBe` Just ["zaz", "zaz.kek"])