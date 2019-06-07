{-#LANGUAGE RankNTypes#-}
module FileSystem
    ( FS (..)
    , getDir
    , name
    , contents
    , ls
    , cd
    , file
    , replaceFileExtension
    , deleteDirectory
    , lsRecursive
    ) where

import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeFileName, splitPath)
import System.FilePath.Posix (replaceExtension)
import Lens.Micro (Lens', Traversal', traversed, filtered, (^.), (^..), (%~), (.~), (&))

--Task 6
data FS 
    = Dir 
      { _name :: FilePath
      , _contents :: [FS]
      }
    | File
      { _name :: FilePath
      }
    deriving (Show, Eq) 

name :: Lens' FS FilePath
name f s = let a = _name s
               rebuildWith b = s {_name = b} 
           in
               rebuildWith <$> f a

contents :: Lens' FS [FS]
contents f s = let a = _contents s
                   rebuildWith b = s {_contents = b} 
               in
                   rebuildWith <$> f a

getDir :: FilePath -> IO FS
getDir path = do
    isD <- doesDirectoryExist path
    if (isD)
      then (do
            scope <- listDirectory path
            Dir (last $ splitPath path) <$> mapM (getDir . (</>) path) scope)
      else (do
            isF <- doesFileExist path
            if (isF)
              then pure $ File $ takeFileName path
              else ioError $ userError "No such file or directory")

--Task 7
isFile :: FS -> Bool
isFile (File _) = True
isFile _ = False

isDir :: FS -> Bool
isDir (File _) = False
isDir _ = True

contentsFiltered :: (FS -> Bool) -> Traversal' FS FS
contentsFiltered pr = contents . traversed . filtered pr

cd :: FilePath -> Traversal' FS FS
cd nm = contentsFiltered (\x -> isDir x && x ^. name == nm)

file :: FilePath -> Traversal' FS FilePath
file nm = contentsFiltered (\x -> isFile x && x ^. name == nm) . name

ls :: Traversal' FS FilePath
ls = contents . traversed . name

--Task 8
replaceFileExtension :: FilePath -> FS -> FS
replaceFileExtension ext = contents . traversed . filtered isFile . name %~ (flip replaceExtension ext)

deleteDirectory :: FilePath -> FS -> FS
deleteDirectory nm fs = fs & contents .~ (fs ^.. contentsFiltered (\x -> not $ x ^. name == nm && isDir x && null (x ^. contents)))

lsRecursive :: FS -> [FilePath]
lsRecursive fs = fs ^.. contentsFiltered isFile . name ++ (fs ^.. contentsFiltered isDir >>= lsRecursive) 
--такая реализация не будет выводить названия папок, если хочется еще и названия папок, то можно сделать так:
--lsRecursive fs = _name fs : fs L.^.. contentsFiltered isFile . name ++ (fs L.^.. contentsFiltered isDir >>= lsRecursive) 