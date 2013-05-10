{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}
module Main (main) where

import           Control.Applicative ((<$>))
import qualified Control.Monad.Trans.State as TS
import           Data.Monoid ((<>),mempty,mconcat)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BLB
import           Data.Char (toUpper)
import qualified Data.Map as M
import           Data.List (intercalate)
import           Data.String (fromString)
import qualified System.Console.CmdArgs as CA
import           System.Directory (doesFileExist, createDirectoryIfMissing)
import           System.Exit (exitSuccess, exitFailure)
import           System.FilePath.Posix (replaceExtension, takeFileName, (</>))

import qualified Language.Slice.Syntax.Parser as SlcP
import           Language.Slice.Syntax.AST    as AST

data CppGenArgs = CppGenArgs { icefile   :: FilePath
                             , targetDir :: FilePath
                             , overwrite :: Bool
                             } deriving (Show, CA.Data, CA.Typeable)
                                
defaultArgs = CppGenArgs { icefile   = CA.def CA.&= CA.args CA.&= CA.typ "INPUTFILE"
                         , targetDir = ""
                         , overwrite = False
                         }


type NameState = TS.State (M.Map String String, Int)

genNm :: String -> NameState String
genNm nm = do
  (nms,i) <- TS.get
  if M.member nm nms
    then return $ nms M.! nm
    else let nnm = "var" ++ show i
         in do TS.put (M.insert nm nnm nms, i+1)
               return nnm

renameFld :: AST.FieldDecl -> NameState AST.FieldDecl
renameFld (AST.FieldDecl tp nm defval) = do tp' <- renameType tp 
                                            nm' <- genNm nm
                                            return $ AST.FieldDecl tp' nm' defval

renameType :: AST.SliceType -> NameState AST.SliceType
renameType (AST.STUserDefined nm) = genNm nm >>= return . AST.STUserDefined
renameType (AST.STUserDefinedPrx nm) = genNm nm >>= return . AST.STUserDefinedPrx
renameType otherwise = return otherwise

renameMthd :: AST.MethodDecl -> NameState AST.MethodDecl
renameMthd (AST.MethodDecl tp nm flds exts anno) = do tp' <- renameType tp
                                                      nm' <- genNm nm
                                                      flds' <- mapM renameFld flds
                                                      exts' <- mapM genNm exts
                                                      return $ AST.MethodDecl tp' nm' flds' exts' anno

renameMOrFld :: AST.MethodOrFieldDecl -> NameState AST.MethodOrFieldDecl
renameMOrFld (AST.MDecl m) = renameMthd m >>= return . AST.MDecl
renameMOrFld (AST.FDecl f) = renameFld f >>= return . AST.FDecl

rename :: AST.SliceDecl -> NameState AST.SliceDecl
rename (ModuleDecl nm decls)         = do { nm' <- genNm nm; decls' <- mapM rename decls; return $ ModuleDecl nm' decls' }
rename (IncludeDecl delim nm)        = do { nm' <- genNm nm; return $ IncludeDecl delim nm'}
rename (EnumDecl nm vals)            = do { nm' <- genNm nm; vals' <- mapM genNm vals; return $ EnumDecl nm' vals' }
rename (StructDecl nm flds)          = do { nm' <- genNm nm; flds' <- mapM renameFld flds; return $ StructDecl nm' flds' }
rename (ClassDecl nm exts mOrFlds)   = do nm' <- genNm nm
                                          exts' <- case exts of Nothing    -> return Nothing
                                                                (Just ext) -> genNm ext >>= return . Just
                                          mOrFlds' <- mapM renameMOrFld mOrFlds
                                          return $ ClassDecl nm' exts' mOrFlds'
rename (InterfaceDecl nm exts mthds) = do { nm' <- genNm nm; exts' <- mapM genNm exts; mthds' <- mapM renameMthd mthds; return $ InterfaceDecl nm' exts' mthds' }
rename (SequenceDecl tp nm)          = do { nm' <- genNm nm; tp' <- renameType tp; return $ SequenceDecl tp' nm' }
rename (DictionaryDecl tp1 tp2 nm)   = do { nm' <- genNm nm; tp1' <- renameType tp1; tp2' <- renameType tp2; return $ DictionaryDecl tp1' tp2' nm' }
rename (ExceptionDecl nm exts flds)  = do { nm' <- genNm nm; exts' <- mapM genNm exts; flds' <- mapM renameFld flds; return $ ExceptionDecl nm' exts' flds' }
rename (ConstDecl tp nm val)         = do { nm' <- genNm nm; tp' <- renameType tp; return $ ConstDecl tp' nm' val }

    

-- generate ice skeletons
sliceIceGen :: FilePath -> FilePath -> AST.SliceDecl -> [(FilePath,BS.ByteString)]
sliceIceGen icefile trgtdir decls = [(trgtdir </> icefile, gen "" decls)]
  where
    fs = fromString
    ext2Bs ext = if null ext then "" else " extends " <> BS.intercalate ", " (map fs ext)
    genBlk idnt bHead ctnt = idnt <> bHead <> "\n" <> idnt <> "{\n" <> ctnt <> idnt <> "};\n"
    gen idnt (ModuleDecl nm decls)         = genBlk idnt ("module " <> fs nm) (BS.intercalate "\n" $ map (gen (idnt <> "\t")) decls)
    gen idnt (IncludeDecl delim nm)        = let (openDelim, closeDelim) = case delim of {Quotes -> ("\"","\""); AngleBrackets -> ("<",">")}
                                             in idnt <> "#include " <> openDelim <> fs nm <> closeDelim <> "\n"
    gen idnt (EnumDecl nm vals)            = genBlk idnt ("enum " <> fs nm) (idnt <> "\t" <> BS.intercalate (",\n" <> idnt <> "\t") (map fs vals) <> "\n")
    gen idnt (StructDecl nm flds)          = genBlk idnt ("struct " <> fs nm) (genFields (idnt <> "\t") ";\n" flds <> ";\n")
    gen idnt (ClassDecl nm exts mOrFlds)   = let extends = maybe "" (\x -> " extends " <> fs x) exts
                                             in genBlk idnt ("class " <> fs nm <> extends) (genMethodsOrFields (idnt <> "\t") mOrFlds)
    gen idnt (InterfaceDecl nm exts mthds) = genBlk idnt ("interface " <> fs nm <> ext2Bs exts) (genMethodHeads (idnt <> "\t") mthds)
    gen idnt (SequenceDecl tp nm)          = idnt <> "sequence<" <> genType tp <> "> " <> fs nm <> ";\n"
    gen idnt (DictionaryDecl tp1 tp2 nm)   = idnt <> "dictionary<" <> genType tp1 <> "," <> genType tp2 <> "> " <> fs nm <> ";\n"
    gen idnt (ExceptionDecl nm exts flds)  = genBlk idnt ("exception " <> fs nm <> ext2Bs exts) (genFields (idnt <> "\t") ";\n" flds <> ";\n")
    gen idnt (ConstDecl tp nm val)         = idnt <> "const " <> genType tp <> " " <> fs nm <> " = " <> fs (show val) <> ";"
    
    genMethodsOrFields idnt mOrFlds = idnt <> BS.intercalate (";\n" <> idnt) (map genMethodOrField mOrFlds) <> ";\n"
    
    genMethodOrField (MDecl mdecl) = genMethodHead mdecl
    genMethodOrField (FDecl fdecl) = genField fdecl
    
    genMethodHeads idnt mthds = idnt <> BS.intercalate idnt (map genMethodHead mthds)
    
    genMethodHead :: MethodDecl -> BS.ByteString
    genMethodHead (MethodDecl tp nm flds expts mAnno) = let anno     = case mAnno of (Just Idempotent) -> "idempotent "
                                                                                     Nothing      -> ""
                                                            bsExcpts = if null expts then "" else " throws " <> BS.intercalate ", " (map fs expts)
                                                        in anno <> genType tp <> " " <> fs nm <> "(" <> genFields "" ", " flds <> ")" <> bsExcpts <> ";\n"
    
    genField (FieldDecl tp nm _) = genType tp <> " " <> fromString nm
    
    genFields idnt sep flds = idnt <> (BS.intercalate (sep <> idnt) $ map genField flds)
    
    genType :: SliceType -> BS.ByteString
    genType STVoid                = "void"
    genType STBool                = "bool"
    genType STByte                = "byte"
    genType STShort               = "short"
    genType STInt                 = "int"
    genType STLong                = "long"
    genType STFloat               = "float"
    genType STDouble              = "double"
    genType STString              = "string"
    genType (STUserDefined nm)    = fromString nm
    genType (STUserDefinedPrx nm) = fromString nm <> "*"


main = CA.cmdArgs defaultArgs >>= \args@(CppGenArgs icef trgtD ovrw) -> do
  slcData <- BS.readFile icef
  createDirectoryIfMissing True trgtD
  case SlcP.parseSlice slcData of
    Left  err -> putStrLn (err ++ "\nexiting...") >> exitFailure
    Right []  -> putStrLn ("Parsing '" ++ icef ++ "' didn't produce any output, probably because the Slice parser is deficient.\nTo improve it, please report your slice file to paul.koerbitz@gmail.com") 
                 >> exitFailure 
    Right asts -> do
      let asts'      = TS.evalState (mapM rename asts) (M.empty,0)
          fileData   = concatMap (sliceIceGen (takeFileName icef) trgtD) asts'
          joinedData = M.toList $ foldl (\m (fn,s) -> M.insert fn (if M.member fn m then (m M.! fn) <> s else s) m) M.empty fileData
          wrtr       = \(fn,ctnt) -> do putStrLn $ "generating '" ++ fn ++ "'"
                                        (BS.writeFile fn ctnt)
          chkwrtr    = if ovrw 
                         then wrtr
                         else \(fn,ctnt) -> do p <- doesFileExist fn
                                               if p 
                                                 then putStrLn ('\'' : fn ++ "' aready exists. To overwrite use '--overwrite=True'")
                                                 else wrtr (fn,ctnt)
      mapM_ chkwrtr joinedData