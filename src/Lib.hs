module Lib (
    runImportGroupValidator,
) where

import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Yaml
import Language.Haskell.Exts
import System.Directory
import System.FilePath

getImports :: String -> IO [ImportDecl SrcSpanInfo]
getImports fileName = do
    parseResult <- parseFile fileName
    case parseResult of
        ParseOk (Module _ _ _ imports _) -> return imports
        ParseFailed _ _ -> error "Parse failed"

prettyPrintImportList :: [ImportDecl SrcSpanInfo] -> [String]
prettyPrintImportList [] = []
prettyPrintImportList (importStatement : remaining) = [printImport importStatement] <> prettyPrintImportList remaining
  where
    printImport (ImportDecl (SrcSpanInfo (SrcSpan fileName startL _ endL _) _) (ModuleName _ moduleName) isqualified _ _ _ importAs _) =
        show startL <> " - " <> show endL <> " " <> "import " <> labelQualified isqualified <> " " <> moduleName <> " " <> labelAs importAs
    labelQualified isqualified = if isqualified then "qualified" else ""
    labelAs (Just (ModuleName _ qualifiedAs)) = "as " <> qualifiedAs
    labelAs Nothing = ""

groupImports :: [ImportDecl SrcSpanInfo] -> [[ImportDecl SrcSpanInfo]] -> Maybe (ImportDecl SrcSpanInfo) -> [[ImportDecl SrcSpanInfo]]
groupImports [] groupedListOfImports _ = groupedListOfImports
groupImports (importDecl : remaining) groupedListOfImports Nothing =
    groupImports remaining (addToLastGroup groupedListOfImports importDecl) (Just importDecl)
groupImports (importDecl : remaining) groupedListOfImports (Just prevImport) =
    if getStartLine importDecl == expectedStartLine
        then groupImports remaining (addToLastGroup groupedListOfImports importDecl) (Just importDecl)
        else groupImports remaining (groupedListOfImports <> [[importDecl]]) (Just importDecl)
  where
    expectedStartLine = getStartLine prevImport + additionalLinesUsed prevImport + 1
    additionalLinesUsed importStatement = getEndLine importStatement - getStartLine importStatement

addToLastGroup :: [[ImportDecl SrcSpanInfo]] -> ImportDecl SrcSpanInfo -> [[ImportDecl SrcSpanInfo]]
addToLastGroup [] importDecl = [[importDecl]]
addToLastGroup groupedListOfImports importDecl = (_last %~ (++ [importDecl])) groupedListOfImports

getStartLine :: ImportDecl SrcSpanInfo -> Int
getStartLine (ImportDecl (SrcSpanInfo (SrcSpan _ startL _ _ _) _) _ _ _ _ _ _ _) = startL

getEndLine :: ImportDecl SrcSpanInfo -> Int
getEndLine (ImportDecl (SrcSpanInfo (SrcSpan _ _ _ endL _) _) _ _ _ _ _ _ _) = endL

validateGrouping :: [[ImportDecl SrcSpanInfo]] -> Either GroupingError [[ImportDecl SrcSpanInfo]]
validateGrouping importGroups = do
    validateNoOfGroups importGroups

data GroupingError = GroupingError
    { message :: String
    , improperlyGroupedImports :: [ImportDecl SrcSpanInfo]
    }
    deriving (Show)

validateNoOfGroups :: [[ImportDecl SrcSpanInfo]] -> Either GroupingError [[ImportDecl SrcSpanInfo]]
validateNoOfGroups importGroups = do
    let groupCount = length importGroups
    if groupCount > 2
        then Left $ GroupingError "imports grouped into more than two groups" []
        else Right importGroups

validateInternalImports :: [String] -> [[ImportDecl SrcSpanInfo]] -> Either GroupingError [[ImportDecl SrcSpanInfo]]
validateInternalImports internalImports imports = do
    let group1Imports = map getModuleNameFromImport (imports !! 1)
        invalidImports = filter (not . flip elem internalImports . getModuleNameFromImport) (imports !! 1)
    if length invalidImports == 0
        then Right imports
        else Left $ GroupingError "second group contains following non Internal imports" invalidImports

validateExternalImports :: [String] -> [[ImportDecl SrcSpanInfo]] -> Either GroupingError [[ImportDecl SrcSpanInfo]]
validateExternalImports internalImports imports = do
    let group2Imports = map getModuleNameFromImport (imports !! 0) :: [String]
        invalidImports = filter (flip elem internalImports . getModuleNameFromImport) (imports !! 0)
    if length invalidImports == 0
        then Right imports
        else Left $ GroupingError "first group contains following non external imports" invalidImports

excludedDirectories directory = map (directory </>) [".stack-work", "setup.hs"]

recursiveHunForHaskellFiles :: [FilePath] -> FilePath -> IO [FilePath]
recursiveHunForHaskellFiles excluded directory = do
    filepaths <- listDirectory directory
    let fullPaths = map (directory </>) filepaths
        filePathsWithoutExcluded = filter (not . flip elem excluded) fullPaths
    subDirectories <- filterM (doesDirectoryExist) filePathsWithoutExcluded
    files <- filterM doesFileExist filePathsWithoutExcluded
    let haskellFiles = filter (\f -> takeExtension f == ".hs") files
    subdirFiles <- concat <$> mapM (recursiveHunForHaskellFiles excluded) subDirectories
    return (haskellFiles ++ subdirFiles)

getModuleName haskellFile = do
    parseResult <- parseFile haskellFile
    case parseResult of
        ParseOk (Module _ moduleHead _ _ _) -> do
            case moduleHead of
                Just (ModuleHead _ (ModuleName _ moduleName) _ _) -> return $ Just moduleName
                _ -> return Nothing
        ParseFailed _ _ -> error "Parse failed"

getModuleNameFromImport (ImportDecl _ (ModuleName _ moduleName) _ _ _ _ _ _) = moduleName

runImportGroupValidator :: IO ()
runImportGroupValidator =
    do
        dir <- getCurrentDirectory
        haskellFiles <- recursiveHunForHaskellFiles (excludedDirectories dir) dir
        maybeModules <- mapM getModuleName haskellFiles
        let internalImports = mapMaybe id maybeModules
        validateImports haskellFiles internalImports

validateImports [] _ = print "import grouping check completed"
validateImports (haskellFile : remaining) internalImports = do
    print $ "--------------" <> haskellFile
    imports <- getImports haskellFile
    if (not $ null imports)
        then do
            -- mapM_ print $ prettyPrintImportList imports
            let groupedImports = groupImports imports [] Nothing
            -- mapM_ print $ prettyPrintImportList <$> groupedImports

            let validationResult =
                    if length groupedImports > 1
                        then
                            validateGrouping groupedImports
                                >>= validateInternalImports internalImports
                                >>= validateExternalImports internalImports
                        else validateExternalImports internalImports groupedImports

            -- print validationResult
            if (length groupedImports <= 1)
                then case validationResult of
                    Left (GroupingError message incorrectlyGroupedImports) -> do
                        if (length incorrectlyGroupedImports /= length imports)
                            then do
                                putStrLn $ "\x1b[31m ERROR: " <> message <> "\x1b[0m "
                                mapM_ print $ prettyPrintImportList incorrectlyGroupedImports
                            else putStrLn "\x1b[32m OK: imports are correctly ordered: only 1 import group with all internal imports \x1b[0m "
                    Right _ -> putStrLn "\x1b[32m OK: imports are correctly ordered: only 1 import group with all external imports \x1b[0m "
                else case validationResult of
                    Left (GroupingError message incorrectlyGroupedImports) -> do
                        putStrLn $ "\x1b[31m ERROR: " <> message <> "\x1b[0m "
                        mapM_ print $ prettyPrintImportList incorrectlyGroupedImports
                    Right imports -> putStrLn "\x1b[32m OK: imports are correctly ordered \x1b[0m "
        else putStrLn "\x1b[32m OK: module has no import statements \x1b[0m "
    validateImports remaining internalImports
