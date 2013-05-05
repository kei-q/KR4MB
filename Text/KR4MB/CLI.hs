{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Text.KR4MB.CLI where

import qualified System.Process.QQ as P

cli_path = "/Applications/KeyRemap4MacBook.app/Contents/Applications/KeyRemap4MacBook_cli.app/Contents/MacOS/KeyRemap4MacBook_cli"

type Identifier = String

set :: (Identifier, Int) -> IO ()
set (key, val) = do
    [P.cmd|#{cli_path} set #{key} #{show val}|]
    return ()

reload :: IO ()
reload = [P.cmd|#{cli_path} reloadxml|] >> return ()

enable :: Identifier -> IO ()
enable ident = [P.cmd|#{cli_path} enable #{ident}|] >> return ()

