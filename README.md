# CmdItem
[![Build Status](https://travis-ci.org/geraud/cmd-item.svg?branch=master)](https://travis-ci.org/geraud/cmd-item)

CmdItem allows you to compose command lines by combining fragments of commands.

## Installation

```haskell
cabal update
cabal install cmd-item
```

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import Data.CmdItem
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
    [] -> return ()
    (name:ns) -> do
        constants <- getConstants name
        makeProject constants

makeProject :: CmdItem -> IO ()
makeProject contants = do
    let cmdItem = pants <> "idea" <> ideaOptions <> constants
    shellCommand <- render cmdItem
    print shellcommand

getConstants :: Text -> IO CmdItem
getConstants name =  do
    home <- getHomeDirectory
    nCpu <- getNumProcessors
    let result = [ ("java_version", "7")
                 , ("project_root", T.pack $ home <> "/workspace/project")
                 , ("num_cpu", T.pack $ show nCpu)
                 , ("project_name", name)
                 ]
    return result

pants :: CmdItem
pants = "%{project_root}/pants"

ideaOptions :: CmdItem
ideaOptions = "--idea-java-language-level=%{java_version}"
            <> "--idea-java-maximum-heap-size=2096"
            <> "--idea-scala-maximum-heap-size=2096"
            <> "--idea-project-name=%{project_name}"
```
