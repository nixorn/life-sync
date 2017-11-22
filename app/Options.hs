{-# LANGUAGE ApplicativeDo #-}

-- | Command line options for Importify

module Options
       ( LifeCommand (..)
       , InitOptions (..)

       , parseCommand
       ) where

import Universum

import Options.Applicative (Parser, ParserInfo, auto, command, execParser, flag', fullDesc, help,
                            helper, info, long, metavar, option, progDesc, short, showDefault,
                            strArgument, strOption, subparser, switch, value)

import Life.Github (Owner (..))

data LifeCommand
    = Init InitOptions
--    | Sync SyncOptions
--    | Add   AddOptions
    deriving (Show)

data InitOptions = InitOptions
     { initOptionsOwner :: Owner
     } deriving (Show)

commandParser :: Parser LifeCommand
commandParser = subparser $
    command "init"
            (info (helper <*> fmap Init initOptionsParser)
                  (fullDesc <> progDesc "Initialize GitHub repository named 'dotfiles' if you don't have one."))
-- <> command "cache"
--            (info (helper <*> cacheParser)
--                  (fullDesc <> progDesc
--                   "Search for .cabal file in current directory. If it's found then cache \
--                   \all dependencies for every target and store them inside ./.importify folder."))

initOptionsParser :: Parser InitOptions
initOptionsParser = do
    initOptionsOwner <- fmap Owner
      $ strArgument
      $ metavar "OWNER"
     <> help "Your github user name"
    pure InitOptions{..}

optionsInfo :: ParserInfo LifeCommand
optionsInfo = info
    (helper  <*> commandParser)
    (fullDesc <> progDesc "life-sync synchronize your personal configs")

parseCommand :: IO LifeCommand
parseCommand = execParser optionsInfo
