-- | Command to update local state from remote state.

module Life.Main.Pull
       ( lifePull
       ) where

import Path (Dir, File, Path, Rel)

import Lens.Micro.Platform ((^.))
import Life.Configuration (LifeConfiguration (..), BranchState(..), branch, parseHomeLife)
import Life.Github (Owner, cloneRepo, pullUpdateFromRepo, master, updateFromRepo, setCurrentBranch, branchState)
import Life.Main.Init (lifeInitQuestion)
import Life.Message (abortCmd, choose, warningMessage)
import Life.Shell (LifeExistence (..), whatIsLife)

lifePull :: Owner -> Set (Path Rel File) -> Set (Path Rel Dir) -> IO ()
lifePull owner withoutFiles withoutDirs = do
    homeLife <- parseHomeLife
    branchState (homeLife^.branch) >>= \case
        (OnlyLocal, _) -> error "Branch from config not exists on remote"
        (NotExists, _) -> error "Branch from config not exists locally and on remote"
        (branchS, branch') -> whatIsLife >>= \case
            OnlyRepo _ -> warningMessage ".life file not found" >> setCurrentBranch branch' branchS >> pullUpdate
            OnlyLife _ -> warningMessage "dotfiles not found" >> clone >> setCurrentBranch branch' branchS >> update
            NoLife     -> initOrPull >> setCurrentBranch branch' branchS
            Both _ _   -> setCurrentBranch branch' branchS >> pullUpdate
  where
    initOrPull :: IO ()
    initOrPull = do
        warningMessage ".life file and dotfiles repo not found"
        action <- choose "Do you want to (F)etch existing repo, (I)nit from scratch or (A)bort operation?"
                         ["f", "i", "a"]
        case action of
            "f" -> clone >> update
            "i" -> lifeInitQuestion "pull" pass
            "a" -> abortCmd "pull" "Cannot find .life and dotfiles"
            _   -> error "Impossible choice"

    life :: LifeConfiguration
    life = LifeConfiguration withoutFiles withoutDirs master

    clone, update, pullUpdate :: IO ()
    clone = cloneRepo owner
    update     = updateFromRepo life
    pullUpdate = pullUpdateFromRepo life
