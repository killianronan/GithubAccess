{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Lib
    ( someFunc
    ) where

import qualified GitHub as GH
import qualified Servant.Client               as SC
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)

import Data.Text hiding (map,intercalate)
import Data.List (intercalate)

someFunc :: IO ()
someFunc = do
  putStrLn "Begin github call to my own account"
  testGitHubCall "killianronan"
  putStrLn "Finished."


testGitHubCall :: Text -> IO ()
testGitHubCall name = 
  (SC.runClientM (GH.getUser (Just "github-access-haskell") name) =<< env) >>= \case
    Left err -> do
      putStrLn $ "Error encountered while retrieving user data: " ++ show err
    Right res -> do
      putStrLn $ "Query results: " ++ show res
      (SC.runClientM (GH.getUserRepos (Just "github-access-haskell") name) =<< env) >>= \case
        Left err -> do
          putStrLn $ "Error encountered while retrieving repos: " ++ show err
        Right res' -> do
          putStrLn $ "List of my repositories:" ++
            intercalate ", " (map (\(GH.GitHubRepo n _ _ ) -> unpack n) res')
     
  where env :: IO SC.ClientEnv
        env = do
          manager <- newManager tlsManagerSettings
          return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")
