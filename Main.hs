module Main where

import Data.Maybe
import Data.Char
import Data.List (last, findIndices)
import Control.Monad
import Network.FastCGI
import Text.StringTemplate
import Database.SQLite
import Network.URI (uriPath)
import Data.Digest.OpenSSL.MD5
import Data.ByteString.Char8 (pack)

dbName = "/home/travis/src/sayit/sayings.db"

split :: Char -> String -> [String]
split delim s
    | [] <- rest = [token]
    | otherwise = token : split delim (tail rest)
        where (token,rest) = span (/=delim) s

slashCount = length . findIndices (\x -> x == '/')
lastChunk = reverse . takeWhile (\x -> x /= '/') . reverse
getPNum uri
    | slashCount uri < 2 = ""
    | otherwise = lastChunk uri

insertSaying ip said = do
    let tag = getTag said
    conn <- openConnection dbName
    let sql = "INSERT INTO sayings (tag, ip, said) VALUES (:tag, :ip, :said)"
    let vals = [(":tag", Text tag), (":ip", Text ip), (":said", Text said)]
    res <- execParamStatement_ conn sql vals
    closeConnection conn
    return tag

getSaying tag = do
    conn <- openConnection dbName
    let sql = "SELECT said FROM sayings WHERE tag = '" ++ tag ++ "' LIMIT 1"
    res <- (execStatement conn sql)::IO (Either String [[Row String]])
    case res of
        Left s -> return s
        Right [[[(_, sx)]]] -> return sx
        _ -> return "nothing here"

getSayings = do
    conn <- openConnection dbName
    let sql = "SELECT created, said FROM sayings ORDER BY created DESC"
    res <- (execStatement conn sql)::IO (Either String [[Row String]])
    return res

getTag s = md5sum $ pack s

handleGet :: STGroup String ->CGI CGIResult
handleGet tg = do
    let baseTemplate = case (getStringTemplate "base" tg) of
                Just tmpl -> tmpl
                _ -> newSTMP "uh oh"
    method <- requestMethod
    case method of
        "GET" -> do
            qu <- queryURI
            let uri = uriPath qu
            case getPNum uri of
                "" -> output $ toString baseTemplate
                snum -> do
                    said <- liftIO $ getSaying snum
                    output $ toString $ setAttribute "said" said baseTemplate
        "POST" -> do
            ip <- getVarWithDefault "REMOTE_ADDR" "null"
            sfo <- getInput "to_be_said"
            case sfo of
                Just said -> do
                    let truncSaid = take 1000 said
                    sayTag <- liftIO $ insertSaying ip truncSaid
                    let url = "/sayit/" ++ sayTag
                    redirect url
                Nothing -> redirect "/sayit"

getGroup = do
    grp <- directoryGroup "/home/travis/src/sayit/templates" :: IO (STGroup String)
    return grp

main = do
    grp <- getGroup
    runFastCGIorCGI (handleGet grp)
--main = runFastCGIConcurrent' forkIO 10 handleGet
--main = runFastCGIorCGI handleGet