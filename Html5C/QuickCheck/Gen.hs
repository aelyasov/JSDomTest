{-# LANGUAGE OverloadedStrings #-}

module Html5C.QuickCheck.Gen where

import Text.Blaze.Html5
import Text.Blaze.Internal
import Test.QuickCheck.Gen 
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Html5C.Tags
import Html5C.Context
import Util.Stack

import Config.Default
import Config.DefaultTest
import Config.DefaultReal
import Data.Configurator (load, Worth(..), require)
import System.IO.Unsafe (unsafePerformIO)

import Debug.Trace
import Data.List (sortBy)

type HasMain = Bool

{-# NOINLINE readRandomConfig #-}
readRandomConfig :: (Int, Int, Int, Bool)
readRandomConfig = unsafePerformIO $ do
  config   <- load [ Required "jsdomtest.cfg"]
  depth    <- require config "generate_html_tree.depth"
  degree   <- require config "generate_html_tree.degree"
  topN     <- require config "generate_html_tree.frequency-table.top"
  equalize <- require config "generate_html_tree.frequency-table.equalize"
  return (depth, degree, topN, equalize)

defaultState :: HtmlState
defaultState =
  let (depth, degree, topN, equalize) = readRandomConfig
      filter = if equalize then univFreqTbl else id
  in  HtmlState { hasMain     = False
                , getDepth    = depth
                , getNodeDegr = degree   
                , getCtx      = empty
                                -- , tagFreqTbl  = normFreqTblReal defTagFreqTblReal
                , tagFreqTbl  = filter $ take topN $ sortBy (\i j -> fst j `compare` fst i) defTagFreqTblReal
                , htmlTags    = []
                , htmlNames   = []
                , htmlIds     = []
                , htmlClasses = []
                , hasDefHead  = True
                } 

data HtmlState = HtmlState { hasMain     :: Bool
                           , getDepth    :: Int 
                           , getNodeDegr :: Int    
                           , getCtx      :: Context 
                           , tagFreqTbl  :: [(Int, HTML_TAG)] 
                           , htmlTags    :: [HTML_TAG]
                           , htmlNames   :: [String]
                           , htmlIds     :: [String]
                           , htmlClasses :: [String]
                           , hasDefHead  :: Bool
                           }
                 deriving Show


type GenHtmlState = GenState HtmlState Html

type GenState s = StateT s Gen

type GetSHtml = GenState HasMain Html

optional :: GenState s Html -> GenState s Html
optional st = do html_ <- st
                 lift $ frequency [(1, return html_), (1, return Empty)] 


-- listOfState :: Show s => StateT s Gen a -> StateT s Gen [a]
-- listOfState genSt = do sizedState $ \n ->
--                            do k <- lift $ choose (0,n)
--                               vectorOfState k genSt


-- listOfState1 :: Show s => StateT s Gen a -> StateT s Gen [a]
-- listOfState1 genSt = do sizedState $ \n ->
--                             do k <- lift $ choose (1,1 `max` n)
--                                vectorOfState k genSt


-- -- | Generates a list of the given length.
-- vectorOfState :: (Show s) => Int -> StateT s Gen a -> StateT s Gen [a]
-- vectorOfState 0 _     = return []
-- vectorOfState k genSt = do g <- genSt
--                            -- st <- get
--                            gs <- vectorOfState (k-1) genSt -- $ withStateT (\_ -> st) genSt 
--                            return (g:gs)

listOfState :: StateT HtmlState Gen Html -> StateT HtmlState Gen [Html]
listOfState genSt = do sizedState $ \n ->
                           do k <- lift $ choose (0,n)
                              vectorOfState k genSt


listOfState1 :: StateT HtmlState Gen Html -> StateT HtmlState Gen [Html]
listOfState1 genSt = do sizedState $ \n ->
                            do k <- lift $ choose (1,1 `max` n)
                               vectorOfState k genSt


vectorOfState :: Int -> StateT HtmlState Gen Html -> StateT HtmlState Gen [Html]
vectorOfState 0 _     = return []
vectorOfState k genSt = do st_pre <- get
                           let ctx   = getCtx st_pre
                               depth = getDepth st_pre
                           g <- genSt
                           st_post <- get
                           let main_ = hasMain st_post
                           put st_post{hasMain = main_, getCtx=ctx, getDepth=depth}
                           gs <- vectorOfState (k-1) genSt
                           -- hasMain = main_
                           -- $ withStateT (\_ -> st) genSt 
                           return (g:gs)


sizedState :: (Int -> StateT s Gen a) -> StateT s Gen a
sizedState f = StateT (\st -> MkGen (\r n -> let MkGen m = runStateT (f n) st in m r n))


-- oneofState :: [StateT s Gen a] -> StateT s Gen a
-- oneofState [] = error "QuickCheck.oneofState used with empty list"
-- oneofState gs = do k <- lift $ choose (0,length gs - 1) 
--                    gs !! k

oneofState :: [StateT s Gen Html] -> StateT s Gen Html
oneofState [] = return Empty
oneofState gs = do k <- lift $ choose (0,length gs - 1) 
                   gs !! k


frequencyState :: [(Int, StateT s Gen Html)] -> StateT s Gen Html
-- frequencyState [] = error "QuickCheck.frequency used with empty list"
frequencyState [] = return Empty
frequencyState xs0 = do let tot = sum (Prelude.map fst xs0)
                            pick n ((k,x):xs)
                                | n <= k    = x
                                | otherwise = pick (n-k) xs
                            pick _ _  = error "QuickCheck.pick used with empty list"    
                        k <- lift $ choose (1, tot)
                        k `pick` xs0    
      


resizeState :: Int -> StateT s Gen a -> StateT s Gen a
resizeState n st = StateT (\s_ -> MkGen(\r _ -> let MkGen m = runStateT st s_ in m r n))

-- MkGen (\r _ -> m r n)

-- oneofStateUntil :: [StateT Bool Gen a] -> StateT Bool Gen a
-- oneofStateUntil [] = error "QuickCheck.oneofStateUntil: all states are False"
-- oneofStateUntil gs = do k  <- lift $ choose (0,length gs - 1) 
--                         g  <- gs !! k
--                         st <- get
--                         if st 
--                         then return g 
--                         else do put True
--                                 oneofStateUntil $ remove k gs
--                         -- trace ("state: " ++ (show st) ++ "; k: " ++ (show k)) $ if st then return g else oneofStateUntil $ remove k gs
                        
-- remove :: Int -> [a] -> [a]
-- remove n xs = let (as, bs) = splitAt n xs
--               in  if Prelude.null bs then [] else as ++ tail bs

-- vectorOfStateBool :: Int -> StateT Bool Gen a -> StateT Bool Gen [a]
-- vectorOfStateBool 0 _     = return []
-- vectorOfStateBool k genSt = do g <- genSt
--                                st <- get
--                                gs <- vectorOfState (k-1) $ withStateT (\_ -> st) genSt 
--                                return  (g:gs)


-- listOfStateOr :: StateT Bool Gen a -> StateT Bool Gen a -> StateT Bool Gen [a]
-- listOfStateOr genSt1 genSt2 = do sizedState $ \n ->
--                                      do k <- lift $ choose (0,n)
--                                         st <- get
--                                         if st
--                                         then vectorOfState k genSt1
--                                         else vectorOfState k genSt2


mkGens :: Context -> Int -> [Context -> Int -> GenState HasMain Html] -> [GenState HasMain Html]
mkGens cm n gens  = Prelude.map (\g ->  g cm n) gens       


