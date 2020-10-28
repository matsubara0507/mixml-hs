{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module MixML.EL.Trace where

import           Prelude                   hiding (log)

import           Control.Lens              (over, view, (^.))
import           Control.Monad             (when)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State (StateT, evalStateT, get, gets,
                                            modify)
import           Data.Extensible           hiding (State)
import qualified MixML.EL.AuxReg           as Aux
import qualified MixML.EL.Pretty           as Pretty
import qualified MixML.EL.Print            as Print
import qualified MixML.EL.Syntax           as EL
import           Text.PrettyPrint

type Trace = StateT TraceState IO

type TraceState = Record
  '[ "nesting"  >: Int
   , "indent"   >: Int
   , "width"    >: Int
   , "on"       >: Bool
   , "onScreen" >: Bool
   , "assertOn" >: Bool
   ]

runTrace :: Trace () -> IO ()
runTrace = flip evalStateT iniTraceState

iniTraceState :: TraceState
iniTraceState
    = #nesting  @= 0
   <: #indent   @= undefined
   <: #width    @= 80
   <: #on       @= False
   <: #onScreen @= True
   <: #assertOn @= False
   <: nil

traceIn :: Trace ()
traceIn = modify $ over #nesting (+ 1)

traceOut :: Trace ()
traceOut = modify $ over #nesting (+ (-1))

traceModl :: String -> EL.Modl -> String -> Trace ()
traceModl func = point func Print.strModl

point :: String -> (a -> String) -> a -> String -> Trace ()
point func strX x = trace' (func ++ " " ++ strX x)

trace' :: String -> String -> Trace ()
trace' func mark = trace ("@" ++ func ++ " [" ++ mark ++ "]")

trace :: String -> Trace ()
trace s = do
  flag <- gets (view #on)
  when (not flag) $ do
    n <- right 0
    liftIO $ putStrLn (tab n ++ s)

tab :: Int -> String
tab n = replicate n ' '

right :: Int -> Trace Int
right off = do
  st <- get
  let n = st ^. #indent * st ^. #nesting + off
      w = st ^. #width
  pure $
    if not (st ^. #onScreen) && n > (w * 3 `div` 4) then
      n `mod` (w `div` 2) + w `div` 4
    else
      n

traceD :: String -> Aux.TypContext -> Trace Aux.TypContext
traceD name delta = dump name Pretty.ppD delta

traceG :: String -> Aux.ModlContext -> Trace Aux.ModlContext
traceG name gamma = dump name Pretty.ppG gamma

dump :: String -> (a -> Doc) -> a -> Trace a
dump name ppX x = do
  st <- get
  when (st ^. #on) $ do
    indent1 <- right 1
    indent2 <- right 2
    let doc = nest indent2 (text (tab indent1 ++ name ++ " =") $+$ ppX x)
    liftIO $ putStrLn (renderStyle (style { lineLength = st ^. #width }) doc)
  pure x
