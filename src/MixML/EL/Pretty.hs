module MixML.EL.Pretty where

import qualified MixML.EL.AuxReg  as Aux
import           Text.PrettyPrint (Doc)

topP, bindP, arrowP, assignP, compP, plusP, timesP, applyP, dotP, atomP :: Int
topP    = 0
bindP   = topP + 1
arrowP  = bindP
assignP = arrowP + 1
compP   = assignP
plusP   = compP + 1
timesP  = plusP + 1
applyP  = timesP + 1
dotP    = applyP
atomP   = dotP + 1

ppD :: Aux.TypContext -> Doc
ppD = undefined

ppG :: Aux.ModlContext -> Doc
ppG = undefined
