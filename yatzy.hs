
import Data.List
import Data.Prob
import Control.Monad

type YatzyHand = [Int]

numberEq :: YatzyHand -> Int
numberEq = maximum . map length . group . sort

dieRoll :: Prob Int
dieRoll = evenDist [1..6]

yatzyRoll :: Prob YatzyHand
yatzyRoll = normalize $ fmap sort $ replicateM 5 dieRoll
