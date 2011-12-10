
import Data.Prob
import Control.Monad

data NFA i s = NFA
    { initialState    :: s
    , stateTransition :: s -> i -> Prob s
    , isGoal          :: s -> Bool
    }

validProb :: NFA i s -> [i] -> Prob s
validProb (NFA initial transition goal) = foldM transition initial

test :: NFA Char Int
test = NFA
    { initialState = 0
    , isGoal       = (==0)
    , stateTransition = f
    } where
        f 0 'a' = evenDist [1,2]
        f 1 'a' = evenDist [0,3]
        f 1 'b' = evenDist [2]
        f 2 'c' = evenDist [1,3]
        f 3 'a' = evenDist [1,2]
        f _  _  = evenDist [4]
