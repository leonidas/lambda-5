
import Data.Prob
import Control.Monad

data PA s i = PA
    { initialState    :: s
    , stateTransition :: s -> i -> Prob s
    , isGoal          :: s -> Bool
    }

runPA :: PA s i -> [i] -> Prob s
runPA (PA initial transition goal) = foldM transition initial

test :: PA Int Char
test = PA
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
