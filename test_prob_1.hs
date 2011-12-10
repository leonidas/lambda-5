
import Test.HUnit
import Test.Hspec
import Test.Hspec.HUnit

import Data.Ratio ((%))
import Data.Prob

specs = describe "Data.Prob" $
    [ it "can be imported succesfully" True

    , describe "evenDist :: [a] -> Prob a" $
        [ it "creates an even distribution of probabilities for one item" $ do
            let p = evenDist ["a"]
            prob p @?= [("a",1)]

        , it "creates an even distribution of probabilities" $ do
            let p = evenDist [1,2,3,4]
            prob p @?= [(1,1%4), (2,1%4), (3,1%4), (4,1%4)]

        , it "works for a single element list" $ do
            let p = evenDist ['a']
            prob p @?= [('a', 1)]
        ]
    ]

main = hspec specs
