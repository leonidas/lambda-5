
import Test.HUnit
import Test.Hspec
import Test.Hspec.HUnit

import Control.Applicative

import Data.Ratio ((%))
import Data.Prob

specs = describe "Data.Prob" $
    [ it "can be imported succesfully" True

    , describe "evenDist :: [a] -> Prob a" $
        [ it "creates an even distribution of probabilities" $ do
            let p = evenDist [1,2,3,4]
            prob p @?= [(1,1%4), (2,1%4), (3,1%4), (4,1%4)]

        , it "works for a single element list" $ do
            let p = evenDist ['a']
            prob p @?= [('a', 1)]

        , it "works on an empty list" $ do
            let p = evenDist [] :: Prob Int
            prob p @?= []

        ]

    , describe "weighted :: [(Rational, a)] -> Prob a" $
        [ it "creates a weighted distribution" $ do
            let p = weighted [('a', 1%8),('b', 4%8),('c', 3%8)]
            prob p @?= [('a', 1%8),('b', 4%8),('c', 3%8)]

        , it "creates a distribution where the sum of weights is one even if input sum > 1" $ do
            let p = weighted [('a', 1),('b', 4),('c', 3)]
            sum (map snd (prob p)) @?= 1

        , it "creates a distribution where the sum of weights is one even if input sum < 1" $ do
            let p = weighted [('a', 1%7),('b', 1%4),('c', 1%3)]
            sum (map snd (prob p)) @?= 1
        ]

    , describe "instance Functor Prob" $
        [ it "maps a function over a probability distribution" $ do
            let p = fmap ("foo"++) $ weighted [("bar", 1), ("asdf", 2)]

            prob p @?= [("foobar", 1%3), ("fooasdf", 2%3)]
        ]

    , describe "instance Applicative Prob" $
        [ it "converts a pure value to probability distribution" $ do
            let p = pure "foo"

            prob p @?= [("foo", 1)]

        , it "applies a probabilistic function to probabilistic value" $ do
            let f = weighted [((+2), 1), ((+5), 2)]
                x = weighted [(0, 1), (1, 2)]
                p = f <*> x

            prob p @?= [(2,1%9), (3, 2%9) ,(5,2%9), (6,4%9)]
        ]
    ]

main = hspec specs
