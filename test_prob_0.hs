
import Test.HUnit
import Test.Hspec
import Test.Hspec.HUnit

import Data.Prob

specs = describe "Data.Prob" $
    [ it "can be imported succesfully" True
    ]

main = hspec specs
