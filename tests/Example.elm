module Example exposing (suite)

import Expect exposing (equal)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "No-op test"
        [ test "works" (\_ -> equal 1234 1234)
        ]
