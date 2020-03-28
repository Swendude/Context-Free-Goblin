module ExampleGrammars exposing (deepGrammarRules, deepGrammarSentences, defaultGrammar, deterministicGrammarRules, jsonGrammarRules, normalGrammarRules, normalGrammarSentences, possibleAnimals, recursiveInvalidGrammarRules, testJSONGrammar)

import Dict exposing (Dict)
import Grammar exposing (..)


defaultGrammar : Grammar
defaultGrammar =
    Grammar (Dict.fromList [ ( "START", [ [ Token "You see a ", Symbol "goblin-with-activity" ] ] ), ( "activity", [ [ Token "laughs" ], [ Token "flies" ], [ Token "points it's stick" ] ] ), ( "goblin-prop", [ [ Token "smelly" ], [ Token "lonely" ], [ Token "drunken" ], [ Token "funny" ] ] ), ( "goblin-with-activity", [ [ Symbol "goblin-prop", Token " goblin. It ", Symbol "activity", Token "." ] ] ) ])


testJSONGrammar : String
testJSONGrammar =
    """{
    "rules": [
        {
            "symbol": "START",
            "productions": [
                [
                    {
                        "type": "token",
                        "str": "He saw a "
                    },
                    {
                        "type": "symbol",
                        "str": "animal"
                    }
                ]
            ]
        },
        {
            "symbol": "animal",
            "productions": [
                [
                    {
                        "type": "token",
                        "str": "dog"
                    }
                ],
                [
                    {
                        "type": "token",
                        "str": "sheep"
                    }
                ]
            ]
        }
    ]
}"""


jsonGrammarRules : Dict String (List Production)
jsonGrammarRules =
    Dict.fromList
        [ ( "START", [ [ Token "He saw a ", Symbol "animal" ] ] )
        , ( "animal", [ [ Token "dog" ], [ Token "sheep" ] ] )
        ]


normalGrammarRules : Dict String (List Production)
normalGrammarRules =
    Dict.fromList
        [ ( "START", [ [ Token "He saw a ", Symbol "animal", Token " in a ", Symbol "location" ] ] )
        , ( "animal", [ [ Token "dog" ], [ Token "sheep" ] ] )
        , ( "location", [ [ Token "forest" ], [ Token "city" ] ] )
        ]


deepGrammarRules : Dict String (List Production)
deepGrammarRules =
    Dict.fromList
        [ ( "START", [ [ Token "He saw a ", Symbol "moody_animal", Token " in a ", Symbol "special_location" ] ] )
        , ( "moody_animal", [ [ Symbol "mood", Token " ", Symbol "animal" ] ] )
        , ( "animal", [ [ Token "dog" ], [ Token "sheep" ] ] )
        , ( "mood", [ [ Token "angry" ], [ Token "happy" ] ] )
        , ( "special_location", [ [ Symbol "special", Token " ", Symbol "location" ] ] )
        , ( "location", [ [ Token "forest" ], [ Token "city" ] ] )
        , ( "special", [ [ Token "abandoned" ], [ Token "sun-lit" ] ] )
        ]


deterministicGrammarRules : Dict String (List Production)
deterministicGrammarRules =
    Dict.fromList
        [ ( "START", [ [ Token "He saw a ", Symbol "animal", Token " in a ", Symbol "location" ] ] )
        , ( "animal", [ [ Token "dog" ] ] )
        , ( "location", [ [ Token "forest" ] ] )
        ]


recursiveInvalidGrammarRules : Dict String (List Production)
recursiveInvalidGrammarRules =
    Dict.fromList
        [ ( "START", [ [ Token "He saw a ", Symbol "animal" ] ] )
        , ( "animal", [ [ Token "dog and ", Symbol "animal" ] ] )
        ]


deepGrammarSentences : List (Result a String)
deepGrammarSentences =
    [ Ok "He saw a angry dog in a abandoned forest"
    , Ok "He saw a happy dog in a abandoned forest"
    , Ok "He saw a angry dog in a sun-lit forest"
    , Ok "He saw a happy dog in a sun-lit forest"
    , Ok "He saw a angry dog in a abandoned city"
    , Ok "He saw a happy dog in a abandoned city"
    , Ok "He saw a angry dog in a sun-lit city"
    , Ok "He saw a happy dog in a sun-lit city"
    , Ok "He saw a angry sheep in a abandoned forest"
    , Ok "He saw a happy sheep in a abandoned forest"
    , Ok "He saw a angry sheep in a sun-lit forest"
    , Ok "He saw a happy sheep in a sun-lit forest"
    , Ok "He saw a angry sheep in a abandoned city"
    , Ok "He saw a happy sheep in a abandoned city"
    , Ok "He saw a angry sheep in a sun-lit city"
    , Ok "He saw a happy sheep in a sun-lit city"
    ]


normalGrammarSentences : List (Result a String)
normalGrammarSentences =
    [ Ok "He saw a dog in a forest", Ok "He saw a sheep in a forest", Ok "He saw a dog in a city", Ok "He saw a sheep in a forest" ]


possibleAnimals : List Production
possibleAnimals =
    [ [ Token "dog" ], [ Token "sheep" ] ]
