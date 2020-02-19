port module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Border as Border
import Html exposing (Html)
import Random exposing (Seed)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    Int


type alias Model =
    { narrative : List String
    , ticksTillNextCard : Int
    , seed : Seed
    }


type Card
    = Battle
    | Greeting
    | Travel


type Msg
    = Tick


init : Flags -> ( Model, Cmd Msg )
init randomInitiator =
    ( { narrative = []
      , ticksTillNextCard = baseTicksTillCard
      , seed = Random.initialSeed randomInitiator
      }
    , Cmd.none
    )


baseTicksTillCard : Int
baseTicksTillCard =
    5


subscriptions : Model -> Sub Msg
subscriptions model =
    tick (\_ -> Tick)


port tick : (Int -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            let
                nextTicksRemaining =
                    model.ticksTillNextCard - 1
            in
            ( if nextTicksRemaining == 0 then
                let
                    ( nextNarrativePoint, nextSeed ) =
                        randomNarrativePoint model.seed
                in
                { model
                    | ticksTillNextCard = baseTicksTillCard
                    , narrative = nextNarrativePoint :: model.narrative
                    , seed = nextSeed
                }

              else
                { model | ticksTillNextCard = nextTicksRemaining }
            , Cmd.none
            )


randomNarrativePoint : Seed -> ( String, Seed )
randomNarrativePoint initialSeed =
    Random.step
        (Random.weighted
            ( 60, Travel )
            [ ( 10, Battle )
            , ( 30, Greeting )
            ]
            |> Random.andThen
                (\card ->
                    case card of
                        Travel ->
                            Random.weighted
                                ( 30, "You continue your journey down the dusty road." )
                                [ ( 10, "You take a short break. Take a sip of water and admire your surroundings. It's a lot of work traveling alone." )
                                , ( 1, "A small stone makes you stumble! You've been traveling so long, maybe you should take a break?" )
                                ]

                        Greeting ->
                            Random.weighted
                                ( 60, "You meet another weary traveler. They give you a swig of their wine pouch, exchange some pleasantries, and then continue on their way." )
                                [ ( 30, "A horse cairrage rushes past you. No time for conversation." )
                                , ( 1, "A band of joyous heros walks past, ranting and raving about the wonderful treasures they just found." )
                                , ( 9, "A couple of heros walks past. They're silent, with a painfully sad look on their face. They look like they want to be left alone" )
                                ]

                        Battle ->
                            Random.weighted
                                ( 20, "A wild boar comes rushing out of the bushes! You swiftly take out your knife but the boar is quicker. After a short and tiring tussle, you win the battle." )
                                [ ( 1, """The shadow of a dragon rushes overhead. Fear strikes your hear! You quickly ready your weapons, preparing to strike.
                                
The dragon lands in the road, scanning for where you went. Luckily for you it landed facing the other way. You leap out of the bushes, striking a weak spot on its tail with your sword. The dragin roars in pain! The tail trashes about but you manage to dodge it.

As it turns to face you, you spot a missing scale on its chest. Before the dragon knows what hit it, you strike a fatal blow.

With the battle won, you take a short rest. A victory like that deserves a break!""" )
                                , ( 79, "You step on an ant. A small victory, but a victory nonetheless!" )
                                ]
                )
        )
        initialSeed


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (viewBody model)


viewBody : Model -> Element Msg
viewBody { narrative } =
    Element.column
        [ Element.centerX
        , Element.width (Element.fill |> Element.maximum 600)
        , Element.spacing 16
        ]
        (List.map viewNarrativePoint narrative |> List.intersperse spacer)


spacer : Element msg
spacer =
    Element.el
        [ Border.widthEach
            { top = 1
            , bottom = 0
            , left = 0
            , right = 0
            }
        , Border.solid
        , Element.height (Element.px 1)
        , Element.width Element.fill
        ]
        Element.none


viewNarrativePoint : String -> Element Msg
viewNarrativePoint narrativePoint =
    Element.paragraph
        []
        [ Element.text narrativePoint ]
