module TileLayout exposing (main)

import Html exposing (..)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput, on, onWithOptions)
import Json.Decode

import Tiles exposing (Tiles, TileContext, Config)


type alias Model =
    { tiles : Tiles String
    , current : Int
    , editingName : Bool
    }


initModel : Model
initModel =
    { tiles = Tiles.init
    , current = 0
    , editingName = False
    }


type Msg
    = SplitHoriz Int
    | SplitVert Int
    | Remove Int
    | Zoom Int
    | ClearZoom
    | EditingName Int
    | SetName Int String
    | EditedName Int
    | SetCurrent Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        SplitHoriz index ->
            { model
                | tiles = Tiles.splitHoriz index model.tiles
                , current = index
            }

        SplitVert index ->
            { model
                | tiles = Tiles.splitVert index model.tiles
                , current = index
            }

        Remove index ->
            { model
                | tiles = Tiles.remove index model.tiles
                , current = 0
            }

        Zoom index ->
            { model
                | tiles = Tiles.zoom index model.tiles
                , current = index
            }

        ClearZoom ->
            { model
                | tiles = Tiles.clearZoom model.tiles
            }

        EditingName index ->
            { model | editingName = True }

        SetName index name ->
            { model
                | tiles = Tiles.setContent index name model.tiles
            }

        EditedName index ->
            { model | editingName = False }

        SetCurrent index ->
            { model
                | current = index
            }


viewModel : Model -> Html Msg
viewModel model =
    div []
        [ Tiles.view (modelConfig model) model.tiles
        ]


render : Int -> Bool -> TileContext String -> Html Msg
render current editingName { index, tile, zoomed, canSplit } =
    let
        headerButton msg icon =
            div
                [ style
                    [ ( "cursor", "pointer" )
                    ]
                , onClickNoProp msg
                ]
                [ text icon ]

        splitHorizButton =
            headerButton (SplitHoriz index) "↕"

        splitVertButton =
            headerButton (SplitVert index) "↔"

        removeButton =
            headerButton (Remove index) "×"

        nameInput =
            input
                [ style
                    [ ( "height", "1em" )
                    ]
                , value (Tiles.tileContents tile |> Maybe.withDefault "")
                , onInput (SetName index)
                , onBlur (EditedName index)
                ]
                []

        nameDisplay =
            div
                [ style
                    [ ( "display", "inline-block" )
                    , ( "margin-right", "0.5em" )
                    , ( "cursor", "pointer" )
                    ]
                , onClickNoProp (EditingName index)
                ]
                [ text <| displayTitle ]

        displayTitle =
            (toString (index + 1))
                ++ " : "
                ++ (Tiles.tileContents tile |> Maybe.withDefault "(unnamed)")
    in
        div
            [ style
                [ ( "height", "500px" )
                ]
            , onClickNoProp (SetCurrent index)
            ]
            [ div
                [ style
                    ([ ( "display", "block" )
                     , ( "padding", "5px" )
                     , ( "background-color"
                       , (if current == index then
                            "#c3c"
                          else
                            "#aaa"
                         )
                       )
                     , ( "color", "white" )
                     , ( "border-radius", "5px 5px 0 0" )
                     , ( "font-family", "Verdana, Helvetica, Arial, sans-serif" )
                     ]
                        ++ (if zoomed then
                                [ ( "height", "auto" ) ]
                            else
                                []
                           )
                    )
                , onClickNoProp
                    (if current == index then
                        if zoomed then
                            ClearZoom
                        else
                            (Zoom index)
                     else
                        (SetCurrent index)
                    )
                ]
                [ div
                    [ style
                        [ ( "display", "inline-block" )
                        , ( "margin-right", "0.5em" )
                        ]
                    ]
                    (if index > 0 && (not zoomed) then
                        [ removeButton ]
                     else
                        []
                    )
                , (if current == index && editingName then
                    nameInput
                   else
                    nameDisplay
                  )
                , div
                    [ style
                        [ ( "display", "inline-block" )
                        , ( "margin-right", "0.5em" )
                        ]
                    ]
                    (if canSplit.horiz && (not zoomed) then
                        [ splitHorizButton ]
                     else
                        []
                    )
                , div
                    [ style
                        [ ( "display", "inline-block" )
                        , ( "margin-right", "0.5em" )
                        ]
                    ]
                    (if canSplit.vert && (not zoomed) then
                        [ splitVertButton ]
                     else
                        []
                    )
                ]
            , div
                []
                []
            ]


onClickNoProp : msg -> Html.Attribute msg
onClickNoProp msg =
    onWithOptions "click"
        { stopPropagation = True, preventDefault = False }
        (Json.Decode.succeed msg)


onBlur : msg -> Html.Attribute msg
onBlur msg =
    on "blur" (Json.Decode.succeed msg)


modelConfig : Model -> Config String Msg
modelConfig { current, editingName } =
    Tiles.customConfig
        { gridAttributes =
            [ style
                [ ( "border", "1px solid #ccc" )
                , ( "margin", "2px" )
                , ( "border-radius", "5px 5px 0 0" )
                ]
            ]
        , view = render current editingName
        }


main : Program Never Model Msg
main =
    beginnerProgram
        { view = viewModel
        , update = update
        , model = initModel
        }

