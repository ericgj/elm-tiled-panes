module Tiles exposing
    ( Tiles
    , Tile
    , CanSplit
    , TileContext
    , init
    , setContent, clearContent, updateContent
    , zoom, clearZoom
    , canSplit, canSplitHoriz, canSplitVert
    , splitHoriz, splitVert
    , view
    )

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (style)



type Splits
    = Single
    | Horiz Int
    | Vert Int
    | HorizThenVert Int Int
    | VertThenHoriz Int Int

type Tile contents
    = Tile
        { contents : Maybe contents
        , splits : Splits
        }


type Tiles contents
    = Tiles
        { tiles : Array (Tile contents)
        , zoom : Maybe Int
        }


type Config contents msg
    = Config
        { gridAttributes : List (Html.Attribute msg)
        , view : (TileContext contents -> Html msg)
        }

-- exposed to calling context

type alias TileContext contents =
    { index : Int
    , canSplit : CanSplit
    , tile : Tile contents
    }

type alias CanSplit =
    { horiz :  Bool
    , vert : Bool
    }


-- Tile functions

tileContents : Tile a -> Maybe a
tileContents (Tile {contents}) =
    contents

singleTile : Tile a
singleTile =
    Tile { contents = Nothing, splits = Single }


toSingleTile : Tile a -> Tile a
toSingleTile (Tile tile) =
    Tile { tile | splits = Single }

toTileContext : Int -> Bool -> Tile a -> TileContext a
toTileContext index zoom tile =
    let
        newTile = 
            if zoom then 
                (toSingleTile tile)
            else
                tile
    in
        { index = index
        , canSplit = canSplitTile tile   -- note based on existing tile
        , tile = newTile
        }

canSplitTile : Tile a -> CanSplit
canSplitTile (Tile tile) =
    case tile.splits of
        Single ->
            { horiz = True
            , vert = True
            }

        Horiz _ ->
            { horiz = False
            , vert = True
            }

        Vert _ ->
            { horiz = True
            , vert = False
            }

        _ ->
            { horiz = False
            , vert = False
            }



-- Tiles functions


init : Tiles a
init =
    Array.empty
        |> Array.push singleTile
        |> (\tiles -> Tiles { tiles = tiles, zoom = Nothing })


setContent : Int -> a -> Tiles a -> Tiles a
setContent index a tiles =
    updateContent index (\_ -> Just a) tiles


clearContent : Int -> Tiles a -> Tiles a
clearContent index tiles =
    updateContent index (\_ -> Nothing) tiles


updateContent : Int -> (Maybe a -> Maybe a) -> Tiles a -> Tiles a
updateContent index func (Tiles data) =
    let
        updTiles i (Tile tile) =
            if index == i then
                Tile { tile | contents = func tile.contents }
            else
                Tile tile
    in
        data.tiles
            |> Array.indexedMap updTiles
            |> flip setTiles (Tiles data)


zoom : Int -> Tiles a -> Tiles a
zoom index (Tiles data) =
    if index < (Array.length data.tiles) then
        Tiles { data | zoom = Just index }
    else
        Tiles data


clearZoom : Tiles a -> Tiles a
clearZoom (Tiles data) =
    Tiles { data | zoom = Nothing }


canSplit : Int -> Tiles a -> CanSplit
canSplit index (Tiles data) =
    data.tiles
        |> Array.get index
        |> Maybe.map canSplitTile
        |> Maybe.withDefault { horiz = False, vert = False }


canSplitHoriz : Int -> Tiles a -> Bool
canSplitHoriz index tiles =
    canSplit index tiles |> .horiz

canSplitVert : Int -> Tiles a -> Bool
canSplitVert index tiles =
    canSplit index tiles |> .vert


splitHoriz : Int -> Tiles a -> Tiles a
splitHoriz index (Tiles data) =
    let
        newTile =
            singleTile

        updTile (Tile tile) newIndex =
            case tile.splits of
                Single ->
                    Tile { tile | splits = Horiz newIndex }

                Vert v ->
                    Tile { tile | splits = VertThenHoriz v newIndex }

                _ ->
                    Tile tile

        updTiles i tile =
            if index == i then
                updTile tile (Array.length data.tiles)
            else
                tile
    in
        data.tiles
            |> Array.indexedMap updTiles
            |> Array.push newTile
            |> flip setTiles (Tiles data)


splitVert : Int -> Tiles a -> Tiles a
splitVert index (Tiles data) =
    let
        newTile =
            singleTile

        updTile (Tile tile) newIndex =
            case tile.splits of
                Single ->
                    Tile { tile | splits = Vert newIndex }

                Horiz h ->
                    Tile { tile | splits = HorizThenVert h newIndex }

                _ ->
                    Tile tile

        updTiles i tile =
            if index == i then
                updTile tile (Array.length data.tiles)
            else
                tile
    in
        data.tiles
            |> Array.indexedMap updTiles
            |> Array.push newTile
            |> flip setTiles (Tiles data)



-- internal


setTiles : Array (Tile a) -> Tiles a -> Tiles a
setTiles tiles (Tiles data) =
    Tiles { data | tiles = tiles }


tileContext : Int -> Tiles a -> Maybe (TileContext a)
tileContext index (Tiles data) =
    let
        toContext tile =
            data.zoom
                |> Maybe.map ( (==) index )
                |> Maybe.withDefault False
                |> (\zoom -> toTileContext index zoom tile)
    in
        data.tiles
            |> Array.get index
            |> Maybe.map toContext


    
-- VIEW

view : Config a msg -> Tiles a -> Html msg
view config (Tiles data) =
    let
        viewTop context =
            viewTile config context (Tiles data) ( 1, 1 )
    in
        data.zoom
            |> Maybe.withDefault 0
            |> flip tileContext (Tiles data)
            |> Maybe.map viewTop
            |> Maybe.withDefault (text "")


-- note: shockingly complex, and not tail-call-optimized


viewTile : Config a msg -> TileContext a -> Tiles a -> ( Int, Int ) -> Html msg
viewTile (Config config) ctx (Tiles data) ( row, col ) =
    let
        splits (Tile tile) =
            tile.splits

        getTileContext index =
            tileContext index (Tiles data)

        getTileContext2 index1 index2 =
            Maybe.map2 (,) (getTileContext index1) (getTileContext index2)

        gridContainer ( rows, cols ) ( row, col ) children =
            div
                [ style
                    [ ( "display", "grid" )
                    , ( "grid-template-rows", String.repeat rows "1fr " )
                    , ( "grid-template-columns", String.repeat cols "1fr " )
                    , ( "grid-row", toString row )
                    , ( "grid-column", toString col )
                    ]
                ]
                children

        gridContents ( row, col ) children =
            div
                ( config.gridAttributes ++
                    [ style
                        [ ( "grid-row", toString row )
                        , ( "grid-column", toString col )
                        ]
                    ]
                )
                children
    in
        case splits ctx.tile of
            Single ->
                gridContainer ( 1, 1 )
                    ( row, col )
                    [ gridContents ( 1, 1 )
                        [ config.view ctx ]
                    ]

            Horiz h ->
                case getTileContext h of
                    Nothing ->
                        gridContainer ( 2, 1 )
                            ( row, col )
                            [ gridContents ( 1, 1 )
                                [ config.view ctx ]
                            ]

                    Just htile ->
                        let
                            subtree =
                                viewTile 
                                    (Config config) 
                                    htile 
                                    (Tiles data) 
                                    ( 2, 1 )
                        in
                            gridContainer ( 2, 1 )
                                ( row, col )
                                [ gridContents ( 1, 1 )
                                    [ config.view ctx ]
                                , subtree
                                ]

            Vert v ->
                case getTileContext v of
                    Nothing ->
                        gridContainer ( 1, 2 )
                            ( row, col )
                            [ gridContents ( 1, 1 )
                                [ config.view ctx ]
                            ]

                    Just vtile ->
                        let
                            subtree =
                                viewTile 
                                    (Config config) 
                                    vtile 
                                    (Tiles data) 
                                    ( 1, 2 )
                        in
                            gridContainer ( 1, 2 )
                                ( row, col )
                                [ gridContents ( 1, 1 )
                                    [ config.view ctx ]
                                , subtree
                                ]

            HorizThenVert h v ->
                case getTileContext2 h v of
                    Nothing ->
                        gridContainer ( 2, 1 )
                            ( row, col )
                            [ gridContainer ( 1, 2 )
                                ( 1, 1 )
                                [ gridContents ( 1, 1 )
                                    [ config.view ctx
                                    ]
                                ]
                            ]

                    Just ( htile, vtile ) ->
                        let
                            hsubtree =
                                viewTile 
                                    (Config config) 
                                    htile 
                                    (Tiles data) 
                                    ( 2, 1 )

                            vsubtree =
                                viewTile 
                                    (Config config) 
                                    vtile 
                                    (Tiles data) 
                                    ( 1, 2 )
                        in
                            gridContainer ( 2, 1 )
                                ( row, col )
                                [ gridContainer ( 1, 2 )
                                    ( 1, 1 )
                                    [ gridContents ( 1, 1 )
                                        [ config.view ctx
                                        ]
                                    , vsubtree
                                    ]
                                , hsubtree
                                ]

            VertThenHoriz v h ->
                case getTileContext2 v h of
                    Nothing ->
                        gridContainer ( 1, 2 )
                            ( row, col )
                            [ gridContainer ( 2, 1 )
                                ( 1, 1 )
                                [ gridContents ( 1, 1 )
                                    [ config.view ctx
                                    ]
                                ]
                            ]

                    Just ( vtile, htile ) ->
                        let
                            hsubtree =
                                viewTile 
                                    (Config config) 
                                    htile 
                                    (Tiles data) 
                                    ( 2, 1 )

                            vsubtree =
                                viewTile 
                                    (Config config) 
                                    vtile 
                                    (Tiles data) 
                                    ( 1, 2 )
                        in
                            gridContainer ( 1, 2 )
                                ( row, col )
                                [ gridContainer ( 2, 1 )
                                    ( 1, 1 )
                                    [ gridContents ( 1, 1 )
                                        [ config.view ctx
                                        ]
                                    , hsubtree
                                    ]
                                , vsubtree
                                ]


-- CONFIG

customConfig : 
    { a | gridAttributes : List (Html.Attribute msg), view : TileContext contents -> Html msg } 
    -> Config contents msg
customConfig { gridAttributes, view } =
    Config { gridAttributes = gridAttributes, view = view }
