module Tiles exposing
    ( Tiles
    , Tile
    , init
    , setContent, clearContent, updateContent
    , zoom, clearZoom
    , canSplitHoriz, canSplitVert
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



-- Tile functions


singleTile : Tile a
singleTile =
    Tile { contents = Nothing, splits = Single }


toSingleTile : Tile a -> Tile a
toSingleTile (Tile tile) =
    Tile { tile | splits = Single }



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


canSplitHoriz : Int -> Tiles a -> Bool
canSplitHoriz index (Tiles data) =
    let
        check (Tile tile) =
            case tile.splits of
                Single ->
                    True

                Vert _ ->
                    True

                _ ->
                    False
    in
        data.tiles
            |> Array.get index
            |> Maybe.map check
            |> Maybe.withDefault False


canSplitVert : Int -> Tiles a -> Bool
canSplitVert index (Tiles data) =
    let
        check (Tile tile) =
            case tile.splits of
                Single ->
                    True

                Horiz _ ->
                    True

                _ ->
                    False
    in
        data.tiles
            |> Array.get index
            |> Maybe.map check
            |> Maybe.withDefault False


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


view : (a -> Html msg) -> Tiles a -> Html msg
view render (Tiles data) =
    let
        viewTop tile =
            viewTile render tile (Tiles data) ( 1, 1 )
    in
        case data.zoom of
            Nothing ->
                data.tiles
                    |> Array.get 0
                    |> Maybe.map viewTop
                    |> Maybe.withDefault (text "")

            Just n ->
                data.tiles
                    |> Array.get n
                    |> Maybe.map (toSingleTile >> viewTop)
                    |> Maybe.withDefault (text "")



-- note: shockingly complex, and not tail-call-optimized


viewTile : (a -> Html msg) -> Tile a -> Tiles a -> ( Int, Int ) -> Html msg
viewTile render (Tile tile) (Tiles data) ( row, col ) =
    let
        getTile index =
            data.tiles
                |> Array.get index

        getTile2 index1 index2 =
            Maybe.map2 (,) (getTile index1) (getTile index2)

        contentsOrEmpty ma =
            Maybe.map render ma |> Maybe.withDefault (text "")

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
                [ style
                    [ ( "grid-row", toString row )
                    , ( "grid-column", toString col )
                    ]
                ]
                children
    in
        case tile.splits of
            Single ->
                gridContainer ( 1, 1 )
                    ( row, col )
                    [ gridContents ( 1, 1 )
                        [ contentsOrEmpty tile.contents ]
                    ]

            Horiz h ->
                case getTile h of
                    Nothing ->
                        gridContainer ( 2, 1 )
                            ( row, col )
                            [ gridContents ( 1, 1 )
                                [ contentsOrEmpty tile.contents ]
                            ]

                    Just htile ->
                        let
                            subtree =
                                viewTile render htile (Tiles data) ( 2, 1 )
                        in
                            gridContainer ( 2, 1 )
                                ( row, col )
                                [ gridContents ( 1, 1 )
                                    [ contentsOrEmpty tile.contents ]
                                , subtree
                                ]

            Vert v ->
                case getTile v of
                    Nothing ->
                        gridContainer ( 1, 2 )
                            ( row, col )
                            [ gridContents ( 1, 1 )
                                [ contentsOrEmpty tile.contents ]
                            ]

                    Just vtile ->
                        let
                            subtree =
                                viewTile render vtile (Tiles data) ( 1, 2 )
                        in
                            gridContainer ( 1, 2 )
                                ( row, col )
                                [ gridContents ( 1, 1 )
                                    [ contentsOrEmpty tile.contents ]
                                , subtree
                                ]

            HorizThenVert h v ->
                case getTile2 h v of
                    Nothing ->
                        gridContainer ( 2, 1 )
                            ( row, col )
                            [ gridContainer ( 1, 2 )
                                ( 1, 1 )
                                [ gridContents ( 1, 1 )
                                    [ contentsOrEmpty tile.contents
                                    ]
                                ]
                            ]

                    Just ( htile, vtile ) ->
                        let
                            hsubtree =
                                viewTile render htile (Tiles data) ( 2, 1 )

                            vsubtree =
                                viewTile render vtile (Tiles data) ( 1, 2 )
                        in
                            gridContainer ( 2, 1 )
                                ( row, col )
                                [ gridContainer ( 1, 2 )
                                    ( 1, 1 )
                                    [ gridContents ( 1, 1 )
                                        [ contentsOrEmpty tile.contents
                                        ]
                                    , vsubtree
                                    ]
                                , hsubtree
                                ]

            VertThenHoriz v h ->
                case getTile2 v h of
                    Nothing ->
                        gridContainer ( 1, 2 )
                            ( row, col )
                            [ gridContainer ( 2, 1 )
                                ( 1, 1 )
                                [ gridContents ( 1, 1 )
                                    [ contentsOrEmpty tile.contents
                                    ]
                                ]
                            ]

                    Just ( vtile, htile ) ->
                        let
                            hsubtree =
                                viewTile render htile (Tiles data) ( 2, 1 )

                            vsubtree =
                                viewTile render vtile (Tiles data) ( 1, 2 )
                        in
                            gridContainer ( 1, 2 )
                                ( row, col )
                                [ gridContainer ( 2, 1 )
                                    ( 1, 1 )
                                    [ gridContents ( 1, 1 )
                                        [ contentsOrEmpty tile.contents
                                        ]
                                    , hsubtree
                                    ]
                                , vsubtree
                                ]
