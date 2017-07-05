module Tiles exposing
    ( Tiles
    , Tile
    , init
    , setContent, clearContent, updateContent
    , splitHoriz
    , splitVert
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
    = Tiles (Array (Tile contents))


single : Tile a
single =
    Tile { contents = Nothing, splits = Single }


init : Tiles a
init =
    Array.empty
        |> Array.push single
        |> Tiles


setContent : Int -> a -> Tiles a -> Tiles a
setContent index a tiles =
    updateContent index (\_ -> Just a) tiles


clearContent : Int -> Tiles a -> Tiles a
clearContent index tiles =
    updateContent index (\_ -> Nothing) tiles


updateContent : Int -> (Maybe a -> Maybe a) -> Tiles a -> Tiles a
updateContent index func (Tiles tiles) =
    let
        updTiles i (Tile tile) =
            if index == i then
                Tile { tile | contents = func tile.contents }
            else
                Tile tile
    in
        tiles
            |> Array.indexedMap updTiles
            |> Tiles


splitHoriz : Int -> Tiles a -> Tiles a
splitHoriz index (Tiles tiles) =
    let
        newTile =
            single

        updTile (Tile tile) newIndex =
            case tile.splits of
                Single ->
                    Tile { tile | splits = Horiz newIndex }

                Horiz h ->
                    Tile tile

                Vert v ->
                    Tile { tile | splits = VertThenHoriz v newIndex }

                HorizThenVert h v ->
                    Tile tile

                VertThenHoriz v h ->
                    Tile tile

        updTiles i tile =
            if index == i then
                updTile tile (Array.length tiles)
            else
                tile
    in
        tiles
            |> Array.indexedMap updTiles
            |> Array.push newTile
            |> Tiles


splitVert : Int -> Tiles a -> Tiles a
splitVert index (Tiles tiles) =
    let
        newTile =
            single

        updTile (Tile tile) newIndex =
            case tile.splits of
                Single ->
                    Tile { tile | splits = Vert newIndex }

                Horiz h ->
                    Tile { tile | splits = HorizThenVert h newIndex }

                Vert v ->
                    Tile tile

                HorizThenVert h v ->
                    Tile tile

                VertThenHoriz v h ->
                    Tile tile

        updTiles i tile =
            if index == i then
                updTile tile (Array.length tiles)
            else
                tile
    in
        tiles
            |> Array.indexedMap updTiles
            |> Array.push newTile
            |> Tiles


view : (a -> Html msg) -> Tiles a -> Html msg
view render (Tiles tiles) =
    let
        viewHead tile =
            viewTile render tile (Tiles tiles) ( 1, 1 )
    in
        tiles
            |> Array.get 0
            |> Maybe.map viewHead
            |> Maybe.withDefault (text "")


-- note: shockingly complex, and not tail-call-optimized
viewTile : (a -> Html msg) -> Tile a -> Tiles a -> ( Int, Int ) -> Html msg
viewTile render (Tile tile) (Tiles tiles) ( row, col ) =
    let
        getTile index =
            tiles
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
                                viewTile render htile (Tiles tiles) ( 2, 1 )
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
                                viewTile render vtile (Tiles tiles) ( 1, 2 )
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
                                viewTile render htile (Tiles tiles) ( 2, 1 )

                            vsubtree =
                                viewTile render vtile (Tiles tiles) ( 1, 2 )
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
                                viewTile render htile (Tiles tiles) ( 2, 1 )

                            vsubtree =
                                viewTile render vtile (Tiles tiles) ( 1, 2 )
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

