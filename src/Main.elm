port module Main exposing (..)

import Array exposing (Array)
import Axis
import Browser
import Chart.Bar as Bar
import Chart.Line as Line
import Color exposing (rgb255)
import Csv exposing (Csv)
import Dict exposing (Dict)
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Http
import Iso8601
import Json.Decode as Decode
import RemoteData exposing (RemoteData, WebData)
import Set
import Shape
import Time exposing (Posix)



-- PORTS


port observeDimensions : String -> Cmd msg


port updateDimensions : (Decode.Value -> msg) -> Sub msg



-- MODEL


type alias Dimensions =
    { width : Float, height : Float }


type alias Datum =
    { country : String
    , date : Posix
    , value : Float
    }


type alias Data =
    List Datum


type alias Model =
    { data : Dict String Data
    , domain : ( Float, Float )
    , dimensions : Result Decode.Error Dimensions
    , serverData : WebData String
    }



-- UPDATE


type Msg
    = DataResponse (WebData String)
    | OnUpdateDimensions Decode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataResponse response ->
            let
                data =
                    prepareData response
            in
            ( { model
                | data = data
                , serverData = response

                --, domain = getDomain data
              }
            , observeDimensions "viz__item"
            )

        OnUpdateDimensions response ->
            ( { model | dimensions = decodeDimensions response }, Cmd.none )


decodeDimensions : Decode.Value -> Result Decode.Error Dimensions
decodeDimensions value =
    Decode.decodeValue
        (Decode.oneOf
            [ Decode.map2 Dimensions
                (Decode.field "height" Decode.float)
                (Decode.field "width" Decode.float)
            , Decode.map2 Dimensions
                (Decode.field "inlineSize" Decode.float)
                (Decode.field "blockSize" Decode.float)
            ]
        )
        value



-- VIEW


footer : Html msg
footer =
    Html.footer
        [ style "margin" "25px"
        ]
        [ Html.a
            [ Html.Attributes.href
                "https://github.com/owid/covid-19-data/tree/master/public/data"
            , style "color" "#fff"
            ]
            [ Html.text "Data source" ]
        , Html.text ", "
        , Html.a
            [ Html.Attributes.href
                "https://github.com/data-viz-lab/covid-dashboard"
            , style "color" "#fff"
            ]
            [ Html.text "Source code" ]
        ]


view : Model -> Html Msg
view model =
    case model.serverData of
        RemoteData.Success _ ->
            let
                countries =
                    model.data
                        |> Dict.keys
            in
            Html.div [ class "wrapper" ]
                [ Html.header [ class "header" ]
                    [ Html.h1 [] [ Html.text "Coronavirus, new deaths per million" ]
                    ]
                , Html.div
                    [ class "viz" ]
                    (charts countries model)
                , footer
                ]

        RemoteData.Loading ->
            Html.div [ class "pre-chart" ] [ Html.text "Please have patience, loading a big dataset..." ]

        _ ->
            Html.div [ class "pre-chart" ] [ Html.text "Something went wrong" ]


charts : List String -> Model -> List (Html Msg)
charts countries model =
    countries
        |> List.map
            (\country ->
                Html.div [ class "viz__wrapper" ]
                    [ Html.div [ class "viz__title" ] [ Html.h2 [] [ Html.text country ] ]
                    , Html.div [ class "viz__item" ] [ chart country model ]
                    ]
            )



-- CHART CONFIGURATION


accessor : Line.Accessor Datum
accessor =
    Line.time
        (Line.AccessorTime (.country >> Just) .date .value)


valueFormatter : Float -> String
valueFormatter =
    FormatNumber.format { usLocale | decimals = 0 }


yAxis : Bar.YAxis Float
yAxis =
    Line.axisLeft
        [ Axis.tickCount 5
        , Axis.tickSizeOuter 0
        , Axis.tickFormat (abs >> valueFormatter)
        ]


xAxis : Bar.XAxis Posix
xAxis =
    Line.axisBottom
        [ Axis.tickSizeOuter 0
        ]



-- CHART


chart : String -> Model -> Html msg
chart country model =
    let
        { width, height } =
            model.dimensions
                |> Result.withDefault { width = 0, height = 0 }

        color =
            Color.rgb255 240 59 32

        data =
            model.data
                |> Dict.get country
                |> Maybe.withDefault []
    in
    Line.init
        { margin = { top = 10, right = 10, bottom = 10, left = 10 }
        , width = width
        , height = height
        }
        |> Line.withCurve (Shape.cardinalCurve 0.5)
        |> Line.withStackedLayout (Line.drawArea Shape.stackOffsetSilhouette)
        |> Line.withColorPalette [ color ]
        |> Line.hideAxis
        |> Line.withYDomain ( -10, 10 )
        |> Line.render ( data, accessor )



-- REMOTE CORONAVIRUS DATA


fetchData : Cmd Msg
fetchData =
    Http.get
        { url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
        , expect = Http.expectString (RemoteData.fromResult >> DataResponse)
        }


countryIdx : Int
countryIdx =
    2


dateIdx : Int
dateIdx =
    3


valueIdx : Int
valueIdx =
    -- new_deaths_smoothed
    --9
    -- new_deaths_smoothed_per_million
    15


prepareData : WebData String -> Dict String Data
prepareData rd =
    rd
        |> RemoteData.map
            (\str ->
                let
                    csv =
                        Csv.parse str
                in
                csv.records
                    |> List.map Array.fromList
                    |> List.map
                        (\r ->
                            { date =
                                Array.get dateIdx r
                                    |> Maybe.withDefault ""
                                    |> Iso8601.toTime
                                    |> Result.withDefault (Time.millisToPosix 0)
                            , country =
                                Array.get countryIdx r
                                    |> Maybe.withDefault ""
                            , value =
                                Array.get valueIdx r
                                    |> Maybe.andThen String.toFloat
                                    |> Maybe.withDefault 0
                            }
                        )
                    |> List.foldl
                        (\r acc ->
                            let
                                k =
                                    r.country
                            in
                            case Dict.get k acc of
                                Just v ->
                                    Dict.insert k (r :: v) acc

                                Nothing ->
                                    Dict.insert k [ r ] acc
                        )
                        Dict.empty
                    -- only keep countries with extensive data
                    |> Dict.filter (\k v -> List.length v > 200)
            )
        |> RemoteData.withDefault Dict.empty


getDomain : Dict String Data -> ( Float, Float )
getDomain data =
    data
        |> Dict.toList
        |> List.map Tuple.second
        |> List.concat
        |> List.map .value
        |> List.maximum
        |> Maybe.withDefault 1
        |> (\max -> ( max * -1, max ))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    updateDimensions OnUpdateDimensions



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( { data = Dict.empty
      , dimensions = Result.Ok { height = 0, width = 0 }
      , serverData = RemoteData.Loading
      , domain = ( 0, 0 )
      }
    , fetchData
    )



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
