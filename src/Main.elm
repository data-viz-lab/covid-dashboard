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
    { data : Data
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


view : Model -> Html Msg
view model =
    case model.serverData of
        RemoteData.Success _ ->
            Html.div [ class "viz" ] (charts model)

        RemoteData.Loading ->
            Html.div [ class "pre-chart" ] [ Html.text "Loading data..." ]

        _ ->
            Html.div [ class "pre-chart" ] [ Html.text "Something went wrong" ]


charts : Model -> List (Html Msg)
charts model =
    let
        countries =
            model.data
                |> List.map .country
                |> Set.fromList
                |> Set.toList
    in
    countries
        |> List.map
            (\location ->
                Html.div [ class "viz__wrapper" ]
                    [ Html.div [ class "viz__title" ] [ Html.h2 [] [ Html.text location ] ]
                    , Html.div [ class "viz__item" ] [ chart location model ]
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
chart location model =
    let
        { width, height } =
            model.dimensions
                |> Result.withDefault { width = 0, height = 0 }

        color =
            Color.rgb255 240 59 32

        data =
            model.data
                |> List.filter (\d -> d.country == location)
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
        |> Line.withXContinuousDomain ( 0, 145 )
        |> Line.render ( data, accessor )



-- REMOTE CORONAVIRUS DATA


fetchData : Cmd Msg
fetchData =
    Http.get
        { url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
        , expect = Http.expectString (RemoteData.fromResult >> DataResponse)
        }


locations : List String
locations =
    [ "United States"
    , "United Kingdom"
    , "Italy"
    , "Germany"
    , "Belgium"
    , "Brazil"
    , "France"
    , "Sweden"
    , "India"
    , "China"
    , "Spain"
    , "Russia"
    ]


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


prepareData : WebData String -> Data
prepareData rd =
    rd
        |> RemoteData.map
            (\str ->
                let
                    csv =
                        Csv.parse str
                in
                csv.records
                    |> Array.fromList
                    |> Array.map Array.fromList
                    --|> Array.filter
                    --    (\r ->
                    --        r
                    --            |> Array.get countryIdx
                    --            |> Maybe.map (\location -> List.member location locations)
                    --            |> Maybe.withDefault False
                    --    )
                    |> Array.map
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
                    |> Array.foldl
                        (\r acc ->
                            let
                                k =
                                    r.date |> Time.posixToMillis
                            in
                            case Dict.get k acc of
                                Just v ->
                                    Dict.insert k (r :: v) acc

                                Nothing ->
                                    Dict.insert k [ r ] acc
                        )
                        Dict.empty
                    -- only keep data shared across all countries
                    --|> Dict.filter (\k v -> List.length v == List.length locations)
                    |> Dict.toList
                    |> List.map Tuple.second
                    |> List.concat
            )
        |> RemoteData.withDefault []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    updateDimensions OnUpdateDimensions



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( { data = []
      , dimensions = Result.Ok { height = 0, width = 0 }
      , serverData = RemoteData.Loading
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
