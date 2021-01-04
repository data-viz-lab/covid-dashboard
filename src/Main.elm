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
import Html.Attributes exposing (class)
import Html.Events
import Http
import Iso8601
import Json.Decode as Decode
import LTTB
import RemoteData exposing (RemoteData, WebData)
import Set
import Shape
import Time exposing (Posix)



-- PORTS


port observeDimensions : String -> Cmd msg


port updateDimensions : (Decode.Value -> msg) -> Sub msg



-- MODEL


type SortMode
    = Alphabetical
    | ByDeathsAsc
    | ByDeathsDesc


stringToSortMode : String -> SortMode
stringToSortMode str =
    case str of
        "alphabetical" ->
            Alphabetical

        "byDeathsAsc" ->
            ByDeathsAsc

        "byDeathsDesc" ->
            ByDeathsDesc

        _ ->
            Alphabetical


sortModeToString : SortMode -> String
sortModeToString sortMode =
    case sortMode of
        Alphabetical ->
            "alphabetical"

        ByDeathsAsc ->
            "byDeathsAsc"

        ByDeathsDesc ->
            "byDeathsDesc"


type alias Stats =
    { totalDeaths : Float }


type alias Dimensions =
    { width : Float, height : Float }


type alias Datum =
    { country : String
    , date : Float
    , value : Float
    }


type alias Data =
    List Datum


type alias Model =
    { data : Dict String ( Data, Stats )
    , dimensions : Result Decode.Error Dimensions
    , domain : ( Float, Float )
    , serverData : WebData String
    , sortMode : SortMode
    }



-- UPDATE


type Msg
    = DataResponse (WebData String)
    | OnSortByUpdate String
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
                , domain = getDomain data
              }
            , observeDimensions "viz__item"
            )

        OnUpdateDimensions response ->
            ( { model | dimensions = decodeDimensions response }, Cmd.none )

        OnSortByUpdate sortMode ->
            ( { model | sortMode = stringToSortMode sortMode }, Cmd.none )


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


sortByView : Model -> Html Msg
sortByView model =
    Html.select [ Html.Events.onInput OnSortByUpdate ]
        [ Html.option
            [ Html.Attributes.value (sortModeToString Alphabetical)
            , Html.Attributes.selected (model.sortMode == Alphabetical)
            ]
            [ Html.text (sortModeToString Alphabetical) ]
        , Html.option
            [ Html.Attributes.value (sortModeToString ByDeathsAsc)
            , Html.Attributes.selected (model.sortMode == ByDeathsAsc)
            ]
            [ Html.text "By deaths ascending" ]
        , Html.option
            [ Html.Attributes.value (sortModeToString ByDeathsDesc)
            , Html.Attributes.selected (model.sortMode == ByDeathsDesc)
            ]
            [ Html.text "By deaths descending" ]
        ]


charts : Model -> List (Html Msg)
charts model =
    sortedCountries model
        |> List.map
            (\country ->
                Html.div [ class "viz__wrapper" ]
                    [ Html.div [ class "viz__title" ] [ Html.h2 [] [ Html.text country ] ]
                    , Html.div [ class "viz__item" ] [ chart country model ]
                    ]
            )


footer : Html msg
footer =
    Html.footer
        []
        [ Html.a
            [ Html.Attributes.href
                "https://github.com/owid/covid-19-data/tree/master/public/data"
            ]
            [ Html.text "Data source" ]
        , Html.text ", "
        , Html.a
            [ Html.Attributes.href
                "https://github.com/data-viz-lab/covid-dashboard"
            ]
            [ Html.text "Source code" ]
        ]


sortedCountries : Model -> List String
sortedCountries model =
    model.data
        |> Dict.map (\k ( d, s ) -> s.totalDeaths)
        |> Dict.toList
        |> (\d ->
                case model.sortMode of
                    Alphabetical ->
                        d
                            |> List.sortBy Tuple.first
                            |> List.map Tuple.first

                    ByDeathsAsc ->
                        d
                            |> List.sortBy Tuple.second
                            |> List.map Tuple.first

                    ByDeathsDesc ->
                        d
                            |> List.sortBy Tuple.second
                            |> List.map Tuple.first
                            |> List.reverse
           )


view : Model -> Html Msg
view model =
    case model.serverData of
        RemoteData.Success _ ->
            Html.div [ class "wrapper" ]
                [ Html.header [ class "header" ]
                    [ Html.h1 [] [ Html.text "Coronavirus, new deaths per million" ]
                    , sortByView model
                    ]
                , Html.div
                    [ class "viz" ]
                    (charts model)
                , footer
                ]

        RemoteData.Loading ->
            Html.div [ class "pre-chart" ]
                [ Html.text "Loading a big dataset... this might take a while..."
                ]

        _ ->
            Html.div [ class "pre-chart" ] [ Html.text "Something went wrong" ]



-- CHART CONFIGURATION


accessor : Line.Accessor Datum
accessor =
    Line.continuous
        (Line.AccessorContinuous (.country >> Just) .date .value)


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
                |> Maybe.withDefault ( [], { totalDeaths = 0 } )
                |> Tuple.first
    in
    Line.init
        { margin = { top = 2, right = 2, bottom = 2, left = 2 }
        , width = width
        , height = height
        }
        |> Line.withCurve (Shape.cardinalCurve 0.5)
        |> Line.withStackedLayout (Line.drawArea Shape.stackOffsetSilhouette)
        |> Line.withColorPalette [ color ]
        |> Line.hideAxis
        |> Line.withoutTable
        |> Line.withYDomain model.domain
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


exclude : List String
exclude =
    -- data outliers
    [ "Bolivia"
    , "Ecuador"
    , "International"
    , "Kyrgyzstan"
    , "Liechtenstein"
    , "Peru"
    , "San Marino"
    , "World"
    , "Vatican"
    ]


prepareData : WebData String -> Dict String ( Data, Stats )
prepareData rd =
    rd
        |> RemoteData.map
            (\str ->
                let
                    records =
                        Csv.parse str
                            |> .records
                            |> List.map Array.fromList
                            |> List.map
                                (\r ->
                                    { date =
                                        Array.get dateIdx r
                                            |> Maybe.withDefault ""
                                            |> Iso8601.toTime
                                            |> Result.withDefault (Time.millisToPosix 0)
                                            |> Time.posixToMillis
                                            |> toFloat
                                    , country =
                                        Array.get countryIdx r
                                            |> Maybe.withDefault ""
                                    , value =
                                        Array.get valueIdx r
                                            |> Maybe.andThen String.toFloat
                                            |> Maybe.withDefault 0
                                    }
                                )

                    dates =
                        records
                            |> List.map .date

                    --start =
                    --    dates
                    --        |> List.minimum
                    --        |> Maybe.withDefault 0
                    --end =
                    --    dates
                    --        |> List.maximum
                    --        |> Maybe.withDefault 0
                in
                records
                    |> List.foldl
                        (\r acc ->
                            let
                                k =
                                    r.country
                            in
                            case Dict.get k acc of
                                Just ( d, s ) ->
                                    Dict.insert k ( r :: d, { totalDeaths = maxDeaths (r :: d) } ) acc

                                Nothing ->
                                    Dict.insert k ( [ r ], { totalDeaths = 0 } ) acc
                        )
                        Dict.empty
                    -- this is just an overview, lets seriouly downsample the data for performance reasons
                    |> Dict.map (\k ( v, s ) -> ( downsampleData v, s ))
                    -- only keep countries with extensive data
                    |> Dict.filter (\k ( v, s ) -> List.length v > 50 && List.member k exclude |> not)
            )
        |> RemoteData.withDefault Dict.empty


downsampleData : Data -> Data
downsampleData data =
    LTTB.downsample
        { data = data
        , threshold = 55
        , xGetter = .date
        , yGetter = .value
        }


maxDeaths : List { a | value : Float } -> Float
maxDeaths country =
    country
        |> List.map .value
        |> List.maximum
        |> Maybe.withDefault 0


getDomain : Dict String ( Data, Stats ) -> ( Float, Float )
getDomain data =
    data
        |> Dict.map (\k v -> Tuple.first v)
        |> Dict.toList
        |> List.map Tuple.second
        |> List.concat
        |> List.map .value
        |> List.maximum
        |> Maybe.withDefault 1
        |> (\max -> ( (max / 2) * -1, max / 2 ))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    updateDimensions OnUpdateDimensions



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( { data = Dict.empty
      , dimensions = Result.Ok { height = 0, width = 0 }
      , domain = ( 0, 0 )
      , serverData = RemoteData.Loading
      , sortMode = Alphabetical
      }
    , fetchData
    )



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
