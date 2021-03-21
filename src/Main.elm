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
import Json.Encode as Encode
import LTTB
import RemoteData exposing (RemoteData, WebData)
import Set
import Shape
import Time exposing (Posix)



-- PORTS


port observeDimensions : String -> Cmd msg


port storeResponseData : String -> Cmd msg


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
    { width : Float
    , height : Float
    , windowHeight : Float
    , windowWidth : Float
    }


defaultDimensions : Dimensions
defaultDimensions =
    { width = 0
    , height = 0
    , windowHeight = 0
    , windowWidth = 0
    }


type alias Datum =
    { country : String
    , date : Float
    , value : Float
    }


type alias Data =
    List Datum


type DataType
    = Full
    | Downsampled


type alias Model =
    { data : WebData (Dict String ( Data, Stats ))
    , fullData : Dict String ( Data, Stats )
    , dimensions : Result Decode.Error Dimensions
    , domain : ( Float, Float )
    , selectedCountry : Maybe String
    , sortMode : SortMode
    }


encodeDatum : Datum -> Encode.Value
encodeDatum { country, date, value } =
    Encode.object
        [ ( "country", Encode.string country )
        , ( "date", Encode.float date )
        , ( "value", Encode.float value )
        ]



-- UPDATE


type Msg
    = DataResponse (WebData String)
    | OnCountrySelect String
    | OnCountryClose
    | OnSortByUpdate String
    | OnUpdateDimensions Decode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataResponse response ->
            let
                responseData =
                    response
                        |> responseToData

                toPortData =
                    responseData
                        |> RemoteData.map (Encode.list encodeDatum >> Encode.encode 0 >> storeResponseData)
                        |> RemoteData.withDefault Cmd.none

                serverData =
                    responseData
                        |> responseDataToDict

                data =
                    prepareData Downsampled serverData

                fullData =
                    prepareData Full serverData
                        |> RemoteData.withDefault Dict.empty

                domain =
                    data
                        |> RemoteData.withDefault Dict.empty
                        |> getDomain
            in
            ( { model
                | data = data
                , fullData = fullData
                , domain = domain
              }
            , Cmd.batch
                [ observeDimensions ".viz__item"
                , toPortData
                ]
            )

        OnCountrySelect target ->
            ( { model | selectedCountry = Just target }, Cmd.none )

        OnCountryClose ->
            ( { model | selectedCountry = Nothing }, Cmd.none )

        OnUpdateDimensions response ->
            ( { model | dimensions = decodeDimensions response }, Cmd.none )

        OnSortByUpdate sortMode ->
            ( { model | sortMode = stringToSortMode sortMode }, Cmd.none )


decodeDimensions : Decode.Value -> Result Decode.Error Dimensions
decodeDimensions value =
    Decode.decodeValue
        (Decode.oneOf
            [ Decode.map4 Dimensions
                (Decode.field "height" Decode.float)
                (Decode.field "width" Decode.float)
                (Decode.field "windowHeight" Decode.float)
                (Decode.field "windowWidth" Decode.float)
            , Decode.map4 Dimensions
                (Decode.field "inlineSize" Decode.float)
                (Decode.field "blockSize" Decode.float)
                (Decode.field "windowHeight" Decode.float)
                (Decode.field "windowWidth" Decode.float)
            ]
        )
        value


decodeData : Decode.Value -> Result Decode.Error (List Datum)
decodeData value =
    Decode.decodeValue
        (Decode.list
            (Decode.map3 Datum
                (Decode.field "country" Decode.string)
                (Decode.field "date" Decode.float)
                (Decode.field "value" Decode.float)
            )
        )
        value



-- VIEW


countryDetailsView : Model -> Html Msg
countryDetailsView model =
    let
        countryKey =
            model.selectedCountry
                |> Maybe.withDefault ""
                |> fromCssClass
    in
    case model.selectedCountry of
        Just country ->
            Html.div
                [ class "country-details"
                , Html.Events.onClick OnCountryClose
                ]
                [ Html.div [ class "country-details__forth" ]
                    [ chartDetails countryKey model
                    ]
                ]

        Nothing ->
            Html.text ""


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


charts : Dict String ( Data, Stats ) -> Model -> List (Html Msg)
charts data model =
    sortedCountries data model
        |> List.map
            (\country ->
                let
                    chartData =
                        data
                            |> Dict.get country
                            |> Maybe.withDefault ( [], { totalDeaths = 0 } )
                            |> Tuple.first
                in
                Html.div
                    [ class "viz__event" ]
                    [ Html.div
                        [ class (toCssClass country)
                        , Html.Events.on "click"
                            (Decode.map OnCountrySelect
                                (Decode.at [ "target", "className" ] Decode.string)
                            )
                        ]
                        []
                    , Html.div
                        [ class "viz__wrapper"
                        ]
                        [ Html.div [ class "viz__title" ] [ Html.h2 [] [ Html.text country ] ]
                        , Html.div [ class "viz__item" ] [ chart chartData model ]
                        ]
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


sortedCountries : Dict String ( Data, Stats ) -> Model -> List String
sortedCountries data model =
    data
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
    case model.data of
        RemoteData.Success d ->
            Html.div [ class "wrapper" ]
                [ countryDetailsView model
                , Html.header [ class "header" ]
                    [ Html.h1 [] [ Html.text "Coronavirus, new deaths per million" ]
                    , sortByView model
                    ]
                , Html.div
                    [ class "viz" ]
                    (charts d model)
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


chart : Data -> Model -> Html msg
chart data model =
    let
        { width, height } =
            model.dimensions
                |> Result.withDefault defaultDimensions

        color =
            Color.rgb255 240 59 32
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


chartDetails : String -> Model -> Html msg
chartDetails country model =
    let
        { width, height } =
            model.dimensions
                |> getDetailsDimensions
                |> Result.withDefault defaultDimensions

        color =
            Color.rgb255 240 59 32

        data =
            model.fullData
                |> Dict.get country
                |> Maybe.withDefault ( [], { totalDeaths = 0 } )
                |> Tuple.first
    in
    Line.init
        { margin = { top = 10, right = 10, bottom = 35, left = 35 }
        , width = width
        , height = height
        }
        |> Line.withCurve (Shape.cardinalCurve 0.5)
        |> Line.withStackedLayout (Line.drawArea Shape.stackOffsetSilhouette)
        |> Line.withColorPalette [ color ]
        |> Line.withoutTable
        |> Line.withYDomain model.domain
        |> Line.render ( data, accessor )



-- REMOTE CORONAVIRUS DATA


fetchData : Cmd Msg
fetchData =
    Http.get
        { url = "https://raw.githubusercontent.com/data-viz-lab/covid-dashboard/master/data/owid-covid-data.csv"
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
    -- new_deaths_smoothed_per_million
    7


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


responseToData : WebData String -> WebData Data
responseToData res =
    res
        |> RemoteData.map
            (\str ->
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
            )


responseDataToDict : WebData Data -> WebData (Dict String ( Data, Stats ))
responseDataToDict res =
    res
        |> RemoteData.map
            (\data ->
                data
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
            )


prepareData : DataType -> WebData (Dict String ( Data, Stats )) -> WebData (Dict String ( Data, Stats ))
prepareData dataType res =
    res
        |> RemoteData.map
            (\dict ->
                let
                    downsample =
                        case dataType of
                            Full ->
                                \_ v -> v

                            Downsampled ->
                                \k ( v, s ) -> ( downsampleData v, s )
                in
                dict
                    -- if this is just an overview, lets seriouly downsample the data for performance reasons
                    |> Dict.map downsample
                    -- only keep countries with extensive data
                    |> Dict.filter (\k ( v, s ) -> List.length v > 50 && List.member k exclude |> not)
            )


downsampleData : Data -> Data
downsampleData data =
    LTTB.downsample
        { data = data
        , threshold = 25
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
    Sub.batch
        [ updateDimensions OnUpdateDimensions
        ]



-- INIT


init : Maybe Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        serverData =
            flags
                |> Maybe.map
                    (decodeData
                        >> Result.map RemoteData.succeed
                        >> Result.map responseDataToDict
                        >> Result.withDefault RemoteData.Loading
                    )
                |> Maybe.withDefault RemoteData.Loading

        cmd =
            case serverData of
                RemoteData.Success _ ->
                    observeDimensions ".viz__item"

                _ ->
                    fetchData

        data =
            prepareData Downsampled serverData

        fullData =
            prepareData Full serverData
                |> RemoteData.withDefault Dict.empty

        domain =
            data
                |> RemoteData.withDefault Dict.empty
                |> getDomain
    in
    ( { data = data
      , fullData = fullData
      , dimensions = Result.Ok defaultDimensions
      , domain = domain
      , selectedCountry = Nothing
      , sortMode = Alphabetical
      }
    , cmd
    )



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- HELPERS


toCssClass : String -> String
toCssClass str =
    str
        |> String.toLower
        |> String.replace " " "-"


fromCssClass : String -> String
fromCssClass str =
    str
        |> String.split "-"
        |> List.map capitalise
        |> String.join " "


capitalise : String -> String
capitalise str =
    str
        |> String.left 1
        |> String.toUpper
        |> (\s -> s ++ String.dropLeft 1 str)


getDetailsDimensions : Result Decode.Error Dimensions -> Result Decode.Error Dimensions
getDetailsDimensions windowDimensions =
    windowDimensions
        |> Result.map
            (\dim ->
                { width = dim.windowWidth * 0.8
                , height = dim.windowHeight * 0.8
                , windowWidth = dim.windowWidth * 0.8
                , windowHeight = dim.windowHeight * 0.8
                }
            )
