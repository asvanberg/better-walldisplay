module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import Task
import Time exposing (Time, second)

type Message
  = Update Time
  | JobListRetrieved (Result Http.Error (List Job))
  | JobDetailsRetrieved String (Result Http.Error JobDetails)

type alias Flags =
  { jenkinsViewName : String
  , height : Int
  , width: Int
  }

type alias Model =
  { viewName : String
  , state : State
  , now : Time
  , jobs : Dict String (Result () JobDetails)
  , height : Int
  , width : Int
  }

type State
  = Working
  | Error

type alias Job =
  { url : String
  , name : String
  , color : String
  }

type alias JobDetails =
  { color : String
  , name : String
  , lastBuild : Maybe Build
  , lastSuccessfulBuild : Maybe Build
  }

type alias Build =
  { duration : Int
  , timestamp : Int
  }

main : Program Flags Model Message
main =
  programWithFlags
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

init : Flags -> ( Model, Cmd Message )
init {jenkinsViewName, height, width} =
  ( { viewName = jenkinsViewName
    , state = Working
    , jobs = Dict.empty
    , now = 0
    , height = height
    , width = width
    }
  , Task.perform Update Time.now
  )

update : Message -> Model -> ( Model, Cmd Message )
update msg model =
  case msg of
    Update now ->
      let
        request =
          Http.get ("/view/" ++ model.viewName ++ "/api/json") (field "jobs" <| Decode.list decodeJob)
        command =
          Http.send JobListRetrieved request
      in
        ({ model | now = now }, command)

    JobListRetrieved (Err e) ->
      ({ model | state = Error}, Cmd.none)

    JobListRetrieved (Ok jobList) ->
      let
        toDetails job =
          { name = job.name
          , color = job.color
          , lastBuild = Nothing
          , lastSuccessfulBuild = Nothing
          }
        details =
          List.map toDetails jobList
            |> List.map (\details -> (details.name, details))
            |> Dict.fromList
        addJob name jobDetail =
          Dict.insert name (Ok jobDetail)
        updateColor name jobDetail oldJobDetail =
          case oldJobDetail of
            Ok f ->
              Dict.insert name (Ok { f | color = jobDetail.color })
            Err () ->
              Dict.insert name (Ok jobDetail)
        removeJob name oldJobDetail =
          identity
        newModel =
          { model
          | jobs = Dict.merge addJob updateColor removeJob details model.jobs Dict.empty
          }
        requestJobDetails job =
          Http.send (JobDetailsRetrieved job.name)
            <| Http.get (job.url ++ "api/json?tree=" ++ treeParameter) decodeJobDetails
      in
        (newModel, Cmd.batch <| List.map requestJobDetails <| List.filter isBuilding jobList)

    JobDetailsRetrieved jobName (Err _) ->
      let
        newJobs =
          Dict.insert jobName (Err ()) model.jobs
      in
        ({ model | jobs = newJobs }, Cmd.none)

    JobDetailsRetrieved jobName (Ok details) ->
      let
        newJobs =
          Dict.insert jobName (Ok details) model.jobs
      in
        ({ model | jobs = newJobs }, Cmd.none)

isBuilding : { c | color : String } -> Bool
isBuilding =
  .color >> String.endsWith "_anime"


treeParameter : String
treeParameter = "color,displayName,lastBuild[timestamp,duration],lastSuccessfulBuild[duration,timestamp]"

decodeJob : Decoder Job
decodeJob =
  Decode.map3 Job
    (field "url" string)
    (field "name" string)
    (field "color" string)

decodeJobDetails : Decoder JobDetails
decodeJobDetails =
  Decode.map4 JobDetails
    (field "color" string)
    (field "displayName" string)
    (field "lastBuild" (Decode.maybe decodeBuild))
    (field "lastSuccessfulBuild" (Decode.maybe decodeBuild))

decodeBuild : Decoder Build
decodeBuild =
  Decode.map2 Build
    (field "duration" int)
    (field "timestamp" int)

subscriptions : Model -> Sub Message
subscriptions {state} =
  let
    updateDelay =
      case state of
        Working ->
          5 * second

        Error ->
          60 * second
  in
    Time.every updateDelay Update

view : Model -> Html msg
view model =
  let
    onlyOkJobs x =
      case x of
        Ok jobDetails ->
          Just jobDetails
        _ ->
          Nothing

    longestName =
      Dict.values model.jobs
        |> List.filterMap onlyOkJobs
        |> List.map (String.length << .name)
        |> List.maximum
        |> Maybe.withDefault 0

    fontSize =
      --clamp 0 30 <| round <| 100 / (toFloat <| Dict.size model.jobs) * 0.70
      --model.documentHeight // (Dict.size model.jobs) * 70 // 100
      model.width // longestName * 4 // 3

    barHeight =
      (model.height - 8) // (Dict.size model.jobs) - 8

    viewJob (jobName, jobDetails) =
      case jobDetails of
        Ok {name, color, lastBuild, lastSuccessfulBuild} ->
          div
            [ class color
            , style
              [ ("height", toString (barHeight) ++ "px")
              , ("line-height", toString (barHeight) ++ "px")
              ]
            ]
            [ if isBuilding { color = color } then
                progressBar lastBuild lastSuccessfulBuild
              else
                div [ class "progress", style [ ("width", "0") ] ] []
            , div [ style [ ("z-index", "1000") ] ] [ text name ]
            ]

        Err () ->
          div [ class "red" ] [ text jobName ]

    progressBar lastBuild lastSuccessfulBuild =
      case (lastBuild, lastSuccessfulBuild) of
        (Just lb, Just lbs) ->
          let
            elapsed = (round model.now) - lb.timestamp
            progress = clamp 0 100 <| elapsed * 100 // lbs.duration
          in
            div [ class "progress", style [ ("width", (toString progress) ++ "%") ] ] []

        _ ->
          div [ class "progress", style [ ("width", "0") ] ] []

  in
    div
      [ class "job-container"
      , style
        [ ("font-size", (toString fontSize) ++ "px" )
        ]
      ]
      (List.map viewJob <| Dict.toList model.jobs)
