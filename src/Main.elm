module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
--import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode 
import Browser


-------Ports



-------Types

type TodoStatus = Todo | OnGoing | Done

type alias Task =
    { name : String
    , status : TodoStatus
    }


type alias Model =
    { taskInput : String
    , tasks : List Task
    , movingTask : Maybe Task
    }


-- initModel : Maybe Model -> ( Model, Cmd msg )
-- initModel modelFromFlags =
--     case modelFromFlags of
--         Just model ->
--             ( model, Cmd.none )

--         Nothing ->
--             ( Model "" [] Nothing, Cmd.none )

initModel : () -> ( Model, Cmd msg )
initModel _ = (Model "" [] Nothing, Cmd.none)


type Msg
    = NoOp
    | TextInput String
    | KeyDown Int
    | Move Task
    | DropTask TodoStatus
    | Delete String


-------Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TextInput input -> ( {model|taskInput=input} , Cmd.none )

        KeyDown keyCode -> switchKey keyCode model

        Move selectedTask -> ({model | movingTask = Just selectedTask}, Cmd.none )

        DropTask status -> moveTaskTo status model

        Delete content -> deleteTask content model




-------Helper: action to create new model
switchKey : Int -> Model -> (Model, Cmd Msg)
switchKey keyCode model = 
    case keyCode of 
        13 -> addNewTask model
        _ -> (model,Cmd.none)


addNewTask : Model -> (Model, Cmd Msg)
addNewTask model = 
    let taskContent = model.taskInput
        prevTasks = model.tasks
        newTask = {name=taskContent,status=Todo}
        newModel = {model | taskInput="", tasks = newTask::prevTasks} 
    in (newModel,Cmd.none) 
    -- Todo: 将model存入Localstorage


moveTaskTo : TodoStatus -> Model -> (Model,Cmd Msg)
moveTaskTo status model = 
    case model.movingTask of 
        Just task -> 
            let tasks = List.map 
                    (\t -> if t.name==task.name then {t|status = status} else t) model.tasks

                newModel = {model | tasks = tasks , movingTask = Nothing}

            in (newModel,Cmd.none)
            --Todo 保存新model到localStorage
                    
        Nothing -> (model,Cmd.none)
    

deleteTask : String -> Model -> (Model ,Cmd Msg)
deleteTask content model = 
    let newModel = {model | tasks = List.filter (\t -> t.name /= content) model.tasks}
    in 
    (newModel, Cmd.none)
            --Todo 保存新model到localStorage



-------View
view : Model -> Html Msg
view model = 
        div [class "container light"] 
            [
                input [type_ "text"
                      ,class "task-input"
                      ,placeholder "What You want to add to list"
                      ,tabindex 0
                      ,onKeyDown KeyDown
                      ,onInput TextInput
                      ,value model.taskInput]
                      [
                      ],
                div [class "kanban-board"] 
                    [
                        taskColumnView Todo model.tasks
                       ,taskColumnView OnGoing model.tasks
                       ,taskColumnView Done model.tasks
                    ]
            ]


statusToString : TodoStatus -> String
statusToString status = 
    case status of 
        Todo -> "Todo"
        OnGoing -> "OnGoing"
        Done -> "Done"

taskColumnView : TodoStatus -> List Task -> Html Msg
taskColumnView status tasks = 
    let tasksShouldShow = List.filter (\t -> t.status == status) tasks
    in 
    div [class <| "category "++ (String.toLower <| statusToString status)
        ,onDrop (DropTask status)
        ,onDragOver NoOp]
        [
            h2 [] 
               [text <| statusToString status],
            span []
                [
                    let len = List.length tasksShouldShow
                    in text <| String.fromInt len ++ " Item(s)"
                ],
            ul [] <| List.indexedMap taskItemView tasksShouldShow

        ]

taskItemView : Int -> Task -> Html Msg
taskItemView index task = 
    li [class "task-item"
       ,attribute "draggable" "true"
       ,onDragStart <| Move task
       ]
       [
           enrichItemContent task.name,
           button [class "btn-delete"
                  ,onClick (Delete task.name)
                  ]
                  [
                      text "X"
                  ]
       ]

enrichItemContent : String -> Html Msg
enrichItemContent str = 
    String.words str
  |> List.map (\w -> if 
                        String.startsWith "http" w 
                     then 
                        a [target "_blank", href w] []
                     else text w)
  |> List.intersperse (text " ")
  |> div []


----EventHelper
onKeyDown : (Int -> msg ) -> Attribute msg
onKeyDown trigger =
    on "keydown" <| Decode.map trigger keyCode

-- onDrop : msg -> Attribute msg
-- onDrop message = custom "drop" <|
--                   Decode.map (\m -> {message=m,preventDefault=True,stopPropagation=True}) (Decode.succeed message)


onDrop : msg -> Attribute msg
onDrop msg =
    preventDefaultOn "drop"
        <| Decode.succeed (msg, True)

--drop事件的触发既要drop preventDefault 也要 dragover preventDefault （同时组件上必须有drop dragover两个属性）
onDragOver : msg -> Attribute msg
onDragOver msg =
    preventDefaultOn "dragover" <| Decode.succeed (msg,True)


onDragStart : msg -> Attribute msg
onDragStart message = on "dragstart" <| Decode.succeed message

onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
  on "keyup" (Decode.map tagger keyCode)







------ Main
--main : Program (Maybe Model) Model Msg
main : Program () Model Msg
main = Browser.element
    { 
       init = initModel
      ,update = update
      ,view = view
      ,subscriptions = (\_ -> Sub.none)
    }

----TODO onDrop存在问题
