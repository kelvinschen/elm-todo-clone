module Test exposing (..)
import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }


type alias Model =
    { beingDragged : Maybe String
    , draggableItems: List String
    , items : List String
    }
    
    
initialModel : Model
initialModel =
    { beingDragged = Nothing
    , draggableItems =
        List.range 1 5
            |> List.map Debug.toString
    , items = []
    }


type Msg
    = Drag String
    | DragEnd
    | DragOver
    | Drop


update : Msg -> Model -> Model
update msg model =
    case msg of
        Drag item ->
            { model | beingDragged = Just item }
            
        DragEnd ->
            { model | beingDragged = Nothing }
            
        DragOver ->
            model
            
        Drop ->
            case model.beingDragged of
                Nothing ->
                    model
                    
                Just item ->
                    { model
                        | beingDragged = Nothing
                        , items = item :: model.items 
                    }


draggableItemView : String -> Html Msg
draggableItemView item =
    Html.div
        [ Attributes.class "card fluid warning"
        , Attributes.draggable "true"
        , onDragStart <| Drag item
        , onDragEnd DragEnd 
        ] 
        [ Html.div 
            [ Attributes.class "section" ] 
            [ Html.text item ] 
        ]


itemView : String -> Html Msg
itemView item =
    Html.div
        [ Attributes.class "card fluid error" ] 
        [ Html.div 
            [ Attributes.class "section" ] 
            [ Html.text item ]
        ]


view : Model -> Html Msg
view model =
    Html.div
        [ Attributes.class "container" ]
        [ Html.div 
            [ Attributes.class "row" ] 
            [ Html.div 
                [ Attributes.class "col-sm-6" ]
                <| (List.map draggableItemView model.draggableItems
                    |> (::) (Html.h4 [] [ Html.text "Draggable" ]))
            , Html.div 
                [ Attributes.class "col-sm-6"
                , onDragOver DragOver
                , onDrop Drop
                ]
                <| (List.map itemView model.items
                    |> (::) (Html.h4 [] [ Html.text "Drop Zone" ]))
            ]
        ]
        
        
onDragStart msg =
    Events.on "dragstart" 
        <| Decode.succeed msg


onDragEnd msg =
    Events.on "dragend"
        <| Decode.succeed msg


onDragOver msg =
    Events.preventDefaultOn "dragover"
        <| Decode.succeed (msg, True)


onDrop msg =
    Events.preventDefaultOn "drop"
        <| Decode.succeed (msg, True)