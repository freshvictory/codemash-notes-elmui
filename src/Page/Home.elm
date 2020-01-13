module Page.Home exposing (Model, Msg, init, update, view)

import Api exposing (Note)
import Dict exposing (Dict)
import Element exposing (Element, rgb255)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Http
import Page



-- MODEL


type alias Model =
    { notes : Dict Int Note
    , requestState : RequestState
    , editState : EditState
    , confirm : ConfirmState
    }


type RequestState
    = Loading
    | Success
    | Failure String


type EditState
    = NotEditing
    | Editing Note


type ConfirmState
    = Hidden
    | Active Int


defaultNote : Note
defaultNote =
    { id = 0
    , title = ""
    , presenter = ""
    , note = ""
    , rating = 1
    }


init : ( Model, Cmd Msg )
init =
    ( { notes = Dict.empty
      , requestState = Loading
      , editState = NotEditing
      , confirm = Hidden
      }
    , Api.getNotes ReceivedNotes
    )



-- UPDATE


type Msg
    = ReceivedNotes (Result Http.Error (List Note))
    | StartEditing Note
    | Save
    | StopEditing
    | SavedNote (Result Http.Error Note)
    | OnNoteChange Note
    | ShowDeleteConfirm Int
    | Delete Int
    | NoteDeleted Int (Result Http.Error ())
    | AddNote
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ReceivedNotes result ->
            case result of
                Ok notes ->
                    ( { model
                        | notes = List.foldl (\n -> Dict.insert n.id n) Dict.empty notes
                        , requestState = Success
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | requestState = Failure "Something went wrong" }, Cmd.none )

        StartEditing note ->
            ( { model | editState = Editing note }, Cmd.none )

        OnNoteChange note ->
            ( { model | editState = Editing note }, Cmd.none )

        StopEditing ->
            ( { model | editState = NotEditing }, Cmd.none )

        Save ->
            case model.editState of
                Editing note ->
                    ( { model | editState = NotEditing }
                    , (if note.id == 0 then
                        Api.addNote

                       else
                        Api.saveNote
                      )
                        SavedNote
                        note
                    )

                _ ->
                    ( model, Cmd.none )

        SavedNote result ->
            case result of
                Ok note ->
                    ( { model | notes = Dict.insert note.id note model.notes }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ShowDeleteConfirm id ->
            ( { model | confirm = Active id }, Cmd.none )

        Delete id ->
            ( model, Api.deleteNote (NoteDeleted id) id )

        NoteDeleted id result ->
            case result of
                Ok _ ->
                    ( { model | notes = Dict.remove id model.notes }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        AddNote ->
            ( { model | editState = Editing defaultNote }, Cmd.none )



-- VIEW


view : Model -> Page.Details Msg
view model =
    { title = "Home"
    , attrs =
        [ Font.size 16
        , Font.family
            [ Font.typeface "-apple-system"
            ]
        ]
    , body =
        [ viewHeader model
        , viewContent model
        ]
    }


viewHeader : Model -> Element Msg
viewHeader model =
    Element.row
        [ Region.navigation
        , Element.padding 20
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Element.width Element.fill
        ]
        [ Element.el
            [ Region.heading 1
            , Font.size 32
            , Font.bold
            ]
            (Element.text "CodeMash Notes")
        , case model.editState of
            Editing n ->
                if n.id /= 0 then
                    viewAddNotesButton

                else
                    Element.text ""

            _ ->
                viewAddNotesButton
        ]


viewAddNotesButton : Element Msg
viewAddNotesButton =
    Input.button
        [ Font.color (rgb255 85 85 85)
        , Border.width 1
        , Border.color (rgb255 85 85 85)
        , Border.rounded 5
        , Element.padding 5
        , Element.alignRight
        ]
        { onPress = Just AddNote
        , label = Element.text "add notes"
        }


viewContent : Model -> Element Msg
viewContent model =
    Element.el
        [ Element.padding 20
        ]
        (case model.requestState of
            Success ->
                viewNotes model

            Failure err ->
                Element.text err

            Loading ->
                Element.text "Loading notes..."
        )


viewNotes : Model -> Element Msg
viewNotes model =
    let
        noteEntries =
            case model.editState of
                Editing n ->
                    if n.id == 0 then
                        [ viewNoteForm n ]

                    else
                        []

                _ ->
                    []
    in
    Element.column
        [ Element.spacing 20
        ]
        (List.concat
            [ noteEntries
            , model.notes
                |> Dict.map (\_ -> viewNoteSummary model)
                |> Dict.values
            ]
        )


viewNoteSummary : Model -> Note -> Element Msg
viewNoteSummary model note =
    case model.editState of
        Editing n ->
            if n.id == note.id then
                viewNoteForm n

            else
                viewNoteDefault note

        _ ->
            viewNoteDefault note


viewNoteDefault : Note -> Element Msg
viewNoteDefault note =
    Element.column
        [ Element.padding 15
        , Border.width 2
        , Border.color (rgb255 170 170 170)
        , Border.rounded 10
        , Element.spacing 10
        , Element.width Element.fill
        ]
        [ Element.row
            [ Element.paddingEach { bottom = 5, top = 0, left = 0, right = 0 }
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , Border.color (rgb255 204 204 204)
            , Element.spacing 15
            , Element.width Element.fill
            ]
            [ Element.row
                [ Element.spacing 5
                ]
                [ Element.row
                    [ Element.spacing 5
                    ]
                    [ Element.el
                        [ Font.bold
                        ]
                        (Element.text note.title)
                    , Element.el
                        []
                        (Element.text "•")
                    ]
                , Element.row
                    [ Element.spacing 5
                    ]
                    [ Element.el
                        [ Font.color (rgb255 85 85 85)
                        ]
                        (Element.text note.presenter)
                    , Element.el
                        []
                        (Element.text "•")
                    ]
                , Element.el
                    [ Font.color
                        (if note.rating > 7 then
                            rgb255 80 185 70

                         else if note.rating > 5 then
                            rgb255 252 164 3

                         else
                            rgb255 255 0 0
                        )
                    ]
                    (Element.text (String.fromInt note.rating ++ "/10"))
                ]
            , Element.row
                [ Element.alignRight
                , Font.color (rgb255 85 85 85)
                , Font.size 13
                , Element.spacing 7
                ]
                [ Input.button
                    []
                    { onPress = Just (StartEditing note)
                    , label = Element.text "edit"
                    }
                , Input.button
                    []
                    { onPress = Just (Delete note.id)
                    , label = Element.text "delete"
                    }
                ]
            ]
        , Element.paragraph
            []
            [ Element.text note.note
            ]
        ]


viewNoteForm : Note -> Element Msg
viewNoteForm note =
    Element.column
        [ Border.width 2
        , Border.color (rgb255 170 170 170)
        , Border.rounded 10
        , Element.padding 15
        , Element.spacing 10
        , Element.width Element.fill
        ]
        [ Element.row
            [ Element.alignRight
            , Font.color (rgb255 85 85 85)
            , Font.size 13
            ]
            [ Input.button
                []
                { onPress = Just Save
                , label = Element.text "save"
                }
            , Element.el
                [ Font.family [ Font.monospace ]
                ]
                (Element.text " | ")
            , Input.button
                []
                { onPress = Just StopEditing
                , label = Element.text "cancel"
                }
            ]
        , Element.column
            [ Element.spacing 10
            , Element.width Element.fill
            ]
            [ formInput "Session title" note.title (\s -> OnNoteChange { note | title = s })
            , formInput "Presenter" note.presenter (\s -> OnNoteChange { note | presenter = s })
            , noteRatingInput note
            , noteTextInput note
            ]
        ]


noteRatingInput : Note -> Element Msg
noteRatingInput note =
    Element.row
        [ Element.width Element.fill
        , Border.width 1
        , Border.rounded 5
        , Border.color (rgb255 170 170 170)
        , Element.padding 10
        ]
        [ Input.slider
            [ Element.behindContent
                (Element.el
                    [ Element.width Element.fill
                    , Element.height (Element.px 2)
                    , Element.centerY
                    , Background.color (rgb255 170 170 170)
                    , Border.rounded 2
                    ]
                    Element.none
                )
            ]
            { label =
                Input.labelAbove
                    []
                    (Element.text "Rating")
            , onChange = \i -> OnNoteChange { note | rating = round i }
            , min = 1.0
            , max = 10.0
            , value = toFloat note.rating
            , step = Just 1
            , thumb = Input.defaultThumb
            }
        , Element.text (String.fromInt note.rating ++ " / 10")
        ]


formInput : String -> String -> (String -> Msg) -> Element Msg
formInput textLabel valueText onInputMsg =
    Element.el
        []
        (Input.text
            [ Border.width 1
            , Border.color (rgb255 170 170 170)
            , Border.rounded 5
            , Element.padding 10
            ]
            { onChange = onInputMsg
            , text = valueText
            , placeholder = Nothing
            , label =
                Input.labelAbove
                    []
                    (Element.text textLabel)
            }
        )


noteTextInput : Note -> Element Msg
noteTextInput note =
    Element.el
        [ Element.width Element.fill
        ]
        (Input.multiline
            [ Element.padding 10
            , Border.width 1
            , Border.color (rgb255 170 170 170)
            ]
            { onChange = \s -> OnNoteChange { note | note = s }
            , text = note.note
            , spellcheck = True
            , placeholder = Nothing
            , label =
                Input.labelAbove
                    []
                    (Element.text "Note text")
            }
        )
