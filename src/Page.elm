module Page exposing (Details, view)

import Browser
import Element exposing (Element)
import Element.Region as Region
import Html.Styled.Attributes
    exposing
        ( id
        )



-- PAGE


type alias Details msg =
    { title : String
    , attrs : List (Element.Attribute msg)
    , body : List (Element msg)
    }



-- VIEW


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    { title = details.title
    , body = [ viewApp toMsg details.attrs details.body |> Element.layout [] ]
    }


viewApp : (a -> msg) -> List (Element.Attribute a) -> List (Element a) -> Element msg
viewApp toMsg attrs body =
    Element.el
        []
        (Element.map toMsg (viewBody attrs body))


viewBody : List (Element.Attribute msg) -> List (Element msg) -> Element msg
viewBody attrs body =
    Element.column
        (List.concat [ [ Region.mainContent ], attrs ])
        body
