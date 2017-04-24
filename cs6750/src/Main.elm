
module Main exposing (main)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- MODEL


type alias Model =
  { modelDisplay : Display
  , modelTiles : List Tile
  , modelUser : User
  }


initializeModel : Model
initializeModel =
  Model DisplayTiles [] initUser


setModelDisplay : Display -> Model -> Model
setModelDisplay x model =
  { model | modelDisplay = x }


setModelTiles : List Tile -> Model -> Model
setModelTiles xs model =
  { model | modelTiles = xs }


setModelUser : User -> Model -> Model
setModelUser x model =
  { model | modelUser = x }


type Display
  = DisplayTiles
  | DisplayDetail Tile
  | DisplayUser


type alias Tile =
  { tileTitle : String
  , tileNumMeetings : Int
  , tileNextMeeting : String
  , tileBudget : String
  , tileEstimatedCompletion : String
  , tileDetail : Detail
  }


type alias Detail =
  { detailId : Int
  , detailTitle : String
  , detailNextMeeting : String
  , detailDescription : String
  , detailPriorMeetings : List String
  , detailBudget : String
  , detailEstimatedCompletion : String
  , detailApproval : Int
  , detailConcerns : List DetailConcern
  }


setDetailId : Int -> Detail -> Detail
setDetailId x detail =
  { detail | detailId = x }


setDetailTitle : String -> Detail -> Detail
setDetailTitle x detail =
  { detail | detailTitle = x }


setDetailNextMeeting : String -> Detail -> Detail
setDetailNextMeeting x detail =
  { detail | detailNextMeeting = x }


setDetailDescription : String -> Detail -> Detail
setDetailDescription x detail =
  { detail | detailDescription = x }


setDetailPriorMeetings : List String -> Detail -> Detail
setDetailPriorMeetings xs detail =
  { detail | detailPriorMeetings = xs }


setDetailBudget : String -> Detail -> Detail
setDetailBudget x detail =
  { detail | detailBudget = x }


setDetailEstimatedCompletion : String -> Detail -> Detail
setDetailEstimatedCompletion x detail =
  { detail | detailEstimatedCompletion = x }


setDetailApproval : Int -> Detail -> Detail
setDetailApproval x detail =
  { detail | detailApproval = x }


setDetailConcerns : List DetailConcern -> Detail -> Detail
setDetailConcerns xs detail =
  { detail | detailConcerns = xs }


type alias DetailConcern =
  { detailConcernId : Int
  , detailConcernDescription : String
  , detailConcernCount : Int
  }


setDetailConcernId : Int -> DetailConcern -> DetailConcern
setDetailConcernId x detailConcern =
  { detailConcern | detailConcernId = x }


setDetailConcernDescription : String -> DetailConcern -> DetailConcern
setDetailConcernDescription x detailConcern =
  { detailConcern | detailConcernDescription = x }


setDetailConcernCount : Int -> DetailConcern -> DetailConcern
setDetailConcernCount x detailConcern =
  { detailConcern | detailConcernCount = x }


type alias User =
  { userName : String
  , userVotes : Int
  , userVotesUp : Int
  }


initUser : User
initUser =
  User "" 0 0


-- UPDATE


type Msg
  = Initialize
  | ShowDisplay Display
  | IncrementConcern Int Int
  | DecrementConcern Int Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    Initialize ->
      ( model, Cmd.none )

    ShowDisplay x ->
      ( model
          |> setModelDisplay x
      , Cmd.none
      )

    DecrementConcern detailId detailConcernId ->
      ( model, Cmd.none )

    IncrementConcern detailId detailConcernId ->
      ( model, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    []


-- VIEW


styles : List Css.Mixin -> Html.Attribute msg
styles =
  asPairs >> Html.Attributes.style


view : Model -> Html Msg
view model =
  case model.modelDisplay of

    DisplayTiles ->
      div
        [ styles
            [ margin (px 20) ]
        ]
        ( (List.map renderTile model.modelTiles)
        ++ [ a
              [ href "#"
              , onClick (ShowDisplay DisplayUser)
              ]
              [ img
                  [ Html.Attributes.src "icons/profile_icon.png"
                  , Html.Attributes.height 48
                  ] []
              ]
           ]
        )
    
    DisplayDetail tile ->
      renderTileDetail tile.tileDetail

    DisplayUser ->
      renderUser model.modelUser


renderTile : Tile -> Html Msg
renderTile tile =
  div
    [ Html.Attributes.class "tile"
    , styles
        [ border3 (px 1) solid (hex "000000")
        , cursor pointer
        , float left
        , marginBottom (px 20)
        , marginRight (px 20)
        , padding (px 10)
        , Css.width (px 200)
        ]
    , onClick (ShowDisplay (DisplayDetail tile))
    ]
    [ span
        [ styles [ fontWeight bold, fontStyle italic ] ]
        [ Html.text tile.tileTitle ]
    , br [] []
    , br [] []
    , span
        [ styles [ fontStyle italic ] ]
        [ Html.text <| (toString tile.tileNumMeetings) ++ " hearings have been held" ]
    , br [] []
    , br [] []
    , span
        [ styles [ fontStyle italic ] ]
        [ Html.text "Next scheduled meeting:" ]
    , br [] []
    , span
        [ styles [ fontWeight bold ] ]
        [ Html.text tile.tileNextMeeting ]
    , br [] []
    , br [] []
    , span
        [ styles [ fontStyle italic ] ]
        [ Html.text "Appropriated budget:" ]
    , br [] []
    , span
        [ styles [ fontWeight bold ] ]
        [ Html.text tile.tileBudget ]
    , br [] []
    , br [] []
    , span
        [ styles [ fontStyle italic ] ]
        [ Html.text "Estimated work completion:" ]
    , br [] []
    , span
        [ styles [ fontWeight bold ] ]
        [ Html.text tile.tileEstimatedCompletion ]
    ]


renderPriorMeeting : String -> Html Msg
renderPriorMeeting x =
  div
    []
    [ Html.text <| x ++ " - "
    , a [ href "#" ] [ Html.text "(see meeting minutes)" ]
    , br [] []
    , br [] []
    ]


renderConcern : Int -> DetailConcern -> Html Msg
renderConcern detailId concern =
  div
    []
    [ button
        [ onClick (DecrementConcern detailId concern.detailConcernId) ]
        [ img
            [ Html.Attributes.src "icons/arrow_down.png"
            , Html.Attributes.height 12
            ] []
        ]
    , div
        [ styles [ fontWeight bold, fontStyle italic, marginLeft (px 10) ] ]
        [ Html.text <| " " ++ (toString concern.detailConcernCount) ++ " " ]
    , button
        [ onClick (IncrementConcern detailId concern.detailConcernId) ]
        [ img
            [ Html.Attributes.src "icons/arrow_up.png"
            , Html.Attributes.height 12
            ] []
        ]
    , Html.text <| " " ++ concern.detailConcernDescription
    , br [] []
    , br [] []
    ]


renderTileDetail : Detail -> Html Msg
renderTileDetail detail =
  div
    [ styles [ margin (px 20) ] ]
    ( [ div
          [ styles [ marginBottom (px 20) ] ]
          [ a
              [ href "#"
              , onClick (ShowDisplay DisplayTiles)
              ]
              [ img
                  [ Html.Attributes.src "icons/back_icon.png"
                  , Html.Attributes.height 32
                  ] []
              ]
          ]
      ]
    ++ ( [ div
          [ styles
              [ float left
              , marginRight (px 20)
              , Css.width (px 400)
              ]
          ]
          [ span
              [ styles [ fontWeight bold, fontStyle italic ] ]
              [ Html.text <| detail.detailTitle ++ " " ]
          , a [ href "#" ] [ Html.text "(follow this issue)" ]
          , br [] []
          , div
              [ styles [ fontStyle italic ] ]
              [ Html.text detail.detailNextMeeting ]
          , br [] []
          , span
              [ styles [ fontStyle italic ] ]
              [ Html.text "Share via: "
              , a
                  [ href "#" ]
                  [ img
                      [ Html.Attributes.src "icons/facebook_icon.png"
                      , Html.Attributes.height 24
                      ] []
                  ]
              , Html.text " "
              , a
                  [ href "#" ]
                  [ img
                      [ Html.Attributes.src "icons/twitter_icon.png"
                      , Html.Attributes.height 24
                      ] []
                  ]
              ]
          , br [] []
          , br [] []
          , span
              [ styles [ fontStyle italic ] ]
              [ Html.text "Last meeting topics: "
              , a [ href "#" ] [ Html.text "(see meeting minutes)" ]
              ]
          , br [] []
          , br [] []
          , Html.text detail.detailDescription
          , br [] []
          , br [] []
          , br [] []
          , div
              [ styles [ fontStyle italic ] ]
              [ Html.text "Prior meetings:" ]
          , br [] []
          , div [] (List.map renderPriorMeeting detail.detailPriorMeetings)
          ]
      , div
          [ styles
              [ float left
              , marginRight (px 20)
              , Css.width (px 400)
              ]
          ]
          [ Html.text "Budget appropriated:"
          , div
              [ styles [ fontWeight bold, fontStyle italic ] ]
              [ Html.text detail.detailBudget ]
          , br [] []
          , Html.text "Estimated work completion:"
          , div
              [ styles [ fontStyle italic ] ]
              [ Html.text detail.detailEstimatedCompletion ]
          , br [] []
          , Html.text "Resident approval:"
          , br [] []
          , span
              [ styles [ fontWeight bold, fontStyle italic ] ]
              [ Html.text <| (toString detail.detailApproval) ++ "% " ]
          , a [ href "#" ] [ Html.text "(vote)" ]
          , br [] []
          , br [] []
          , div
              [ styles [ fontStyle italic ] ]
              [ Html.text "Concerns:" ]
          , br [] []
          , div [] (List.map (renderConcern detail.detailId) detail.detailConcerns)
          , a [ href "#" ] [ Html.text "(add item)" ]
          ]
      ]
    )
  )


renderUser : User -> Html Msg
renderUser user =
  div
    [ styles [ margin (px 20) ] ]
    ( [ div
          [ styles [ marginBottom (px 20) ] ]
          [ a
              [ href "#"
              , onClick (ShowDisplay DisplayTiles)
              ]
              [ img
                  [ Html.Attributes.src "icons/back_icon.png"
                  , Html.Attributes.height 32
                  ] []
              ]
          ]
      ]
    ++ ( [ div
          [ styles
              [ border3 (px 1) solid (hex "000000")
              , float left
              , marginRight (px 20)
              , padding (px 10)
              , Css.width (px 200)
              ]
          ]
          [ div
              [ styles
                  [ border3 (px 1) solid (hex "000000")
                  , Css.height (px 100)
                  , Css.width (px 100)
                  , lineHeight (px 100)
                  , textAlign center
                  ]
              ]
              [ img
                  [ Html.Attributes.src "icons/profile_photo.png"
                  , Html.Attributes.height 100
                  ]
                  []
              ]
          , br [] []
          , div
              [ styles [ fontWeight bold ] ]
              [ Html.text user.userName ]
          , br [] []
          , Html.text <| "Voted " ++ (toString user.userVotes) ++ " times"
          , br [] []
          , Html.text <| "Comments voted up " ++ (toString user.userVotesUp) ++ " times"
          ]
      ]
    )
  )


-- ENTRY POINT


main : Program Never Model Msg
main =
  Html.program
    { init = initializeApp
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


initializeApp : (Model, Cmd Msg)
initializeApp =
  ( initializeModel
      |> setModelTiles
          [ Tile "Downtown Dog Park" 3 "May 14, 2017" "$8,000 (on budget)" "Fall 2017"
              ( Detail
                  1
                  "Downtown Dog Park"
                  "3pm, May 14, 2017 @ City Hall, Room 213"
                  "Cleanliness and waste/disease control. Lot location and size."
                  [ "January 17, 2017"
                  , "December 9, 2016"
                  , "September 13, 2016"
                  ]
                  "$8,000 (on budget)"
                  "Fall 2017"
                  73
                  [ DetailConcern 1 "What hours will the park be open?" 23
                  , DetailConcern 2 "What breeds of dogs will be allowed?" 6
                  ]
              )
          , Tile "Bus Station @ 1st & Main" 6 "May 3, 2017" "$213,000 (over budget)" "August 2017"
              ( Detail
                  2
                  "Bus Station @ 1st & Main"
                  "1pm, May 3, 2017 @ City Hall, Room 147"
                  "Water main repair damaged by construction."
                  [ "January 5, 2017"
                  , "December 17, 2016"
                  , "September 19, 2016"
                  , "July 15, 2016"
                  , "May 11, 2016"
                  , "March 28, 2016"
                  ]
                  "$213,000 (over budget)"
                  "August 2017"
                  52
                  [ DetailConcern 1 "What is being done to control cost overruns?" 17
                  ]
              )
          , Tile "River Bike Trail" 4 "N/A" "N/A" "Canceled"
              ( Detail
                  3
                  "River Bike Trail"
                  ""
                  "Project canceled due to lack of public interest."
                  [ "February 9, 2017"
                  , "November 13, 2016"
                  , "October 23, 2016"
                  , "August 17, 2016"
                  ]
                  "N/A"
                  "N/A"
                  13
                  []
              )
          ]
      |> setModelUser (User "Jane Doe" 13 6)
  , Cmd.none
  )
