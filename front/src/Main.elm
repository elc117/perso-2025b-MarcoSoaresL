module Main exposing (main)

import Browser
import Html exposing (Html, div, text, button, table, thead, tbody, tr, th, td, h2, h3)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import String


-- MODELO

type alias Rune =
    { id : Int
    , slot : Int
    , setName : String
    , mainStat : String
    , mainValue : Int
    , sub1 : String
    , sub1Value : Int
    , sub2 : String
    , sub2Value : Int
    , sub3 : String
    , sub3Value : Int
    , sub4 : String
    , sub4Value : Int
    }


type alias Model =
    { loading : Bool
    , valorTotal : Maybe Int
    , statEscolhido : Maybe String
    , tipoRuna : Maybe String
    , melhorCombinacao : List Rune
    , currentRequest : Maybe (String, String)  -- Adicionar para rastrear requisição atual
    }


initModel : Model
initModel =
    { loading = False
    , valorTotal = Nothing
    , statEscolhido = Nothing
    , tipoRuna = Nothing
    , melhorCombinacao = []
    , currentRequest = Nothing
    }


-- MENSAGENS

type Msg
    = FetchCombination String String  -- (set, stat)
    | GotResponse (Result Http.Error Resp)


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchCombination setName statName ->
            fetchData model setName statName

        GotResponse result ->
            case result of
                Ok resp ->
                    ( { model
                        | loading = False
                        , valorTotal = Just resp.valorTotal
                        , statEscolhido = Just resp.statEscolhido
                        , tipoRuna = Just resp.tipoRuna
                        , melhorCombinacao = resp.melhorCombinacao
                        , currentRequest = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    let
                        _ = Debug.log "HTTP Error" error
                    in
                    ( { model 
                        | loading = False
                        , melhorCombinacao = []
                        , currentRequest = Nothing
                      }
                    , Cmd.none
                    )


-- HELPER FUNCTIONS

fetchData : Model -> String -> String -> ( Model, Cmd Msg )
fetchData model setName statName =
    let
        -- Mapear os stats para o formato esperado pelo backend
        mappedStat = 
            case String.toLower statName of
                "atk" -> "ATK"
                "def" -> "DEF" 
                "hp" -> "HP"
                "spd" -> "SPD"
                _ -> statName
        
        url = "http://localhost:3000/requisitos/" ++ setName ++ "/" ++ mappedStat
    in
    ( { model 
        | loading = True
        , melhorCombinacao = []
        , currentRequest = Just (setName, statName)
      }
    , Http.get { url = url, expect = Http.expectJson GotResponse decoderResponse }
    )


-- CLASSES CSS (mantidas as mesmas)

baseButtonClass : String
baseButtonClass =
    "padding: 10px 20px; color: white; border: none; border-radius: 5px; cursor: pointer; margin-right: 10px; margin-bottom: 10px; font-weight: bold; transition: opacity 0.2s;"

statButtonClass : String -> String
statButtonClass color =
    baseButtonClass ++ " background-color: " ++ color ++ ";"

setButtonClass : String -> String
setButtonClass color =
    baseButtonClass ++ " background-color: " ++ color ++ ";"

containerClass : String
containerClass =
    "padding: 20px; font-family: Arial, sans-serif;"

sectionClass : String
sectionClass =
    "margin: 20px 0;"

titleClass : String
titleClass =
    "color: #333; margin-bottom: 10px;"

resultClass : String
resultClass =
    "margin: 20px 0; font-weight: bold; font-size: 16px; color: #2c5530; background-color: #e8f5e8; padding: 15px; border-radius: 8px; border-left: 4px solid #4caf50;"

tableClass : String
tableClass =
    "border: 1px solid #ddd; border-collapse: collapse; width: 100%; text-align: center; font-size: 14px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"

tableHeaderClass : String
tableHeaderClass =
    "background-color: #fff9c4; font-weight: bold;"

tableCellClass : String
tableCellClass =
    "padding: 8px; border: 1px solid #ddd;"


-- DECODERS CORRIGIDOS

type alias Resp =
    { tipoRuna : String
    , statEscolhido : String
    , valorTotal : Int
    , melhorCombinacao : List Rune
    }


decoderRune : Decode.Decoder Rune
decoderRune =
    Decode.map8 (\id slot setName mainStat mainValue sub1 sub1Value rest ->
        { id = id
        , slot = slot
        , setName = setName
        , mainStat = mainStat
        , mainValue = mainValue
        , sub1 = sub1
        , sub1Value = sub1Value
        , sub2 = rest.sub2
        , sub2Value = rest.sub2Value
        , sub3 = rest.sub3
        , sub3Value = rest.sub3Value
        , sub4 = rest.sub4
        , sub4Value = rest.sub4Value
        })
        (Decode.field "id" Decode.int)
        (Decode.field "slot" Decode.int)
        (Decode.field "setName" Decode.string)
        (Decode.field "mainStat" Decode.string)
        (Decode.field "mainValue" Decode.int)
        (Decode.field "sub1" Decode.string)
        (Decode.field "sub1Value" Decode.int)
        (Decode.map6 (\sub2 sub2Value sub3 sub3Value sub4 sub4Value ->
            { sub2 = sub2
            , sub2Value = sub2Value
            , sub3 = sub3
            , sub3Value = sub3Value
            , sub4 = sub4
            , sub4Value = sub4Value
            })
            (Decode.field "sub2" Decode.string)
            (Decode.field "sub2Value" Decode.int)
            (Decode.field "sub3" Decode.string)
            (Decode.field "sub3Value" Decode.int)
            (Decode.field "sub4" Decode.string)
            (Decode.field "sub4Value" Decode.int)
        )


decoderResponse : Decode.Decoder Resp
decoderResponse =
    Decode.map4 Resp
        (Decode.field "tipoRuna" Decode.string)
        (Decode.field "stat_escolhido" Decode.string)
        (Decode.field "valor_total" Decode.int)
        (Decode.field "melhor_combinação" (Decode.list decoderRune))


-- VIEW HELPERS ATUALIZADOS

createCombinationButton : String -> String -> String -> String -> Model -> Html Msg
createCombinationButton setName statName label color model =
    let
        -- Só mostra loading para o botão específico que foi clicado
        isThisButtonLoading = 
            case model.currentRequest of
                Just (reqSet, reqStat) -> 
                    reqSet == setName && reqStat == statName
                Nothing -> False
    in
    button 
        [ onClick (FetchCombination setName statName)
        , disabled model.loading
        , style "cssText" (statButtonClass color)
        ] 
        [ text (if isThisButtonLoading then "Carregando..." else label) ]

viewRuneRow : Int -> Rune -> Html Msg
viewRuneRow index rune =
    let
        backgroundColor =
            if modBy 2 index == 0 then
                "#ffffff"   -- linha branca
            else
                "#cccccc"   -- linha cinza clarinho

        rowStyle =
            tableCellClass ++ "; background-color: " ++ backgroundColor ++ "; box-shadow: inset 0 -1px 0 #ddd;"
    in
    tr []
        [ td [ style "cssText" rowStyle ] [ text (String.fromInt rune.slot) ]
        , td [ style "cssText" (rowStyle ++ " font-weight: bold;") ] [ text rune.setName ]
        , td [ style "cssText" rowStyle ] [ text (rune.mainStat ++ ": " ++ String.fromInt rune.mainValue) ]
        , td [ style "cssText" (rowStyle ++ " text-align: left; font-size: 12px;") ] 
            [ text (rune.sub1 ++ ": " ++ String.fromInt rune.sub1Value ++ " | "
                    ++ rune.sub2 ++ ": " ++ String.fromInt rune.sub2Value ++ " | "
                    ++ rune.sub3 ++ ": " ++ String.fromInt rune.sub3Value ++ " | "
                    ++ rune.sub4 ++ ": " ++ String.fromInt rune.sub4Value) ]
        ]



viewRuneTable : List Rune -> Html Msg
viewRuneTable runas =
    table [ style "cssText" tableClass ]
        [ thead [ style "cssText" (tableHeaderClass ++ " background-color: #fff9c4;") ]  -- amarelo clarinho
            [ tr []
                [ th [ style "cssText" tableCellClass ] [ text "Slot" ]
                , th [ style "cssText" tableCellClass ] [ text "Set" ]
                , th [ style "cssText" tableCellClass ] [ text "Main Stat" ]
                , th [ style "cssText" tableCellClass ] [ text "Substats" ]
                ]
            ]
        , tbody []
            (List.indexedMap viewRuneRow runas)
        ]



-- VIEW ATUALIZADA

view : Model -> Html Msg
view model =
    div [ style "cssText" containerClass ]
        [ h2 [ style "cssText" titleClass ] [ text "🛡️ GERENCIADOR DE RUNAS" ]
        
        -- Debug info (remover depois)
        , div [ style "cssText" "background: #f0f0f0; padding: 10px; margin: 10px 0; font-size: 12px;" ]
            [ text ("Loading: " ++ (if model.loading then "Sim" else "Não"))
            , text (" | Request: " ++ (case model.currentRequest of
                Just (s, st) -> s ++ "/" ++ st
                Nothing -> "Nenhuma"
            ))
            ]
        
        -- Seção Combinada: Set + Stat
        , h3 [ style "cssText" titleClass ] [ text "🎯 Combinações Set + Stat:" ]
        
        -- Violent combinations
        , div [ style "cssText" sectionClass ]
            [ h3 [ style "cssText" (titleClass ++ " font-size: 16px;") ] [ text "⚔️ Violent:" ]
            , createCombinationButton "Violent" "atk" "Violent + ATK" "#8b0000" model
            , createCombinationButton "Violent" "def" "Violent + DEF" "#8b0000" model
            , createCombinationButton "Violent" "hp" "Violent + HP" "#8b0000" model
            , createCombinationButton "Violent" "spd" "Violent + SPD" "#8b0000" model
            ]
            
        -- Swift combinations
        , div [ style "cssText" sectionClass ]
            [ h3 [ style "cssText" (titleClass ++ " font-size: 16px;") ] [ text "💨 Swift:" ]
            , createCombinationButton "Swift" "atk" "Swift + ATK" "#00bfff" model
            , createCombinationButton "Swift" "def" "Swift + DEF" "#00bfff" model
            , createCombinationButton "Swift" "hp" "Swift + HP" "#00bfff" model
            , createCombinationButton "Swift" "spd" "Swift + SPD" "#00bfff" model
            ]
            
        -- Rage combinations
        , div [ style "cssText" sectionClass ]
            [ h3 [ style "cssText" (titleClass ++ " font-size: 16px;") ] [ text "🔥 Rage:" ]
            , createCombinationButton "Rage" "atk" "Rage + ATK" "#dc143c" model
            , createCombinationButton "Rage" "def" "Rage + DEF" "#dc143c" model
            , createCombinationButton "Rage" "hp" "Rage + HP" "#dc143c" model
            , createCombinationButton "Rage" "spd" "Rage + SPD" "#dc143c" model
            ]
            
        -- Blade combinations  
        , div [ style "cssText" sectionClass ]
            [ h3 [ style "cssText" (titleClass ++ " font-size: 16px;") ] [ text "⚡ Blade:" ]
            , createCombinationButton "Blade" "atk" "Blade + ATK" "#4682b4" model
            , createCombinationButton "Blade" "def" "Blade + DEF" "#4682b4" model
            , createCombinationButton "Blade" "hp" "Blade + HP" "#4682b4" model
            , createCombinationButton "Blade" "spd" "Blade + SPD" "#4682b4" model
            ]
            
        -- Opção para qualquer set
        , div [ style "cssText" sectionClass ]
            [ h3 [ style "cssText" (titleClass ++ " font-size: 16px;") ] [ text "🎲 Qualquer Set:" ]
            , createCombinationButton "Sem_seet" "atk" "Melhor ATK (Qualquer Set)" "#666666" model
            , createCombinationButton "Sem_seet" "def" "Melhor DEF (Qualquer Set)" "#666666" model
            , createCombinationButton "Sem_seet" "hp" "Melhor HP (Qualquer Set)" "#666666" model
            , createCombinationButton "Sem_seet" "spd" "Melhor SPD (Qualquer Set)" "#666666" model
            ]
        
        -- Resultado
        , case (model.valorTotal, model.statEscolhido, model.tipoRuna) of
            (Just valor, Just stat, Just tipo) ->
                div [ style "cssText" resultClass ] 
                    [ text ("🎯 Melhor combinação: " ++ tipo ++ " + " ++ stat ++ " - Valor Total: " ++ String.fromInt valor) ]

            _ ->
                text ""
                
        -- Tabela
        , if List.isEmpty model.melhorCombinacao then
            div [ style "cssText" "text-align: center; color: #666; margin-top: 40px; font-style: italic;" ]
                [ text (if model.loading then "Processando..." else "Selecione uma combinação para ver as runas...") ]
          else
            viewRuneTable model.melhorCombinacao
        ]


-- PROGRAM

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }