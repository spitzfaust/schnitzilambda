module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

module Browser = Fable.Import.Browser

// MODEL
type SizeOption = string

type MainDishOption = string

type MeatOption = string

type SideOrderOption = string

type SchnitziOrder =
    { Size : SizeOption option
      MainDish : MainDishOption option
      Meat : MeatOption option
      SideOrder : SideOrderOption option }

type Model =
    { SelectedOptions : SchnitziOrder
      SizeOptions : SizeOption list
      MainDishOptions : MainDishOption list
      MeatOptions : MeatOption list
      SideOrderOptions : SideOrderOption list }

type Msg =
    | SelectSize of SizeOption
    | SelectMainDish of MainDishOption
    | SelectMeat of MeatOption
    | SelectSideOrder of SideOrderOption

let init() : Model =
    { SelectedOptions =
          { Size = None
            MainDish = None
            Meat = None
            SideOrder = None }
      SizeOptions = [ "n/a"; "Klein"; "Mittel"; "Groß"; "XL" ]
      MainDishOptions = [ "n/a"; "Schnitzel"; "Gemüsetaler"; "Gebackener Emmentaler" ]
      MeatOptions = [ "n/a"; "Pute"; "Schwein" ]
      SideOrderOptions = [ "n/a"; "Kartoffelsalat"; "Gurkensalat"; "Pommes"; "Gemischter Salat"; "Wedges" ] }

// UPDATE
let update (msg : Msg) (model : Model) =
    match msg with
    | SelectSize size -> { model with SelectedOptions = { model.SelectedOptions with Size = Some size } }
    | SelectMainDish mainDish -> 
        { model with SelectedOptions = { model.SelectedOptions with MainDish = Some mainDish } }
    | SelectMeat meat -> { model with SelectedOptions = { model.SelectedOptions with Meat = Some meat } }
    | SelectSideOrder sideOrder -> 
        { model with SelectedOptions = { model.SelectedOptions with SideOrder = Some sideOrder } }

// VIEW (rendered with React)
open Fulma

let radioOption (v : string) (name : string) onSelect =
    Field.div [] [ Control.div [] [ Radio.radio [] [ Radio.input [ Radio.Input.Name name
                                                                   Radio.Input.Props [ OnChange
                                                                                           (fun ev -> onSelect ev.Value)
                                                                                       Value v ] ]
                                                     str v ] ] ]

let radioOptions (values : string list) (name : string) onSelect =
    div [] (values |> List.map (fun v -> radioOption v name onSelect))

let selectionCard (options : string list) (title : string) (id : string) onSelect =
    Card.card [] [ Card.header [ ] [ Heading.h2 [ Heading.Is5  ] [ str title ] ]
                   Card.content [] [ (radioOptions options id onSelect) ] ]

let view (model : Model) dispatch =
    div [] 
        [ Container.container [ Container.IsFluid ] 
              [ Heading.h1 [ ] [ str "schnitziλ" ]
                
                Columns.columns [] 
                    [ Column.column [] 
                          [ (selectionCard model.SizeOptions "Select size!" "SizeOptions" 
                                 (fun v -> SelectSize v |> dispatch)) ]
                      
                      Column.column [] 
                          [ (selectionCard model.MainDishOptions "Select main dish!" "MainDishOptions" 
                                 (fun v -> SelectMainDish v |> dispatch)) ]
                      
                      Column.column [] 
                          [ (selectionCard model.MeatOptions "Select meat!" "MeatOptions" 
                                 (fun v -> SelectMeat v |> dispatch)) ]
                      
                      Column.column [] 
                          [ (selectionCard model.SideOrderOptions "Select side order!" "SideOrderOptions" 
                                 (fun v -> SelectSideOrder v |> dispatch)) ] ] ] ]
                                 
#if DEBUG

open Elmish.Debug
open Elmish.HMR
#endif


// App
Program.mkSimple init update view
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
// |> Program.withConsoleTrace
|> Program.withDebugger
#endif

|> Program.run
