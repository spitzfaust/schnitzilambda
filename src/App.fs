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
      SideOrderOptions : SideOrderOption list
      Orders : SchnitziOrder list }

type Msg =
    | SelectSize of SizeOption
    | SelectMainDish of MainDishOption
    | SelectMeat of MeatOption
    | SelectSideOrder of SideOrderOption
    | SubmitOrder

let emptySchnitziOrder =
          { Size = None
            MainDish = None
            Meat = None
            SideOrder = None }

let init() : Model =
    { SelectedOptions = emptySchnitziOrder
      SizeOptions = [ "n/a"; "Klein"; "Mittel"; "Groß"; "XL" ]
      MainDishOptions = [ "n/a"; "Schnitzel"; "Gemüsetaler"; "Gebackener Emmentaler" ]
      MeatOptions = [ "n/a"; "Pute"; "Schwein" ]
      SideOrderOptions = [ "n/a"; "Kartoffelsalat"; "Gurkensalat"; "Pommes"; "Gemischter Salat"; "Wedges" ]
      Orders = []}


// UPDATE
let update (msg : Msg) (model : Model) =
    match msg with
    | SelectSize size -> { model with SelectedOptions = { model.SelectedOptions with Size = Some size } }
    | SelectMainDish mainDish ->
        { model with SelectedOptions = { model.SelectedOptions with MainDish = Some mainDish } }
    | SelectMeat meat -> { model with SelectedOptions = { model.SelectedOptions with Meat = Some meat } }
    | SelectSideOrder sideOrder ->
        { model with SelectedOptions = { model.SelectedOptions with SideOrder = Some sideOrder } }
    | SubmitOrder -> { model with Orders = model.SelectedOptions :: model.Orders; SelectedOptions = emptySchnitziOrder}


// VIEW (rendered with React)
open Fulma

let areAllOptionsSet (order : SchnitziOrder) =
    match order with
    | {Size = Some _; MainDish = Some _; Meat = Some _; SideOrder = Some _} -> true
    | _ -> false

let radioOption (option : string) (name : string) onSelect selectedOption =
    Field.div [] [ Control.div [] [ Radio.radio [] [ Radio.input [ Radio.Input.Name name
                                                                   Radio.Input.Props [ OnChange
                                                                                           (fun ev -> onSelect ev.Value)
                                                                                       Value option
                                                                                       selectedOption
                                                                                       |> Option.map (fun key -> key = option)
                                                                                       |> Option.defaultValue false
                                                                                       |> Checked ] ]
                                                     str option ] ] ]

let selectionCard (options : string list) (selectedOption : string option) (title : string) (id : string) onSelect =
    Card.card [] [ Card.header [ ] [ Heading.h2 [ Heading.Is5  ] [ str title ] ]
                   Card.content [] (List.map (fun option -> radioOption option title onSelect selectedOption) options) ]

let orderRow (id : int) (order : SchnitziOrder) =
    tr []
      [ td [] [str <| string id ]
        td [] [str <| Option.defaultValue "" order.Size]
        td [] [str <| Option.defaultValue "" order.MainDish]
        td [] [str <| Option.defaultValue "" order.Meat]
        td [] [str <| Option.defaultValue "" order.SideOrder] ]

let view (model : Model) dispatch =
    div []
        [ Container.container [ Container.IsFluid ]
              [ Heading.h1 [ ] [ str "schnitziλ" ]

                Columns.columns []
                    [ Column.column []
                          [ (selectionCard model.SizeOptions model.SelectedOptions.Size "Select size!" "SizeOptions"
                                (dispatch << SelectSize)) ]

                      Column.column []
                          [ (selectionCard model.MainDishOptions model.SelectedOptions.MainDish "Select main dish!" "MainDishOptions"
                                (dispatch << SelectMainDish)) ]

                      Column.column []
                          [ (selectionCard model.MeatOptions model.SelectedOptions.Meat "Select meat!" "MeatOptions"
                                (dispatch << SelectMeat)) ]

                      Column.column []
                          [ (selectionCard model.SideOrderOptions model.SelectedOptions.SideOrder "Select side order!" "SideOrderOptions"
                                (dispatch << SelectSideOrder)) ] ]

                Button.button [ Button.Disabled (not <| areAllOptionsSet model.SelectedOptions)
                              ; Button.Props [OnClick (fun _ -> dispatch SubmitOrder) ] ] [str "Submit Order"]

                Table.table []
                    [ thead []
                        [ tr []
                            [ th [] [ str "Position"]
                              th [] [ str "Size"]
                              th [] [ str "Main dish"]
                              th [] [ str "Meat"]
                              th [] [ str "Side order"] ] ]
                      tbody []
                        ( List.mapi orderRow (List.rev model.Orders))
                    ]
              ]
        ]



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
