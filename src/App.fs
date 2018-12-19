

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

type SelectedOptions =
    { Size : SizeOption option
      MainDish : MainDishOption option
      Meat : MeatOption option
      SideOrder : SideOrderOption option }

type SchnitziOrder =
    { Size : SizeOption
      MainDish : MainDishOption
      Meat : MeatOption
      SideOrder : SideOrderOption }

type Model =
    { SelectedOptions : SelectedOptions
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

let (noOptionsSelected : SelectedOptions) =
    { Size = None
      MainDish = None
      Meat = None
      SideOrder = None }

let init() : Model =
    { SelectedOptions = noOptionsSelected
      SizeOptions = [ "n/a"; "Klein"; "Mittel"; "Groß"; "XL" ]
      MainDishOptions = [ "n/a"; "Schnitzel"; "Gemüsetaler"; "Gebackener Emmentaler" ]
      MeatOptions = [ "n/a"; "Pute"; "Schwein" ]
      SideOrderOptions = [ "n/a"; "Kartoffelsalat"; "Gurkensalat"; "Pommes"; "Gemischter Salat"; "Wedges" ]
      Orders = [] }


// UPDATE
let makeSchnitziOrder (selectedOptions : SelectedOptions) =
    match selectedOptions with
    | { Size = Some s; MainDish = Some md; Meat = Some m; SideOrder = Some so } ->
        Some { Size = s; MainDish = md; Meat = m; SideOrder = so }
    | _ -> None

let update (msg : Msg) (model : Model) =
    match msg with
    | SelectSize size -> { model with SelectedOptions = { model.SelectedOptions with Size = Some size } }
    | SelectMainDish mainDish ->
        { model with SelectedOptions = { model.SelectedOptions with MainDish = Some mainDish } }
    | SelectMeat meat -> { model with SelectedOptions = { model.SelectedOptions with Meat = Some meat } }
    | SelectSideOrder sideOrder ->
        { model with SelectedOptions = { model.SelectedOptions with SideOrder = Some sideOrder } }
    | SubmitOrder ->
        match makeSchnitziOrder model.SelectedOptions with
        | Some order -> { model with Orders = order :: model.Orders; SelectedOptions = noOptionsSelected }
        | None -> model


// VIEW (rendered with React)
open Fulma

let radioOption (option : string) (name : string) onSelect selected =
    Field.div [] [ Control.div [] [ Radio.radio [] [ Radio.input [ Radio.Input.Name name
                                                                   Radio.Input.Props [ OnChange
                                                                                           (fun ev -> onSelect ev.Value)
                                                                                       Value option
                                                                                       Checked selected ] ]
                                                     str option ] ] ]

let selectionCard (options : string list) (selectedOption : string option) (title : string) (id : string) onSelect =
    Card.card [] [ Card.header [] [ Heading.h2 [ Heading.Is5 ] [ str title ] ]
                   Card.content [] (List.map (fun option -> radioOption option title onSelect (selectedOption = Some option)) options) ]

let orderRow (index : int) (order : SchnitziOrder) =
    tr []
      [ td [] [ str (string index) ]
        td [] [ str order.Size ]
        td [] [ str order.MainDish ]
        td [] [ str order.Meat ]
        td [] [ str order.SideOrder ] ]

let view (model : Model) dispatch =
    div []
        [ Container.container [ Container.IsFluid ]
              [ Heading.h1 [] [ str "schnitziλ" ]

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

                Button.button [ Button.Disabled((makeSchnitziOrder model.SelectedOptions) = None)
                             ; Button.Props [ OnClick(fun _ -> dispatch SubmitOrder) ] ] [ str "Submit Order" ]

                Table.table []
                    [ thead []
                        [ tr []
                            [ th [] [ str "Position" ]
                              th [] [ str "Size" ]
                              th [] [ str "Main dish" ]
                              th [] [ str "Meat" ]
                              th [] [ str "Side order" ] ] ]
                      tbody []
                        (List.mapi orderRow (List.rev model.Orders))
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
