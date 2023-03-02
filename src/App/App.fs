module App

open Sutil
open type Feliz.length
open Server
open Sutil.CoreElements
open Sutil.Styling
open UI
open Types
open Browser.Types
open Types.Schema

type Session = {
    Server : Server
    User : User
    }
    with
        static member Create( server, user) = { Server = server; User = user }

type Model = {
    Seq : int
    Page : Page
    Session : Session  option
    }

type Message =
    | SignOut
    | SetPage of Page*string // Change page directly, no change to URL or browse history
    | SignedIn of Schema.User
    | SignedOut
    | External of ExternalMessage
    | Initialized
    | Error of System.Exception
    | SetUrl of string // Change page via URL, and add to browse history

module UrlParser =
    let parseSearch (location : Location) : Map<string,string> =
        match location.search.Length with
        | n when n > 1 && location.search.[0] = '?' ->
            location.search.Substring(1).Split('&')
            |> Array.fold (fun map token ->
                    match token.Split('=') with
                    | pair when pair.Length = 2 -> map.Add( pair.[0], pair.[1] )
                    | _ -> map) Map.empty
        | _ -> Map.empty

    let parseQuery (query : string) : Map<string,string> =
        query.Split('&')
            |> Array.fold (fun map token ->
                    match token.Split('=') with
                    | pair when pair.Length = 2 -> map.Add( pair.[0], pair.[1] )
                    | _ -> map) Map.empty

    let parseHash (location: Location) =
        let hash =
            if location.hash.Length > 1 then location.hash.Substring 1
            else ""
        if hash.Contains("?") then
            let h = hash.Substring(0, hash.IndexOf("?"))
            h, parseQuery(hash.Substring(h.Length+1))
        else
            hash, Map.empty

    let parseUrl (location: Location) =
        parseHash location

    let parseMessage(loc:Location) : Message =
        let hash, query = (parseUrl loc)
        match hash with
        |"profile" -> SetPage (Profile,"navigate")
        |"logout" -> SignOut
        |"signout" -> SignOut
        |"help" -> SetPage (Help,"navigate")
        |"signin" -> SetPage (Login,"navigate")
        |"login" -> SetPage (Login,"navigate")
        |"verify" -> SetPage (AwaitingVerification,"navigate")
        |"register" -> SetPage (Register,"navigate")
        |"browse" -> SetPage (Browse,"navigate")
        | _ -> SetPage (Home,"navigate")

    let toUrl (p : Page) : string =
        match p with
        //| Editor d -> "#editor?d=" + d._id
        | _ -> "#" + (string p)

let init (server : Server) : Model * Cmd<Message> =
    let serverInit dispatch =
        server.Init(dispatch << External, UrlParser.parseSearch Browser.Dom.window.location)
        |> Promise.map (fun _ -> dispatch Initialized)
        |> ignore

    let initPage = Home
    {
        Seq = 0
        Page = initPage
        Session = None
        },
        [ serverInit ]

let rec update (server : Server) msg (model:Model) =
    match msg with

    | Error x ->
        Fable.Core.JS.console.error(x)
        model, Cmd.none

    | SetUrl url ->
        model, Cmd.OfFunc.attempt (fun _ -> Browser.Dom.window.location.href <- url) () Error

    | SignOut ->
        model, Cmd.OfPromise.attempt (server.SignOut) () Error

    | Initialized ->
        let subscribe dispatch =
            let unsub = (server.State |> Store.map (fun s -> s.User) |> Observable.distinctUntilChanged).Subscribe( fun sessionUserOpt ->
                match sessionUserOpt with
                | Some sessionUser ->
                    dispatch (SignedIn sessionUser.User)
                | None ->
                    dispatch SignedOut
            )
            ()

        model, [ subscribe ]

    | External m ->
        match m with
        | Verified result ->
            let confirm dispatch =
                let message =
                    match result with
                    | Ok m -> "Email verified. Please sign in!"
                    | Result.Error s -> "Email verification failed: " + s

                { UI.ModalOptions.Create() with
                    Content = fun close ->
                        Html.div message
                    Buttons = [
                        ("OK", fun close -> close())
                    ]
                } |> UI.modal
            model, [ confirm ]

        | RegisteredNewAccount ->
            model, Cmd.ofMsg (SetPage (Registered,"registered new account"))

        | RegisterNewAccount ->
            model, Cmd.ofMsg (SetPage (Register,"new account"))

    | SetPage (p,who) ->
        { model with Page = p }, Cmd.none

    | SignedOut ->
        { model with Session = None }, Cmd.ofMsg (SetUrl "#home")

    | SignedIn user ->
        if user.emailVerification then
            { model with Session = Session.Create(server,user) |> Some }, Cmd.ofMsg (SetUrl "#home")
        else
            model, Cmd.ofMsg (SetPage (AwaitingVerification,"SignedIn"))

let viewMain server (model : System.IObservable<Model>) dispatch =
    Bind.el( model, fun m ->
        match m.Session, m.Page with
        | _, Register -> Register.view (dispatch << External) server
        | _, Registered -> Verify.view true server
        | _, AwaitingVerification -> Verify.view false server
        | None, Profile -> Verify.view false server
        | Some _, _ -> Html.div (string m.Page)
        | None, _ -> Login.view (dispatch << External) server
    )

let appStyle = [

    rule ".page-content" [
        Css.padding (rem 1)
    ]

    CssMedia.minWidth( UI.BreakPoint, [
        rule ".page-content" [
            Css.paddingTop (rem 2)
            Css.paddingBottom (rem 1)
            Css.paddingLeft (rem 4.5)
            Css.paddingRight (rem 4.5)
        ]
    ])
]

let view() =
    let server = new Server()

    let model, dispatch = server |> Store.makeElmish init (update server) ignore

    let unsubnav = Navigable.listenLocation (UrlParser.parseMessage,dispatch)

    Html.div [
        unsubscribeOnUnmount [ unsubnav ]
        disposeOnUnmount [ model ]

        UI.header [
            UI.UI.navLogo "Example" "#home"
            Bind.el(server.State |> Store.map (fun s -> s.User), fun userOpt ->
                UI.nav [
                    match userOpt with
                    |Some su ->
                        let suffix = if su.User.emailVerification then "" else " (awaiting email verification)"
                        fragment [
                            UI.navLabelMuted "Welcome"
                            UI.navUrl (su.User.name + suffix) "#profile"
                            UI.navLabelIfWide ("|")
                            UI.navUrl "Browse" "#browse"
                            UI.navUrl "Help" "#help"
                            UI.navUrl "Sign Out" "#signout"
                        ]
                    | _ ->
                        fragment [
                            UI.navUrl "Help" "#help"
                            UI.navUrl "Sign In" "#login"
                        ]
                ]
            )
        ]

        Html.div [
            Attr.className "page-content container"
            viewMain server model dispatch
        ]
    ] |> withStyle appStyle

view() |> Program.mount
