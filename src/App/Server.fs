module Server

open System
open Fable.Core
open AppwriteSdk
open Sutil
open Types

let appUrl = "http://localhost:8080/"
//let appUrl = "https://example.net/"

//let private serviceUrl = "https://appwrite.doodletoy.net/v1"
let private serviceUrl = "https://solochimp.com/v1"
let private exampleProjectId = "63ffcf3c7aa4665e4de1"
let private databaseId = "default"
let private peopleCollectionId = "63ffcfb80a490121f6de"

type ServerModel = {
    User : SessionUser option
}

type Server() =
    let initModel() = {
        User = None
    }

    let model = ObservableStore.makeStore initModel ignore

    let current() = model |> Store.get

    // -- Initialize Appwrite ---------------------------------------------------------------------
    let client = Appwrite.Client.Create()
    let account = Appwrite.Account.Create(client)
    let db = Appwrite.Databases.Create(client)

    let initAppwrite() =
        client
            .setEndpoint(serviceUrl)
            .setProject(exampleProjectId)
            |> ignore

    let filterVisitor (userOpt : Schema.User option) =
        match userOpt with
        | Some u when u.email = "" ->
            None
        | _ ->
            userOpt

    let logError (msg : string) =
        JS.console.error(msg)

    let logDebug (msg : string) =
        ignore msg //JS.console.log("Server: " + msg)

    let setSessionUser (user : Option<SessionUser>) =
        logDebug("setSessionUser " + (user |> function None -> "None"| Some u -> u.User.name))
        Store.modify (fun m -> { m with ServerModel.User = user }) model

    let startSession() = promise {
        logDebug("startSession")

        let! userOrVisitor = promise {
            try
                // We may already have an active session, so pick it up
                let! session = (account.get() : JS.Promise<Schema.User>)
                logDebug(" - resume session: " + session.email)
                return Some session
            with
            |x ->
                return None
        }

        let sessionUser =
            userOrVisitor
            |> filterVisitor
            |> Option.map (fun u -> {
                SessionUser.User = u
                IsAdmin = false
            })

        setSessionUser sessionUser

        return ()
    }

    let ignoreError (f : unit -> unit) (p : JS.Promise<'T>) =
        p |> Promise.map (fun _ -> f()) |> Promise.catch (fun _ -> f()) |> ignore

    let catchError (p : JS.Promise<'T>) =
        p
            |> Promise.map (fun u ->
                JS.console.dir(u)
            )
            |> Promise.catch (fun x -> logError(x.Message))
            |> ignore

    member _.State : IObservable<ServerModel> = upcast model
    member _.SessionUser = current().User

    member this.Init( dispatch : ExternalMessage -> unit, urlParams : Map<string,string> ) =
        promise {
            initAppwrite()

            do! startSession()

            if urlParams.ContainsKey("userId") && urlParams.ContainsKey("secret") then
                // history.pushState(null, "", location.href.split("?")[0]);
                let userId, secret = urlParams.["userId"], urlParams.["secret"]

                Browser.Dom.history.pushState(null, "", Browser.Dom.window.location.href.Split('?').[0])

                try
                    let! _ = account.updateVerification(userId, secret)
                    do! this.SignOut()

                    (Ok "Email verified - please sign in") |> Verified |> dispatch
                with
                | x ->
                    (Result.Error x.Message) |> Verified |> dispatch
        }

    member _.SendVerificationEmail() = account.createVerification( appUrl )

    member this.Register(email:string, password:string, name : string) =
        promise {
            let! _ = account.create( "ID.unique()", email, password, name)
            do! this.SignIn(email,password)
        }

    member _.SignIn(email:string, password:string) =
        promise {
            try
                let! _ = account.deleteSession "current"
                logDebug("Signed out")
                ()
            with
                _ -> ()

            logDebug("createSession")
            let! _ = account.createEmailSession( email, password )

            logDebug("starting session")
            do! startSession()
        }

    member _.SignInWith(provider : string) =
        account.deleteSession "current"
        |> ignoreError (fun () -> // FIXME
            account.createOAuth2Session( provider, appUrl, appUrl ) |> ignore
        )

    member _.SignOut() =
        promise {
            try
                let! _ = account.deleteSession "current"
                ()
            with x ->
                logError("SignOut: " + x.Message)
                ()
            do! startSession()
            return ()
        }

    /// Get all document chunks into a single array
    member x.ListAll<'T when 'T :> Models.Document>( collectionId : string, filter : string array ) : JS.Promise<'T array> =
        promise {
            let mutable chunks : ('T array) list = []
            let mutable received = 0
            let mutable total = 999 // Yuck. Allow initial iteration (received < total)

            while received < total do
                let! chunk =
                    db.listDocuments(databaseId, collectionId,
                    [|
                        Appwrite.Query.limit(25)
                        Appwrite.Query.offset(received)
                    |] |> Array.append filter
                ) //: JS.Promise<ListDocumentsResult<'T>>

                if (received = 0) then
                    total <- int(chunk.total)

                received <- received + chunk.documents.Length
                chunks <- chunk.documents :: chunks

            return chunks |> Array.concat
        }
