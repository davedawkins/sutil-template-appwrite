module Types

open AppwriteSdk
open Fable.Core.Util
open Fable.Core.JsInterop
open Fable.Core

module Schema =

    type User = AppwriteSdk.Models.Account<Models.Preferences>

    type SchemaHelper =
        static member Create<'T when 'T :> Models.Document>() =
            {|
                ``$id`` = Unchecked.defaultof<string>
            |} :> obj :?> 'T

    [<AutoOpen>]
    module Extensions =

        let create<'T when 'T :> Models.Document>() : 'T =
            !!{| ``$id`` = Unchecked.defaultof<string> |}

        [<Emit("Object.assign({}, $0, $1)")>]
        let assignNew (source: 'T) (newProps: 'R): 'T = jsNative


type SessionUser = {
    User          : Schema.User
    IsAdmin       : bool
}

type Page =
    | Login
    | Register
    | AwaitingVerification
    | Registered
    | Profile
    | Browse
    | Home
    | Help

type ExternalMessage =
    | Verified of Result<string,string>
    | RegisteredNewAccount
    | RegisterNewAccount
