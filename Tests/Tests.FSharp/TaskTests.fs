module TaskTests

// http://stackoverflow.com/questions/19093585/using-foq-verify-methodsmatch-fails-on-interface-with-type-parameter

open System
open System.Threading.Tasks
open Foq
open NUnit.Framework

type IAppDatabase = 
    abstract Get<'T> : seq<Guid> -> Task<seq<'T>>
    abstract Set<'T> : seq<'T> -> Task<bool>
    abstract GetIds<'T> : unit -> Task<seq<Guid>>

module DT =
    type Result () = do ()
    let Results results = results

let results = seq [DT.Result (); DT.Result (); DT.Result ()]
let resultInfos = DT.Results results
let guids = seq [Guid.NewGuid(); Guid.NewGuid(); Guid.NewGuid()]

let taskGetIds = Task.Factory.StartNew<seq<Guid>>(fun () -> guids)
let taskSet = Task.Factory.StartNew<bool>(fun () -> true )
let taskGet = Task.Factory.StartNew<seq<DT.Result>>(fun () -> results)

let db = Mock<IAppDatabase>.With(fun x -> 
            <@  
               x.GetIds () --> taskGetIds
               x.Get guids --> taskGet
               x.Set results --> taskSet @>)

type IApp =
    abstract Create : infos:DT.Result seq -> Async<bool>

type App (db:IAppDatabase) =
    interface IApp with 
        member __.Create (infos) = 
            db.Set infos |> Async.AwaitTask

let [<Test>] ``Set the resultInfos in de app database`` () =
    let app = App (db) :> IApp
    let res = app.Create resultInfos |> Async.RunSynchronously 
    let x = db :?> Foq.IMockObject
    let xs = x.Invocations
    verify <@ db.Set results @> once    
    Assert.IsTrue(res)