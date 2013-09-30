module ``Reported Issues``

open System
open Foq
open NUnit.Framework

type Queue = obj
type Exchange = IncomeExchange
type RoutingKey = RoutingKey of string
type Persistent = Persistent of bool
type Priority = Priority of byte

type IMessageConfirmer = interface end

type IChannel =
    inherit IDisposable
    inherit IMessageConfirmer
    abstract QueueDeclare: Queue -> Queue
    abstract Publish: Exchange * RoutingKey * Persistent * Priority * TimeSpan option * body:obj -> unit
    //some other members here...

[<Test>]
let ``Cannot create mock of a interface`` () =
    let aQueue = null
    let channel = 
        Mock<IChannel>()
            .Setup(fun x -> <@ x.QueueDeclare(any()) @>).Returns(aQueue)
            .Setup(fun x -> <@ x.Publish(IncomeExchange, 
                                            RoutingKey "key1", 
                                            Persistent true, 
                                            Priority 0uy,
                                            any(), 
                                            any()) @>).Returns(())
            .Setup(fun x -> <@ x.Publish(IncomeExchange, 
                                            RoutingKey "key2", 
                                            Persistent true, 
                                            Priority 0uy,
                                            any(), 
                                            any()) @>).Returns(())
            .Create()
    let x = channel.QueueDeclare(null)
    channel.Publish(IncomeExchange, RoutingKey "key1", Persistent true, Priority 0uy, None, null)    

// From http://pastebin.com/C5vPSYsA

open Foq
open Foq.It
open System

type IBar = abstract member Bar : unit -> unit

type IFoo =
    // .Create explodes if this member is exposed
    abstract member Do<'a> : Action<'a> -> bool
    abstract member DoTyped : Action<IBar> -> bool
    abstract member DoDummy : Action -> bool

type Foo () =
    interface IFoo with
        member x.Do (a) = true
        member x.DoTyped (a) = true
        member x.DoDummy (a)  = true

let asrt = Assert.IsTrue //System.Diagnostics.Debug.Assert

let [<Test>] ``should handle generic methods`` () =
    (Foq.Mock<IFoo>()
        .Setup(fun x -> <@ x.DoTyped(any()) @>)
        .Returns(true)
        .Create())
        .DoTyped (fun x -> ())
    |> asrt

    (Foq.Mock<IFoo>()
        .Setup(fun x -> <@ x.DoDummy(any()) @>)
        .Returns(true)
        .Create()
        .DoDummy(fun () -> ()))
    |> asrt
    
    (Foq.Mock<IFoo>()
        .Setup(fun x -> <@ x.Do<IBar>(any()) @>)
            .Returns(true)
        .Create()
        .Do<IBar>(any()))
    |> asrt

// From: http://stackoverflow.com/questions/16167068/can-you-set-up-recursive-mocks-in-foq/

type IPublishChannel =
    abstract Bus : IBus
and IBus =
    abstract OpenPublishChannel : unit -> IPublishChannel

let [<Test>] ``Can you set up recursive mocks in foq?`` () =

    let mockBus = ref (Mock.Of<IBus>())
    let mockChannel = ref (Mock.Of<IPublishChannel>())

    mockChannel :=
        Mock<IPublishChannel>()
            .Setup(fun x -> <@ x.Bus @>).Returns(fun () -> !mockBus)
            .Create()
    
    mockBus :=
        Mock<IBus>()
            .Setup(fun x -> <@ x.OpenPublishChannel() @>).Returns(fun () -> !mockChannel)   
            .Create()

    Assert.AreEqual(!mockChannel, (!mockBus).OpenPublishChannel())
    Assert.AreEqual(!mockBus, (!mockChannel).Bus)


type Clock =
        abstract Now: DateTime
        abstract Foo : int -> int

[<Test>]
let ``can setup same property multiple times``() =
    let time1 = DateTime(2013, 1, 1)
    let time2 = DateTime(2013, 2, 2)
 
    let clock = Mock<Clock>.With(fun x -> <@ x.Foo(1) --> 10
                                             x.Foo(2) --> 20
                                             x.Foo(any()) --> 30                 
                                             x.Now --> time1
                                             x.Now --> time2 @>)
 
    Assert.That(clock.Now, Is.EqualTo time1)
    //Assert.That(clock.Now, Is.EqualTo time2) // Fails: currently first matching member is used
    
    Assert.AreEqual(10, clock.Foo(1))
    Assert.AreEqual(20, clock.Foo(2))
    Assert.AreEqual(30, clock.Foo(3))


// http://stackoverflow.com/questions/19093585/using-foq-verify-methodsmatch-fails-on-interface-with-type-parameter

open System.Threading.Tasks

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