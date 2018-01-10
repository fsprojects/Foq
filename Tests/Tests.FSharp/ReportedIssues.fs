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

let asrt : bool->unit = Assert.IsTrue //System.Diagnostics.Debug.Assert

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

// From http://foq.codeplex.com/workitem/13 - Mock Call 8 arguments

open System.Collections.Generic

[<AllowNullLiteral>] type DataTable () = class end

type IInterface =
    abstract Arity1Method : 'a -> bool
    abstract Arity2Method : string -> string -> bool
    abstract Arity3Method : string -> string -> string -> bool
    abstract Arity4Method : string -> string -> string -> DataTable -> bool
    abstract Arity5Method : string -> string -> string -> DataTable -> 'a seq -> bool
    abstract Arity6Method : string -> string -> string -> DataTable -> 'a seq -> ('a seq -> bool) -> bool
    abstract Arity7Method : string -> string -> string -> DataTable -> 'a seq -> ('a seq -> bool) -> IDictionary<int,int> -> bool
    abstract Arity8Method : string -> string -> string -> string -> DataTable -> 'a seq -> ('a seq -> bool) -> IDictionary<int,int> -> bool

[<Test>]
let ``an implemented interface method with 1 any arg``() =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity1Method (any():int) @>)
            .Calls<int>(fun a -> true)
            .Create()
    Assert.IsTrue(stub.Arity1Method 1)

[<Test>]
let ``an implemented interface method with 2 any args``() =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity2Method (any()) (any()) @>)
            .Calls<string * string>(fun (a,b) -> true)
            .Create()
    Assert.IsTrue(stub.Arity2Method "1" "2")

[<Test>]
let ``an implemented interface method with 3 any args``() =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity3Method (any()) (any()) (any()) @>)
            .Calls<string * string * string>(fun (a,b,c) -> true)
            .Create()
    Assert.IsTrue(stub.Arity3Method "1" "2" "3")

[<Test>]
let ``an implemented interface method with 4 any args``() =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity4Method (any()) (any()) (any()) (any()) @>)
            .Calls<string * string * string * DataTable>(fun (a,b,c,d) -> true)
            .Create()
    Assert.IsTrue(stub.Arity4Method "1" "2" "3" null)

[<Test>]
let ``an implemented interface method with 5 any args``() =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity5Method (any()) (any()) (any()) (any()) (It.IsAny<int seq>()) @>)
            .Calls<string * string * string * DataTable * int seq>(fun (a,b,c,d,e) -> true)
            .Create()
    Assert.IsTrue(stub.Arity5Method "1" "2" "3" null [|1|])

[<Test>]
let ``an implemented interface method with 6 any args``() =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity6Method (any()) (any()) (any()) (any()) (any():int seq) (any()) @>)
            .Calls<string * string * string * DataTable * int seq * (int seq -> bool)>(fun (a,b,c,d,e,f) -> true)
            .Create()
    Assert.IsTrue(stub.Arity6Method "1" "2" "3" null [|1|] (fun xs -> true))

[<Test>]
let ``an implemented interface method with 7 args``() =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity7Method (any()) (any()) (any()) (any()) (any():string seq) (any()) (any()) @>)
            .Calls<string * string * string * DataTable * string seq * (string seq -> bool) * IDictionary<int, int>>(fun (a,b,c,d,e,f,g) -> true)
            .Create()
    Assert.IsTrue(stub.Arity7Method "1" "2" "3" (new DataTable()) [|"1"|] (fun _ -> true) (dict [1,1]))

[<Test>]
let ``an implemented interface method with 8 args``() =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity8Method (any()) (any()) (any()) (any()) (any()) (any():string seq) (any()) (any()) @>)
            .Calls<string * string * string * string * DataTable * string seq * (string seq -> bool) * IDictionary<int, int>>(fun (a,b,c,d,e,f,g,h) -> true)
            .Create()
    Assert.IsTrue(stub.Arity8Method "1" "2" "3" "4" (new DataTable()) [|"1"|] (fun _ -> true) (dict []))

// Reported issue https://github.com/fsprojects/Foq/issues/4
// Unable to reproduce

type IFoo' =
    abstract N : int64

[<Test>]
let ``a property returns a 64-bit integer value``() =
    let mock1 = Mock<IFoo'>().Setup(fun foo -> <@ foo.N @>).Returns(7L).Create()
    Assert.AreEqual(7L, mock1.N)

[<Test>]
let ``a property returns a 64-bit integer via a function``() =
    let mock2 = Mock<IFoo'>().Setup(fun foo -> <@ foo.N @>).ReturnsFunc(fun () -> 7L).Create()
    Assert.AreEqual(7L, mock2.N)

