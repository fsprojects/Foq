﻿module ``Reported Issues``

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
