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