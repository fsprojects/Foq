module Foq.WithTests

open Foq
open NUnit.Framework

[<Test>]
let ``test mock with multiple members`` () =
    let xs =
        Mock<System.Collections.Generic.IList<char>>.With(fun xs ->
            <@ xs.Count --> 2 
               xs.Item(0) --> '0'
               xs.Item(1) --> '1'
               xs.Contains(any()) --> true
               xs.RemoveAt(2) ==> System.ArgumentOutOfRangeException()
            @>
        )
    Assert.AreEqual(2, xs.Count)
    Assert.AreEqual('0', xs.Item(0))
    Assert.AreEqual('1', xs.Item(1))
    Assert.AreEqual(true, xs.Contains('0'))
    Assert.Throws<System.ArgumentOutOfRangeException>(fun () ->
        xs.RemoveAt(2)
    ) |> ignore

[<Test>]
let ``test fallthrough when matching a specific member`` () =
    let xs =
        Mock<System.Collections.Generic.IList<char>>.With(fun xs ->
            <@ xs.Item(0) --> '0'
               xs.Item(1) --> '1'
               xs.Item(any()) --> '2' @>
        )
    Assert.AreEqual('0', xs.Item(0))
    Assert.AreEqual('1', xs.Item(1))
    Assert.AreEqual('2', xs.Item(2))

[<Test>]
let ``test mock with exception on member`` () =
    let xs =
        Mock<System.Collections.Generic.IList<char>>.With(fun xs ->
            <@
               xs.Insert(any(),any()) ==> System.ArgumentOutOfRangeException()
            @>
        )
    Assert.Throws<System.ArgumentOutOfRangeException>(fun () ->
        xs.Insert(1, 'A')
    ) |> ignore
    

type Side = Bid | Ask
type TimeInForce = GoodForDay | GoodTillCancel

type IOrder =
    abstract Price : decimal
    abstract Quantity : int
    abstract Side : Side
    abstract TimeInForce : TimeInForce

[<Test>]
let ``test single value inline/sequential setup`` () =
    let order =
        Mock<IOrder>.With(fun order -> <@ order.Quantity --> 5 @>)
    Assert.AreEqual(5, order.Quantity)

[<Test>]
let ``test multiple value sequential setup`` () =
    let order =
        Mock<IOrder>
            .With(fun order -> 
            <@  order.Price         --> 99.99M
                order.Quantity      --> 5
                order.Side          --> Side.Bid
                order.TimeInForce   --> TimeInForce.GoodForDay @>)
    Assert.AreEqual(99.99M, order.Price)

type PersonRecord = { Name: string; Age: int }

type PersonClass (name:string, age:int) =
    member this.Name = name
    member this.Age = age

type IFoo =
    abstract RecordProperty : PersonRecord
    abstract TupleProperty : string * int
    abstract ReferenceProperty : PersonClass
    abstract ArrayProperty : int[]

[<Test>]
let ``test with record property`` () =   
    let foo = Mock<IFoo>.With(fun foo -> <@ foo.RecordProperty --> { Name = "Phil"; Age = 27 } @>)
    Assert.AreEqual("Phil", foo.RecordProperty.Name)
    Assert.AreEqual(27, foo.RecordProperty.Age)

[<Test>]
let ``test with tuple property`` () =    
    let foo = Mock<IFoo>.With(fun foo -> <@ foo.TupleProperty --> ("Phil", 27) @>)
    Assert.AreEqual(("Phil", 27), foo.TupleProperty)

[<Test>]
let ``test with reference property`` () =   
    let foo = Mock<IFoo>.With(fun foo -> <@ foo.ReferenceProperty --> PersonClass("Phil", 27) @>)
    Assert.AreEqual("Phil", foo.ReferenceProperty.Name)
    Assert.AreEqual(27, foo.ReferenceProperty.Age)

[<Test>]
let ``test with array property`` () =    
    let foo = Mock<IFoo>.With(fun foo -> <@ foo.ArrayProperty --> [|1;2;3|] @>)
    Assert.AreEqual([|1;2;3|], foo.ArrayProperty)