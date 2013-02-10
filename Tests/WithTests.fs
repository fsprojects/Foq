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