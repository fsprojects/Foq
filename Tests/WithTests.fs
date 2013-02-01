module Foq.SetupTests

open Foq
open NUnit.Framework

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