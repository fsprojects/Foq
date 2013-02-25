/// Shop example based on the code in Mark Seeman's MSDN article -
/// Exploring The Continuum Of Test Doubles: http://msdn.microsoft.com/en-us/magazine/cc163358.aspx
module ``Shop Example``

type IShopDataAccess =
    abstract GetProductPrice: int -> decimal
    abstract Save : int * Order -> unit
and Order(orderId:int, dataAccess:IShopDataAccess) as order =
    do if obj.ReferenceEquals(dataAccess,null) then nullArg "dataAccess"
    let orderLines = OrderLineCollection(order)
    member order.Lines = orderLines
    member order.Save() = dataAccess.Save(orderId, order)
    member internal order.DataAccess = dataAccess
and OrderLineCollection(owner:Order) =
    inherit System.Collections.Generic.List<OrderLine>() 
    member collection.Add(productId, quantity) =
        collection.Add(OrderLine(productId, quantity, owner))
and OrderLine(productId:int,quantity:int,owner:Order) =
    let getUnitPrice () = owner.DataAccess.GetProductPrice(productId)
    let total = lazy (getUnitPrice() * decimal quantity)
    member line.Total = total.Value

open System
open NUnit.Framework
open Foq

[<ExpectedException(typeof<ArgumentNullException>)>]
let [<Test>] CreateOrderWithNullDataAccess() =
    Order(1, Unchecked.defaultof<IShopDataAccess>) |> ignore

let [<Test>] CreateOrder() =
    let dataAccess = Mock.Of<IShopDataAccess>()

    let o = Order(2, dataAccess)
    o.Lines.Add(1234, 1)
    o.Lines.Add(4321, 3)

    Assert.AreEqual(2, o.Lines.Count)
    // More asserts could go here...

let [<Test>] SaveOrder() =
    let dataAccess = Mock.Of<IShopDataAccess>()

    let o = Order(3, dataAccess)
    o.Lines.Add(1234, 1)
    o.Lines.Add(4321, 3)

    o.Save()

let [<Test>] SaveOrderWithDataAccessVerification() =
    let dataAccess = Mock.Of<IShopDataAccess>()

    let o = Order(4, dataAccess)
    o.Lines.Add(1234, 1)
    o.Lines.Add(4321, 3)
    o.Save()

    Mock.Verify(<@ dataAccess.Save(any(), any()) @>)

let [<Test>] SaveOrderWithCountBasedDataAccessVerification() =
    let dataAccess = Mock.Of<IShopDataAccess>()

    let o = Order(5, dataAccess)
    o.Lines.Add(1234, 1)
    o.Lines.Add(4321, 3)

    o.Save()

    Mock.Verify(<@ dataAccess.Save(any(), any()) @>, once)

let [<Test>] SaveOrderAndVerifyExpectations() =
    let dataAccess = Mock.Of<IShopDataAccess>()

    let o = Order(6, dataAccess)
    o.Lines.Add(1234, 1)
    o.Lines.Add(4321, 3)

    o.Save()

    Mock.Verify(<@ dataAccess.Save(6, o) @>)

let [<Test>] CalculateSingleLineTotal() =
    let dataAccess = 
        Mock<IShopDataAccess>()
            .Setup(fun dataAccess -> <@ dataAccess.GetProductPrice(1234) @>).Returns(25M)
            .Create()

    let o = Order(7, dataAccess)
    o.Lines.Add(1234, 2)

    let lineTotal = o.Lines.[0].Total;
    Assert.AreEqual(50M, lineTotal);

let [<Test>] CalculateTwoLineTotals() =
    let dataAccess =
        Mock<IShopDataAccess>()
            .Setup(fun dataAccess -> <@ dataAccess.GetProductPrice(1234) @>).Returns(25M)
            .Setup(fun dataAccess -> <@ dataAccess.GetProductPrice(2345) @>).Returns(10M)
            .Create()

    let o = Order(8, dataAccess)
    o.Lines.Add(1234, 1)
    o.Lines.Add(2345, 3)

    Assert.AreEqual(25M, o.Lines.[0].Total)
    Assert.AreEqual(30M, o.Lines.[1].Total)


let [<Test>] CalculateLineTotalsUsingFake() =
    let dataAccess = 
        Mock<IShopDataAccess>.With(fun dataAccess ->
            <@
            dataAccess.GetProductPrice(1234) --> 45M
            dataAccess.GetProductPrice(2345) --> 15M
            @>)

    let o = new Order(9, dataAccess)
    o.Lines.Add(1234, 3)
    o.Lines.Add(2345, 2)

    Assert.AreEqual(135M, o.Lines.[0].Total)
    Assert.AreEqual(30M, o.Lines.[1].Total)

let [<Test>] CalculateLineTotalsUsingDelegate() =
    let dataAccess =
        Mock<IShopDataAccess>()
            .Setup(fun dataAccess -> <@ dataAccess.GetProductPrice(any()) @>).Calls<int>(function
                | 1234 -> 45M
                | 2345 -> 15M
                | productID -> raise <| ArgumentOutOfRangeException("productId") 
                )
            .Create()

    let o = Order(10, dataAccess)
    o.Lines.Add(1234, 3)
    o.Lines.Add(2345, 2)

    Assert.AreEqual(135, o.Lines.[0].Total)
    Assert.AreEqual(30, o.Lines.[1].Total)

let [<Test>] CalculateLineTotalsUsingManualMock() =
    let dataAccess =
        { new IShopDataAccess with
            member __.GetProductPrice(productId) =
                match productId with
                | 1234 -> 45M
                | 2345 -> 15M
                | _ -> raise <| ArgumentOutOfRangeException("productId")
            member __.Save(_,_) = failwith "Not implemented"
        }

    let o = Order(11, dataAccess)
    o.Lines.Add(1234, 3)
    o.Lines.Add(2345, 2)

    Assert.AreEqual(135, o.Lines.[0].Total)
    Assert.AreEqual(30, o.Lines.[1].Total)

let [<Test>] CalculateLineTotalsUsingDelegateMock() =
    let mockDataAccess f =
        { new IShopDataAccess with
            member __.GetProductPrice(productId) = f productId
            member __.Save(_,_) = failwith "Not implemented"
        }

    let getProductId = function
        | 1234 -> 45M
        | 2345 -> 15M
        | _ -> raise <| ArgumentOutOfRangeException("productId")

    let o = Order(12, mockDataAccess getProductId)
    o.Lines.Add(1234, 3)
    o.Lines.Add(2345, 2)

    Assert.AreEqual(135, o.Lines.[0].Total)
    Assert.AreEqual(30, o.Lines.[1].Total)

let [<Test>] CalculateLineTotalsUsingDynamicMock() =
    let dataAccess = 
        Mock<IShopDataAccess>.With(fun dataAccess ->
            <@
            dataAccess.GetProductPrice(1234) --> 45M
            dataAccess.GetProductPrice(2345) --> 15m
            @>)

    let o = new Order(13, dataAccess)
    o.Lines.Add(1234, 3)
    o.Lines.Add(2345, 2)

    Assert.AreEqual(135, o.Lines.[0].Total)
    Assert.AreEqual(30, o.Lines.[1].Total)

    Mock.Verify(<@ dataAccess.GetProductPrice(1234) @>)
    Mock.Verify(<@ dataAccess.GetProductPrice(2345) @>)