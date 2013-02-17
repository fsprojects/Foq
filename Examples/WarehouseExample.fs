/// Warehouse example based on Martin Fowler's code samples in
/// "Mock's Aren't Stubs" article: http://martinfowler.com/articles/mocksArentStubs.html
module ``Warehouse example``

type  Product = string
type  Quantity = int

type Warehouse =
    abstract member HasInventory: Product * Quantity -> bool
    abstract member Remove : Product * Quantity -> unit

type MailService =
    abstract Send : Message -> unit
and  Message = string

type Order(product, quantity) =
    let mutable filled = false
    let mutable mailer = { new MailService with member __.Send(_) = () }
    member order.SetMailer(newMailer) = mailer <- newMailer 
    member order.Fill(warehouse:Warehouse) =
        if warehouse.HasInventory(product, quantity) then 
            warehouse.Remove(product, quantity)
            filled <- true
        else mailer.Send("Unfilled")
    member order.IsFilled = filled

open Foq
open NUnit.Framework

let [<Test>] ``filling removes inventory if in stock`` () =
    // setup data
    let product, quantity = "TALISKER", 50
    let order = Order(product, quantity)
    // setup mock behavior
    let warehouse =
        Mock<Warehouse>.With(fun mock -> 
            <@ mock.HasInventory(product, quantity) --> true @>
        )
    // exercise
    order.Fill(warehouse)
    // verify expectations
    Mock.Verify(<@ warehouse.HasInventory(product,quantity) @>, once)
    Mock.Verify(<@ warehouse.Remove(product, quantity) @>, once)
    Assert.IsTrue(order.IsFilled)

let [<Test>] ``filling does not remove if not enough in stock`` () =
    // setup data
    let product, quantity = "TALISKER", 51
    let order = Order(product, quantity)
    // setup mock behavior
    let warehouse =
        Mock<Warehouse>.With(fun mock -> 
            <@ mock.HasInventory(product, quantity) --> false @>
        )
    // exercise
    order.Fill(warehouse)
    // verify expectations
    Mock.Verify(<@ warehouse.Remove(product, quantity) @>, never)
    Assert.IsFalse(order.IsFilled)

let [<Test>] ``order sends mail if unfilled`` () =
    // setup data
    let order = Order("TALISKER", 51)
    let mailer = Mock.Of<MailService>()
    order.SetMailer(mailer)
    // exercise
    order.Fill(Mock.Of<Warehouse>())
    // verify
    Mock.Verify(<@ mailer.Send(any()) @>, once)