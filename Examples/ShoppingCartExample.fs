/// Shopping Cart Example based on Maxime Rouiller's code samples in
/// Basics of mocking with Moq: https://blog.maximerouiller.com/post/part-3-advanced-mocking-functionalities/
module ``Shopping Cart Example``

type IProduct =
    abstract IsValid : bool

type ProductEventArgs (product) =
    inherit System.EventArgs()
    member __.Product = product

type ShoppingCart () =
    let mutable products = []
    let added = Event<_>()
    member cart.Add(product:IProduct) =
        if product.IsValid then
            products <- product :: products
            added.Trigger(cart, ProductEventArgs(product))
    [<CLIEvent>]
    member card.ProductAdded = added.Publish

open NUnit.Framework
open FsUnit.TopLevelOperators
open Foq

let [<Test>] ``adding a valid product fire event`` () =
    // Setup our product so that it always returns true on a IsValid verification
    let product = 
        Mock<IProduct>.With(fun currentProduct -> 
            <@ currentProduct.IsValid --> true @>
        )

    // setup an event argument for our event
    let productEventArgs = ProductEventArgs(product);

    // creating our objects and events
    let myShoppingCart = ShoppingCart()
    let isCalled = ref false
    use subscription = myShoppingCart.ProductAdded.Subscribe(fun _ -> isCalled := true)

    // Testing the Add method if it fires the event
    myShoppingCart.Add(product)

    // make sure the event was called
    !isCalled |> should be True
