/// Store sample based on Moq's store sample:
//  http://code.google.com/p/moq/source/browse/trunk/Samples/StoreSample/#StoreSample
module ``Store Sample``

open System

type Category = { Id : int; Name : string }
type Product = { Id : int; Name : string }
type Order(product:Product, quantity:int) =
    member val Product = product
    member val Quantity = quantity
    member val Filled = false with get, set

type ICatalogService = 
    abstract GetCategories : unit -> Category seq
    abstract GetProducts : categoryId:int -> Product seq
    abstract HasInventory : productId:int * quantity:int -> bool
    abstract Remove : productId:int * quantity:int -> unit

type IProductsView =
    [<CLIEvent>]
    abstract CategorySelected : IEvent<CategoryEventArgs>
    abstract SetCategories : categories:Category seq -> unit
    abstract SetProducts : products:Product seq -> unit
and CategoryEventArgs(category:Category) =
    inherit EventArgs()
    member args.Category = category

type ProductsPresenter(catalog:ICatalogService, view:IProductsView) =
    do  view.SetCategories(catalog.GetCategories()) 
        view.CategorySelected.Add(fun args -> 
            view.SetProducts(catalog.GetProducts(args.Category.Id))
        )
    member presenter.PlaceOrder(order:Order) =
        if catalog.HasInventory(order.Product.Id, order.Quantity) then
            try catalog.Remove(order.Product.Id, order.Quantity)
                order.Filled <- true
            with :? InvalidOperationException -> () // LOG?

open NUnit.Framework
open Foq

let [<Test>] ``should set view categories`` () =
    // Arrange
    let catalog = Mock.Of<ICatalogService>()
    let view = Mock.Of<IProductsView>()
    // Act
    let presenter = ProductsPresenter(catalog, view)
    // Assert
    Mock.Verify(<@ view.SetCategories(any()) @>)

let [<Test>] ``should category selection set products`` () =
    // Arrange
    let catalog = Mock.Of<ICatalogService>()
    let event = Event<_,_>()
    let view = 
        Mock<IProductsView>()
            .SetupEvent(fun x -> <@ x.CategorySelected @>)
            .Publishes(event.Publish)
            .Create()
    let presenter = ProductsPresenter(catalog, view)
    // Act
    event.Trigger(view, CategoryEventArgs({Category.Id=1; Name="" }))
    // Assert
    Mock.Verify(<@ view.SetProducts(any()) @>)

let [<Test>] ``should not place order if not enough inventory`` () =
    // Arrange
    let catalog = 
        Mock<ICatalogService>.With(fun c -> <@ c.HasInventory(1,5) --> false @>)
    let view = 
        Mock.Of<IProductsView>()
    let presenter = ProductsPresenter(catalog, view);
    let order = Order(product={Id=1; Name=""}, quantity=5)
    // Act
    presenter.PlaceOrder(order);
    // Assert
    Assert.IsFalse(order.Filled)
    Mock.Verify(<@ catalog.HasInventory(1, 5) @>)

let [<Test>] ``should not place order if fails to remove`` () = 
    // Arrange
    let catalog = 
        Mock<ICatalogService>.With(fun c ->
            <@ c.HasInventory(1, 5) --> true
               c.Remove(1,5) ==> InvalidOperationException() @>)
    let view = Mock.Of<IProductsView>();
    let presenter = ProductsPresenter(catalog, view);
    let order = Order(product={Id=1; Name=""}, quantity=5)
    // Act
    presenter.PlaceOrder(order)
    // Assert
    Assert.IsFalse(order.Filled);
    Mock.Verify(<@ catalog.HasInventory(1, 5) @>)
    Mock.Verify(<@ catalog.Remove(1, 5) @>)