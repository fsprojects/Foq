/// Shopping example based on the code samples in Mark Seeman's article -
/// Mock Objects to the Rescue! Test Your .NET Code with NMock:
/// http://msdn.microsoft.com/en-us/magazine/cc163904.aspx
module ``Shopping Example``

open System
open System.Collections.Generic

type IShoppingDataAccess =   
    abstract GetProductName : productID:int -> string
    abstract GetUnitPrice : productID:int -> int
    abstract SaveBasketItems : basketID:Guid * basketItems:BasketItem[] -> unit
and BasketItem(productID:int, quantity:int, dataAccess:IShoppingDataAccess) =   
    let unitPrice = dataAccess.GetUnitPrice(productID)
    let productName = dataAccess.GetProductName(productID) 
    member item.UnitPrice = unitPrice
    member item.ProductID = productID
    member item.Quantity = quantity
    member item.ProductName = productName
    member item.GetPrice() = unitPrice * quantity
and Basket(dataAccess:IShoppingDataAccess) =
    let basketItems = List<_>()
    let basketID = Guid.NewGuid()    
    member basket.AddItem(item:BasketItem) = basketItems.Add(item)
    member basket.Save() =  
        dataAccess.SaveBasketItems(basketID,basketItems.ToArray())
    member basket.CalculateSubTotal() =        
        let mutable subTotal = 0M;
        for item in basketItems do           
            subTotal <- subTotal + decimal (item.GetPrice())
        subTotal

open NUnit.Framework
open Foq

let [<Test>] ``basket item should retrieve values from data access`` () =
    let dataAccess = 
        Mock<IShoppingDataAccess>.With(fun dataAccess ->
            <@ 
              dataAccess.GetUnitPrice(any()) --> 99
              dataAccess.GetProductName(any()) --> "The Moon" 
             @>)

    let item = BasketItem(1, 2, dataAccess)

    Assert.AreEqual(99, item.UnitPrice);
    Assert.AreEqual("The Moon", item.ProductName)
    Assert.AreEqual(198, item.GetPrice())

let [<Test>] ``calculate subtotal from 2 basket items`` () =
    let dataAccess = 
        Mock<IShoppingDataAccess>.With(fun dataAccess ->
            <@ 
              dataAccess.GetUnitPrice(1) --> 99
              dataAccess.GetProductName(1) --> "The Moon"
              dataAccess.GetUnitPrice(5) --> 47
              dataAccess.GetProductName(5) --> "Love" 
             @>)    

    let basket = Basket(dataAccess)
    basket.AddItem(BasketItem(1,2,dataAccess))
    basket.AddItem(BasketItem(5,1,dataAccess))
    basket.Save()

    let subTotal = basket.CalculateSubTotal()
    Assert.AreEqual(245, subTotal)