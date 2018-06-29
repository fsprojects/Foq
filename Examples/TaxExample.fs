/// Tax Example based on Maxime Rouiller's code samples in
/// Basics of mocking with Moq: http://blog.decayingcode.com/post/part-2-basic-of-mocking-with-moq.aspx
module ``Tax Example``

type ITaxCalculator =
    abstract GetTax : rawPrice:decimal -> decimal

type Product (id, name, rawPrice) =
    member product.ID = id
    member product.Name = name
    member product.RawPrice = rawPrice
    member product.GetPriceWithTax(calculator:ITaxCalculator) =
        rawPrice + calculator.GetTax(rawPrice)

open NUnit.Framework
open FsUnit.TopLevelOperators
open Foq
 
let [<Test>] ``the price with tax for a $25 product with $5 tax should be $30`` () =
    // Initialize our product
    let myProduct = Product(id=1,name="Simple Product",rawPrice=25.0M)
    
    // Create a mock with Foq
    let fakeTaxCalculator = 
        Mock<ITaxCalculator>.With(fun tax ->
            // make sure a 25$ product returns 5$ of tax
            <@ tax.GetTax(25.0M) --> 5.0M @> 
        )
    
    // Retrived the calculated tax
    let calculatedTax = myProduct.GetPriceWithTax(fakeTaxCalculator)
    
    // Verify that the "GetTax" method was called from  the interface
    verify <@ fakeTaxCalculator.GetTax(25.0M) @> once

    // Retrived the calculated tax
    let calculatedTax = myProduct.GetPriceWithTax(fakeTaxCalculator)
 
    // Make sure that the taxes were calculated
    calculatedTax |> should equal 30.0M