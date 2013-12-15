// Based on code samples in Don't Mock Me article
// http://langrsoft.com/articles/mocking.shtml
module PortfolioExample

type Holding (symbol, numberOfShares) =
    member holding.GetSymbol() = symbol
    member holding.GetNumberOfShares() = numberOfShares

type StockLookupService =
    abstract LookupStockValue: symbol:string -> int

type Portfolio (service:StockLookupService) =
    let mutable holding : Holding option = None

    member portfolio.CurrentValue() =
        match holding with
        | None -> 0 
        | Some holding ->           
            service.LookupStockValue(holding.GetSymbol()) *
            holding.GetNumberOfShares()

    member portfolio.Add(newHolding) = holding <- Some newHolding

open Foq
open NUnit.Framework

let [<Test>] testCreate () =
    let portfolio = Portfolio(mock())
    Assert.AreEqual(0, portfolio.CurrentValue())

let [<Test>] testSingleHoldingSingleShare() =
    let numberOfShares = 1
    let stockSymbol = "VITR"
    let holding = Holding(stockSymbol, numberOfShares)
    let service = 
        Mock<StockLookupService>()
            .Setup(fun s -> <@ s.LookupStockValue(stockSymbol) @>)
            .Returns(1)
    let portfolio = Portfolio(service.Create())
    portfolio.Add(holding)
    Assert.AreEqual(1, portfolio.CurrentValue())