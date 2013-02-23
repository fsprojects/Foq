/// Balance Calculator based on Richard Carr's code samples in
/// Using Mocks: http://www.blackwasp.co.uk/Mocks.aspx
module ``Balance Example``

type ITransactionRetriever = 
    abstract GetTransactions : int -> decimal[]

type BalanceCalculator(retriever:ITransactionRetriever) =

    member this.GetBalance(startBalance:decimal, period:int) =    
        try        
            let transactions = retriever.GetTransactions(period)
            startBalance + (transactions |> Array.sum)
        with 
            :? System.ArgumentException -> startBalance
           
    member this.GetBalance(startBalance:decimal, startPeriod:int, endPeriod:int) =    
        let mutable runningTotal = startBalance
        for period = startPeriod to endPeriod do       
            runningTotal <- this.GetBalance(runningTotal, period);        
        runningTotal
    
open NUnit.Framework
open FsUnit
open Foq

[<Test>]
let ``total is correct for a period with transactions`` () =
    let mockRetriever = 
        Mock<ITransactionRetriever>()
            .Setup(fun m -> <@ m.GetTransactions(1) @>).Returns([|1M; 2M; 3M; 4M|])            
    let calculator = BalanceCalculator(mockRetriever.Create())
    Assert.AreEqual(15, calculator.GetBalance(5M, 1))

[<Test>]
let ``total is correct for multiple periods with transactions`` () =    
    let mockRetriever = 
        Mock<ITransactionRetriever>.With(fun m ->
            <@
                m.GetTransactions(1) --> [|1M; 2M; 3M; 4M|]
                m.GetTransactions(2) --> [|5M; 6M; 7M|]
                m.GetTransactions(3) --> [|8M; 9M; 10M|]
            @>)
    let calculator = BalanceCalculator(mockRetriever);
    Assert.AreEqual(60, calculator.GetBalance(5M, 1, 3));


[<Test>]
let ``total is zero for an invalid period`` () =
    let mockRetriever = 
        Mock<ITransactionRetriever>()
            .Setup(fun m -> <@ m.GetTransactions(0) @>).Raises(System.ArgumentException())          
    let calculator = BalanceCalculator(mockRetriever.Create())
    Assert.AreEqual(5, calculator.GetBalance(5M, 0))


[<Test>]
let ``total is correct for multiple matching periods with transactions`` () =
    let mockRetriever = 
        Mock<ITransactionRetriever>()
            .Setup(fun m -> <@ m.GetTransactions(any()) @>).Returns([|1M;2M;3M;4M|])
    let calculator = new BalanceCalculator(mockRetriever.Create())
    Assert.AreEqual(35, calculator.GetBalance(5M, 1, 3))