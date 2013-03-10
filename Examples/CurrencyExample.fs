/// Currency example based on Telerik's Just* teams article:
/// Doing Your First Mock: 
/// http://blogs.telerik.com/justteam/posts/12-05-14/doing-your-first-mock.aspx
module ``Currency Example``

type ICurrencyService =
    abstract GetConversionRate: fromCurrency:string * toCurrency:string -> decimal

type AccountService(currencyService:ICurrencyService) =
    member service.TransferFunds(from:Account, ``to``:Account, amount:decimal) =
        from.Withdraw(amount)
        let conversionRate = currencyService.GetConversionRate(from.Currency, ``to``.Currency)
        let convertedAmount = amount * conversionRate
        ``to``.Deposit(convertedAmount)
and Account (startingBalance:decimal, currency:string) =
    let mutable balance = startingBalance
    let transfer amount= balance <- balance + amount 
    member account.Withdraw(amount) = transfer -amount
    member account.Deposit(amount) = transfer +amount
    member account.Currency = currency
    member account.Balance = balance

open NUnit.Framework
open Foq

[<Test>]
let ``a transfer of funds between a british and canadian account should use the currency conversion rate``
    () =
    // Arrange
    let currencyService = 
        Mock<ICurrencyService>()
            .Setup(fun mock -> <@ mock.GetConversionRate("GBP", "CAD") @>).Returns(2.20M)
            .Create()

    let accountService = AccountService(currencyService)
  
    let canadianAccount = Account(0M, "CAD")
    let britishAccount = Account(0M, "GBP")

    britishAccount.Deposit(100M)
    
    // Act
    accountService.TransferFunds(britishAccount, canadianAccount, 100M);

    // Assert
    Assert.AreEqual(0, britishAccount.Balance);
    Assert.AreEqual(220, canadianAccount.Balance);

    Mock.Verify <@ currencyService.GetConversionRate(any(),any()) @> 