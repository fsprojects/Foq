/// Account Exammple based on code samples in a Code Project article
/// Introduction to Mocking: http://www.codeproject.com/Articles/30381/Introduction-to-Mocking
module ``Account Example``

type User = { UserName:string; Password:string }

type IAverageJoeBankService =
    abstract Authenticate : userName:string * password:string -> bool

type AccountBalanceService (averageJoeService:IAverageJoeBankService) =
    member service.GetAccountBalanceByUser(user:User) =
        let isAuthenticated = averageJoeService.Authenticate(user.UserName, user.Password)
        if not isAuthenticated
        then raise (System.Security.SecurityException("User is not authenticated"))
        100.0

open NUnit.Framework
open Foq

[<Test>]
let ``should be able to get the balance successfully`` () =
    let user = { User.UserName = "JohnDoe"; Password = "JohnPassword" }
    let bankService = Mock<IAverageJoeBankService>.Method(fun x -> <@ x.Authenticate @>).Returns(true)
    let balanceService = AccountBalanceService(bankService)
    Assert.AreEqual(100., balanceService.GetAccountBalanceByUser(user))


