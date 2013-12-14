// Based on code samples in Philip Japikse's article
// An Introduction to Mocking (Now that we are SOLID)
// http://www.skimedic.com/blog/post/2010/12/21/an-introduction-to-mocking-(now-that-we-are-solid).aspx
module LoginExample

type ILoginService =
    abstract ValidateUser : string * string -> int

type SecurityHandler(service:ILoginService) =
    let mutable userId = 0
    member handler.UserID = userId
    member handler.LoginUser(userName, password) =
        userId <- service.ValidateUser(userName, password)
        userId <> 0

open Foq
open NUnit.Framework

[<Test>]
let Should_Return_True_With_Valid_Username_And_Password() =
    let userName = "Bob"
    let password = "Password"
    let service = 
        Mock<ILoginService>()
            .Setup(fun service -> <@ service.ValidateUser(userName,password) @>)
            .Returns(5)            
    let handler = SecurityHandler(service.Create())
    Assert.IsTrue(handler.LoginUser(userName, password))
    Assert.AreEqual(handler.UserID, 5)

[<Test>]
let Should_Return_False_With_Invalid_Username_And_Password() =
    let userName = "Bob"
    let password = "Password"
    let service = 
        Mock<ILoginService>()
            .Setup(fun service -> <@ service.ValidateUser(userName,password) @>)
            .Returns(0)            
    let handler = SecurityHandler(service.Create())
    Assert.IsFalse(handler.LoginUser(userName, password))
    Assert.AreEqual(handler.UserID, 0)