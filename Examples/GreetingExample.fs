// Based on code samples in
// Getting started with JMock1
// http://www.whiteboxtest.com/Mocking-JMock.php
module GreetingExample

type GreetingTime =
    abstract GetGreeting: unit -> string

type Greeting(gt:GreetingTime) =
    member greeting.SayGreeting(name) =
        let greeting = gt.GetGreeting()
        greeting + "," + name
       
open Foq
open NUnit.Framework

[<Test>]
let ``expect greeting`` () =
    // Arrange
    let goodNight = 
        Mock<GreetingTime>
            .Method(fun gt -> <@ gt.GetGreeting @>)
            .Returns("Good Night")
    expect <@ goodNight.GetGreeting() @> once
    let greet = Greeting(goodNight)
    // Act
    let out = greet.SayGreeting("Mr. Sam")
    // Assert
    Assert.AreEqual("Good Night,Mr. Sam", out)
    verifyAll goodNight