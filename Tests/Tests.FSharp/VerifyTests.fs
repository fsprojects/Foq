module Foq.VerifyTests

open Foq
open NUnit.Framework
open System.Collections.Generic

[<Test>]
let ``expect method call with any argument value`` () =
    // Arrange 
    let xs = Mock<IList<int>>.With(fun xs -> <@ xs.Contains(any()) --> true @>)
    Mock.Expect(<@ xs.Contains(0) @>, never)
    Mock.Expect(<@ xs.Contains(any()) @>, once)
    // Act
    xs.Contains(1) |> ignore
    // Assert
    Mock.VerifyAll(xs)

[<Test>]
let ``verify method call with any argument value`` () =
    // Arrange 
    let xs = Mock<IList<int>>.With(fun xs -> <@ xs.Contains(any()) --> true @>)
    // Act
    let _ = xs.Contains(1)
    // Assert
    Mock.Verify(<@ xs.Contains(0) @>, never)
    Mock.Verify(<@ xs.Contains(any()) @>, once)

[<Test>]
let ``verify method call with specific argument value`` () =
    let xs = Mock<IList<int>>.With(fun xs -> <@ xs.Contains(any()) --> true @>)
    Mock.Verify(<@ xs.Contains(1) @>, never)
    let _ = xs.Contains(0)
    Mock.Verify(<@ xs.Contains(1) @>, never)
    let _ = xs.Contains(1)
    Mock.Verify(<@ xs.Contains(1) @>, once)

[<Test>]
let ``verify method call with matching argument value`` () =
    let xs = Mock<IList<int>>.With(fun xs -> <@ xs.Contains(any()) --> true @>)
    Mock.Verify(<@ xs.Contains(is(fun x -> x > 0)) @>, never)
    let _ = xs.Contains(1)
    Mock.Verify(<@ xs.Contains(is(fun x -> x > 0)) @>, once)

[<Test>]
let ``expect property getter`` () =
    let xs = Mock<IList<int>>.With(fun xs -> <@ xs.Count --> 1 @>)
    Mock.Expect(<@ xs.Count @>, once)
    let _ = xs.Count
    Mock.VerifyAll xs

[<Test>]
let ``verify property getter`` () =
    let xs = Mock.Of<IList<int>>()
    Mock.Verify(<@ xs.Count @>, never)
    let _ = xs.Count
    Mock.Verify(<@ xs.Count @>, once)

let [<Test>] ``expect action`` () =
    let xs = Mock.Of<IList<int>>()
    expect <@ xs.Clear() @> once
    xs.Clear()
    verifyAll xs

[<Test>]
let ``verify action`` () =
    let xs = Mock.Of<IList<int>>()
    Mock.Verify(<@ xs.Clear() @>, never)
    let _ = xs.Clear()
    Mock.Verify(<@ xs.Clear() @>, once)

[<Test>]
let ``expect property setter`` () =
    let xs = Mock.Of<IList<int>>()
    Mock.Expect(<@ xs.[0] <- 1 @>, once)
    xs.[0] <- 1
    Mock.VerifyAll xs

[<Test>]
let ``verify property setter`` () =
    let xs = Mock.Of<IList<int>>()    
    Mock.Expect(<@ xs.[0] <- 1 @>, once)
    xs.[0] <- 1
    Mock.VerifyAll xs

[<Test>]
let ``expect sequence of calls`` () =
    let xs = Mock<IList<int>>.ExpectSequence(fun xs ->
        <@
            xs.Clear()
            xs.[any()] --> any()
            xs.[any()] <- any()
            xs.Count --> any()
            xs.Contains(any()) --> any()
        @>)
    xs.Clear()
    let value = xs.[0]
    xs.[0] <- value
    let count = xs.Count
    xs.Contains(1) |> ignore
    Mock.VerifyAll xs

[<Test>]
let ``verify sequence`` () =
    let xs = Mock.Of<IList<int>>()
   
    xs.Clear()
    let value = xs.[0]
    xs.[0] <- value
    let count = xs.Count
    xs.Contains(1) |> ignore

    Mock.VerifySequence
        <@
            xs.Clear()
            xs.[any()] --> any()
            xs.[any()] <- any()
            xs.Count --> any()
            xs.Contains(any()) --> any()
        @>

[<Test>]
let ``expect repeat`` () =
    // Arrange
    let xs = Mock<IList<int>>.ExpectSequence(fun mock ->
        <@
            for i = 1 to 10 do mock.Add(any())
        @>)
    // Act
    for i = 1 to 10 do xs.Add(1)
    // Assert
    Mock.VerifyAll(xs)

[<Test>]
let ``verify repeat`` () =
    // Arrange
    let xs = Mock.Of<IList<int>>()
    // Act
    for i = 1 to 10 do xs.Add(any())
    // Assert
    Mock.VerifySequence
        <@
            for i = 1 to 10 do xs.Add(any())
        @>