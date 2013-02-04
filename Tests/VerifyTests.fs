module Foq.VerifyTests

open Foq
open NUnit.Framework
open System.Collections.Generic

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
let ``verify property getter`` () =
    let xs = Mock<IList<int>>.With(fun xs -> <@ xs.Count --> 1 @>)
    Mock.Verify(<@ xs.Count @>, never)
    let _ = xs.Count
    Mock.Verify(<@ xs.Count @>, once)

[<Test>]
let ``verify action`` () =
    let xs = Mock<IList<int>>().Create()
    Mock.Verify(<@ xs.Clear() @>, never)
    let _ = xs.Clear()
    Mock.Verify(<@ xs.Clear() @>, once)
