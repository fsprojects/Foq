module Foq.VerifyTests

open Foq
open NUnit.Framework
open System.Collections.Generic

[<Test>]
let ``test verify method call with any argument values`` () =
    let xs = Mock<IList<int>>.With(fun xs -> <@ xs.Contains(any()) --> true @>)
    Mock.Verify(<@ xs.Contains(any()) @>, 0)
    let _ = xs.Contains(1)
    Mock.Verify(<@ xs.Contains(any()) @>, 1)

[<Test>]
let ``test verify method call with specific argument value`` () =
    let xs = Mock<IList<int>>.With(fun xs -> <@ xs.Contains(any()) --> true @>)
    Mock.Verify(<@ xs.Contains(1) @>, 0)
    let _ = xs.Contains(0)
    Mock.Verify(<@ xs.Contains(1) @>, 0)
    let _ = xs.Contains(1)
    Mock.Verify(<@ xs.Contains(1) @>, 1)

[<Test>]
let ``test verify method call with matching argument value`` () =
    let xs = Mock<IList<int>>.With(fun xs -> <@ xs.Contains(any()) --> true @>)
    Mock.Verify(<@ xs.Contains(is(fun x -> x > 0)) @>, 0)
    let _ = xs.Contains(1)
    Mock.Verify(<@ xs.Contains(is(fun x -> x > 0)) @>, 1)