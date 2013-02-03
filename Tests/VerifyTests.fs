module Foq.VerifyTests

open Foq
open NUnit.Framework
open System.Collections.Generic

[<Test>]
let ``test verify method calls with any argument values`` () =
    let xs = Mock<IList<int>>.With(fun x -> <@ x.Contains(any()) --> true @>)
    let _ = xs.Contains(1)
    Mock.Verify(xs, (fun order -> <@ xs.Contains(any()) @>), 1)

[<Test>]
let ``test verify method calls with specific argument value`` () =
    let xs = Mock<IList<int>>.With(fun x -> <@ x.Contains(any()) --> true @>)
    let _ = xs.Contains(1)
    Mock.Verify(xs, (fun order -> <@ xs.Contains(1) @>), 1)

[<Test>]
let ``test verify method calls with matching argument value`` () =
    let xs = Mock<IList<int>>.With(fun x -> <@ x.Contains(any()) --> true @>)
    let _ = xs.Contains(1)
    Mock.Verify(xs, (fun order -> <@ xs.Contains(is(fun x -> x > 0)) @>), 1)