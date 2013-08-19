module ``ParamArray Tests``

open System
open Foq
open NUnit.Framework

type MyInterface =
    abstract Foo : [<ParamArray>] xs:obj[] -> bool

[<Test>]
let ``should support method with any param array args`` () =
    let mock =
        Mock<MyInterface>()
            .Setup(fun x -> <@ x.Foo(any()) @>).Returns(true)
            .Create()
    Assert.That(mock.Foo())
    Assert.That(mock.Foo(1))
    Assert.That(mock.Foo(1,2,3))

[<Test>]
let ``should support method with empty param array args`` () =
    let mock =
        Mock<MyInterface>()
            .Setup(fun x -> <@ x.Foo() @>).Returns(true)
            .Create()
    Assert.IsTrue(mock.Foo())
    Assert.IsFalse(mock.Foo(1))

[<Test>]
let ``should support method with specific param array args`` () =
    let mock =
        Mock<MyInterface>()
            .Setup(fun x -> <@ x.Foo(1,2,3) @>).Returns(true)
            .Create()
    Assert.IsFalse(mock.Foo())
    Assert.IsFalse(mock.Foo(1,2))
    Assert.IsTrue(mock.Foo(1,2,3))
    Assert.IsFalse(mock.Foo(1,2,3,4))

[<Test>]
let ``can verify method with specific param array args`` () =
    let mock =
        Mock<MyInterface>()
            .Setup(fun x -> <@ x.Foo(any()) @>).Returns(true)
            .Create()
    mock.Foo(1,2,3) |> ignore
    verify <@ mock.Foo(1,2,3) @> once

[<Test>]
let ``can quick setup method with param array args`` () =
    let mock = Mock<MyInterface>.Method(fun x -> <@ x.Foo @>).Returns(true)
    Assert.That(mock.Foo())
