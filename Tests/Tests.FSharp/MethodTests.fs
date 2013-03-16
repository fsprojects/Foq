module ``Function Tests``

open Foq
open NUnit.Framework

type IFoo =
    abstract F0 : unit -> bool
    abstract F1 : int -> bool
    abstract F2 : int * int -> bool
    abstract F3 : int * int * int -> bool

let [<Test>] ``f()`` () =
    let x = Mock<IFoo>.Method(fun x -> <@ x.F0 @>).Returns(true)
    Assert.IsTrue(x.F0())

let [<Test>] ``f(1)`` () =
    let x = Mock<IFoo>.Method(fun x -> <@ x.F1 @>).Returns(true)
    Assert.IsTrue(x.F1(1))

let [<Test>] ``f(1,2)`` () =
    let x = Mock<IFoo>.Method(fun x -> <@ x.F2 @>).Returns(true)
    Assert.IsTrue(x.F2(1,2))

let [<Test>] ``f(1,2,3)`` () =
    let x = Mock<IFoo>.Method(fun x -> <@ x.F3 @>).Returns(true)
    Assert.IsTrue(x.F3(1,2,3))
