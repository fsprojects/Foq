module ``Function Tests``

open Foq
open NUnit.Framework

type IFoo =
    abstract F0 : unit -> bool
    abstract F1 : int -> bool
    abstract F2 : int * int -> bool
    abstract F3 : int * int * int -> bool
    abstract F4 : int * int * int * int -> bool
    abstract F5 : int * int * int * int * int -> bool
    abstract F6 : int * int * int * int * int * int -> bool
    abstract F7 : int * int * int * int * int * int * int -> bool
    abstract F8 : int * int * int * int * int * int * int * int -> bool

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

let [<Test>] ``f(1,2,3,4)`` () =
    let x = Mock<IFoo>.Method(fun x -> <@ x.F4 @>).Returns(true)
    Assert.IsTrue(x.F4(1,2,3,4))

let [<Test>] ``f(1,2,3,4,5)`` () =
    let x = Mock<IFoo>.Method(fun x -> <@ x.F5 @>).Returns(true)
    Assert.IsTrue(x.F5(1,2,3,4,5))

let [<Test>] ``f(1,2,3,4,5,6)`` () =
    let x = Mock<IFoo>.Method(fun x -> <@ x.F6 @>).Returns(true)
    Assert.IsTrue(x.F6(1,2,3,4,5,6))

let [<Test>] ``f(1,2,3,4,5,6,7)`` () =
    let x = Mock<IFoo>.Method(fun x -> <@ x.F7 @>).Returns(true)
    Assert.IsTrue(x.F7(1,2,3,4,5,6,7))

let [<Test>] ``f(1,2,3,4,5,6,7,8)`` () =
    let x = Mock<IFoo>.Method(fun x -> <@ x.F8 @>).Returns(true)
    Assert.IsTrue(x.F8(1,2,3,4,5,6,7,8))

let [<Test>] ``f(...)`` () =
    let x = 
        Mock<IFoo>()
            .SetupMethod(fun x -> <@ x.F0 @>).Returns(true)
            .SetupMethod(fun x -> <@ x.F1 @>).Returns(true)
            .SetupMethod(fun x -> <@ x.F2 @>).Returns(true)
            .SetupMethod(fun x -> <@ x.F3 @>).Returns(true)
            .SetupMethod(fun x -> <@ x.F4 @>).Returns(true)
            .SetupMethod(fun x -> <@ x.F5 @>).Returns(true)
            .SetupMethod(fun x -> <@ x.F6 @>).Returns(true)
            .SetupMethod(fun x -> <@ x.F7 @>).Returns(true)
            .SetupMethod(fun x -> <@ x.F8 @>).Returns(true)
            .Create()
    Assert.IsTrue(x.F0())
    Assert.IsTrue(x.F1(1))
    Assert.IsTrue(x.F2(1,2))
    Assert.IsTrue(x.F3(1,2,3))
    Assert.IsTrue(x.F4(1,2,3,4))
    Assert.IsTrue(x.F5(1,2,3,4,5))
    Assert.IsTrue(x.F6(1,2,3,4,5,6))
    Assert.IsTrue(x.F7(1,2,3,4,5,6,7))
    Assert.IsTrue(x.F8(1,2,3,4,5,6,7,8))