module ``Generics Tests``

open System
open NUnit.Framework
open Foq

type IInterface10 =
    abstract Arity1<'a> : 'a -> bool 

let [<Test>] ``one generic argument`` () =
    Mock<IInterface10>()
        .Setup(fun mock -> <@ mock.Arity1(any()) @>).Returns(true)
        .Create()
        .Arity1(any())
    |> Assert.IsTrue

let [<Test>] ``one generic argument with value specified`` () =
    Mock<IInterface10>()
        .Setup(fun mock -> <@ mock.Arity1(1) @>).Returns(true)
        .Create()
        .Arity1(1)
    |> Assert.IsTrue

type IInterface11 =
    abstract Arity2<'a> : 'a * int -> bool 

let [<Test>] ``one generic argument & one non-generic`` () =
    Mock<IInterface11>()
        .Setup(fun mock -> <@ mock.Arity2(any(),any()) @>).Returns(true)
        .Create()
        .Arity2(any(), any())
    |> Assert.IsTrue

let [<Test>] ``one generic argument one & one int value`` () =
    Mock<IInterface11>()
        .Setup(fun mock -> <@ mock.Arity2(any(), 1) @>).Returns(true)
        .Create()
        .Arity2(any(), 1)
    |> Assert.IsTrue

type IInterface21 =
    abstract Arity3<'a,'b> : 'a * 'b * int -> bool 

let [<Test>] ``two generic arguments & one non-generic`` () =
    Mock<IInterface21>()
        .Setup(fun mock -> <@ mock.Arity3(any(),any(),any()) @>).Returns(true)
        .Create()
        .Arity3(any(),any(),any())
    |> Assert.IsTrue

type IInterface<'a> =
    abstract Arity1 : 'a -> bool

let [<Test>] ``generic interface`` () =
    Mock<IInterface<_>>()
        .Setup(fun mock -> <@ mock.Arity1(any()) @>).Returns(true)
        .Create()
        .Arity1(any())
    |> Assert.IsTrue

type IInterface' =
    abstract Arity0 : unit -> 'a

let [<Test>] ``generic return value`` () =
    Mock<IInterface'>()
        .Setup(fun mock -> <@ mock.Arity0() @>).Returns(any())
        .Create()
        .Arity0()
    |> ignore
