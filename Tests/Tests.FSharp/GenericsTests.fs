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

type ISettings =
    abstract Get : key:string -> 'b
    abstract Get : key:string * fallback:'a -> 'a 

let [<Test>] ``generic argument and return value`` () =
    let mock =
        Mock<ISettings>()
            .Setup(fun mock -> <@ mock.Get(any(),any()) @>).Calls<string*int>(fun (_,x) -> x)
            .Create()
    let expected = 1
    Assert.AreEqual(expected, mock.Get("Key", expected))

let [<Test>] ``generic argument and return value setup over multiple members`` () =
    let expected = 1.0
    let mock =
        Mock<ISettings>()
            .Setup(fun mock -> <@ mock.Get(any(),any()) @>).Returns(2)
            .Setup(fun mock -> <@ mock.Get(any(),any()) @>).Returns(expected)   
            .Create()    
    Assert.AreEqual(expected, mock.Get("Key", expected))

let [<Test>] ``generic argument and return value with multiple members`` () =
    let expected = 1.0
    let mock =
        Mock<ISettings>.With(fun mock ->
         <@ mock.Get(any(),any()) --> 2
            mock.Get(any(),any()) --> expected @>)    
    Assert.AreEqual(expected, mock.Get("Key", expected))

let [<Test>] ``generic method with multiple members`` () =
    let expected = 1.0
    let mock =
        Mock<ISettings>.With(fun mock ->
         <@ mock.Get(any()) --> 2
            mock.Get(any()) --> expected @>)    
    Assert.AreEqual(expected, mock.Get<float>("Key"))