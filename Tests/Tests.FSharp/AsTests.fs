module AsTests

open Foq
open NUnit.Framework

type IFoo = 
    abstract Foo : unit -> int

type IBar =
    abstract Bar : unit -> int

[<Test>]
let ``can mock multiple interface types using setup`` () =
    let x = 
        Mock<IFoo>().Setup(fun x -> <@ x.Foo() @>).Returns(2)
         .As<IBar>().Setup(fun x -> <@ x.Bar() @>).Returns(1)
         .Create()    
    Assert.AreEqual(1, x.Bar())
    Assert.AreEqual(2, (x :?> IFoo).Foo())

[<Test>]
let ``can mock multiple interface types without initial setup`` () =
    let x = 
        Mock<IFoo>()
         .As<IBar>().Setup(fun x -> <@ x.Bar() @>).Returns(1)
         .Create()    
    Assert.AreEqual(1, x.Bar())
    Assert.AreEqual(0, (x :?> IFoo).Foo())

[<Test>]
let ``can mock multiple interface types without any setup`` () =
    let x = 
        Mock<IFoo>()
         .As<IBar>()
         .Create()    
    Assert.AreEqual(0, x.Bar())
    Assert.AreEqual(0, (x :?> IFoo).Foo())

[<Test>]
let ``can handle inherited interfaces direct`` () =
    let xs =
        Mock<System.Collections.IEnumerable>()
            .As<System.Collections.IList>()
            .Create()
    Assert.AreEqual(0, xs.Count)

[<Test>]
let ``can handle inherited interfaces via cast`` () =
    let xs =
        Mock<System.Collections.IList>()
            .As<System.Collections.IEnumerable>()
            .Create()
    Assert.AreEqual(0, (xs :?> System.Collections.IList).Count)