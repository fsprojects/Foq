module ``Abstract Class Tests``

open System
open NUnit.Framework
open Foq

[<AbstractClass>]
type MyAbstractBaseClass() =
    abstract MyMethod : int -> bool
    abstract MyProperty : int

[<Test>]
let ``can mock an abstract base class method`` () =
    let mock = 
        Mock<MyAbstractBaseClass>
            .Method(fun x -> <@ x.MyMethod @>).Returns(true)
    Assert.IsTrue(mock.MyMethod(1))

[<Test>]
let ``can mock an abstract base class property`` () =
    let mock = 
        Mock<MyAbstractBaseClass>
            .Property(fun x -> <@ x.MyProperty @>).Returns(1)
    Assert.AreEqual(1, mock.MyProperty)

[<AbstractClass>]
type MyAbstractClass () =
    abstract MyMethod : int -> bool
    member __.MyProperty = 1

[<Test>]
let ``can mock an abstract class method`` () =
    let mock = 
        Mock<MyAbstractClass>
            .Method(fun x -> <@ x.MyMethod @>).Returns(true)
    Assert.IsTrue(mock.MyMethod(1))

[<AbstractClass>]
type MyInheritedAbstractClass () =
    inherit MyAbstractBaseClass ()
    override __.MyProperty = 1

[<Test>]
let ``can mock an inherited abstract class method`` () =
    let mock = 
        Mock<MyInheritedAbstractClass>
            .Method(fun x -> <@ x.MyMethod @>).Returns(true)
    Assert.IsTrue(mock.MyMethod(1))



