module Foq.Tests

open Foq
open NUnit.Framework

type IInterface =
    abstract MethodReturnsSomething : unit -> int
    abstract MethodReturnsNothing : unit -> unit
    abstract MethodReturnsOption: unit -> int option
    abstract Arity1Method : int -> bool
    abstract Arity1MethodReturnsNothing : int -> unit
    abstract Arity2Method : int * string -> bool
    abstract Arity2MethodReturnsNothing : int * string -> unit
    abstract Arity3Method : int * string * float -> bool
    abstract Arity3MethodReturnsNothing : int * string * float -> unit
    abstract StringProperty : string

[<Test>]
let ``an interface method that is not implemented should return the default value`` () =
    let stub = Mock<IInterface>().Create()
    Assert.AreEqual(Unchecked.defaultof<int>, stub.MethodReturnsSomething())

[<Test>]
let ``an interface method that is not implemented and returns nothing should not throw`` () =
    let stub = Mock<IInterface>().Create()
    Assert.DoesNotThrow( fun () -> stub.MethodReturnsNothing() )

[<Test>]
let ``an implemented interface method should return the specified value`` () =
    let stub = 
        Mock<IInterface>()
            .Setup(fun x -> <@ x.MethodReturnsSomething() @>).Returns(2)
            .Create()
    let returnValue = stub.MethodReturnsSomething()
    Assert.AreEqual(returnValue,2)

[<Test>]
let ``an implemented interface method should return a specified option value of None`` () =
    let stub = 
        Mock<IInterface>()
            .Setup(fun x -> <@ x.MethodReturnsOption() @>).Returns(None)
            .Create()
    let returnValue = stub.MethodReturnsOption()
    Assert.IsTrue(returnValue.IsNone)

[<Test>]
let ``an implemented interface method should return a specified option value of Some`` () =
    let stub = 
        Mock<IInterface>()
            .Setup(fun x -> <@ x.MethodReturnsOption() @>).Returns(Some 1)
            .Create()
    let returnValue = stub.MethodReturnsOption()
    Assert.AreEqual(returnValue, Some 1)

[<Test>]
let ``an implemented interface property getter should return the specified value`` () =
    let stub = 
        Mock<IInterface>()
            .Setup(fun x -> <@ x.StringProperty @>).Returns("Fock")
            .Create()
    let returnValue = stub.StringProperty
    Assert.AreEqual(returnValue,"Fock")

[<Test>]
let ``an implemented interface property getter should return the specified computed value`` () =
    let counter = 1
    let stub = 
        Mock<System.Collections.IList>()
            .Setup(fun x -> <@ x.Count @>).Returns(fun () -> counter)
            .Create()
    let returnValue = stub.Count
    Assert.AreEqual(returnValue, 1)

[<Test>]
let ``an implemented interface method with arity/1 should accept any arguments`` 
    ([<Values(-1,0,9)>] n) =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity1Method(any()) @>).Returns(true)
            .Create()
    Assert.AreEqual(true, stub.Arity1Method(n))

[<Test>]
let ``an implemented interface method with arity/1 which returns nothing should not throw`` 
    ([<Values(-1,0,9)>] n) =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity1MethodReturnsNothing(any()) @>).Returns(())
            .Create()
    stub.Arity1MethodReturnsNothing(n)

[<Test>]
let ``an implemented composite interface method with arity/1 should accept any arguments`` 
    ([<Values("","NotEmpty")>] x) =
    let stub =
        Mock<System.Collections.IList>()
            .Setup(fun x -> <@ x.Contains(any()) @>).Returns(true)
            .Create()
    Assert.True(stub.Contains(x))

[<Test>]
let ``an implemented interface method with arity/2 should accept any arguments`` 
    ([<Values(9,0,-1)>] n) =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity2Method(any(),any()) @>).Returns(true)
            .Create()
    Assert.AreEqual(true, stub.Arity2Method(n,"string"))

[<Test>]
let ``an implemented interface method with arity/2 which returns nothing should not throw`` 
    ([<Values(9,0,-1)>] n) =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity2MethodReturnsNothing(any(),any()) @>).Returns(())
            .Create()
    stub.Arity2MethodReturnsNothing(n,"string")

[<Test>]
let ``reference type arguments should accept and match null`` () =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity2Method(any(), null) @>).Returns(true)
            .Create()
    Assert.AreEqual(true, stub.Arity2Method(1,null))

[<Test; Combinatorial>]
let ``an implemented interface method with arity/2 should be callable`` 
    ([<Values(9,0,-1)>] n,
     [<Values("","NotEmpty")>] s) =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity2Method(any(),any()) @>)
                .Calls<int * string>(fun (i,s) -> true)
            .Create()
    Assert.AreEqual(true, stub.Arity2Method(n,s))

[<Test; Combinatorial>]
let ``an implemented interface method with arity/3 should be callable`` 
    ([<Values(9,0,-1)>] n,
     [<Values("","NotEmpty")>] s,
     [<Values(System.Double.NegativeInfinity,0,System.Double.MaxValue)>] d) =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity3Method(any(),any(),any()) @>)
                .Calls<int * string * float>(fun (i,s,d) -> true)
            .Create()
    Assert.AreEqual(true, stub.Arity3Method(n,s,d))

[<Test; Combinatorial>]
let ``an implemented interface method with arity/3 should match specified arguments`` 
    ([<Values(9,0,-1)>] n,
     [<Values("","NotEmpty")>] s,
     [<Values(System.Double.NegativeInfinity,0,System.Double.MaxValue)>] d) =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity3Method(n,s,d) @>).Returns(true)
            .Create()
    Assert.AreEqual(true, stub.Arity3Method(n,s,d))

[<Test; Combinatorial>]
let ``an implemented interface method with arity/3 should match specified argument predicates`` 
    ([<Values(9,0,-1)>] n,
     [<Values("","NotEmpty")>] s,
     [<Values(System.Double.NegativeInfinity,0,System.Double.MaxValue)>] d) =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity3Method(is((=) n),is((=) s),is((=) d)) @>).Returns(true)
            .Create()
    Assert.AreEqual(true, stub.Arity3Method(n,s,d))

[<Test>]
let ``an implemented interface method should match a predicate with named arguments`` () =
    let instance =
        Mock<System.Collections.Generic.IList<int>>()
            .Setup(fun x -> <@ x.Remove(is(fun i -> i >= 0)) @>).Returns(true)
            .Create()
    Assert.AreEqual(true, instance.Remove(99))

[<Test; Combinatorial>]
let ``an implemented interface method with arity/3 should match correct method pattern`` 
    ([<Values(9,0,-1)>] n,
     [<Values("","NotEmpty")>] s,
     [<Values(System.Double.NegativeInfinity,0,System.Double.MaxValue)>] d) =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.Arity3Method(is((<>) n),is((<>) s),is((<>) d)) @>).Returns(false)
            .Setup(fun x -> <@ x.Arity3Method(any(),any(),any()) @>).Returns(true)
            .Create()
    Assert.AreEqual(true, stub.Arity3Method(n,s,d))

[<Test>]
let ``an implemented interface method can raise a specified exception type`` () =
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.MethodReturnsNothing() @>).Raises<System.ApplicationException>()
            .Create()
    Assert.Throws<System.ApplicationException>( fun () -> 
        stub.MethodReturnsNothing()) |> ignore

[<Test>]
let ``an implemented interface method can raise a specified exception value`` () =
    let message = "Message"
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.MethodReturnsNothing() @>).Raises(System.ApplicationException(message))
            .Create()
    Assert.Throws<System.ApplicationException>((fun () -> 
        stub.MethodReturnsNothing()), message) |> ignore

[<Test>]
let ``an implemented interface returning method can raise a specified exception value`` () =
    let message = "Message"
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.MethodReturnsSomething() @>).Raises(System.ApplicationException(message))
            .Create()
    Assert.Throws<System.ApplicationException>((fun () -> 
        stub.MethodReturnsSomething() |> ignore), message) |> ignore

[<Test>]
let ``an implemented interface property can raise a specified exception value`` () =
    let message = "Message"
    let stub =
        Mock<IInterface>()
            .Setup(fun x -> <@ x.StringProperty @>).Raises(System.ApplicationException(message))
            .Create()
    Assert.Throws<System.ApplicationException>((fun () -> 
        stub.StringProperty |> ignore), message) |> ignore

[<AbstractClass>]
type Shape2D(x0 : float, y0 : float) =
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0

    // These properties are not declared abstract. They 
    // cannot be overriden. 
    member this.CenterX with get() = x and set xval = x <- xval
    member this.CenterY with get() = y and set yval = y <- yval

    // These properties are abstract, and no default implementation 
    // is provided. Non-abstract derived classes must implement these. abstract Area : floatwith get
    abstract Perimeter : float with get
    abstract Name : string with get

    // This method is not declared abstract. It cannot be 
    // overriden. 
    member this.Move dx dy =
       x <- x + dx
       y <- y + dy

    // An abstract method that is given a default implementation 
    // is equivalent to a virtual method in other .NET languages. 
    // Rotate changes the internal angle of rotation of the square. 
    // Angle is assumed to be in degrees. 
    abstract member Rotate: float -> unit
    default this.Rotate(angle) = rotAngle <- rotAngle + angle

[<Test>]
let ``an implemented abstract class property should return the specified value`` () =
    let stub =
        Mock<Shape2D>()
            .Setup(fun x -> <@ x.Name @>).Returns("Name")
            .Create()
    Assert.AreEqual("Name", stub.Name)

[<AbstractClass>]
type AbstractBaseClass() =
   // abstract method
   abstract member Add: int * int -> int

   // abstract immutable property
   abstract member Pi : float 

   // abstract read/write property
   abstract member Area : float with get,set

[<Test>]
let ``an implemented abstract base class method should return the specified value`` () =
    let stub =
        Mock<AbstractBaseClass>()
            .Setup(fun x -> <@ x.Add(any(), any()) @>).Returns(2)
            .Create()
    Assert.AreEqual(stub.Add(1,1), 2)

[<Test>]
let ``an implemented abstract base class property should return the specified value`` () =
    let stub =
        Mock<AbstractBaseClass>()
            .Setup(fun x -> <@ x.Pi @>).Returns(4.0)
            .Create()
    Assert.AreEqual(stub.Pi, 4.0)

[<Test>]
let ``an implemented abstract base class property setter should accept the specified value`` () =
    let specifiedValue = ref None
    let stub =
        Mock<AbstractBaseClass>()
            .Setup(fun x -> <@ x.Area <- any() @>).Calls<float>(fun x -> specifiedValue := Some x)
            .Create()
    let area = 16.0
    stub.Area <- area
    Assert.AreEqual(!specifiedValue, Some(area))

open System.ComponentModel

let [<Test>] ``an implemented interface event can add handlers`` () =
    let event = Event<_,_>()
    let instance =
        Mock<System.ComponentModel.INotifyPropertyChanged>()
            .SetupEvent(fun x -> <@ x.PropertyChanged @>).Publishes(event.Publish) 
            .Create()
    let triggered = ref false
    instance.PropertyChanged.Add(fun x -> triggered := true)
    event.Trigger(instance, PropertyChangedEventArgs("X"))
    Assert.IsTrue(!triggered)

let [<Test>] ``an implemented interface event can add/remove handlers`` () =
    let event = Event<_,_>()
    let instance =
        Mock<System.ComponentModel.INotifyPropertyChanged>()
            .SetupEvent(fun x -> <@ x.PropertyChanged @>).Publishes(event.Publish) 
            .Create()
    let triggered = ref false
    let setTriggered s e = triggered := true
    let handler = PropertyChangedEventHandler(setTriggered)
    instance.PropertyChanged.AddHandler(handler)
    instance.PropertyChanged.RemoveHandler(handler)
    event.Trigger(instance, PropertyChangedEventArgs("X"))
    Assert.IsFalse(!triggered)

[<TestFixture>]
type ``test members`` () =
    let n = 1
    [<Test>]
    member __.``test field argument`` () =
        let mock =
            Mock<IInterface>()
                .Setup(fun x -> <@ x.Arity1Method(n) @>).Returns(true)
                .Create()
        Assert.IsTrue(mock.Arity1Method(n))
    member __.N = n
    [<Test>]
    member this.``test property argument`` () =
        let mock =
            Mock<IInterface>()
                .Setup(fun x -> <@ x.Arity1Method(this.N) @>).Returns(true)
                .Create()
        Assert.IsTrue(mock.Arity1Method(this.N))

open System.Collections.Generic

[<Test>]
let ``test strict mode`` () =
    let mock = Mock<IList<int>>(MockMode.Strict).Create()
    Assert.Throws<System.NotImplementedException>(fun() -> ignore mock.Count)
    |> ignore

[<Test>]
let ``test loose mode`` () =
    let mock = Mock<IList<int>>(MockMode.Loose).Create()
    Assert.AreEqual(Unchecked.defaultof<int>, mock.Count)