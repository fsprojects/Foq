#r "..\packages\FSPowerPack.Linq.Community.2.0.0.1\Lib\Net40\FSharp.PowerPack.Linq.dll"
#load "foq.fs"

open Foq

let inline Assert success = if not success then failwith "Failed"

module ``Method Example`` =
    let instance =
        Mock<System.Collections.IList>()
            .Setup(fun x -> <@ x.Contains(any()) @>).Returns(true)
            .Create()
    Assert(instance.Contains("Anything"))

module ``Method Matching Example`` =
    let instance =
        Mock<System.Collections.IList>()
            .Setup(fun x -> <@ x.Contains(1) @>).Returns(true)
            .Setup(fun x -> <@ x.Contains(2) @>).Returns(false)
            .Create()
    Assert(instance.Contains(1) = true)
    Assert(instance.Contains(2) = false)

module ``Method Predicate Example`` =
    let instance =
        Mock<System.Collections.Generic.IList<int>>()
            .Setup(fun x -> <@ x.Remove(is(fun i -> i >= 0)) @>).Returns(true)
            .Setup(fun x -> <@ x.Remove(is(fun i -> i <  0)) @>).Raises<System.ArgumentOutOfRangeException>()
            .Create()
    Assert(instance.Remove(99))

module ``Property Get Example`` =
    let instance =
        Mock<System.Collections.IList>()
            .Setup(fun x -> <@ x.Count @>).Returns(1)
            .Create()
    Assert(instance.Count = 1)

module ``Counting Property Get Example`` =
    let counter = ref 0
    let instance =
        Mock<System.Collections.IList>()
            .Setup(fun x -> <@ x.Count @>).Returns(fun () -> incr counter; !counter)
            .Create()
    Assert(instance.Count = 1)
    Assert(instance.Count = 2)

module ``Item Get Example`` =
    let instance =
        Mock<System.Collections.Generic.IList<double>>()
            .Setup(fun x -> <@ x.Item(0) @>).Returns(0.0)
            .Setup(fun x -> <@ x.Item(any()) @>).Returns(-1.0)
            .Create()
    Assert(instance.[0] = 0.0)
    Assert(instance.[1] = -1.)

module ``Item Set Example`` =
    let instance =
        Mock<System.Collections.IList>()
            .Setup(fun x -> <@ x.Item(any()) <- any()  @>).Raises<System.ApplicationException>()
            .Create()
    try instance.Item(-1) <- 0; false with :? System.ApplicationException -> true
    |> Assert

module ``Raise Example`` =
    let instance =
        Mock<System.IComparable>()
            .Setup(fun x -> <@ x.CompareTo(any()) @>).Raises<System.ApplicationException>()
            .Create()
    try instance.CompareTo(1) |> ignore; false with e -> true
    |> Assert

module ``Event Example`` =
    let event = Event<_,_>()
    let instance =
        Mock<System.ComponentModel.INotifyPropertyChanged>()
            .SetupEvent(fun x -> <@ x.PropertyChanged @>).Publishes(event.Publish)
            .Create()
    let triggered = ref false
    instance.PropertyChanged.Add(fun x -> triggered := true)
    event.Trigger(instance, System.ComponentModel.PropertyChangedEventArgs("X"))
    Assert(!triggered)

module ``Call Example`` =
    let mutable called = false
    let instance =
        Mock<System.Collections.Generic.IList<string>>()
            .Setup(fun x -> <@ x.Insert(any(), any()) @>)
                .Calls<int * string>(fun (index,item) -> called <- true)
            .Create()
    instance.Insert(6, "Six")
    Assert(called)

module ``Calculator Example`` =
    type ICalculator =
        abstract Push : int -> unit
        abstract Sum : unit -> unit
        abstract Total : int

    let Mock = 
        Mock<ICalculator>()
            .Setup(fun x -> <@ x.Push(any()) @>).Returns(())
            .Setup(fun x -> <@ x.Total @>).Returns(2)
    let instance = Mock.Create()
    let returnValue = instance.Push(2)
    Assert(instance.Total = 2)