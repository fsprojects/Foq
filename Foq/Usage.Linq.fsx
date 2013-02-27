#load "Foq.fs"
#load "Foq.Linq.fs"

open Foq.Linq

let inline Assert success = if not success then failwith "Failed"

module ``Method Example`` =
    let instance =
        Mock<System.Collections.IList>()
            .SetupFunc(fun x -> x.Contains(It.IsAny())).Returns(true)
            .Create()
    Assert(instance.Contains("Anything"))

module ``Method Matching Example`` =
    let instance =
        Mock<System.Collections.IList>()
            .SetupFunc(fun x -> x.Contains(1)).Returns(true)
            .SetupFunc(fun x -> x.Contains(2)).Returns(false)
            .Create()
    Assert(instance.Contains(1) = true)
    Assert(instance.Contains(2) = false)

module ``Property Get Example`` =
    let instance =
        Mock<System.Collections.IList>()
            .SetupPropertyGet(fun x -> x.Count).Returns(1)
            .Create()
    Assert(instance.Count = 1)

module ``Counting Property Get Example`` =
    let counter = ref 0
    let instance =
        Mock<System.Collections.IList>()
            .SetupPropertyGet(fun x -> x.Count).Returns(fun () -> incr counter; !counter)
            .Create()
    Assert(instance.Count = 1)
    Assert(instance.Count = 2)

module ``Item Get Example`` =
    let instance =
        Mock<System.Collections.Generic.IList<double>>()
            .SetupPropertyGet(fun x -> x.Item(0)).Returns(0.0)
            .SetupPropertyGet(fun x -> x.Item(It.IsAny())).Returns(-1.0)
            .Create()
    Assert(instance.[0] = 0.0)
    Assert(instance.[1] = -1.)

module ``Item Set Example`` =
    let instance =
        Mock<System.Collections.IList>()
            .SetupPropertySet(fun x -> x.Item(It.IsAny())).Raises<System.ApplicationException>()
            .Create()
    try instance.Item(-1) <- 0; false with :? System.ApplicationException -> true
    |> Assert

module ``Raise Example`` =
    let instance =
        Mock<System.IComparable>()
            .SetupFunc(fun x -> x.CompareTo(It.IsAny())).Raises<System.ApplicationException>()
            .Create()
    try instance.CompareTo(1) |> ignore; false with e -> true
    |> Assert

module ``Event Example`` =
    open System.ComponentModel
    let event = Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()
    let instance =
        Mock<INotifyPropertyChanged>()
            .SetupEvent("PropertyChanged").Publishes(event.Publish)
            .Create()
    let triggered = ref false
    instance.PropertyChanged.Add(fun x -> triggered := true)
    event.Trigger(instance, PropertyChangedEventArgs("X"))
    Assert(!triggered)

module ``Calculator Example`` =
    type ICalculator =
        abstract Push : int -> unit
        abstract Sum : unit -> unit
        abstract Total : int

    let mock = 
        Mock<ICalculator>()
            .SetupPropertyGet(fun x -> x.Total).Returns(2)
    let instance = mock.Create()
    let returnValue = instance.Push(2)
    Assert(instance.Total = 2)