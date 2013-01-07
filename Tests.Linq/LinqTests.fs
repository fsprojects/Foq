module Foq.Linq.Tests

open System
open System.Collections.Generic
open System.ComponentModel
open Foq.Linq
open NUnit.Framework

[<Test>]
let ``test func`` () =
    let mock =
        Mock<IList<int>>()
            .SetupFunc(fun x -> x.Contains(It.IsAny<int>())).Returns(true)
            .Create()
    Assert.IsTrue(mock.Contains(1))

[<Test>]
let ``test func predicate`` () =
    let mock =
        Mock<IList<int>>()
            .SetupFunc(fun x -> x.Contains(It.Is<int>(Func<int,bool>(fun key -> true)))).Returns(true)
            .Create()
    Assert.IsTrue(mock.Contains(1))
    
[<Test>]
let ``test action`` () =
    let mock =
        Mock<IList<int>>()
            .SetupAction(fun x -> x.Clear()).Raises<System.ApplicationException>()
            .Create();
    Assert.Throws<ApplicationException>(fun () ->
        mock.Clear()
    ) |> ignore

[<Test>]
let ``test property get`` () =
    let mock =
        Mock<IList<int>>()
            .SetupPropertyGet(fun x -> x.Count).Returns(1)
            .Create()
    Assert.AreEqual(1, mock.Count)

[<Test>]
let ``test property get with index parameters`` () =
    let index = 1
    let value = 9
    let mock =
        Mock<IList<int>>()
            .SetupPropertyGet(fun x -> x.[index]).Returns(value)
            .Create()
    Assert.AreEqual(value, mock.[index])
[<Test>]

let ``test property set`` () =
    let mock =
        Mock<IList<int>>()
            .SetupPropertySet(fun x -> x.[1]).Raises<System.ApplicationException>()
            .Create()
    Assert.Throws<ApplicationException>(fun () ->
        mock.[1] <- 1
    ) |> ignore    

[<Test>]
let ``test event`` () =
    let event = Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()
    let mock =
        Mock<INotifyPropertyChanged>()
            .SetupEvent("PropertyChanged").Publishes(event.Publish)
            .Create()       
    let triggered = ref false
    let name = "Name"
    mock.PropertyChanged.Add(fun e -> triggered := (e.PropertyName = name))
    event.Trigger(mock, PropertyChangedEventArgs(name))
    Assert.IsTrue(triggered.Value)