#r @"..\packages\FSPowerPack.Linq.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.Linq.dll"
#r @"..\packages\Foq.0.7\Lib\net45\Foq.dll"

open Foq

module Assert =
    let inline IsTrue(success) = if not success then failwith "Expected true"
    let inline AreEqual(expected, actual) =
        if not (expected = actual) then 
            sprintf "Expected '%A' Actual '%A'" expected actual |> failwith
    let inline Throws<'T when 'T :> exn> (f) =
        let fail () = failwith "Expected %s" typeof<'T>.Name
        try f (); fail () with :? 'T as e -> e | _ -> fail()

module ``Method Example`` =
    let instance =
        Mock<System.Collections.IList>()
            .Setup(fun x -> <@ x.Contains(any()) @>).Returns(true)
            .Create()
    Assert.IsTrue(instance.Contains("Anything"))

module ``Method Matching Example`` =
    let instance =
        Mock<System.Collections.IList>()
            .Setup(fun x -> <@ x.Contains(1) @>).Returns(true)
            .Setup(fun x -> <@ x.Contains(2) @>).Returns(false)
            .Create()
    Assert.AreEqual(true, instance.Contains(1))
    Assert.AreEqual(false,instance.Contains(2))

module ``Method Predicate Example`` =
    let instance =
        Mock<System.Collections.Generic.IList<int>>()
            .Setup(fun x -> <@ x.Remove(is(fun i -> i >= 0)) @>).Returns(true)
            .Setup(fun x -> <@ x.Remove(is(fun i -> i <  0)) @>).Raises<System.ArgumentOutOfRangeException>()
            .Create()
    Assert.AreEqual(true, instance.Remove(99))

module ``Property Get Example`` =
    let instance =
        Mock<System.Collections.IList>()
            .Setup(fun x -> <@ x.Count @>).Returns(1)
            .Create()
    Assert.AreEqual(1, instance.Count)

module ``Counting Property Get Example`` =
    let counter = ref 0
    let instance =
        Mock<System.Collections.IList>()
            .Setup(fun x -> <@ x.Count @>).Returns(fun () -> incr counter; !counter)
            .Create()
    Assert.AreEqual(1, instance.Count)
    Assert.AreEqual(2, instance.Count)

module ``Item Get Example`` =
    let instance =
        Mock<System.Collections.Generic.IList<double>>()
            .Setup(fun x -> <@ x.Item(0) @>).Returns(0.0)
            .Setup(fun x -> <@ x.Item(any()) @>).Returns(-1.0)
            .Create()
    Assert.AreEqual(0.0, instance.[0])
    Assert.AreEqual(-1., instance.[1])

module ``Item Set Example`` =
    let instance =
        Mock<System.Collections.IList>()
            .Setup(fun x -> <@ x.Item(any()) <- any()  @>).Raises<System.ApplicationException>()
            .Create()
    try instance.Item(-1) <- 0; false with :? System.ApplicationException -> true
    |> Assert.IsTrue

module ``Raise Example`` =
    let instance =
        Mock<System.IComparable>()
            .Setup(fun x -> <@ x.CompareTo(any()) @>).Raises<System.ApplicationException>()
            .Create()
    try instance.CompareTo(1) |> ignore; false with e -> true
    |> Assert.IsTrue

module ``Event Example`` =
    let event = Event<_,_>()
    let instance =
        Mock<System.ComponentModel.INotifyPropertyChanged>()
            .SetupEvent(fun x -> <@ x.PropertyChanged @>).Publishes(event.Publish)
            .Create()
    let triggered = ref false
    instance.PropertyChanged.Add(fun x -> triggered := true)
    event.Trigger(instance, System.ComponentModel.PropertyChangedEventArgs("X"))
    Assert.IsTrue(!triggered)

module ``Call Example`` =
    let mutable called = false
    let instance =
        Mock<System.Collections.Generic.IList<string>>()
            .Setup(fun x -> <@ x.Insert(any(), any()) @>)
                .Calls<int * string>(fun (index,item) -> called <- true)
            .Create()
    instance.Insert(6, "Six")
    Assert.IsTrue(called)

module ``Verify Example`` =
    open System.Collections.Generic    
    let xs = Mock<IList<int>>.With(fun xs -> <@ xs.Contains(any()) --> true @>)    
    let _ = xs.Contains(1)    
    Mock.Verify(<@ xs.Contains(0) @>, never)
    Mock.Verify(<@ xs.Contains(any()) @>, once)

module ``Calculator Example`` =
    type ICalculator =
        abstract Push : int -> unit
        abstract Sum : unit -> unit
        abstract Total : int

    let instance = 
        Mock<ICalculator>()
            .Setup(fun x -> <@ x.Push(any()) @>).Returns(())
            .Setup(fun x -> <@ x.Total @>).Returns(2)
            .Create()
    let returnValue = instance.Push(2)
    Assert.AreEqual(2, instance.Total)

module ``IList example`` =
    open System.Collections.Generic
    let xs =
        Mock<IList<char>>.With(fun xs ->
            <@ xs.Count --> 2 
               xs.Item(0) --> '0'
               xs.Item(1) --> '1'
               xs.Contains(any()) --> true
               xs.RemoveAt(2) ==> System.ArgumentOutOfRangeException()
            @>
        )
    Assert.AreEqual(2, xs.Count)
    Assert.AreEqual('0', xs.Item(0))
    Assert.AreEqual('1', xs.Item(1))
    Assert.IsTrue(xs.Contains('0'))
    Assert.Throws<System.ArgumentOutOfRangeException>(fun () ->
        xs.RemoveAt(2)
    ) |> ignore