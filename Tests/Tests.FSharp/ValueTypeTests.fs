module Foq.ValueTypeTests

open System
open Foq
open NUnit.Framework

type Record = { Name : string }

type Union = A | B

type IFoo =
    abstract ToTuple : unit -> int * int
    abstract ToRecord : unit -> Record 
    abstract ToUnion : unit -> Union
    abstract ToInt32 : unit -> Int32

[<Test>]
let ``default loose mock should throw on tuple return type`` () =
    let mock : IFoo = mock()
    Assert.Throws<NotImplementedException>(fun () ->
        mock.ToTuple() |> ignore
    ) |> ignore

[<Test>]
let ``default loose mock should throw on record return type`` () =
    let mock : IFoo = mock()
    Assert.Throws<NotImplementedException>(fun () ->
        mock.ToRecord() |> ignore
    ) |> ignore

[<Test>]
let ``default loose mock should throw on union return type`` () =
    let mock : IFoo = mock()
    Assert.Throws<NotImplementedException>(fun () ->
        mock.ToUnion() |> ignore
    ) |> ignore
    
[<Test>]
let ``default loose mock should return default value type`` () =            
    let mock : IFoo = mock()        
    Assert.AreEqual(Unchecked.defaultof<int>, mock.ToInt32())

[<Test>]
let ``default strict mock should throw on value return type`` () =    
    let lastMode = MockMode.Default
    try
        MockMode.Default <- MockMode.Strict
        let mock : IFoo = mock()
        Assert.Throws<NotImplementedException>(fun () ->
            mock.ToInt32() |> ignore
        ) |> ignore
    finally
        MockMode.Default <- lastMode