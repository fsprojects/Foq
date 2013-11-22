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