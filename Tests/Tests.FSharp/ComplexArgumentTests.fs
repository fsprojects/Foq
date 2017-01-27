module Foq.ComplexArgumentTests

open System
open Foq
open NUnit.Framework

type SimpleTinyType = SimpleTinyType of string

[<AutoOpen>]
module AutoOpenedModule =
    let testValue = SimpleTinyType "Dummy"

type IDummy =
    abstract member TinyType : SimpleTinyType -> unit
    abstract member IntArray : int array -> bool

[<Test>]
let ``can handle auto-opened module value`` () =
    let x = Mock<IDummy>().Create()
    x.TinyType testValue
    Mock.Verify <@ x.TinyType testValue @>

[<Test>]
let ``can handle discriminated union case`` () =
    let x = Mock<IDummy>().Create()
    x.TinyType (SimpleTinyType "Sample")
    Mock.Verify <@ x.TinyType (SimpleTinyType "Sample") @>
