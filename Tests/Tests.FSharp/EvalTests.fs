module ``Eval Tests``

type PersonRecord = { Name:string; Age:int }

type PersonClass (name, age) =
    member person.Name = name
    member person.Age = age
    override a.GetHashCode() = hash a.Name ^^^ hash a.Age
    override a.Equals(b) = b |> function
        | :? PersonClass as b -> a.Name = b.Name && a.Age = b.Age
        | _ -> false

open Foq.Eval
open Microsoft.FSharp.Quotations
open NUnit.Framework

[<TestFixture>]
type Tests () =

    let localField = 1

    static member Expressions = 
        let (==) quote value = TestCaseData([|quote|]).Returns(value)
        [
        <@ 1 @> == 1
        <@ 1 + 1 @> == 2
        <@ 2 * 2 @> == 4
        <@ 9.9M @> == 9.9M
        <@ "Hello World" @> == "Hello World"
        <@ [1;2;3] @> == [1;2;3]
        <@ [|1;2;3|] @> == [|1;2;3|]
        <@ (1,2,3) @> == (1,2,3)
        <@ Some(1) @> == Some(1)
        <@ {Name="Phil"; Age=27} @> == {Name = "Phil"; Age=27}
        <@ PersonClass("Phil", 27) @> == PersonClass("Phil", 27)
        ]
    
    [<Test;TestCaseSource("Expressions")>]
    static member ``evaluates`` (expr:Expr) = eval [] expr

    member test.LocalProperty = 1

    [<Test>]
    member test.``local property getter`` () =
        Assert.AreEqual(test.LocalProperty, eval [] <@ test.LocalProperty @>)

    member test.LocalMethod() = 1

    [<Test>]
    member test.``local property method`` () =
        Assert.AreEqual(test.LocalMethod(), eval [] <@ test.LocalMethod() @>)

    static member GlobalProperty = 1

    [<Test>]
    member test.``global property getter`` () =
        Assert.AreEqual(Tests.GlobalProperty, eval [] <@ Tests.GlobalProperty @>)

    static member GlobalMethod() = 1

    [<Test>]
    member test.``global property method`` () =
        Assert.AreEqual(Tests.GlobalMethod(), eval [] <@ Tests.GlobalMethod() @>)

    [<Test>]
    member test.``local field`` () =
        Assert.AreEqual(localField, eval [] <@ localField @>)

    [<Test>]
    member test.``function application`` () =
        let f (x:int) = x
        Assert.AreEqual(1, eval [] <@ f 1 @>)

    [<Test>]
    member test.``function application * 2`` () =
        let add (a:int) (b:int) = a + b
        Assert.AreEqual(3, eval [] <@ add 1 2 @>)

    [<Test>]
    member test.``function application * 3`` () =
        let add (a:int) (b:int) (c:int) = (a + b + c)
        Assert.AreEqual(6, eval [] <@ add 1 2 3 @>)