module Foq.PartialApplicationTests

open System
open Foq
open NUnit.Framework

type IDummy = 
    abstract member F1 : Int32 -> bool
    abstract member F2 : Int32 -> Int32 -> bool

module WithLiterals = 

    [<Test>]
    let ``f x`` () = 
        let x = Mock<IDummy>().Setup(fun dummy -> <@ dummy.F1 1 @>).Returns(true).Create()
        Assert.IsTrue (x.F1 1)

    [<Test>]
    let ``f x y`` () = 
        let x = Mock<IDummy>().Setup(fun dummy -> <@ dummy.F2 1 2 @>).Returns(true).Create()
        Assert.IsTrue (x.F2 1 2)

module WithAny = 

    [<TestCase (0)>]
    [<TestCase (10)>]
    [<TestCase (12345)>]
    let ``f (any ())`` arg = 
        let x = Mock<IDummy>().Setup(fun dummy -> <@ dummy.F1 (any ()) @>).Returns(true).Create()
        Assert.IsTrue (x.F1 arg)

    [<TestCase (0, 12345)>]
    [<TestCase (10, 10)>]
    [<TestCase (12345, 0)>]
    let ``f (any ()) (any ())`` arg0 arg1  =
        let x = Mock<IDummy>().Setup(fun dummy -> <@ dummy.F2 (any ()) (any ()) @>).Returns(true).Create()
        Assert.IsTrue (x.F2 arg0 arg1)

module WithIs = 

    [<TestCase (5)>]
    [<TestCase (10)>]
    [<TestCase (12345)>]
    let ``f (is (>=5))`` arg = 
        let x = Mock<IDummy>().Setup(fun dummy -> <@ dummy.F1 (is (fun n -> n >= 5)) @>).Returns(true).Create()
        Assert.IsTrue (x.F1 arg)

    [<TestCase (5, 5)>]
    [<TestCase (10, 3)>]
    [<TestCase (12345, 0)>]    
    let ``f (is (>=5)) (is (<= 5))`` arg0 arg1 = 
        let x = Mock<IDummy>().Setup(fun dummy -> <@ dummy.F2 (is (fun n -> n >= 5)) (is (fun n -> n <= 5)) @>).Returns(true).Create()
        Assert.IsTrue (x.F2 arg0 arg1)