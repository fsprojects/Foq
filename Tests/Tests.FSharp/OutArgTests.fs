module OutArgTests

open NUnit.Framework
open Foq

let [<Test>] ``one out arg no result`` () =
    let mock = Mock<IHaveOutArgs>().SetupByName("NoInOneOutNoResult").Returns(1).Create()
    Assert.AreEqual(1, mock.NoInOneOutNoResult())

let [<Test>] ``one out arg result`` () =
    let mock = Mock<IHaveOutArgs>().SetupByName("NoInOneOutResult").Returns((true,2)).Create()
    let success, n = mock.NoInOneOutResult()
    Assert.IsTrue(success)
    Assert.AreEqual(2, n)

let [<Test>] ``one in one out arg result`` () =
    let mock = Mock<IHaveOutArgs>().SetupByName("OneInOneOutResult").Returns((true,3)).Create()
    let success, n = mock.OneInOneOutResult(1)
    Assert.IsTrue(success)
    Assert.AreEqual(3, n)

let [<Test>] ``two out args no result`` () =
    let mock = Mock<IHaveOutArgs>().SetupByName("NoInTwoOutNoResult").Returns((1,2)).Create()
    let x, y = mock.NoInTwoOutNoResult()
    Assert.AreEqual(1, x)
    Assert.AreEqual(2, y)

let [<Test>] ``three out args result`` () =
    let mock = Mock<IHaveOutArgs>().SetupByName("NoInThreeOutResult").ReturnsFunc(fun () -> (true,1,2,3)).Create()
    let success, x, y, z = mock.NoInThreeOutResult()
    Assert.AreEqual(1, x)
    Assert.AreEqual(2, y)
    Assert.AreEqual(3, z)

let [<Test>] ``one out arg no result via function`` () =
    let mock = Mock<IHaveOutArgs>().SetupByName("NoInOneOutNoResult").ReturnsFunc(fun () -> 1).Create()
    Assert.AreEqual(1, mock.NoInOneOutNoResult())

let [<Test>] ``one out arg result  via function`` () =
    let mock = Mock<IHaveOutArgs>().SetupByName("NoInOneOutResult").ReturnsFunc(fun () -> (true,2)).Create()
    let success, n = mock.NoInOneOutResult()
    Assert.IsTrue(success)
    Assert.AreEqual(2, n)

let [<Test>] ``one in one out arg result  via function`` () =
    let mock = Mock<IHaveOutArgs>().SetupByName("OneInOneOutResult").ReturnsFunc(fun () -> (true,3)).Create()
    let success, n = mock.OneInOneOutResult(1)
    Assert.IsTrue(success)
    Assert.AreEqual(3, n)

let [<Test>] ``two out args no result  via function`` () =
    let mock = Mock<IHaveOutArgs>().SetupByName("NoInTwoOutNoResult").Returns((1,2)).Create()
    let x, y = mock.NoInTwoOutNoResult()
    Assert.AreEqual(1, x)
    Assert.AreEqual(2, y)

let [<Test>] ``three out args result via function`` () =
    let mock = Mock<IHaveOutArgs>().SetupByName("NoInThreeOutResult").Returns((true,1,2,3)).Create()
    let success, x, y, z = mock.NoInThreeOutResult()
    Assert.AreEqual(1, x)
    Assert.AreEqual(2, y)
    Assert.AreEqual(3, z)
