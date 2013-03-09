/// Time example based on the code in Ben Hall's article -
/// Beginning to Mock with Rhino Mocks and MbUnit - Part 1: 
/// http://aspalliance.com/1400_Beginning_to_Mock_with_Rhino_Mocks_and_MbUnit__Part_1.all
module ``Time Example``

type ITime =
    abstract GetHour : unit -> int

type ImageCalculator (time:ITime) =
    member x.GetImageForTimeOfDay() =
        let hour = time.GetHour()
        if hour > 6 && hour < 21
        then "sun.jpg"
        else "moon.jpg"

open NUnit.Framework
open Foq

let [<Test>] ``at 15:00 the sun image should be expected`` () =
    let time = 
        Mock<ITime>()
            .Setup(fun mock -> <@ mock.GetHour() @>).Returns(15)
            .Create()
    let calculator = ImageCalculator(time)
    let image = calculator.GetImageForTimeOfDay()
    Assert.AreEqual("sun.jpg",  image)

let [<Test>] ``at 01:00 the moon image should be expected`` () =
    let time = Mock<ITime>.With(fun mock -> <@ mock.GetHour() --> 01 @>)
    let calculator = ImageCalculator(time)
    let image = calculator.GetImageForTimeOfDay()
    Assert.AreEqual("moon.jpg",  image)
