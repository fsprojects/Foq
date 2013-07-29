/// Based on FakeItEasy's Candy shop example: https://github.com/FakeItEasy/FakeItEasy
module CandyShop

type ICandy = interface end
type IShop =
    abstract GetTopSellingCandy : unit -> ICandy
    abstract BuyCandy : ICandy -> unit 
type SweetTooth() =
    member x.BuyTastiestCandy(shop:IShop) = ()

open Foq
open NUnit.Framework

let [<Test>] ``It's faking amazing!`` () =
    let lollipop = Mock.Of<ICandy>()

    let shop = Mock<IShop>.Method(fun shop -> 
        <@ shop.GetTopSellingCandy @>).Returns(lollipop)

    let developer = SweetTooth()
    developer.BuyTastiestCandy(shop)

    Mock.Expect(<@ shop.BuyCandy(lollipop) @>, once)