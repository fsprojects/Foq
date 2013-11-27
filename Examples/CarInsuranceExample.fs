/// Based on code samples from Mocking and Isolation in Unit Testing article
/// http://www.typemock.com/Mocking_and_Isolation_in_Unit_Testing.php
module ``Car Insurance Example``

open System

type Customer =
    abstract DateOfBirth : DateTime with get, set

type DataLayer () =
    abstract CreateCustomer : string * string * DateTime -> int
    default layer.CreateCustomer(firstName,lastName,dateOfBirth) : int = 
        failwith "Unable to connect to database"
    abstract GetCustomer : int -> Customer
    default layer.GetCustomer(customerId:int) : Customer = 
        failwith "Unable to connect to database"
    abstract OpenConnection : unit -> unit
    default layer.OpenConnection() : unit =
        failwith "Unable to connect to database"
    abstract CloseConnection : unit -> unit
    default layer.CloseConnection() : unit =
        failwith "Unable to connect to database"

type PriceGroup = Child = 0 | Junior = 1 | Adult = 2 | Senior = 3

type CarInsurance (dataLayer:DataLayer) =
    member insurance.GetCustomerPriceGroup(customerID:int) =
        dataLayer.OpenConnection()
        let customer = dataLayer.GetCustomer(customerID)
        dataLayer.CloseConnection()
        let now = DateTime.Now
        if (customer.DateOfBirth > now.AddYears(-16))
        then PriceGroup.Child
        elif (customer.DateOfBirth > now.AddYears(-25))
        then PriceGroup.Junior
        elif (customer.DateOfBirth < now.AddYears(-65))
        then PriceGroup.Senior
        else PriceGroup.Adult

open NUnit.Framework
open Foq

[<Test>]
let GetCustomerPriceGroup_Adult () =
    let behaviour (customer:Customer) = <@ customer.DateOfBirth --> DateTime(1970, 1, 1, 0, 0, 0) @>
    let customer = Mock.With(behaviour)

    let behaviour (dataLayer:DataLayer) =
        <@ dataLayer.OpenConnection()
           dataLayer.GetCustomer(0) --> customer
           dataLayer.CloseConnection() @>
    let dataLayer = Mock.With(behaviour)

    let carInsurance = new CarInsurance(dataLayer)
    let priceGroup = carInsurance.GetCustomerPriceGroup(0); 
    Assert.AreEqual(PriceGroup.Adult, priceGroup, "Incorrect price group");
