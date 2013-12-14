// Based on code samples from Alex Ford's article
// What is a mocking framework? Why is it useful?
// http://codetunnel.com/blog/post/what-is-a-mocking-framework-why-is-it-useful
module DriverExample

type IVehicle =
    /// <summary>Honks the vehicle's horn.</summary>
    /// <returns>True if the action was successful.</returns>
    abstract HonkHorn : unit -> bool
    ///<summary>Applies the vehicle's brakes.</summary>
    ///<returns>True if the action was successful.</returns>
    abstract ApplyBrakes : unit -> bool

type Driver(vehicleToDrive:IVehicle) = 
    member driver.EvasiveManeuvers(alertOffendingDriver) =           
        if alertOffendingDriver
        then vehicleToDrive.ApplyBrakes() && vehicleToDrive.HonkHorn()
        else vehicleToDrive.ApplyBrakes()

open Foq
open NUnit.Framework

let fake (vehicle:IVehicle) =
    <@ vehicle.HonkHorn() --> true
       vehicle.ApplyBrakes() --> true @>

[<Test>]
let Can_Evade_Trouble() =
    // Arrange (set up a scenario)
    let fakeVehicle = Mock.With(fake)

    let target = Driver(fakeVehicle)

    // Act (attempt the operation)
    let success = target.EvasiveManeuvers(false)

    // Assert (verify the result)
    Assert.IsTrue(success)
    verify <@ fakeVehicle.HonkHorn() @> never
    verify <@ fakeVehicle.ApplyBrakes() @> once

[<Test>]
let Can_Evade_Trouble_And_Alert_Offending_Driver() =
    // Arrange (set up a scenario)
    let fakeVehicle = Mock.With(fake)
    let target = new Driver(fakeVehicle)

    // Act (attempt the operation)
    let success = target.EvasiveManeuvers(true)

    // Assert (verify the result)
    Assert.IsTrue(success)
    verify <@ fakeVehicle.HonkHorn() @> once
    verify <@ fakeVehicle.ApplyBrakes() @> once