/// Turtle example based on ScalaTest's User Guide code samples in
/// http://www.scalatest.org/user_guide/testing_with_mock_objects
module ``Turtle Example``

type Turtle =
    abstract GetPosition : unit -> float * float
    abstract SetPosition : float * float -> unit
    abstract PenDown : unit -> unit
    abstract PenUp : unit -> unit
    abstract Forward : float -> unit
    abstract Turn : float -> unit

open NUnit.Framework
open Foq

[<Test>]
let ``turtle behaves as expected`` () =
    // Arrange
    let sequence (m1:Turtle) =
     <@ m1.SetPosition(0.0, 0.0)
        m1.PenDown()
        m1.Forward(10.0)
        m1.PenUp() @>
    let m1 = expectSeq sequence
    let sequence (m2:Turtle) =
     <@ m2.SetPosition(1.0, 1.0)
        m2.Turn(90.0)
        m2.Forward(1.0)
        m2.GetPosition() --> (2.0, 1.0) @>
    let m2 = expectSeq sequence
    // Act
    m2.SetPosition(1.0, 1.0)
    m1.SetPosition( 0.0, 0.0)
    m1.PenDown()
    m2.Turn(90.0)
    m1.Forward(10.0)
    m2.Forward(1.0)
    m1.PenUp()
    let position = m2.GetPosition()
    // Assert
    Assert.AreEqual( (2.0,1.0), position)
    verifyAll m1
    verifyAll m2



