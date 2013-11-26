/// User example based on the code samples in Mark Seeman's article -
/// Mocks for Commands, Stubs for Queries:
/// http://blog.ploeh.dk/2013/10/23/mocks-for-commands-stubs-for-queries/
module ``User Example``

type User = { Id : int }

type IUserRepository =
    abstract Read : userId:int -> User
    abstract Create : userId:int -> unit
 
type SomeController(users:IUserRepository) =
    member this.GetUser(userId:int) = 
        let user = users.Read(userId)
        if user.Id = 0 then users.Create(1234)
        user

open NUnit.Framework
open Foq

[<TestCase(1234)>]
[<TestCase(9766)>]
let ``GetUser returns correct value`` (userId:int) =
    let expected = { User.Id = userId }
    let behaviour (users:IUserRepository) = <@ users.Read(userId) --> expected @>
    let sut = SomeController(Mock.With(behaviour))

    let actual = sut.GetUser(userId)
    
    Assert.AreEqual(expected, actual)