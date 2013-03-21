/// Book example based on the code in Ben Hall's article -
/// Beginning to Mock with Rhino Mocks and MbUnit - Part 2 
/// http://aspalliance.com/1456_Beginning_to_Mock_with_Rhino_Mocks_and_MbUnit__Part_2.all
module ``Book Example``

type Book(id:int, title:string, price:double) =
    member book.ID =id
    member book.Title = title
    member book.Price = price

type IBooksDataAccess =
    abstract GetAllBooks : unit -> Book[]

type BookService(dal:IBooksDataAccess) =
    member service.GetAllBooks() = dal.GetAllBooks()
    member service.GetBooksWithPriceBelow(price) =
        [|for book in dal.GetAllBooks() do if book.Price < price then yield book|]  

open NUnit.Framework
open Foq

let [<Test>] ``get books below 10`` () =
    let expectedBook = Book(1, "BeginningtoMock Part 2", 1.00)

    let dal =
        Mock<IBooksDataAccess>
            .Method(fun mock -> <@ mock.GetAllBooks @> ).Returns([|expectedBook|])

    let bookService = BookService(dal)
    let books = bookService.GetBooksWithPriceBelow(10.0)
    Assert.AreEqual(1, books.Length)
    Assert.AreEqual(expectedBook, books.[0])

let [<Test>] ``get books below 10 doesn't return any above`` () =
    let notExpected = Book(2, "BeginningtoMock Part 2", 11.0)
    
    let dal =
        Mock<IBooksDataAccess>
            .Method(fun mock -> <@ mock.GetAllBooks @>).Returns([|notExpected|])

    let bookService = BookService(dal)
    let books = bookService.GetBooksWithPriceBelow(10.0)
    Assert.IsEmpty(books)

type CannotConnectToDatabaseException () = inherit System.Exception()

[<ExpectedException(typeof<CannotConnectToDatabaseException>)>]
let [<Test>] ``book service handles database exception correctly`` () =
    let dal =
        Mock<IBooksDataAccess>()
            .Setup(fun mock -> <@ mock.GetAllBooks() @>)
                .Raises<CannotConnectToDatabaseException>()
            .Create()
    let bookService = BookService(dal)
    bookService.GetAllBooks() |> ignore

