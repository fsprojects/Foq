// Based on code samples in Joshua Flanagan's article
// Auto mocking Explained
// http://lostechies.com/joshuaflanagan/2009/02/04/auto-mocking-explained/
module PhotoExample

type PhotoDetails (n) = class end

type IPhotoCatalog =
    abstract FindAll : unit -> PhotoDetails[]
    abstract AddPhoto : PhotoDetails -> unit

type PhotoController(photoCatalog:IPhotoCatalog) =
    member this.List() = photoCatalog.FindAll()
    member this.Save(photo:PhotoDetails) = photoCatalog.AddPhoto(photo)

open Foq
open NUnit.Framework

[<TestFixture>]
type when_visiting_the_list_page () =    
    let mutable Output = [||]
    let mutable AllPhotos = [||]
 
    [<SetUp>]
    member test.Setup() =    
        AllPhotos <- [|PhotoDetails(1); PhotoDetails(2) |]
        let photoCatalog = 
            Mock<IPhotoCatalog>
                .Method(fun c -> <@ c.FindAll @>)
                .Returns(AllPhotos)
        let controller = new PhotoController(photoCatalog);
        Output <- controller.List()
   
    [<Test>]
    member test.should_display_all_of_the_photos_in_the_catalog() =    
        Assert.AreEqual(AllPhotos, Output)

[<TestFixture>]
type when_saving_a_photo () =

    let mutable thePhoto = Unchecked.defaultof<PhotoDetails>
    let mutable photoCatalog = Unchecked.defaultof<IPhotoCatalog>
 
    [<SetUp>]
    member test.Setup() =    
        photoCatalog <- Mock.Of<IPhotoCatalog>()
        let controller = PhotoController(photoCatalog)
 
        thePhoto <- PhotoDetails(4)
        controller.Save(thePhoto)
 
    [<Test>]
    member test.should_store_the_photo_details_in_the_catalog() =
        verify <@ photoCatalog.AddPhoto(thePhoto) @> once
    