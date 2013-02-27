namespace global

type TestAttribute() = inherit System.Attribute()

module Assert =
    let inline IsTrue(success) = if not success then failwith "Expected true"
    let inline AreEqual(expected, actual) =
        if not (expected = actual) then 
            sprintf "Expected '%A' Actual '%A'" expected actual |> failwith
    let inline Throws<'T when 'T :> exn> (f) =
        let fail () = failwith "Expected %s" typeof<'T>.Name
        try f (); fail () with :? 'T as e -> e | _ -> fail()