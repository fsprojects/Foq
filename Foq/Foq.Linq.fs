namespace Foq.Linq

open System
open System.Linq.Expressions
open System.Reflection
open Foq.Emit

module private Reflection =
    /// Converts argument expressions to Arg array    
    let hasAttribute a (mi:MethodInfo) = mi.GetCustomAttributes(a, true).Length > 0
    let isWildcard mi = hasAttribute typeof<Foq.WildcardAttribute> mi
    let isPredicate mi = hasAttribute typeof<Foq.PredicateAttribute> mi
    /// Resolves Expression to Arg
    let rec resolve : Expression -> Arg = function
        | :? ConstantExpression as constant ->          
            Arg(constant.Value)
        | :? UnaryExpression as unary ->
            unary.Operand |> resolve
        | :? MethodCallExpression as call when isWildcard call.Method ->
            Any
        | :? MethodCallExpression as call when isPredicate call.Method ->
            let lambda = call.Arguments.[0] :?> LambdaExpression
            let del = lambda.Compile()
            let f = fun x -> del.DynamicInvoke([|x|]) :?> bool
            PredUntyped(f)
        | :? MemberExpression as call ->
            let instance = 
                match call.Expression with
                | :? ConstantExpression as ce -> ce.Value
                | null -> null
                | _ -> raise <| NotSupportedException() 
            let value =
                match call.Member with
                | :? FieldInfo as fi -> fi.GetValue(instance)
                | :? PropertyInfo as pi -> pi.GetValue(instance, [||])
                | _ -> raise <| NotSupportedException()
            Arg(value)
        | arg -> raise <| NotSupportedException(arg.GetType().ToString())
    // Return resolved arguments
    let toArgs (args:Expression seq) =
        [| for arg in args -> resolve arg |]
    /// Converts expression to a tuple of MethodInfo and Arg array
    let toMethodInfo (expr:Expression) =
        match expr with
        | :? MethodCallExpression as call ->           
            call.Method,            
            Array.zip (call.Method.GetParameters()) [|for arg in call.Arguments -> arg|]
            |> Array.map (fun (pi,arg) -> 
                if pi.IsOut || pi.ParameterType.IsByRef 
                then resolve arg |> function Arg(value) -> OutArg(value) | any -> any 
                else resolve arg
            )
        | _ -> raise <| NotSupportedException(expr.GetType().ToString())    
    /// Converts expression to a tuple of PropertyInfo and Arg array
    let toPropertyInfo (expr:Expression) =
        match expr with
        | :? MemberExpression as call ->
            let pi = call.Member :?> PropertyInfo
            pi, [||]
        | :? MethodCallExpression as call ->
            let pi = 
                call.Method.DeclaringType.GetProperties() 
                |> Seq.find (fun pi -> pi.GetGetMethod() = call.Method)
            pi, toArgs call.Arguments
        | _ -> raise <| NotSupportedException(expr.GetType().ToString())

/// Mock mode
type MockMode = Strict = 0 | Loose = 1

open Reflection

/// Generic stub type over abstract types and interfaces
type Mock<'TAbstract when 'TAbstract : not struct> internal (mode, calls) =
    /// Abstract type
    let abstractType = typeof<'TAbstract>
    /// Constructs mock builder
    new () = Mock(MockMode.Loose,[])
    new (mode) = Mock(mode,[])
    /// Specifies a member method of the abstact type
    member this.Setup(expr:Expression<Func<'TAbstract,'TReturnValue>>) = this.SetupFunc(expr)
    member this.Setup(expr:Expression<Action<'TAbstract>>) = this.SetupAction(expr)
    member this.Setup(name:string) = this.SetupByName(name)
    /// Specifies a member function of the abstract type
    member this.SetupFunc(expr:Expression<Func<'TAbstract,'TReturnValue>>) =
        FuncBuilder<'TAbstract,'TReturnValue>(mode,toMethodInfo expr.Body,calls)
    /// Specifies a member action of the abstract type
    member this.SetupAction(expr:Expression<Action<'TAbstract>>) =
        ActionBuilder<'TAbstract>(mode,toMethodInfo expr.Body,calls)
    /// Specifies a member subroutine of the abstract type
    member this.SetupSub(expr:Expression<Action<'TAbstract>>) = this.SetupAction(expr)
    /// Specifies a property getter of the abstract type
    member this.SetupPropertyGet(expr:Expression<Func<'TAbstract,'TReturnValue>>) =
        let pi, args = toPropertyInfo expr.Body
        let call = pi.GetGetMethod(), args 
        FuncBuilder<'TAbstract, 'TReturnValue>(mode,call,calls)
    /// Specifies a property setter of the abstract type
    member this.SetupPropertySet(expr:Expression<Func<'TAbstract,'TReturnValue>>) =
        let pi, args = toPropertyInfo expr.Body
        let call = pi.GetSetMethod(), args
        ActionBuilder<'TAbstract>(mode,call,calls)
    /// Specifies an event of the abstract type
    member this.SetupEvent(name:string) =
        let e = abstractType.GetEvent(name)
        let handlers = e.GetAddMethod(), e.GetRemoveMethod()
        EventBuilder<'TAbstract>(mode,handlers,calls)
    /// Specifies properties of the abstract type
    member this.SetupProperties(anonymousObject:obj) =
        let ps = anonymousObject.GetType().GetProperties()
        let types = [|yield abstractType; yield! abstractType.GetInterfaces()|]
        let properties =
            [for p in ps do
                let pi = types |> Seq.tryPick (fun t -> 
                    match t.GetProperty(p.Name) with
                    | null -> None
                    | pi -> Some pi
                )
                match pi with
                | Some pi ->
                    let mi = pi.GetGetMethod()
                    let value = p.GetValue(anonymousObject, [||])
                    yield mi, ([||],ReturnValue(value, pi.PropertyType))
                | None -> ()]
        Mock<'TAbstract>(mode, properties @ calls)
    /// Setup member by name
    member this.SetupByName<'TReturnValue>(name:string) =
        let attr = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance
        let mi = typeof<'TAbstract>.GetMethod(name, attr)
        let args = [|for arg in mi.GetParameters() -> Any|] 
        FuncBuilder<'TAbstract,'TReturnValue>(mode,(mi,args),calls)
    /// Creates a mocked instance of the abstract type
    member this.Create() = 
        mock(MockMode.Strict = mode,abstractType,calls,[||],None) :?> 'TAbstract
    /// Creates a mocked instance of the abstract type
    member this.Create([<ParamArray>] args:obj[]) = 
        mock(MockMode.Strict = mode,abstractType,calls,args,None) :?> 'TAbstract
and ActionBuilder<'TAbstract when 'TAbstract : not struct>
    internal (mode,call,calls) =
    let mi, args = call
    /// Specifies the exception a method or property raises
    [<RequiresExplicitTypeArguments>]
    member this.Raises<'TException when 'TException : (new : unit -> 'TException) 
                                   and  'TException :> exn>() =
        Mock<'TAbstract>(mode,(mi, (args, Raise(typeof<'TException>)))::calls)
    /// Specifies the exception value a method or property raises
    member this.Raises(exnValue:exn) =
        Mock<'TAbstract>(mode,(mi, (args, RaiseValue(exnValue)))::calls)
and FuncBuilder<'TAbstract,'TReturnValue when 'TAbstract : not struct>
    internal (mode,call,calls) =
    inherit ActionBuilder<'TAbstract>(mode,call,calls)
    let mi, args = call    
    /// Specifies the return value of a method or property
    member this.Returns(value:'TReturnValue) =
        let result = 
            if typeof<'TReturnValue> = typeof<unit> then Unit 
            else ReturnValue(value,typeof<'TReturnValue>)
        Mock<'TAbstract>(mode,(mi, (args, result))::calls)
    /// Specifies a computed return value of a method or property
    member this.Returns(f:Func<'TReturnValue>) =
        Mock<'TAbstract>(mode,(mi, (args, ReturnFunc(fun () -> f.Invoke())))::calls)
/// Generic builder for specifying event values
and EventBuilder<'TAbstract when 'TAbstract : not struct> 
    internal (mode,handlers,calls) =
    let add, remove = handlers
    /// Specifies the published event value
    member this.Publishes(value:IDelegateEvent<'TDelegate>) =
        Mock<'TAbstract>(mode,
                         (add, ([|Any|], Handler("AddHandler",value)))::
                         (remove, ([|Any|], Handler("RemoveHandler",value)))::
                         calls)

type Mock =
    /// Creates a mocked instance of the abstract type
    static member Of<'TAbstractType>() = 
        mock(false, typeof<'TAbstractType>, [], [||], None) :?> 'TAbstractType

module private Verification =
    open Foq.Verification
    /// Gets instance value from member expression
    let getInstance (expr:Expression) =
        match expr with
        | :? MemberExpression as me ->
            let getExpression = Expression<Func<obj>>.Lambda<Func<obj>>(me)
            let caller = getExpression.Compile()
            caller.Invoke()
        | _ -> raise <| NotSupportedException(expr.ToString())
    /// Gets method from lambda expression
    let getMethod (expr:LambdaExpression) =
        match expr.Body with
        | :? MethodCallExpression as call -> 
            getInstance call.Object, call.Method, toArgs call.Arguments    
        | _ -> raise <| NotSupportedException(expr.ToString())
    // Gets property from lambda expression
    let getProperty (expr:LambdaExpression) = 
        match expr.Body with
        | :? MemberExpression as call ->
            let pi = call.Member :?> PropertyInfo
            getInstance call.Expression, pi, toArgs [||]
        | :? MethodCallExpression as call -> 
            let pi = 
                call.Method.DeclaringType.GetProperties() 
                |> Seq.find (fun pi -> pi.GetGetMethod() = call.Method)
            getInstance call.Object, pi, toArgs call.Arguments
        | _ -> raise <| NotSupportedException(expr.ToString())
    /// Verifies if method called on instance the specified number of times
    let verify (times:Foq.Times) (instance:obj, mi, args) =
        let mock = getMock instance
        let actualCalls = countInvocations mock mi args
        if not <| times.Match(actualCalls) then
            failwith "Expected invocations on the mock not met"
    /// Expects method call on instance the specified number of times
    let expect (times:Foq.Times) (instance:obj, mi, args) =
        let mock = getMock instance
        mock.Invoked.Subscribe(fun _ ->
            let last = mock.Invocations.[mock.Invocations.Count-1]
            if invokeMatch mi args last then
                let actualCalls = countInvocations mock mi args
                if not <| times.Match(actualCalls) then
                    failwith "Expected invocations on the mock not met"
        ) |> ignore

open Verification

type Times internal (predicate) = inherit Foq.Times(predicate)

type Mock with
    /// Verifies specified function is called at least once on specified mock
    static member VerifyFunc<'TReturnValue>(expr:Expression<Func<'TReturnValue>>) =
        getMethod expr |> verify Foq.Times.atleastonce
    /// Verifies specified action is called at least once on specified mock
    static member VerifyAction(expr:Expression<Action>) =
        getMethod expr |> verify Foq.Times.atleastonce
    /// Verifies specified subroutine is called at least once on specified mock
    static member VerifySub(expr:Expression<Action>) = 
        Mock.VerifyAction(expr)
    /// Verifies specified method is called at least once on specified mock
    static member Verify<'TReturnValue>(expr:Expression<Func<'TReturnValue>>) =
        Mock.VerifyFunc<'TReturnValue>(expr)
    static member Verify(expr:Expression<Action>) =
        Mock.VerifyAction(expr)
    /// Verifies specified property getter is called at least once on specified mock
    static member VerifyPropertyGet(expr:Expression<Func<'TReturnValue>>) =
        getProperty expr 
        |> function (o,pi,args) -> o, pi.GetGetMethod(), args
        |> verify Foq.Times.atleastonce
    /// Verifies specified property setter is called at least once on specified mock
    static member VerifyPropertySet(expr:Expression<Func<'TReturnValue>>) =
        getProperty expr
        |> function (o,pi,args) -> o, pi.GetSetMethod(), [|yield! args; yield Arg.Any|]
        |> verify Foq.Times.atleastonce
    /// Expects specified function is called on specified mock
    static member ExpectFunc<'TReturnValue>(expr:Expression<Func<'TReturnValue>>, times:Foq.Times) =
        getMethod expr |> expect times
    /// Expects specified action is called on specified mock
    static member ExpectAction(expr:Expression<Action>, times:Foq.Times) =
        getMethod expr |> expect times
    /// Expects specified subroutine is called on specified mock
    static member ExpectSub(expr:Expression<Action>, times:Foq.Times) = 
        Mock.ExpectAction(expr, times)
    /// Expects specified method is called on specified mock
    static member Expect<'TReturnValue>(expr:Expression<Func<'TReturnValue>>, times:Foq.Times) =
        Mock.ExpectFunc<'TReturnValue>(expr, times)
    static member Expect(expr:Expression<Action>, times:Foq.Times) =
        Mock.ExpectAction(expr, times)
    /// Expects specified property getter is called on specified mock
    static member ExpectPropertyGet(expr:Expression<Func<'TReturnValue>>, times:Foq.Times) =
        getProperty expr 
        |> function (o,pi,args) -> o, pi.GetGetMethod(), args
        |> expect times
    /// Expects specified property setter is called on specified mock
    static member ExpectPropertySet(expr:Expression<Func<'TReturnValue>>, times:Foq.Times) =
        getProperty expr
        |> function (o,pi,args) -> o, pi.GetSetMethod(), [|yield! args; yield Arg.Any|]
        |> expect times
    /// Records expectations in scope of IDisposable (e.g. C# using block)
    static member RecordExpectations(mock:obj) =
        let mock = mock :?> Foq.IMockObject
        let past = mock.Invocations |> Seq.toArray
        mock.Invocations.Clear()
        let recorded () =
            let anyArgs = Array.map (fun _ -> Foq.Emit.Any)
            mock.Invocations
            |> Seq.map (fun i -> i.Method, i.Args |> anyArgs)
            |> Seq.toList
        let setup calls =
            mock.Invocations.Clear()
            for invoke in past do mock.Invocations.Add invoke
            Foq.Expectations.setup mock calls 
        { new IDisposable with member x.Dispose() = recorded () |> setup }
    /// Verify all expectations
    static member VerifyAll(mock) = Mock.VerifyAll mock 

type [<Sealed>] It private () =
    /// Marks argument as matching any value
    [<Foq.Wildcard>] static member IsAny<'TArg>() = Unchecked.defaultof<'TArg>
    /// Marks argument as matching specific values
    [<Foq.Predicate>] static member Is<'TArg>(f:Func<'TArg,bool>) = Unchecked.defaultof<'TArg>

type [<Sealed>] A<'TArg> private () =
    /// Marks argument as matching any value
    [<Foq.Wildcard>] static member Ignored = Unchecked.defaultof<'TArg>
    /// Marks argument as matching specific values
    [<Foq.Predicate>] static member When(f:Func<'TArg,bool>) = Unchecked.defaultof<'TArg>