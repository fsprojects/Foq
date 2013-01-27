namespace Foq.Linq

open System
open System.Linq.Expressions
open System.Reflection
open Foq.CodeEmit

/// Mock mode
type MockMode = Strict = 0 | Loose = 1

/// Generic stub type over abstract types and interfaces
type Mock<'TAbstract when 'TAbstract : not struct> internal (mode, calls) =
    /// Abstract type
    let abstractType = typeof<'TAbstract>
    /// Converts argument expressions to Arg array
    let toArgs (args:Expression seq) =
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
        [| for arg in args -> resolve arg |]
    /// Converts expression to a tuple of MethodInfo and Arg array
    let toMethodInfo (expr:Expression) =
        match expr with
        | :? MethodCallExpression as call ->
            call.Method, toArgs call.Arguments
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
    /// Constructs mock builder
    new () = Mock(MockMode.Strict,[])
    new (mode) = Mock(mode,[])
    /// Specifies a member function of the abstract type
    member this.SetupFunc(expr:Expression<Func<'TAbstract,'TReturnValue>>) =
        FuncBuilder<'TAbstract,'TReturnValue>(mode,toMethodInfo expr.Body,calls)
    /// Specifies a member action of the abstract type
    member this.SetupAction(expr:Expression<Action<'TAbstract>>) =
        ActionBuilder<'TAbstract>(mode,toMethodInfo expr.Body,calls)
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
    /// Specifies an event of the abstract type as a quotation
    member this.SetupEvent(name:string) =
        let e = abstractType.GetEvent(name)
        let handlers = e.GetAddMethod(), e.GetRemoveMethod()
        EventBuilder<'TAbstract>(mode,handlers,calls)
    /// Creates a generic instance of the abstract type
    member this.Create() = 
        mock(mode = MockMode.Strict,abstractType,calls) :?> 'TAbstract
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

[<Sealed>]
type It private () =
    /// Marks argument as matching any value
    [<Foq.Wildcard>] static member IsAny<'TArg>() = Unchecked.defaultof<'TArg>
    /// Marks argument as matching specific values
    [<Foq.Predicate>] static member Is<'TArg>(f:Func<'TArg,bool>) = Unchecked.defaultof<'TArg>