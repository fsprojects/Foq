namespace Foq

open System
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Reflection

/// Mock object interface for verification
type IMockObject =
    abstract Invocations : Invocations
/// Member invocation record
and Invocation = { Method : MethodBase; Args : obj[] }
/// List of invocations
and Invocations = System.Collections.Generic.List<Invocation>

module internal Emit =
    /// Boxed value
    type Value = obj
    /// Boxed function
    type Func = obj
    /// Boxed event
    type PublishedEvent = obj
    /// Method argument type
    type Arg = Any | Arg of Value | Pred of Func | PredUntyped of Func
    /// Method result type
    type Result = 
        | Unit
        | ReturnValue of Value * Type
        | ReturnFunc of Func
        | Handler of string * PublishedEvent
        | Call of Func
        | Raise of Type
        | RaiseValue of exn

    /// Generates constructor
    let generateConstructor (typeBuilder:TypeBuilder) ps (genBody:ILGenerator -> unit) =
        let cons = typeBuilder.DefineConstructor(MethodAttributes.Public,CallingConventions.Standard,ps)
        let il = cons.GetILGenerator()
        // Call base constructor
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Call, typeof<obj>.GetConstructor(Type.EmptyTypes))
        // Generate body
        genBody il
        il.Emit(OpCodes.Ret)

    /// Defines method
    let defineMethod (typeBuilder:TypeBuilder) (abstractMethod:MethodInfo) =
        let attr = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.Virtual
        let args = abstractMethod.GetParameters() |> Array.map (fun arg -> arg.ParameterType)
        typeBuilder.DefineMethod(abstractMethod.Name, attr, abstractMethod.ReturnType, args)

    /// Generates method overload args match
    let generateArgs 
        (il:ILGenerator) (argsLookup:ResizeArray<Value[]>,argsField:FieldBuilder) 
        (mi:MethodInfo,args) (unmatched:Label) =
        /// Index of argument values for current method overload
        let argsLookupIndex = argsLookup.Count
        // Add arguments to lookup
        args |> Array.map (function Any -> null | Arg(value) -> value | Pred(f) -> f | PredUntyped(f) -> f) |> argsLookup.Add
        // Emit argument matching
        args |> Seq.iteri (fun argIndex arg ->
            let emitArgBox () =
                il.Emit(OpCodes.Ldarg, argIndex+1)
                il.Emit(OpCodes.Box, mi.GetParameters().[argIndex].ParameterType)
            let emitArgLookup value =
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldfld, argsField)
                il.Emit(OpCodes.Ldc_I4, argsLookupIndex)
                il.Emit(OpCodes.Ldelem_Ref)
                il.Emit(OpCodes.Ldc_I4, argIndex)
                il.Emit(OpCodes.Ldelem_Ref)
            match arg with
            | Any -> ()
            | Arg(value) ->
                emitArgBox ()
                emitArgLookup value
                // Emit Object.Equals(box args.[argIndex+1], _args.[argsLookupIndex].[argIndex])
                il.EmitCall(OpCodes.Call, typeof<obj>.GetMethod("Equals",[|typeof<obj>;typeof<obj>|]), null) 
                il.Emit(OpCodes.Brfalse_S, unmatched)
            | Pred(f) ->
                emitArgLookup f
                il.Emit(OpCodes.Ldarg, argIndex+1)
                let argType = mi.GetParameters().[argIndex].ParameterType
                let invoke = FSharpType.MakeFunctionType(argType,typeof<bool>).GetMethod("Invoke")
                il.Emit(OpCodes.Callvirt, invoke)
                il.Emit(OpCodes.Brfalse_S, unmatched)
            | PredUntyped(f) ->
                emitArgLookup f
                il.Emit(OpCodes.Ldarg, argIndex+1)
                let argType = mi.GetParameters().[argIndex].ParameterType
                il.Emit(OpCodes.Box, argType)
                let invoke = FSharpType.MakeFunctionType(typeof<obj>,typeof<bool>).GetMethod("Invoke")
                il.Emit(OpCodes.Callvirt, invoke)
                il.Emit(OpCodes.Brfalse_S, unmatched)
        )

    /// Generates method return
    let generateReturn
        (il:ILGenerator) (returnValues:ResizeArray<Value>,returnValuesField:FieldBuilder) (mi:MethodInfo,result) =
        // Emits _returnValues.[returnValuesIndex]
        let emitReturnValueLookup value =
            let returnValuesIndex = returnValues.Count
            returnValues.Add(value)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldfld, returnValuesField)
            il.Emit(OpCodes.Ldc_I4, returnValuesIndex)
            il.Emit(OpCodes.Ldelem_Ref)
        /// Emits AddHandler/RemoveHandler
        let emitEventHandler handlerName e =
            emitReturnValueLookup e
            let handlerType = e.GetType().GetGenericArguments().[0]
            il.Emit(OpCodes.Ldarg_1)
            let t = typedefof<IDelegateEvent<_>>.MakeGenericType(handlerType)
            let invoke = t.GetMethod(handlerName)
            il.Emit(OpCodes.Callvirt, invoke)
            il.Emit(OpCodes.Ret)
        // Emit result
        match result with
        | Unit -> il.Emit(OpCodes.Ret)
        | ReturnValue(value, returnType) ->
            emitReturnValueLookup value
            il.Emit(OpCodes.Unbox_Any, returnType)
            il.Emit(OpCodes.Ret)
        | ReturnFunc(f) ->
            emitReturnValueLookup f
            // Emit Invoke
            il.Emit(OpCodes.Ldnull)
            let invoke = typeof<FSharpFunc<unit,obj>>.GetMethod("Invoke")
            il.Emit(OpCodes.Callvirt, invoke)
            if mi.ReturnType = typeof<unit> || mi.ReturnType = typeof<Void> then 
                il.Emit(OpCodes.Pop)
            il.Emit(OpCodes.Ret)
        | Handler(handlerName, e) -> emitEventHandler handlerName e
        | Call(f) ->
            emitReturnValueLookup f
            // Emit Invoke
            let args = mi.GetParameters() |> Array.map (fun arg -> arg.ParameterType)
            if args.Length = 1 then il.Emit(OpCodes.Ldarg_1)
            else
                for i = 1 to args.Length do il.Emit(OpCodes.Ldarg, i)
                il.Emit(OpCodes.Newobj, FSharpType.MakeTupleType(args).GetConstructor(args))
            let invoke = typeof<FSharpFunc<obj,obj>>.GetMethod("Invoke")
            il.Emit(OpCodes.Callvirt, invoke)
            if mi.ReturnType = typeof<unit> || mi.ReturnType = typeof<Void> then il.Emit(OpCodes.Pop)
            il.Emit(OpCodes.Ret)
        | Raise(exnType) -> il.ThrowException(exnType)
        | RaiseValue(exnValue) ->
            emitReturnValueLookup exnValue
            il.Emit(OpCodes.Throw)

    /// Generates invocation add
    let generateAddInvocation
        (il:ILGenerator) (invocationsField:FieldBuilder) (abstractMethod:MethodInfo) =
        let ps = abstractMethod.GetParameters()
        // Create local array to store arguments
        let local0 = il.DeclareLocal(typeof<obj[]>).LocalIndex
        il.Emit(OpCodes.Ldc_I4, ps.Length)
        il.Emit(OpCodes.Newarr, typeof<obj>)
        il.Emit(OpCodes.Stloc,local0)
        // Store arguments
        for argIndex = 0 to ps.Length - 1 do
            il.Emit(OpCodes.Ldloc, local0)
            il.Emit(OpCodes.Ldc_I4, argIndex)
            il.Emit(OpCodes.Ldarg, argIndex + 1)
            let argType = ps.[argIndex].ParameterType
            il.Emit(OpCodes.Box, argType)
            il.Emit(OpCodes.Stelem_Ref)
        // Add invocation to invocations list
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, invocationsField)
        il.Emit(OpCodes.Call, typeof<MethodBase>.GetMethod("GetCurrentMethod"))
        il.Emit(OpCodes.Ldloc, local0)
        il.Emit(OpCodes.Newobj, typeof<Invocation>.GetConstructor([|typeof<MethodBase>;typeof<obj[]>|]))
        let invoke = typeof<System.Collections.Generic.List<obj[]>>.GetMethod("Add")
        il.Emit(OpCodes.Callvirt, invoke)

    /// Generates method overload
    let generateOverload 
        (il:ILGenerator)
        (argsLookup:ResizeArray<Value[]>,argsField:FieldBuilder)
        (returnValues:ResizeArray<Value>,returnValuesField:FieldBuilder) 
        (mi:MethodInfo,(args, result)) =
        /// Label to goto if argument fails
        let unmatched = il.DefineLabel()
        generateArgs il (argsLookup,argsField) (mi,args) unmatched
        generateReturn il (returnValues,returnValuesField) (mi,result)
        il.MarkLabel(unmatched)

    /// Builds a mock from the specified calls
    let mock (isStrict, abstractType:Type, calls:(MethodInfo * (Arg[] * Result)) list) =
        /// Stub name for abstract type
        let mockName = "Mock" + abstractType.Name
        /// Builder for assembly
        let assemblyBuilder =
            AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName(mockName),AssemblyBuilderAccess.Run)
        /// Builder for module
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(mockName+".dll")
        /// Builder for abstract type
        let typeBuilder = 
            let parent, interfaces = 
                if abstractType.IsInterface
                then typeof<obj>, [|abstractType|]
                else abstractType, [||]
            let attributes = TypeAttributes.Public ||| TypeAttributes.Class
            let interfaces = [|yield typeof<IMockObject>; yield! interfaces|]
            moduleBuilder.DefineType(mockName, attributes, parent, interfaces)
        /// Field settings
        let fields = FieldAttributes.Private ||| FieldAttributes.InitOnly 
        /// Field for method return values
        let returnValuesField = typeBuilder.DefineField("_returnValues", typeof<obj[]>, fields)
        /// Field for method arguments 
        let argsField = typeBuilder.DefineField("_args", typeof<obj[][]>, fields)
        /// Field for method invocations
        let invocationsField = typeBuilder.DefineField("_invocations", typeof<Invocations>, fields)
        // Generate default constructor
        generateConstructor typeBuilder [||] (fun il -> ())
        // Set fields from constructor arguments
        let setFields (il:ILGenerator) =
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Stfld, returnValuesField)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_2)
            il.Emit(OpCodes.Stfld, argsField)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Newobj, typeof<Invocations>.GetConstructor([||]))
            il.Emit(OpCodes.Stfld, invocationsField)
        // Generate constructor overload
        generateConstructor typeBuilder [|typeof<obj[]>;typeof<obj[][]>|] setFields
        /// Generates Invocations property getter
        let generateInvocationsPropertyGetter () =
            let mi = (typeof<IMockObject>.GetProperty("Invocations").GetGetMethod())
            let getter = defineMethod typeBuilder mi
            let il = getter.GetILGenerator()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldfld, invocationsField)
            il.Emit(OpCodes.Ret)
        // Generate Recorder.Invocations property getter
        generateInvocationsPropertyGetter ()
        /// Method overloads grouped by type
        let groupedMethods = calls |> Seq.groupBy fst
        /// Method argument lookup
        let argsLookup = ResizeArray<obj[]>()
        /// Method return values
        let returnValues = ResizeArray<obj>()
        /// Abstract type's methods including interfaces
        let abstractMethods = seq {
            yield! abstractType.GetMethods()
            for interfaceType in abstractType.GetInterfaces() do
                yield! interfaceType.GetMethods()
            }
        /// Generates a default value
        let generateDefaultValueReturn (il:ILGenerator) (returnType:Type) =
            let x = il.DeclareLocal(returnType)
            il.Emit(OpCodes.Ldloca_S, x.LocalIndex)
            il.Emit(OpCodes.Initobj, returnType)
            il.Emit(OpCodes.Ldloc, x.LocalIndex)
            il.Emit(OpCodes.Ret)
        // Implement abstract type's methods
        for abstractMethod in abstractMethods do
            /// Method builder
            let methodBuilder = defineMethod typeBuilder abstractMethod
            /// IL generator
            let il = methodBuilder.GetILGenerator()
            // Record invocation
            generateAddInvocation il invocationsField abstractMethod
            /// Method overloads defined for current method
            let overloads = groupedMethods |> Seq.tryFind (fst >> (=) abstractMethod)
            match overloads with
            | Some (_, overloads) ->
                let toOverload = generateOverload il (argsLookup,argsField) (returnValues,returnValuesField)
                overloads |> Seq.toList |> List.rev |> Seq.iter toOverload
            | None -> ()
            if abstractMethod.ReturnType = typeof<System.Void> || abstractMethod.ReturnType = typeof<unit>
            then il.Emit(OpCodes.Ret)
            elif isStrict 
            then il.ThrowException(typeof<NotImplementedException>)
            else generateDefaultValueReturn il abstractMethod.ReturnType
            if abstractType.IsInterface then
                typeBuilder.DefineMethodOverride(methodBuilder, abstractMethod)
        /// Mock type
        let mockType = typeBuilder.CreateType()
        /// Generated object instance
        let generatedObject = 
            Activator.CreateInstance(
                mockType, [|box (returnValues.ToArray());box (argsLookup.ToArray())|])
        generatedObject

/// Mock mode
type MockMode = Strict = 0 | Loose = 1

/// Wildcard attribute
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property)>]
type WildcardAttribute() = inherit Attribute()

/// Predicate attribute
[<AttributeUsage(AttributeTargets.Method)>]
type PredicateAttribute() = inherit Attribute()

/// Returns attribute
[<AttributeUsage(AttributeTargets.Method)>]
type ReturnsAttribute() = inherit Attribute()

/// Raises attribute
[<AttributeUsage(AttributeTargets.Method)>]
type RaisesAttribute() = inherit Attribute()

open Emit
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

module private Eval =
#if POWERPACK
    open Microsoft.FSharp.Linq.QuotationEvaluation // F# PowerPack dependency
    let eval (expr:Expr) = expr.EvalUntyped()
#else
    let rec eval = function
        | Value(v,t) | Coerce(Value(v,t),_) -> v
        | Coerce(NewObject(ci,args),_) -> ci.Invoke(evalAll args)
        | NewUnionCase(case,args) -> FSharpValue.MakeUnion(case, evalAll args)
        | FieldGet(Some(Value(v,_)),fi) -> fi.GetValue(v)
        | PropertyGet(None, pi, args) -> pi.GetValue(null, evalAll args)
        | PropertyGet(Some(Value(v,_)),pi,args) -> pi.GetValue(v, evalAll args)
        | Call(None,mi,args) -> mi.Invoke(null, evalAll args) 
        | Lambda(v,Call(None,mi,args)) ->
            let ft = FSharpType.MakeFunctionType(v.Type, typeof<bool>)                        
            FSharpValue.MakeFunction(ft, fun x ->
                let args =
                    Array.zip (mi.GetParameters()) (args |> Array.ofList) 
                    |> Array.map (fun (pi,arg) -> if pi.Name = v.Name then x else eval arg)
                mi.Invoke(null, args)
            )            
        | arg -> raise <| NotSupportedException(arg.ToString())
    and evalAll args = [|for arg in args -> eval arg|]
#endif

module private Reflection =
    open Eval
    /// Returns true if method has specified attribute
    let hasAttribute a (mi:MethodInfo) = mi.GetCustomAttributes(a, true).Length > 0
    /// Converts expression to a tuple of MethodInfo and Arg array
    let toArgs args =
        [|for arg in args ->
            match arg with
            | Call(_, mi, _) when hasAttribute typeof<WildcardAttribute> mi -> Any
            | Call(_, mi, [pred]) when hasAttribute typeof<PredicateAttribute> mi -> Pred(eval pred)
            | expr -> eval expr |> Arg |]
    /// Converts expression to a tuple of Expression, MethodInfo and Arg array
    let toCall = function
        | Call(Some(x), mi, args) -> x, mi, toArgs args
        | PropertyGet(Some(x), pi, args) -> x, pi.GetGetMethod(), toArgs args
        | PropertySet(Some(x), pi, args, value) -> x, pi.GetSetMethod(), toArgs args
        | expr -> raise <| NotSupportedException(expr.ToString())
    /// Converts expression to a tuple of MethodInfo and Arg array
    let toCallOfType abstractType expr =
        match toCall expr with
        | x, mi, args when x.Type = abstractType -> mi, args
        | _ -> raise <| NotSupportedException(expr.ToString())
    /// Converts expression to a tuple of MethodInfo, Arg array and Result
    let rec toCallResult abstractType = function
        | Sequential(x,y) ->
            toCallResult abstractType x @ toCallResult abstractType y
        | Call(None, mi, [lhs;rhs]) when hasAttribute typeof<ReturnsAttribute> mi -> 
            let mi, args = toCallOfType abstractType lhs
            let returns = ReturnValue(eval rhs, mi.ReturnType)
            [mi,(args,returns)]
        | Call(None, mi, [lhs;rhs]) when hasAttribute typeof<RaisesAttribute> mi -> 
            let mi, args = toCallOfType abstractType lhs
            let raises = RaiseValue(eval rhs :?> exn)
            [mi,(args,raises)]
        | expr -> invalidOp(expr.ToString())
    /// Converts expression to corresponding event Add and Remove handlers
    let toHandlers abstractType = function
        | Call(None, mi, [Lambda(_,Call(Some(x),addHandler,_));
                          Lambda(_,Call(Some(_),removeHandler,_));_]) 
                          when x.Type = abstractType -> 
            addHandler, removeHandler
        | expr -> raise <| NotSupportedException(expr.ToString())

open Reflection

/// Generic mock type over abstract types and interfaces
type Mock<'TAbstract when 'TAbstract : not struct> internal (mode,calls) =
    /// Abstract type
    let abstractType = typeof<'TAbstract>
    /// Constructs mock builder
    new () = Mock(MockMode.Loose,[])
    new (mode) = Mock(mode,[])
    /// Specifies a method or property of the abstract type as a quotation
    member this.Setup(f:'TAbstract -> Expr<'TReturnValue>) =
        let default' = Unchecked.defaultof<'TAbstract>
        let call = toCallOfType abstractType (f default')
        ResultBuilder<'TAbstract,'TReturnValue>(mode,call,calls)
    /// Specifies an event of the abstract type as a quotation
    member this.SetupEvent(f:'TAbstract -> Expr<'TEvent>) =
        let default' = Unchecked.defaultof<'TAbstract>
        let handlers = toHandlers abstractType (f default')
        EventBuilder<'TAbstract,'TEvent>(mode,handlers,calls)
    /// Creates a mocked instance of the abstract type
    member this.Create() = mock(MockMode.Strict = mode, typeof<'TAbstract>, calls) :?> 'TAbstract
    /// Creates a boxed instance of the abstract type
    static member Create(abstractType:Type) = mock(false, abstractType, [])
    /// Creates a mocked instance of the abstract type
    static member With(f:'TAbstract -> Expr<_>) =
        let default' = Unchecked.defaultof<'TAbstract>
        let calls = toCallResult (typeof<'TAbstract>) (f default')
        Mock<'TAbstract>(MockMode.Strict, calls).Create()
/// Generic builder for specifying method or property results
and ResultBuilder<'TAbstract,'TReturnValue when 'TAbstract : not struct> 
    internal (mode, call, calls) =
    let mi, args = call
    /// Specifies the return value of a method or property
    member this.Returns(value:'TReturnValue) =
        let result = 
            if typeof<'TReturnValue> = typeof<unit> then Unit 
            else ReturnValue(value,typeof<'TReturnValue>)
        Mock<'TAbstract>(mode,(mi, (args, result))::calls)
    /// Specifies a computed return value of a method or property
    member this.Returns(f:unit -> 'TReturnVaue) =
        Mock<'TAbstract>(mode,(mi, (args, ReturnFunc(f)))::calls)
    /// Calls the specified function to compute the return value
    [<RequiresExplicitTypeArguments>]
    member this.Calls<'TArgs>(f:'TArgs -> 'TReturnValue) =
        Mock<'TAbstract>(mode,(mi, (args, Call(f)))::calls)
    /// Specifies the exception a method or property raises
    [<RequiresExplicitTypeArguments>]
    member this.Raises<'TException when 'TException : (new : unit -> 'TException) 
                                   and  'TException :> exn>() =
        Mock<'TAbstract>(mode,(mi, (args, Raise(typeof<'TException>)))::calls)
    /// Specifies the exception value a method or property raises
    member this.Raises(exnValue:exn) =
        Mock<'TAbstract>(mode,(mi, (args, RaiseValue(exnValue)))::calls)
/// Generic builder for specifying event values
and EventBuilder<'TAbstract,'TEvent when 'TAbstract : not struct> 
    internal (mode, handlers, calls) =
    let add, remove = handlers
    /// Specifies the published event value
    member this.Publishes(value:'TEvent) =
        Mock<'TAbstract>(mode, 
                         (add, ([|Any|], Handler("AddHandler",value)))::
                         (remove, ([|Any|], Handler("RemoveHandler",value)))::
                         calls)

type Mock =
    /// Creates a mocked instance of the abstract type
    static member Of<'TAbstractType>() = 
        mock(false, typeof<'TAbstractType>, []) :?> 'TAbstractType   

/// Specifies valid invocation count
type Times private (predicate:int -> bool) =
    member __.Match(n) = predicate(n)       
    static member Exactly(n:int) = Times((=) n)
    static member AtLeast(n:int) = Times((<=) n)
    static member AtLeastOnce() = Times.AtLeast(1)
    static member AtMost(n:int) = Times((>=) n)
    static member AtMostOnce() = Times.AtMost(1)
    static member Never() = Times((=) 0)
    static member Once() = Times((=) 1)

[<AutoOpen;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Times =
    let exactly = Times.Exactly
    let atleast = Times.AtLeast
    let atleastonce = Times.AtLeastOnce()
    let atmost = Times.AtMost
    let atmostonce = Times.AtMostOnce()
    let never = Times.Never()
    let once = Times.Once()

module internal Verification =
    /// Return true if methods match
    let methodsMatch (a:MethodBase) (b:MethodBase) =
        a.Name = b.Name &&
        a.GetParameters().Length = b.GetParameters().Length &&
        Array.zip (a.GetParameters()) (b.GetParameters())       
        |> Array.forall (fun (a,b) -> a.ParameterType = b.ParameterType)
    /// Returns true if arguments match
    let argsMatch argType expectedArg actualValue =
        match expectedArg with
        | Any -> true
        | Arg(expected) -> obj.Equals(expected,actualValue)
        | Pred(p) ->
            let f = FSharpType.MakeFunctionType(argType,typeof<bool>).GetMethod("Invoke")
            f.Invoke(p,[|actualValue|]) :?> bool
        | PredUntyped(p) -> raise <| NotSupportedException()
    /// Returns invocation count matching specificed expression
    let countInvocations (mock:IMockObject) (expectedMethod:MethodBase) (expectedArgs:Arg[]) =
        mock.Invocations
        |> Seq.filter (fun invoked ->
            let ps = expectedMethod.GetParameters()
            methodsMatch expectedMethod invoked.Method &&
            Array.zip expectedArgs invoked.Args
            |> Array.mapi (fun i (e,a) -> ps.[i].ParameterType,e,a)
            |> Array.forall (fun (t,e,a) -> argsMatch t e a)
        )
        |> Seq.length

open Eval
open Verification

type Mock with
    /// Verifies expected call count against instance member invocations on specified mock
    static member Verify(expr:Expr, expectedTimes:Times) =
        let (x,expectedMethod,expectedArgs) = toCall expr
        let mock =
            match eval x with
            | :? IMockObject as mock -> mock
            | _ -> failwith "Object instance is not a mock"           
        let actualCalls = countInvocations mock expectedMethod expectedArgs
        if not <| expectedTimes.Match(actualCalls) then 
            failwith "Expected invocations on the mock not met" 
    /// Verifies expression was invoked at least once
    static member Verify(expr:Expr) = Mock.Verify(expr, atleastonce)

[<Sealed>]
type It private () =
    /// Marks argument as matching any value
    [<Wildcard>] static member IsAny<'TArg>() = Unchecked.defaultof<'TArg>
    /// Marks argument as matching specific values
    [<Predicate>] static member Is<'TArg>(f:'TArg -> bool) = Unchecked.defaultof<'TArg>

[<AutoOpen;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module It =
    /// Marks argument as matching any value
    let [<Wildcard>] inline any () : 'TArg = It.IsAny()
    /// Marks argument as matching specific values
    let [<Predicate>] inline is (f) : 'TArg = It.Is(f)

[<AutoOpen>]
module Operators =
    /// Signifies source expression returns specified value
    let [<Returns>] (-->) (source:'T) (value:'T) = ()
    /// Signifies source expression raises specified exception
    let [<Raises>] (==>) (source:'T) (value:exn) = ()