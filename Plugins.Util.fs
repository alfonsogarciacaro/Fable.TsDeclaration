module internal Fable.Plugins

open Fable
open Fable.AST
open System
open System.Linq
open System.Text.RegularExpressions

module Patterns =
    let (|NestedLambdaType|_|) e =
        let rec nestedLambda acc = function
            | Fable.LambdaType(arg, returnType) ->
                nestedLambda (arg::acc) returnType
            | returnType -> List.rev acc, returnType
        match e with
        | Fable.LambdaType(arg, t) -> nestedLambda [arg] t |> Some
        | _ -> None

type MemberInfo(?info: Fable.MemberInfo,
                ?isValue: bool) =
    let infoOr f v =
        match info with
        | Some i -> f i
        | None -> v
    let argOrInfoOr arg f v =
        match arg, info with
        | Some arg, _ -> arg
        | None, Some i -> f i
        | None, None -> v
    interface Fable.MemberInfo with
        member _.IsValue = argOrInfoOr isValue (fun i -> i.IsValue) false
        member _.Attributes = infoOr (fun i -> i.Attributes) Seq.empty
        member _.HasSpread = infoOr (fun i -> i.HasSpread) false
        member _.IsPublic = infoOr (fun i -> i.IsPublic) true
        member _.IsInstance = infoOr (fun i -> i.IsInstance) true
        member _.IsMutable = infoOr (fun i -> i.IsMutable) false
        member _.IsGetter = infoOr (fun i -> i.IsGetter) false
        member _.IsSetter = infoOr (fun i -> i.IsSetter) false
        member _.IsEnumerator = infoOr (fun i -> i.IsEnumerator) false
        member _.IsMangled = infoOr (fun i -> i.IsMangled) false

type EntityPrinter = int -> Fable.Entity -> (* genArgs *) Fable.Type list -> string

type Util =
    static member makeIdent name: Fable.Ident =
        { Name = name
          Type = Fable.Any
          IsCompilerGenerated = true
          IsThisArgument = false
          IsMutable = false
          Range = None }

    static member makeValue r value =
        Fable.Value(value, r)

    static member makeStrConst (x: string) =
        Fable.StringConstant x
        |> Util.makeValue None

    static member makeImport (selector: string) (path: string) =
        Fable.Import({ Selector = selector
                       Path = path
                       IsCompilerGenerated = true }, Fable.Any, None)

    static member objValue (k, v): Fable.MemberDecl =
        {
            Name = k
            FullDisplayName = k
            Args = []
            Body = v
            UsedNames = Set.empty
            Info = MemberInfo(isValue=true)
            ExportDefault = false
        }

    static member objExpr kvs =
        Fable.ObjectExpr(List.map Util.objValue kvs, Fable.Any, None)

    static member nullValue =
        Fable.Expr.Value(Fable.ValueKind.Null(Fable.Type.Any), None)

    static member emitJs macro args  =
        let callInfo: Fable.CallInfo =
            { ThisArg = None
              Args = args
              SignatureArgTypes = []
              HasSpread = false
              IsJsConstructor = false
              CallMemberInfo = None }

        let emitInfo : Fable.AST.Fable.EmitInfo =
            { Macro = macro
              IsJsStatement = false
              CallInfo = callInfo }

        Fable.Expr.Emit(emitInfo, Fable.Type.Any, None)

    static member flattenList (head: Fable.Expr) (tail: Fable.Expr) =
        [
            yield head;
            match tail with
            | Fable.Expr.Value (value, range) ->
                match value with
                | Fable.ValueKind.NewList(Some(nextHead, nextTail), listType) ->
                    yield! Util.flattenList nextHead nextTail
                | Fable.ValueKind.NewList(None, listType) ->
                    yield! [ ]
                | _ ->
                    yield! [ Fable.Expr.Value (value, range) ]

            | _ ->
                yield! [ ]
        ]

    static member isRecord (compiler: PluginHelper) (fableType: Fable.Type) =
        match fableType with
        | Fable.Type.AnonymousRecordType _ -> true
        | Fable.Type.DeclaredType (entity, genericArgs) -> compiler.GetEntity(entity).IsFSharpRecord
        | _ -> false

    static member isAnonymousRecord (fableType: Fable.Type) =
        match fableType with
        | Fable.Type.AnonymousRecordType  _ -> true
        | _ -> false

    static member isReactElement (fableType: Fable.Type) =
        match fableType with
        | Fable.Type.DeclaredType (entity, genericArgs) -> entity.FullName.EndsWith "ReactElement"
        | _ -> false

    static member recordHasField name (compiler: PluginHelper) (fableType: Fable.Type) =
        match fableType with
        | Fable.Type.AnonymousRecordType (fieldNames, genericArgs) ->
            fieldNames
            |> Array.exists (fun field -> field = name)

        | Fable.Type.DeclaredType (entity, genericArgs) ->
            compiler.GetEntity(entity).FSharpFields
            |> List.exists (fun field -> field.Name = name)

        | _ ->
            false

    static member makeCall callee args =
        let callInfo: Fable.CallInfo =
            { ThisArg = None
              Args = args
              SignatureArgTypes = []
              HasSpread = false
              IsJsConstructor = false
              CallMemberInfo = None }
        Fable.Call(callee, callInfo, Fable.Any, None)

    static member isPascalCase (input: string) = not (String.IsNullOrWhiteSpace input) && List.contains input.[0] ['A' .. 'Z']

    static member isCamelCase (input: string) = not (Util.isPascalCase input)

    static member capitalize (input: string) =
        if String.IsNullOrWhiteSpace input
        then ""
        else input.First().ToString().ToUpper() + String.Join("", input.Skip(1))

    static member camelCase (input: string) =
        if String.IsNullOrWhiteSpace input
        then ""
        else input.First().ToString().ToLower() + String.Join("", input.Skip(1))

    static member cleanFullDisplayName str =
        Regex.Replace(str, @"`\d+", "").Replace(".", "_")

    static member LowerFirst (str: string) =
            str.[0].ToString().ToLower() + str.[1..]

    static member Indented(indent, str) =
        (String.replicate (4 * indent) " ") + str

    static member PrintFunction(h, args, ?t, ?indent, ?arrow, ?printEnt) =
        let i = defaultArg indent 0

        let printArg (name: string, arg) =
            $"{name}: {Util.PrintType(h, arg, i, ?printEnt=printEnt)}"

        let separator = if defaultArg arrow true then " => " else ": "
        let t = match t with Some t -> Util.PrintType(h, t, i, ?printEnt=printEnt) | None -> "void"

        $"""({args |> List.map printArg |> String.concat ", "}){separator}{t}"""

    static member PrintType(h: Fable.PluginHelper, t: Fable.Type, ?indent, ?printEnt: EntityPrinter) =
        let indent = defaultArg indent 0

        let makeArgs argTpypes =
            argTpypes |> List.mapi (fun i t -> $"a{i}", t)

        let printRecord fields =
            [
                "{"
                for (name, t) in fields do
                    $"""{Util.Indented(indent + 1, name)}: {Util.PrintType(h, t, indent + 1, ?printEnt=printEnt)},"""
                Util.Indented(indent, "}")
            ] |> String.concat Environment.NewLine

        match t with
        | Fable.Any -> "any"
        | Fable.Unit -> "undefined"
        | Fable.Boolean -> "boolean"
        | Fable.Char
        | Fable.String -> "string"
        | Fable.Regex -> "RegExp"
        | Fable.Enum _
        | Fable.Number _ -> "number"
        | Fable.Option t -> $"({Util.PrintType(h, t, indent, ?printEnt=printEnt)}|undefined)"
        | Fable.Tuple ts -> $"""[{ts |> List.map (fun t -> Util.PrintType(h, t, indent, ?printEnt=printEnt)) |> String.concat ", "}]"""
        | Fable.Array t -> Util.PrintType(h, t, indent, ?printEnt=printEnt) + "[]"
        | Fable.List t -> $"Iterable<{Util.PrintType(h, t, indent, ?printEnt=printEnt)}>"
        | Fable.DelegateType(args, ret)
        | Patterns.NestedLambdaType(args, ret) ->
            Util.PrintFunction(h, makeArgs args, ret, indent, ?printEnt=printEnt)
        // This is caught by the active pattern above but we need it
        // for the pattern matching to be exhaustive
        | Fable.LambdaType _ -> failwith "unexpected"
        | Fable.GenericParam name -> $"/* {name} */ any" // name // TODO: print generic args in function/type
        | Fable.DeclaredType(e, genArgs) ->
            let e = h.GetEntity(e)
            if e.IsFSharpRecord then
                e.FSharpFields |> List.map (fun f -> f.Name, f.FieldType) |> printRecord
            else
                match e.FullName, genArgs with
                | "System.Guid", _ -> "string"
                | "Microsoft.FSharp.Collections.FSharpMap`2", [k; v] ->
                    $"Iterable<[{Util.PrintType(h, k, indent, ?printEnt=printEnt)}, {Util.PrintType(h, v, indent, ?printEnt=printEnt)}]>"
                | fullname, _ ->
                    match printEnt with
                    | Some p -> p indent e genArgs
                    | None -> $"/* {fullname} */ any"
        | Fable.AnonymousRecordType(fieldNames, genArgs) -> List.zip (List.ofArray fieldNames) genArgs |> printRecord
        | Fable.MetaType -> "any" // failwith "System.Type is not supported"

    static member MakeJsDispatcherFromUnion(dispatchRef, msgEntity: Fable.Entity, msgGenArgs) =
        let makeMember uciTag (uci: Fable.UnionCase) =
            let outerArgs =
                uci.UnionCaseFields
                |> List.map (fun fi -> Util.makeIdent fi.Name)

            let msgArgs = outerArgs |> List.map Fable.IdentExpr
            let msg = Fable.Value(Fable.NewUnion(msgArgs, uciTag, msgEntity.Ref, msgGenArgs), None)
            let dispatchAction = Util.makeCall (Fable.IdentExpr dispatchRef) [msg]

            Util.LowerFirst uci.Name, Fable.Delegate(outerArgs, dispatchAction, None)

        msgEntity.UnionCases
        |> List.mapi makeMember
        |> Util.objExpr
