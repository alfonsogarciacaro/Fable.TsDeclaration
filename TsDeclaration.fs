namespace global

open System
open System.Text.RegularExpressions
open Fable.AST
open Fable.Plugins

[<assembly:Fable.ScanForPlugins>]
do()

type TsDispatcher<'Msg> =
    interface end

type MakeDispatcherAttribute() =
    inherit Fable.MemberDeclarationPluginAttribute()
    override _.FableMinimumVersion = "3.0.0"
    override _.Transform(_,_,decl) = decl
    override _.TransformCall(helper, _member, expr) =
        let fail() =
            helper.LogError("Cannot generate declaration", ?range=expr.Range)
            expr

        let printDispatcher (e: Fable.Entity) =
            let indent = 0
            [
                "{"
                for uci in e.UnionCases do
                    let args = uci.UnionCaseFields |> List.map (fun f -> f.Name, f.FieldType)
                    let fn = Util.PrintFunction(helper, args, indent=indent, arrow=false)
                    $"""{Util.Indented(indent + 1, Util.LowerFirst uci.Name)}{fn},"""
                Util.Indented(indent, "}")

            ] |> String.concat Environment.NewLine

        match expr with
        | Fable.Call(_,info,t,_) ->
            match info.Args with
            | [model; dispatch] ->
                let dispatcher, dispatcherDecl =
                    match dispatch.Type.Generics with
                    | Fable.DeclaredType(msgEntityRef, msgGenArgs)::_ ->
                        let msgEntity = helper.GetEntity(msgEntityRef)
                        if msgEntity.IsFSharpUnion then
                            let bindDispatch, dispatchRef =
                                match dispatch with
                                | Fable.IdentExpr ident -> false, ident
                                | _ -> true, Util.makeIdent "$dispatch"

                            let dispatcher = Util.MakeJsDispatcherFromUnion(dispatchRef, msgEntity, msgGenArgs)
                            let dispatcher =
                                if bindDispatch then Fable.Let(dispatchRef, dispatch, dispatcher)
                                else dispatcher
                            Fable.TypeCast(dispatcher, t, None), printDispatcher msgEntity
                        else
                            dispatch, "any"

                    | msgType::_ -> dispatch, Util.PrintFunction(helper, ["msg", msgType], arrow=true)
                    | [] -> dispatch, "any"

                // Print declaration
                let dTsExt = Regex.Replace(helper.Options.FileExtension, @"\.js$", ".d.ts")
                let path = Regex.Replace(helper.CurrentFile, @"\.fs$", dTsExt)

                IO.File.WriteAllText(path, $"""type Model = {Util.PrintType(helper, model.Type)}

type Dispatch = {dispatcherDecl}""")

                [model; dispatcher]
                |> Fable.NewTuple
                |> Util.makeValue None

            | _ -> fail()
        | _ -> fail()

module TsDeclaration =
    [<MakeDispatcher>]
    let makeDispatcher (model: 'Model) (dispatch: 'Msg->unit): 'Model * TsDispatcher<'Msg> = model, { new TsDispatcher<'Msg> }
