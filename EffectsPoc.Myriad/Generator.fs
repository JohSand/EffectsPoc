namespace EffectsPoc.Myriad

open System
open FsAst.AstRcd
open FsAst.AstCreate
open FSharp.Compiler.Range
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.XmlDoc
open Myriad.Core

[<AutoOpen>]
module GeneratorHelpers =
    type SynValSig with
        static member Create(memberName, typeName) =
            ValSpfn
                (SynAttributes.Empty,
                 Ident.Create(memberName),
                 SynValTyparDecls([], true, []),
                 SynType.LongIdent(LongIdentWithDots.Create([ typeName ])),
                 SynValInfo([], SynArgInfo([], false, None)),
                 false,
                 false,
                 PreXmlDoc.Empty,
                 None,
                 None,
                 range ())

    let toIdent s = Ident.Create s

    let createOpen ns =
        let a = LongIdentWithDots.Create(ns).Lid

        let b =
            SynOpenDeclTarget.ModuleOrNamespace(a, range ())

        SynModuleDecl.CreateOpen(b)

    let createType providerName memberName typeName =
        let componentInfo =
            SynComponentInfoRcd.Create [ Ident.Create providerName ]

        let mem =
            SynMemberDefn.AbstractSlot(SynValSig.Create(memberName, typeName), MemberFlags.InstanceMember, range ())

        SynModuleDecl.CreateType(componentInfo, [ mem ])

    let parseMemberName (m: SynMemberDefn) =
        match m with
        | SynMemberDefn.AbstractSlot(slotSig = (ValSpfn(ident = id))) -> id.idText
        | _ -> ""
        
    let parseInParamType (m: SynMemberDefn) =
        match m with
        | SynMemberDefn.AbstractSlot(slotSig = (ValSpfn(synType = d))) ->          
            match d with
            | (SynType.Fun (argType, returnType, _range)) ->
                match argType with
                | SynType.LongIdent t -> Some(t.Lid.Head.ToString()) 
                | _ -> None
            | _ -> None          
        | _ -> None
        
    let parseInParamCount (m: SynMemberDefn) =
        match m with
        | SynMemberDefn.AbstractSlot(slotSig = s) ->
            let (ValSpfn(synType = d)) = s

            let rec getReturnTypeNames synType acc =
                match synType with
                | (SynType.Fun (argType, returnType, _range)) ->
                    (match returnType with
                    //if returnType is a fun, we are of type 'a -> 'b -> ...
                    //count 'a as 1, and continue
                    | (SynType.Fun _) -> returnType, (acc + 1)
                    | _ -> argType, acc)
                    ||> getReturnTypeNames
                | SynType.LongIdent t -> t.Lid.Length + acc
                | SynType.Paren(innerType = inner) -> getReturnTypeNames inner acc
                | SynType.Tuple(elementTypes = types) -> types.Length + acc
                | SynType.Var _ -> 1 + acc
                | _ -> acc

            getReturnTypeNames d 0

        | _ -> 0
   
    let createLambdaExpr providerName methodName (args: string list) instanceName =
        let a =
            SynSimplePat.CreateTyped(toIdent "env", SynType.HashConstraint(SynType.Create providerName, range ()))

        let pat =
            SynSimplePats.SimplePats([ a ], range ())

        let idents =
            [ "env"; instanceName; methodName ]
            |> LongIdentWithDots.Create
            |> SynExpr.CreateLongIdent

        let argExpr =
            args
            |> List.map SynExpr.CreateIdentString
            |> SynExpr.CreateTuple
            |> SynExpr.CreateParen

        let body = SynExpr.CreateApp(idents, argExpr)

        let lambda =
            SynExpr.Lambda(false, false, pat, body, None, range ())

        SynExpr.CreateParen(lambda)

    let createSynPatRcd s =
        SynPatRcd.CreateNamed(Ident.Create s, SynPatRcd.CreateWild)

    let toCamelCase (name: string) =
        if name.Length > 0 then
            name.[0].ToString().ToLowerInvariant()
            + name.Substring(1)
        else
            name

    let createEffectFunction methodName paramNames providerName instanceName =
        let funcName = toCamelCase methodName
        let args =
            if List.isEmpty paramNames then
                let abc = SynPatRcd.CreateParen(SynPatRcd.CreateTuple([]))  
                [abc]
            else
                paramNames |> List.map createSynPatRcd
        
        let lambda =
            createLambdaExpr providerName methodName paramNames instanceName

        let instance =
            LongIdentWithDots.Create([ "Effect"; "Create" ])

        let wrapper =
            SynExpr.CreateInstanceMethodCall(instance, lambda)

        SynModuleDecl.CreateLet
            [ { SynBindingRcd.Let with
                    Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString funcName, args)
                    Expr = wrapper } ]

    let createRecordModule (ident: Ident) (rpr: SynTypeDefnReprObjectModelRcd): SynModuleDecl list =
        //get name of type
        //add provider
        //strip i
        let name = ident.ToString()
        let providerName = name + "Provider"
        let instanceName = name.TrimStart('I')
        //this creates the provider, seems done!
        let providerType =
            createType providerName instanceName name

        let params' = [ 'a' .. 'z' ]

        let funcs =
            [ for m in rpr.Members do
                let funcName = parseMemberName m
                let paramCount = parseInParamCount m
                let paramNames =
                                    match parseInParamType m with
                                    | Some s when s = "unit" -> []
                                        //is it unit?     
                                    | _ -> params' |> List.take paramCount |> List.map string

                let a = 1
                yield createEffectFunction funcName paramNames providerName instanceName ]

        [ providerType
          //effect funcs
          SynModuleDecl.CreateNestedModule(SynComponentInfoRcd.Create [ Ident.Create instanceName ], funcs) ]


    let createObjModel (a: SynTypeDefn) =
        match a.ToRcd.Repr.ToRcd with
        | SynTypeDefnReprRcd.ObjectModel o -> createRecordModule a.ToRcd.Info.Id.Head o
        | SynTypeDefnReprRcd.Simple _ -> []

type EffectAttribute() =
    inherit Attribute()

[<MyriadGenerator("Effect")>]
type EffectGenerator() =
    interface IMyriadGenerator with
        member this.ValidInputExtensions: seq<string> = seq { ".fs" }

        member __.Generate(context) =
            Ast.fromFilename context.InputFileName
            |> Async.RunSynchronously
            |> (Array.head >> fst) 
            |> Ast.extractTypeDefn
            |> List.map (fun (ns, types) ->
                let modules =
                    types
                    |> List.filter (Ast.hasAttribute<EffectAttribute>)
                    |> List.collect createObjModel
                if modules |> List.isEmpty then
                    None
                else
                    Some { SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong ns.Head.idText) with
                              IsRecursive = true
                              Declarations =
                                  [ yield createOpen ([ "EffectsPoc" ])
                                    yield! modules ] })
            |> List.choose id

