namespace Testing

open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open Xunit
open Myriad.Core
open FSharp.Compiler.SyntaxTree
open FsAst.AstRcd
open FsAst.AstCreate
open Fantomas
open EffectsPoc.Myriad

type Test() =
    let writeTmp (s: string) = 
        let file = Path.Combine(Directory.GetCurrentDirectory(), "debug.fsx") |> FileInfo
        use fs = file.OpenWrite()
        use fw = new StreamWriter(fs)
        fw.Write(s)
        file
    let parseString s =
        let file = writeTmp s
        let parsingOpts = {FSharpParsingOptions.Default with SourceFiles = [| file.FullName |] }
        let checker = FSharpChecker.Create()
        CodeFormatter.ParseAsync(file.FullName, SourceOrigin.SourceString s, parsingOpts, checker)
        |> Async.RunSynchronously
        |> Array.head |> fst
        
    let writeFormated input =
        CodeFormatter.FormatASTAsync(input, "/tmp.fsx", [], None, Fantomas.FormatConfig.FormatConfig.Default)
        |> Async.RunSynchronously

    let createTmpFile (rcd: SynModuleOrNamespaceRcd) =
        ParsedInput.CreateImplFile
            ({ ParsedImplFileInputRcd.CreateFs("") with
                   Modules = [ rcd.FromRcd ] })

    let getSourceCode decls =
        let namespaceOrModule =
            { SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong "MyriadTest") with
                  IsRecursive = false
                  Declarations = decls }

        namespaceOrModule
        |> createTmpFile
        |> writeFormated

    let toIdent s = Ident.Create s

    let testCreateHelper (a: SynTypeDefn) =
        match a.ToRcd.Repr.ToRcd with
        | SynTypeDefnReprRcd.ObjectModel o ->
            //let ident = a.ToRcd.Info.Id.Head
            //createRecordModule ident o
            o.Members
        | SynTypeDefnReprRcd.Simple _ -> []

    let createTreeFromFile fileName =
        Ast.fromFilename fileName
        |> Async.RunSynchronously
        |> Array.head
        |> fst
        
    [<Fact>]
    let ``Test round-trip`` () =
        let dir = Directory.GetCurrentDirectory() |> DirectoryInfo
        let file = dir.GetFiles("*.fs") |> Array.find(fun s -> s.Name = "TestData.fs")
        
        let ast =
            createTreeFromFile file.FullName

        let sourceCode = writeFormated ast

        let expected = "\
namespace Effects

[<EffectsPoc.Myriad.Effect>]
type ILoggingService =
    abstract Log: string -> unit
    abstract Log2: string -> Result<string, exn>
"
        Assert.Equal(expected, sourceCode)
        
    [<Fact>]
    let ``Test string -> parsed input`` () =
        let s = "\
namespace Effects

[<MyriadLib.Effect>]
type ILoggingService =
    abstract Log: string -> unit
    abstract Log2: string -> Result<string, exn>
"
        let input = parseString s 
        let sourceCode = writeFormated input
        Assert.Equal(s, sourceCode)
            
    [<Fact>]
    let ``Test open decls`` () =
        let decls =
            createOpen ([ "EnableFormat"; "Effect" ])

        let sourceCode = getSourceCode [ decls ]

        let expected = "\
namespace MyriadTest

open EnableFormat.Effect
"

        Assert.Equal(expected, sourceCode)


    [<Fact>]
    let ``Test create type`` () =
        let sourceCode =
            getSourceCode [ createType "LoggingProvider" "Log" "ILoggingService" ]

        let expected = "\
namespace MyriadTest

type LoggingProvider =
    abstract Log: ILoggingService
"

        Assert.Equal(expected, sourceCode)
        
    [<Fact>]
    let ``Test create effect creating functions`` () =
        let dir = Directory.GetCurrentDirectory() |> DirectoryInfo
        let file = dir.GetFiles("*.fs") |> Array.find(fun s -> s.Name = "Effects.fs")
        let ast =
            createTreeFromFile file.FullName

        let (_ns, records) = Ast.extractTypeDefn ast |> List.head
        let record = records |> List.head
        //let member' = testCreateHelper record |> List.head
        let recordMember = testCreateHelper record |> List.head
        let paramCount = parseInParamCount recordMember
        let paramType = parseInParamType recordMember |> Option.get
        Assert.Equal("unit", paramType)
        // Assert.Equal(1, paramCount)
        let name = parseMemberName recordMember

        //let sourceCode = createObjModel record |> getSourceCode

        Assert.Equal("Log", name)

        let moduleName = "LogEffects"
        let funcName = name

        let paramNames =
            [ 'a' .. 'z' ]
            |> List.take paramCount
            |> List.map string

        let lambda =
            [ createEffectFunction funcName [] "ILoggingServiceProvider" "LoggingService" ]
            |> getSourceCode

        let expected = "\
namespace MyriadTest

let log () =
    Effect.Create(fun (env: #ILoggingServiceProvider) -> env.LoggingService.Log())
"

        Assert.Equal(expected, lambda)
        ()