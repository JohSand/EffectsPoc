// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
namespace Program
open System
open System.IO
open System.Threading.Tasks
open EffectsPoc
open Ply
open FSharp.Control.Tasks
module Main =
    
    type TestEnv() =

        interface IConsoleServiceProvider with
            member val ConsoleService = { new IConsoleService with
                                       member __.WriteLine s = Console.WriteLine(s) |> Ok
                                       member __.ReadLine() = Ok (Console.ReadLine())
                                       
                                       member __.Debug() = Ok("")
                                       }

        interface ILoggingServiceProvider with
            member val LoggingService = { new ILoggingService with
                                       member __.Log(s) = Console.WriteLine(s) }

        interface IPersistenceServiceProvider with
            member val PersistenceService = { new IPersistenceService with
                                           member __.Persist s =
                                               task {
                                                   do! Task.FromException(IOException("I got here"))
                                                   return 69
                                               } }
        
// Define a function to construct a message to print
    let from whom =
        sprintf "from %s" whom

    [<EntryPoint>]
    let main argv =
        let message = from "F#" // Call the function
        let myEnv = TestEnv()
        let a = env {
            
            let! e = ConsoleService.writeLine "1"
            let! x = PersistenceService.persist ""
            let! b = ConsoleService.readLine ()
            let! c = LoggingService.log ""
            let! d = ConsoleService.debug ()

            return "1"
        }
        
        //prove existence of implementation.
        let res = task {
                    return! Effect.run myEnv a
                }

        let res' =
            res
            |> Async.AwaitTask
            |> Async.RunSynchronously
        
        match res' with
        | Ok o -> printfn "%s" o |> ignore
        | Error e -> match e with
                        | Choice1Of3 io -> io |> ignore
                        | Choice2Of3 io -> io |> ignore
                        | Choice3Of3 io -> io |> ignore

        printfn "Hello world %s" message
        0 // return an inte ger exit code