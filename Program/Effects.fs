namespace Program

open System
open System.IO
open System.Threading.Tasks
open EffectsPoc.Myriad

[<Effect>]
type ILoggingService =
    abstract Log: string -> unit

[<Effect>]
type IConsoleService =
    abstract WriteLine: string -> Result<unit, IOException>
    abstract ReadLine: unit -> Result<string, ArgumentException>
    
    abstract Debug: unit -> Result<string, NullReferenceException>

[<Effect>]
type IPersistenceService =
    abstract Persist: 'a -> Task<int>