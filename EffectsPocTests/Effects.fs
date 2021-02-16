﻿namespace Testing

open System.Threading.Tasks
open EffectsPoc.Myriad

[<Effect>]
type ILoggingService =
    abstract Log: unit -> unit

[<Effect>]
type IConsoleService =
    abstract WriteLine: string -> Result<unit, exn>
    abstract ReadLine: unit -> Result<string, exn>

[<Effect>]
type IPersistenceService =
    abstract Persist: 'a -> Task<int>