namespace Effects

[<EffectsPoc.Myriad.Effect>]
type ILoggingService =
    abstract Log: string -> unit
    abstract Log2: string -> Result<string, exn>
