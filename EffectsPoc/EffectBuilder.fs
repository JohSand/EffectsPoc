namespace EffectsPoc

[<AutoOpen>]
module EffectBuilder =
    open Effect

    type EnvBuilder() =
        member __.Bind(env: Effect<'r, 'a>, f: 'a -> Effect<'r, 'b>) = env >>= f
        member __.Return<'r, 'a>(value: 'a) = pure'<'r, 'a> value
        member __.ReturnFrom(value) = value

        member __.Yield(value) = Effect.pure' value

        member __.Zero() = Effect.pure' ()

        member __.Combine(s1: Effect<'s, unit>, s2: Effect<'s, 'a>) = Effect.map2 (fun _ -> id) s1 s2

        member __.Delay(f) = f ()

        member __.For(xs: seq<'a>, f: 'a -> Effect<'s, 'a>) = xs |> Seq.map f

        member __.Run(value) = value

        member __.TryFinally(body, compensation) =
            try
                __.ReturnFrom(body ())
            finally
                compensation ()

        member __.Using(disposable: #System.IDisposable, body) =
            let body' = fun () -> body disposable
            __.TryFinally(body', (fun () -> match disposable with | null -> () | disp -> disp.Dispose()))
