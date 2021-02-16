namespace EffectsPoc

[<AutoOpen>]
module EffectBuilder =
    open Effect

    type EnvBuilder() =
        member __.Bind(env: Effect<'r, 'a, 'e>, f: 'a -> Effect<'r, 'b, 'e>) = env >>= f        
       // member __.Bind(env: Effect<'r, 'a, 'e1>, f: 'a -> Effect<'r, 'b, 'e2>, [<OptionalArgument>]_a: int) = bind2 f env
        member __.Return<'r, 'a, 'e>(value: 'a) = pureE<'r, 'a, 'e> value
        member __.ReturnFrom(value) = value

        member __.Yield(value) = pure' value

        member __.Zero() = Effect.pure' ()

        member __.Combine(s1: Effect<'s, unit, 'e>, s2: Effect<'s, 'a, 'e>) = Effect.map2 (fun _ -> id) s1 s2

        member __.Delay(f) = f ()

        member __.For(xs: seq<'a>, f: 'a -> Effect<'s, 'a, 'e>) = xs |> Seq.map f

        member __.Run(value) = value

        member __.TryFinally(body, compensation) =
            try
                __.ReturnFrom(body ())
            finally
                compensation ()

        member __.Using(disposable: #System.IDisposable, body) =
            let body' = fun () -> body disposable
            __.TryFinally(body', (fun () -> match disposable with | null -> () | disp -> disp.Dispose()))

    let env = EnvBuilder()