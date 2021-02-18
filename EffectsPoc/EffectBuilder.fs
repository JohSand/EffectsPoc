namespace EffectsPoc

open System
open System.Runtime.InteropServices

[<AutoOpen>]
module EffectBuilder =
    open Effect
    open FSharp.Control.Tasks.Affine.Unsafe
    
    type EnvBuilder() =
        member __.Bind(env: Effect<'r, 'a, 'e>, f: 'a -> Effect<'r, 'b, 'e>) = env >>= f   
        
        member __.Bind(
                        eff: Effect<'r, 'a, 'e1>,
                        f: 'a -> Effect<'r, 'b, 'e2>,
                        [<ParamArray>]_mostGenericCase: int array) =
                        Eff(fun env ->
                                uply {
                                    match! run env eff with
                                    | Ok a ->
                                            let! b2 = run env (f a)
                                            return b2 |> Result.mapError (Choice2Of2)
                                    | Error e -> return Error (Choice1Of2 e)
                                })

        member __.Bind(
                        eff: Effect<'r, 'a, 'e1>
                        , f: 'a -> Effect<'r, 'b, Choice<'e2, 'e3>>
                        , [<OptionalArgument>]_flattenChoices:byte
                                                                      ) =
                        Eff(fun env ->
                                uply {
                                    match! run env eff with
                                    | Ok a ->
                                            let! b2 = run env (f a)
                                            let deb = b2 |> Result.mapError (function
                                                | Choice1Of2 a -> Choice2Of3 a
                                                | Choice2Of2 b -> Choice3Of3 b)
                                            return deb
                                    | Error e -> return Error (Choice1Of3 e)            
                                })
                                            
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