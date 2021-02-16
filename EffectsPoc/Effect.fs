namespace EffectsPoc

open System.Collections.Generic
open FSharp.Control
open FSharp.Control.Tasks.Affine.Unsafe
open System.Threading.Tasks
open Ply


type AsyncResult<'a, 'e> = Ply<Result<'a, 'e>>

[<NoEquality; NoComparison>]
type Effect<'r, 'a, 'e> = private Eff of ('r -> AsyncResult<'a, 'e>)

//[<NoEquality; NoComparison>]
//type Effect2<'r, 'a, 'e> = private Eff2 of ('r -> Ply<Result<'a, 'e>>)


module Effect =
    let run env (Eff e) = e env
    
    //let run2 env (Eff2 e) = e env
    
    let pure'<'r, 'a> (a: 'a) : Effect<'r, 'a, unit>  =
        Eff(fun (_env: 'r) -> Ok a |> Ply)
    
    let pureE<'r, 'a, 'e> (a: 'a) : Effect<'r, 'a, 'e>  =
        Eff(fun (_env: 'r) -> Ok a |> Ply)
    
    let pureT<'r, 'a> (a: Task<'a>) =
        Eff(fun (r: 'r) ->
                uply {
                    try
                        let! a' = a
                        return Result<'a, _>.Ok a'
                    with e -> return Result<'a, _>.Error e
                })
    let inline ply (x: 'a -> ^taskLike)(a: 'a) =
        uply {
            let! a' = x a
            return Ok a'
        }
        
    let bind (f: 'a -> Effect<'r,'b, 'e>) (env: Effect<'r, 'a, 'e>) =
        Eff(fun s ->
                uply {
                    match! run s env with
                    | Ok a -> return! run s (f a)
                    | Error e -> return Error e
                })

    let bind2 (f: 'a -> Effect<'r,'b, 'e2>) (eff: Effect<'r, 'a, 'e1>) =
        Eff(fun env ->
                uply {
                    match! run env eff with
                    | Ok a ->
                            let! b2 = run env (f a)
                            return b2 |> Result.mapError (Choice2Of2)
                    | Error e -> return Error (Choice1Of2 e)
                })
        
    let inline (>>=) m f = bind f m

    let map (f: 'a -> 'b) (env: Effect<'r, 'a, 'e>) =
        Eff(fun s ->
                uply {
                    match! run s env with
                    | Ok a -> return Ok(f a)
                    | Error e -> return Error e
                })

    let map2 f s1 s2 =
        bind (fun a -> map (f a) s2) s1
        
        
type Effect =
    static member Create(x: ('a -> 'b), [<OptionalArgument>]_a: int) : Effect<'a, 'b, unit> = Eff(x >> Ok >> Ply)
        
    static member Create(x: ('a -> AsyncResult<'b, 'e>)) = Eff(x)

    static member Create(x: ('a -> Result<'b, exn>)) = Eff(x >> Ply)

    static member Create(x: ('a -> Task<'b>)) = Eff(Effect.ply x)
    
    static member Create(x: ('a -> Async<'b>)) = Eff(Effect.ply x)
    
    static member Create(x: ('a -> ValueTask<'b>)) = Eff(Effect.ply x)
    