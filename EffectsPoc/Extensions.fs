namespace EffectsPoc
open FSharp.Control
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe
open FSharp.Control.Tasks
open Ply
open Ply.TplPrimitives

[<AutoOpen>]
module Extensions =
    type Builders.Unsafe.UnsafePlyBuilder with
        member this.For(seq: AsyncSeq<'a>, body: 'a -> Ply<unit>) : Ply<unit> =
            let enumeratorLoop (e: IAsyncEnumerator<_>) = uply {
                let mutable hasMore = true
                while hasMore do
                    let! next = e.MoveNext()
                    if Option.isSome next then
                        do! Option.get next |> body
                    else
                        hasMore <- false               
            }
            using (seq.GetEnumerator()) enumeratorLoop

