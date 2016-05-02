module Monads

open System

type Cor<'a, 's> = 's -> CorStep<'a, 's>
and CorStep<'a, 's> =
  | Done of 'a*'s
  | Yield of Cor<'a, 's>*'s

let cret x = fun s -> Done(x, s)
let rec cbind p k =
  fun s ->
    match p s with
    | Done(a, s') -> k a s'
    | Yield(p', s') -> Yield((cbind p' k), s')

type CoroutineBuilder() =
  member this.Return(x) = cret x
  member this.ReturnFrom(c) = c
  member this.Bind(p,k) = cbind p k
  member this.Zero() = cret ()
let cor = CoroutineBuilder()

let yield_ = fun s -> Yield((fun s -> Done((),s)), s)

let wait_ interval =
  let getTime = fun s -> Done(DateTime.Now, s)
  cor{
    let! t0 = getTime
    let rec wait() =
      cor{
      let! t = getTime
      let dt = (t-t0).TotalSeconds
      if dt > interval then
        return ()
      else
        return! wait()
    }
    do! wait()
  }

//or operator
let rec (.||) p k =
  fun s ->
    match p s, k s with
    | Done(pa, ps'), Done(ka, ks')    -> Done(ka, ks')
    | Done(pa, ps'), Yield(k', ks')   -> Yield(((.||) (cret pa) k'), ks')
    | Yield(p', ps'), Done(ka, ks')   -> Done(ka, ks')
    | Yield(p', ps'), Yield(k', ks')  -> Yield(((.||) p' k'), ks')

//and operator
let rec (.&&) p k =
  fun s ->
    match p s, k s with
    | Done(pa, ps'), Done(ka, ks')    -> Done(ka, ks')
    | Done(pa, ps'), Yield(k', ks')   -> Yield(((.||) (cret pa) k'), ks')
    | Yield(p', ps'), Done(ka, ks')   -> Yield(((.||) p' (cret ka)), ks')
    | Yield(p', ps'), Yield(k', ks')  -> Yield(((.||) p' k'), ks')