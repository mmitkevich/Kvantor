module Kvant.Behavior

open System.Reactive.Subjects

let create<'T> v = new BehaviorSubject<'T>(v)
let update<'T> (f:'T->unit) (b:BehaviorSubject<'T>) = f(b.Value); b.OnNext(b.Value)
