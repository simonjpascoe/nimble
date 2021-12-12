// Learn more about F# at http://fsharp.org

open System
open Argu

type PuzzleSet = AOC
type AB = A | B

type Arguments =
  | [<Unique>] AOC of year:int * day:int * part:AB
  with
    interface IArgParserTemplate with
      member s.Usage =
        match s with
          | AOC (_) -> "Run an adventofcode.com puzzle solution"

[<EntryPoint>]
let main argv =
  let parser = ArgumentParser.Create<Arguments>()
  let results = parser.Parse argv

  printfn "Hi"
  0


