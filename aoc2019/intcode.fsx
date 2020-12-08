// this is mutable
type IntCode = int array

let rec eval pointer (input : IntCode) =
  let opcode = input.[pointer]
  match opcode with
    | 1  -> input.[input.[pointer+3]] <- input.[input.[pointer+1]] + input.[input.[pointer+2]]
            eval (pointer+4) input
    | 2  -> input.[input.[pointer+3]]  <- input.[input.[pointer+1]] * input.[input.[pointer+2]]
            eval (pointer+4) input
    | 99 -> input

let run1 (code : IntCode) noun verb =
  let initial = Array.copy code
  initial.[1] <- noun
  initial.[2] <- verb
  eval 0 initial |> ignore
  initial.[0]