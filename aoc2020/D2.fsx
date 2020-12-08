open System.Text.RegularExpressions
open System.IO

let input = File.ReadAllLines("./inputs/d2_input.txt")
              |> List.ofArray

let isValidPwd p =
  let r = Regex("^(\d*)-(\d*)\s([a-z]):\s(\w*)$")
  let m = r.Matches(p).[0].Groups
  let parameters = {|min = m.[1].Value |> int;
                     max = m.[2].Value |> int;
                     char = m.[3].Value;
                     pwd = m.[4].Value |}
  let cnt = Regex.Matches(parameters.pwd, parameters.char).Count
  let valid = (parameters.min <= cnt) && (cnt <= parameters.max)
  valid

let isValidPwd2 p =
  let r = Regex("^(\d*)-(\d*)\s([a-z]):\s(\w*)$")
  let m = r.Matches(p).[0].Groups
  let parameters = {|min = m.[1].Value |> int;
                     max = m.[2].Value |> int;
                     char = m.[3].Value.[0];
                     pwd = m.[4].Value.ToCharArray() |}
  let ta = parameters.pwd.[parameters.min-1] = parameters.char
  let tb = parameters.pwd.[parameters.max-1] = parameters.char
  let valid = ta <> tb
  valid