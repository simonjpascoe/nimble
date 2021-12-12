module D4

open System.IO
open System
open System.Text.RegularExpressions

let first f (a, b) = (f a, b)
let second f (a, b) = (a, f b)

let preprocess = List.fold (fun (s1, s2) t -> if t <> "" then (s1, s2 @ [t]) else (s1 @ [s2], [])) ([], [])
                  >> fun (a,b) -> a @ [b]
                  >> List.map (fun xs -> String.Join(" ", xs))

let input4A = [
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
  "byr:1937 iyr:2017 cid:147 hgt:183cm"
  ""
  "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
  "hcl:#cfa07d byr:1929"
  ""
  "hcl:#ae17e1 iyr:2013"
  "eyr:2024"
  "ecl:brn pid:760753108 byr:1931"
  "hgt:179cm"
  ""
  "hcl:#cfa07d eyr:2025 pid:166559648"
  "iyr:2011 ecl:brn hgt:59in"
]

let input4B = [
  "eyr:1972 cid:100"
  "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
  ""
  "iyr:2019"
  "hcl:#602927 eyr:1967 hgt:170cm"
  "ecl:grn pid:012533040 byr:1946"
  ""
  "hcl:dab227 iyr:2012"
  "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
  ""
  "hgt:59cm ecl:zzz"
  "eyr:2038 hcl:74454a iyr:2023"
  "pid:3556412378 byr:2007"
  ""
  "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
  "hcl:#623a2f"
  ""
  "eyr:2029 ecl:blu cid:129 byr:1989"
  "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
  ""
  "hcl:#888785"
  "hgt:164cm byr:2001 iyr:2015 cid:88"
  "pid:545766238 ecl:hzl"
  "eyr:2022"
  ""
  "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
]

let day4a (data : string list) =
  let required = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"] |> Set.ofList
  let keys = data |> List.map (fun passport -> Regex.Matches(passport, "(\w*):") |> Seq.map (fun w -> w.Groups.[1].Value) |> Set.ofSeq)
  let valid = keys |> List.filter (fun s -> required - s |> Set.isEmpty)
  valid

let day4b (data : string list) =
  let pairs = data |> List.map (fun passport -> Regex.Matches(passport, "(\w*):([#\w]*)") |> Seq.map (fun w -> (w.Groups.[1].Value, w.Groups.[2].Value)) |> Map.ofSeq)

  let isBetween a b x = (x >= a) && (x <= b)
  let eyeColours = Set.ofList ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]

  let validations = [
    "byr", int >> isBetween 1920 2002
    "iyr", int >> isBetween 2010 2020
    "eyr", int >> isBetween 2020 2030
    "hgt", fun t -> let m = Regex.Match(t, "^(\d*)(\w*)$")
                    match m.Success with
                      | false -> false
                      | true  -> let v = m.Groups.[1].Value |> int
                                 match m.Groups.[2].Value with
                                   | "cm" -> isBetween 150 193 v
                                   | "in" -> isBetween 59 76 v
                                   | _ -> false
    "hcl", fun t -> Regex.Match(t, "^#[0-9a-f]{6}$").Success
    "ecl", fun t -> Set.contains t eyeColours
    "pid", fun t -> Regex.Match(t, "^\d{9}$").Success
  ]

  // everything must be there and all must be valid

  let isValid p =
    validations |> List.map (fun (k, fn) -> Map.tryFind k p |> function | Some v -> fn v | None -> false)
                |> List.fold (&&) true
  List.map isValid pairs |> List.filter id |> List.length