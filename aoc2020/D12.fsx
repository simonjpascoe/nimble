type P = int * int * int


let input12A = [
  "F10"
  "N3"
  "F7"
  "R90"
  "F11"
]

let preprocess (input : string list) =
  let p (st : string) = (st.[0], st.Substring(1) |> int)
  input |> List.map p

let a2d = function | 0 -> 'N'
                   | 90  | -270 -> 'E'
                   | 180 | -180 -> 'S'
                   | 270 | -90 -> 'W'
                   | x -> failwith (sprintf "%A" x)

let day12a (input : string list) =
  let position0 = (0,0,90) : P
  let cmds = preprocess input
  let rec step (pos : P) cmd =
    let c, q = cmd
    let (x,y,d) = pos
    match c with
      | 'N' -> (x, y + q, d)
      | 'E' -> (x + q, y, d)
      | 'S' -> (x, y - q, d)
      | 'W' -> (x - q, y, d)
      | 'F' -> step pos (a2d d, q)
      | 'L' -> (x, y, (d - q) % 360)
      | 'R' -> (x, y, (d + q) % 360)

  let positionN = cmds |> List.fold step position0
  positionN |> fun (a, b, _) -> abs a + abs b

let rotateWaypointR (wx, wy) d =
  match d with
    | 0   -> (wx, wy)
    | 180 -> (-wx, -wy)
    | 90  -> (wy, -wx)
    | 270 -> (-wy, wx)
    | x -> failwith (sprintf "%A" x)

let day12b (input : string list) =
  let waypoint0 = (10,1)  // (relative to ship)
  let position0 = (0,0)
  let cmds = preprocess input
  let rec step (waypoint, position) cmd =
    let c, q = cmd
    let (wx, wy) = waypoint
    let (sx, sy) = position
    match c with
      | 'N' -> (wx, wy+q), position
      | 'E' -> (wx+q, wy), position
      | 'S' -> (wx, wy-q), position
      | 'W' -> (wx-q, wy), position
      | 'F' -> waypoint, (sx + q * wx, sy + q * wy)
      | 'L' -> rotateWaypointR waypoint (360-q), position
      | 'R' -> rotateWaypointR waypoint q, position

  let positionN = cmds |> List.fold step (waypoint0, position0)
  positionN |> snd |> fun (a, b) -> abs a + abs b


