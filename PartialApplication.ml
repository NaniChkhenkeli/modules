type tree = Empty 
          | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop


let crawl cmds tree =
  let rec impl cmds stack tree =
    match cmds, stack, tree with
    | [], s, t -> false, [], s, t
    | Up::cs, ss, t -> true, cs, ss, t
    | Left::cs, ss, Node (x, l, r) ->
      let u, nc, ns, nt = impl cs ss l in
      if u then impl nc ns (Node (x, nt, r)) else u, nc, ns, Node (x, nt, r)
    | Right::cs, ss, Node (x, l, r) ->
      let u, nc, ns, nt = impl cs ss r in
      if u then impl nc ns (Node (x, l, nt)) else u, nc, ns, Node (x, l, nt)
    | New v::cs, ss, _ -> impl cs ss (Node (v, Empty, Empty))
    | Delete::cs, ss, _ -> impl cs ss Empty
    | Push::cs, ss, t -> impl cs (t::ss) t
    | Pop::cs, s::ss, _ -> impl cs ss s
    | _ -> failwith "unexpected"
  in
  let _,_,_, t = impl cmds [] tree in t



(* soccer game  *)
type result = Win (* 3 points *)
| Draw (* 1 point *)
| Loss (* 0 points *)

let series g p =
if g = 0 then (if p = 0 then [[]] else [])
else map (fun r -> Win::r) (series (g-1) (p-3))
@ map (fun r -> Draw::r) (series (g-1) (p-1))
@ map (fun r -> Loss::r) (series (g-1) p)



(* alternative 2: *)
let series g p =
let rec impl g p s acc =
if g = 0 then (if p = 0 then s::acc else acc)
else impl (g-1) p (Loss::s) acc
|> impl (g-1) (p-1) (Draw::s)
|> impl (g-1) (p-3) (Win::s)
in
impl g p [] []


  