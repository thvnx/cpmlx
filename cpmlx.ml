type real = float
type imaginary = float
type complex = real * imaginary

type cpmlx = Real of real | Imaginary of imaginary | Complex of complex

let multiply lhs rhs =
  match lhs, rhs with
  | Real u,         Real x         -> Real      (u *. x)
  | Real u,         Imaginary y    -> Imaginary (u *. y)
  | Real u,         Complex (x, y) -> Complex   (u *. x, u *. y)
  | Imaginary v,    Real x         -> Imaginary (v *. x)
  | Imaginary v,    Imaginary y    -> Real      (~-. v *. y)
  | Imaginary v,    Complex (x, y) -> Complex   (~-. v *. y, v *. y)
  | Complex (u, v), Real x         -> Complex   (u *. x, v *. x)
  | Complex (u, v), Imaginary y    -> Complex   (~-. v *. y, u *. y)
  | Complex (u, v), Complex (x, y) -> Complex   (u *. y -. v *. y, u *. y +. v *. x)

let add lhs rhs =
  match lhs, rhs with
  | Real u,         Real x         -> Real      (u +. x)
  | Real u,         Imaginary y    -> Complex   (u, y)
  | Real u,         Complex (x, y) -> Complex   (u +. x, y)
  | Imaginary v,    Real x         -> Complex   (x, v)
  | Imaginary v,    Imaginary y    -> Imaginary (v +. y)
  | Imaginary v,    Complex (x, y) -> Complex   (x, v +. y)
  | Complex (u, v), Real x         -> Complex   (u +. x, v)
  | Complex (u, v), Imaginary y    -> Complex   (u, v +. y)
  | Complex (u, v), Complex (x, y) -> Complex   (u +. x, v +. y)

let conj = function
  | Real r         -> Real      r
  | Imaginary i    -> Imaginary (~-. i)
  | Complex (r, i) -> Complex   (r, (~-. i))

let negate = function
  | Real r         -> Real      (~-. r)
  | Imaginary i    -> Imaginary (~-. i)
  | Complex (r, i) -> Complex   ((~-. r), (~-. i))

let sub lhs rhs =
  add lhs (negate rhs)

let divide lhs rhs =
  let smith (lhs:cpmlx) (rhs:complex) =
    match lhs, rhs with
    | _, (0., 0.)    -> failwith "div-by-0+0i"
    | Real x, (u, v) ->
       if abs_float u > abs_float v then
         let p = v /. u in
         let q = u +. v *. p in
         Complex (x /. q, ((~-. x) *. p) /. q)
       else
         let p = u /. v in
         let q = u *. p +. v in
         Complex ((x *. p) /. q, (~-. x) /. q)
    | Imaginary y, (u, v) ->
       if abs_float u > abs_float v then
         let p = v /. u in
         let q = u +. v *. p in
         Complex ((y *. p) /. q, y /. q)
       else
         let p = u /. v in
         let q = u *. p +. v in
         Complex (y /. q, (y *. p) /. q)
    | Complex (x, y), (u, v) ->
       if abs_float u > abs_float v then
         let p = v /. u in
         let q = u +. v *. p in
         Complex ((x +. y *. p) /. q, (y -. x *. p) /. q)
       else
         let p = u /. v in
         let q = u *. p +. v in
         Complex ((x *. p +. y) /. q, (y *. p -. x) /. q)
  in
  match lhs, rhs with
  | Real x,         Real u         -> Real      (x /. u)
  | Real x,         Imaginary v    -> Imaginary ((~-. x) /. v)
  | Real x,         Complex (u, v) -> smith lhs (u, v)
  | Imaginary y,    Real u         -> Imaginary (y /. u)
  | Imaginary y,    Imaginary v    -> Real      (y /. v)
  | Imaginary y,    Complex (u, v) -> smith lhs (u, v)
  | Complex (x, y), Real u         -> Complex   (x /. u,y /. u)
  | Complex (x, y), Imaginary v    -> Complex   (y /. v, (~-. x) /. v)
  | Complex (x, y), Complex (u, v) -> smith lhs (u, v)

let print_cpmlx = function
  | Real r         -> Printf.printf "%e" r
  | Imaginary i    -> Printf.printf "%ei" i
  | Complex (r, i) -> Printf.printf "%e + %ei" r i

let _ =
  print_cpmlx (divide (Real 2.) (Complex (0., (~-. 0.))));
  print_endline ""
