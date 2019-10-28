module BigInt = struct
  type t

  type op = t -> t -> t
  type mutop = t -> t -> unit

  type cmp = t -> t -> bool

  let (>) : cmp = [%raw "(a, b) => a > b"]
  let (>=) : cmp = [%raw "(a, b) => a >= b"]
  let (<) : cmp = [%raw "(a, b) => a < b"]
  let (<=) : cmp = [%raw "(a, b) => a <= b"]

  let zero : t = [%raw "BigInt(0)"]
  let one : t = [%raw "BigInt(1)"]

  let add : op = [%raw "(a, b) => a + b"]
  let sub : op = [%raw "(a, b) => a - b"]
  let mul : op = [%raw "(a, b) => a * b"]
  let div : op = [%raw "(a, b) => a / b"]
  let rem : op = [%raw "(a, b) => a % b"]

  let shiftLeft : op = [%raw "(a, b) => a << b"]
  let shiftRight : op = [%raw "(a, b) => a >> b"]

  let logAnd : op = [%raw "(a, b) => a & b"]
  let logOr : op = [%raw "(a, b) => a | b"]

  let ofString : string -> t = [%raw "(s) => BigInt(s)"]
  let ofInt : int -> t = [%raw "(n) => BigInt(n)"]

  let ofBits bits =
    let n = Array.length bits in
    let rec go acc i = 
      if i = n
      then acc
      else 
        let acc =
          if bits.(i)
          then logOr acc (shiftLeft one (ofInt i))
          else acc
        in
        go acc (i + 1)
    in
    go zero 0

  let toString : t -> string = [%raw "(s) => s.toString()"]

  let one = ofInt 1

  let testBit t i =
    ((=) one (logAnd one (shiftRight t (ofInt i)) ))

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div

  let (=) : cmp = [%raw "(a, b) => a === b"]
end

module Params = struct
  let fieldSize : Curve.t -> BigInt.t = function
    | Curve.Bn128 ->
      BigInt.ofString "21888242871839275222246405745257275088548364400416034343698204186575808495617"

  let sizeInBits : Curve.t -> int = function
    | Curve.Bn128 -> 254
end

module Make0 (C : sig val size : BigInt.t end) = struct
  open C

  type t = BigInt.t

  let size = size

  let toString : t -> string = BigInt.toString

  let add x y =
    let open BigInt in
    let z = add x y in
    if z >= size
    then z - size
    else z

  let sub x y =
    let open BigInt in
    let z = sub x y in
    if z < zero
    then z + size
    else z

  let mul x y = BigInt.(rem (mul x y) size)

  let invert a =
    let open BigInt in
    let rec go t newt r newr =
      if newr = zero
      then (t, r)
      else
        let quotient = r / newr in
        go newt (t - quotient * newt)
          newr (r - quotient * newr)
    in
    let t, r = go zero one size a in
    if r > one
    then failwith "a is zero"
    else if t < zero
    then t + size
    else t

  let inv = invert

  let equal = BigInt.(=)

  let div x y = mul x (invert y)

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div

  let one = BigInt.one
  let zero = BigInt.zero

  let negate x =
    if equal x zero
    then zero
    else BigInt.sub size x

  let ofString x = BigInt.(rem (ofString x) size)
  let ofInt = BigInt.ofInt

  let ofBits = BigInt.ofBits

  let ofBigInt x = BigInt.(rem x size)

  let square x = x * x

  let testBit : t -> int -> bool = BigInt.testBit

  let ( = ) = equal
end

module Make (C : sig val curve : Curve.t end) = struct
  include Make0(struct let size = Params.fieldSize C.curve end)
  let sizeInBits = Params.sizeInBits C.curve
end

