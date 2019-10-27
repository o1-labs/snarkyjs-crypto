type 'a params = {a: 'a; b: 'a; one: 'a * 'a}

let bn128 =
  { a=
      "7296080957279758407415468581752425029516121466805344781232734728849116493472"
  ; b=
      "16213513238399463127589930181672055621146936592900766180517188641980520820846"
  ; one=
      ( "6142832666126200108502958025802235821181485456421382423166796358396261897243"
      , "11248300941377020445838278434488928391563663598577146687858078672191633663164"
      ) }

module BabyJubJub (Fq : sig
  type t
        
  val equal : t -> t -> bool

  val zero : t

  val one : t

  val inv : t -> t

  val ( * ) : t -> t -> t

  val square : t -> t

  val negate : t -> t

  val ( - ) : t -> t -> t

  val ( + ) : t -> t -> t

  val ofString : string -> t
  val toString : t -> string
  end ) = struct

  module Scalar = struct
    module T = Field.Make0(
        struct
          let size =
            Field.BigInt.ofString
              "2736030358979909402780800718157159386076813972158567259200215660948447373041"
        end)

    let size_in_bits = 251

    let num_bits _ = size_in_bits

    let test_bit = Field.BigInt.testBit

    (* Just an inclusion *)
    let to_field t = t

    (* The base field is bigger. This projects *)
    let of_field t = T.ofBigInt t

    include T
  end

  module Params = struct
    let a = Fq.ofString bn128.a

    let b = Fq.ofString bn128.b
  end

  include Elliptic_curve.Make
            (Scalar)
            (Fq)
            (Params)

  let map_pr f (x, y) = (f x, f y)

  let one = of_affine (map_pr Fq.ofString bn128.one )

  let scale ?init t s =
    let p = scale t s in
    match init with None -> p | Some init -> p + init

  let add_exn = ( + )

  let toJSON ({ x; y; z } : t) : <  > Js.t =
    Obj.magic
    [%bs.obj {
      x= Fq.toString x;
      y= Fq.toString y;
      z= Fq.toString z;
    }]
end
