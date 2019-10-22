module Params = Params
module Intf = Intf.Sponge

module State = struct
  type 'a t = 'a array
end

let for_ n ~init ~f =
  let rec go i acc = if (i = n) then acc else go (i + 1) (f i acc) in
  go 0 init

module Make_operations (Field : Intf.Field) = struct
  let add_block ~state block =
    Array.iteri (fun i bi -> state.(i) <- Field.( + ) state.(i) bi) block

  let reduce_exn arr f =
    let n = Array.length arr in
    let rec go acc i =
      if i = n
      then acc
      else go (f arr.(i) acc) (i + 1)
    in
    go arr.(0) 1

  let map2 f xs ys =
    Array.init (Array.length xs) (fun i -> f xs.(i) ys.(i))

  let apply_matrix matrix v =
    let dotv row =
      reduce_exn (map2 Field.( * ) row v) Field.( + )
    in
    Array.map dotv matrix

  let copy = Array.copy
end

let _rounds_for_alpha_equal_11 = 11

let m = 3

let map_inplace f arr =
  for i = 0 to Array.length arr - 1 do
    arr.(i) <- f arr.(i)
  done

module Rescue (Inputs : Intf.Inputs.Rescue) = struct
  (*
   We refer below to this paper: https://eprint.iacr.org/2019/426.pdf.

I arrived at this value for the number of rounds in the following way.
As mentioned on page 34, the cost of performing the Grobner basis attack is estimated as

( (n + d) choose d ) ^ omega
where 

- omega is some number which is known to be >= 2
- n = 1 + m*N is the number of variables in the system of equations on page 3
- d is a quantity which they estimate as ((alpha - 1)(m*N + 1) + 1) / 2
- m is the state size, which we can choose
- N is the number of rounds which we can choose

In our case, `alpha = 11`, and I took `m = 3` which is optimal for binary Merkle trees.
Evaluating the above formula with these values and `N = 11` and `omega = 2` yields an attack complexity
of a little over 2^257, which if we take the same factor of 2 security margin as they use in the paper,
gives us a security level of 257/2 ~= 128.

NB: As you can see from the analysis this is really specialized to alpha = 11 and the number of rounds
should be higher for smaller alpha.
*)

  open Inputs
  include Operations
  module Field = Field

  let sbox0, sbox1 = (alphath_root, to_the_alpha)

  let block_cipher {Params.round_constants; mds} state =
    add_block ~state round_constants.(0) ;
    for_ (2 * rounds) ~init:state ~f:(fun r state ->
        let sbox = if (r mod 2 = 0) then sbox0 else sbox1 in
        map_inplace sbox state ;
        let state = apply_matrix mds state in
        add_block ~state round_constants.(r + 1) ;
        state )
end

let _rounds_full_for_alpha_equal_11 = 8

let _rounds_partial_for_alpha_equal_11 = 33

module Poseidon (Inputs : Intf.Inputs.Poseidon) = struct
  open Inputs
  include Operations
  module Field = Field

  let half_rounds_full = rounds_full / 2

  let for_ n init ~f = for_ n ~init ~f

  let block_cipher {Params.round_constants; mds} state =
    let sbox = to_the_alpha in
    let full_half start =
      for_ half_rounds_full ~f:(fun r state ->
          add_block ~state round_constants.(start + r) ;
          map_inplace sbox state ;
          apply_matrix mds state )
    in
    full_half 0 state
    |> for_ rounds_partial ~f:(fun r state ->
           add_block ~state round_constants.(half_rounds_full + r) ;
           state.(0) <- sbox state.(0) ;
           apply_matrix mds state )
    |> full_half (half_rounds_full + rounds_partial)
end

module Make (P : Intf.Permutation) = struct
  open P

  let sponge perm blocks ~state =
    Array.fold_left (fun state block ->
        add_block ~state block ; perm state ) state blocks

  let to_blocks r a =
    let n = Array.length a in
    Array.init
      ((n + r - 1) / r)
      (fun i ->
        Array.init r (fun j ->
            let k = (r * i) + j in
            if k < n then a.(k) else Field.zero ) )

  let r = m - 1

  let update params ~state inputs =
    let state = copy state in
    sponge (block_cipher params) (to_blocks r inputs) ~state

  let digest state = state.(0)

  let initial_state = Array.init m (fun _ -> Field.zero)

  let hash ?(init = initial_state) params inputs =
    update params ~state:init inputs |> digest
end
