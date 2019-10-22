module Make(C : sig val curve : Curve.t end) : Intf.S = struct
  module Field = Field.Make(C)

  module Hash = struct
    let params = Params.map Field.ofString Sponge_params.Bn128.t

    type t = Field.t

    module Rounds = struct
      let rounds_full = 8

      let rounds_partial = 83
    end

    module Inputs = struct
      include Rounds
      module Field = Field

      let alpha = 5

      let to_the_alpha x =
        let open Field in
        let x4 = square (square x) in
        x * x4

      module Operations = Sponge.Make_operations (Field)
    end

    include Sponge.Make (Sponge.Poseidon (Inputs))

    (* TODO: Make javascripty *)
    let update ~state = update ~state params

    (* TODO: have optional init variable *)
    let hash = hash ?init:None params
  end

  module MerkleTree = Merkle_tree.Make(struct
      type t = Hash.t
      let equal = Field.equal
      let hash arr = Hash.hash arr
    end)
end

let make_obj (type field) (module M : Intf.S with type Field.t = field)
    : field Intf.Obj.obj
  = 
  let field =
    let open M.Field in
    [%bs.obj {
      equal= equal;
      add=add;
      mul=mul;
      sub=sub;
      div=div;
      ofInt=ofInt;
      ofString=ofString;
      square=square;
      invert=invert;
      negate=negate;
      zero=zero;
      one=one;
    }]
  in
  let hash : field Intf.Obj.Hash.obj =
    [%bs.obj {
      hash= M.Hash.hash;
    }]
  in
  let membership_proof : field Intf.Obj.Membership_proof.obj =
    let to_obj { M.MerkleTree.MembershipProof.index; path }
        : field Intf.Obj.Membership_proof.t
      =
      [%bs.obj
        { index= index; path=path }]
    in
    let of_obj (p : field Intf.Obj.Membership_proof.t) =
      { M.MerkleTree.MembershipProof.index = p##index; path=p##path }
    in
    Obj.magic
    [%bs.obj
        {
          create=(fun t i -> to_obj (M.MerkleTree.MembershipProof.create t i));
        check=(fun p r h -> M.MerkleTree.MembershipProof.check (of_obj p) r h );
      }]
  in
  let merkle_tree : field Intf.Obj.Merkle_tree.obj =
    Obj.magic
    [%bs.obj{
      ofArray= M.MerkleTree.ofArray;
      _MembershipProof= membership_proof;
    }]
  in

  [%bs.obj {
    _Field= field;
    _Hash = hash;
    _MerkleTree = merkle_tree
    ;
  }]

module Bn128 = Make(struct let curve = Curve.Bn128 end)
let bn128 = make_obj (module Bn128)
