module Field0 = Field

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

  let () = assert (C.curve = Bn128)

  module Group = Group.BabyJubJub(Field)

  module Schnorr = struct
    module T = Signature.Make_signer(struct
        module Hash = Hash
        module Group = Group
        module Scalar = Group.Scalar
        module Field = struct
          include Field
          let is_even t = not (Field0.BigInt.testBit t 0)

          let size_in_bits = sizeInBits
        end

        module Bool = struct
          type t = bool
          let (&&) = (&&)
        end
      end)

    let sign = T.sign

    module Signature = struct
      include T.Signature
      let check = T.check

      let toJSON ((rx, s) : t) =
        Obj.magic [| Field.toString rx; Group.Scalar.toString s |]
    end

    module PublicKey = struct
      type t = T.Public_key.t

      let ofPrivateKey = T.Public_key.of_private_key
      let toJSON : t -> _ = Group.toJSON
    end

    module PrivateKey = struct
      type t = T.Private_key.t

      (* The type is a slight fib... *)

      type buffer

      let bufferGet : buffer -> int -> int = [%raw "(b, i) => b[i]"]

      let () = [%raw "(global.crypto || (global.crypto = require('crypto')))"]

      let randomBytes : int -> buffer = [%raw "(x) => crypto.randomBytes(x)"]

      let create () =
        let n = 31 in
        let arr = randomBytes n in
        let eight = Field0.BigInt.ofInt 8 in
        let rec go acc i =
          if i = n
          then acc
          else (
            let acc = Field0.BigInt.shiftLeft acc eight in
            go 
              Field0.BigInt.(logOr acc (ofInt (bufferGet arr i)))
              (i + 1)
          )
        in
        go (Field0.BigInt.ofInt 0) 0

      let toJSON t : < > Js.t =
        Obj.magic (Field0.BigInt.toString t)
    end
  end

  module MerkleTree = Merkle_tree.Make(struct
      type t = Hash.t
      let equal = Field.equal
      let hash arr = Hash.hash arr
    end)
end

let make_obj (type field) (module M : Intf.S with type Field.t = field)
  = 
  let schnorr =
    let open M.Schnorr in
    [%bs.obj {
      _PrivateKey= {
        create=PrivateKey.create;
        toJSON=PrivateKey.toJSON;
      };
      _PublicKey= {
        ofPrivateKey=PublicKey.ofPrivateKey;
        toJSON=PublicKey.toJSON;
      };
      _Signature= {
        check=Signature.check;
        toJSON=Signature.toJSON;
      };
      sign=sign;
    }]
  in
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
      toString=toString;
      ofBits=ofBits;
      testBit=testBit;
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
      depth=M.MerkleTree.depth;
      root=M.MerkleTree.root;
      _MembershipProof= membership_proof;
    }]
  in
  Obj.magic
  [%bs.obj {
    _Field= field;
    _Hash = hash;
    _MerkleTree = merkle_tree;
    _Schnorr=schnorr;
    _Group={
      toAffine=M.Group.toAffine;
    }
  }]

module Bn128 = Make(struct let curve = Curve.Bn128 end)
let bn128 = make_obj (module Bn128)
