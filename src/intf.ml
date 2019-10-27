type json = < > Js.t

module Sponge = struct
  module type Field = sig
    type t

    val zero : t

    val ( * ) : t -> t -> t

    val ( + ) : t -> t -> t
  end

  module type Operations = sig
    module Field : sig
      type t
    end

    val add_block : state:Field.t array -> Field.t array -> unit

    val apply_matrix : Field.t array array -> Field.t array -> Field.t array

    val copy : Field.t array -> Field.t array
  end

  module Inputs = struct
    module type Common = sig
      module Field : sig
        type t

        val zero : t
      end

      val to_the_alpha : Field.t -> Field.t

      module Operations : Operations with module Field := Field
    end

    module type Rescue = sig
      include Common

      val rounds : int

      val alphath_root : Field.t -> Field.t
    end

    module type Poseidon = sig
      include Common

      val rounds_full : int

      val rounds_partial : int
    end
  end

  module type Permutation = sig
    module Field : sig
      type t

      val zero : t
    end

    val add_block : state:Field.t array -> Field.t array -> unit

    val copy : Field.t array -> Field.t array

    val block_cipher : Field.t Params.t -> Field.t array -> Field.t array
  end
end

module type S = sig
  module Field : sig
    (* Arithmetic and other operations on a prime order field. *)
    type t

    val (=) : t -> t -> bool
    val equal : t -> t -> bool

    val ( * ) : t -> t -> t
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( / ) : t -> t -> t

    val mul : t -> t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val div : t -> t -> t

    val negate : t -> t
    val invert : t -> t
    val square : t -> t

    val one : t
    val zero : t

    val ofString : string -> t
    val ofInt : int -> t

    val toString : t -> string
  end

  module Hash : sig
    type t = Field.t

    (* Hash an array of elements using the Poseidon hash function.
       This hash function is intended to be an ideal cryptographic hash function.
    *)
    val hash : Field.t array -> t
  end

  module MerkleTree : sig
    type 'a t  = ('a, Hash.t) Merkle_tree.t
    type 'a merkle_tree = 'a t

    module MembershipProof : sig
      type t =
        { index : int
        ; path  : Hash.t array
        }

      (* Create a membership proof for a leaf at the given index. *)
      val create : 'a merkle_tree -> int -> t

      (* Check a membership proof. *)
      val check
        : t
        -> 
        Hash.t (* root hash *)
        ->  
        Hash.t (* element hash *)
        -> bool
    end

    (* Create a binary merkle tree with values of type 'a at the leaves.
      The first argument is for hashing the leaves.
      The second argument is a default value which will be used to padt
      the given array to a power of 2 length. *)
    val ofArray : ('a -> Hash.t) -> 'a -> 'a array -> 'a t
  end

  module Schnorr : sig
    module PrivateKey : sig
      type t

      (* Generate a private key using randomness from node's crypto API. *)
      val create : unit -> t

      val toJSON : t -> json
    end

    module PublicKey : sig
      type t

      (* Derive the public key corresponding to a given private key. *)
      val ofPrivateKey : PrivateKey.t -> t
      val toJSON : t -> json
    end

    module Signature : sig
      type t

      (* Verify a signature against the given public key and message. *)
      val check : t -> PublicKey.t -> Field.t array -> bool
      val toJSON : t -> json
    end

    (* Sign a message. *)
    val sign :
      PrivateKey.t -> Field.t array -> Signature.t
  end
end

module Obj = struct
  module Field = struct
    type 'field obj =
      < equal : 'field -> 'field -> bool
      ; mul : 'field -> 'field -> 'field
      ; add : 'field -> 'field -> 'field
      ; sub : 'field -> 'field -> 'field
      ; div : 'field -> 'field -> 'field
      ; negate : 'field -> 'field
      ; square : 'field -> 'field
      ; invert : 'field -> 'field
      ; one : 'field
      ; zero : 'field
      ; ofString : string -> 'field
      ; ofInt : int -> 'field > Js.t
  end

  module Hash = struct
    type 'field obj =
    < hash : 'field array -> 'field > Js.t
  end

  module Membership_proof = struct
    type 'hash t =
      < index : int; path : 'hash array > Js.t

    type 'hash obj =
      < create : 'a.
          ('a, 'hash) Merkle_tree.t
            -> int -> 'hash t
      ; check
        : 'hash t -> 'hash -> 'hash -> bool
      > Js.t
  end

  module Merkle_tree = struct
    type ('a, 'hash) t = ('a, 'hash) Merkle_tree.t

    type 'hash obj =
      < ofArray : 'a. ('a -> 'hash) -> 'a -> 'a array -> ('a, 'hash) t
      ; _MembershipProof : 'hash Membership_proof.obj
      > Js.t
  end

  module Schnorr = struct
    module Private_key = struct
      type t
    end
  end

  (* TODO: Everything needs to/of JSON on it. (Or maybe just to) *)
  type 'field obj =
    < _MerkleTree : 'field Merkle_tree.obj
    ; _Field : 'field Field.obj
    ; _Hash : 'field Hash.obj
    > Js.t
end
