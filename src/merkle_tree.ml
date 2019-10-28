module Tree = struct
  type ('a, 'h) t =
    | Element of 'a
    | Node of 'h * ('a, 'h) t * ('a, 'h) t
end

  type ('a, 'h) t0 =
    { tree : ('a, 'h) Tree.t
    ; hash_element : 'a -> 'h
    ; depth: int
    }

module Make
    (Hash : sig
       type t

       val equal : t -> t -> bool

       val hash : t array -> t
     end) = struct

  type 'a t = ('a, Hash.t) t0

  module Tree0 = Tree

  module Tree = struct
    type 'a t = ('a, Hash.t) Tree.t
  end

  type 'a merkle_tree = 'a t

  let hash_tree hash_element : _ Tree.t -> Hash.t = function
    | Element x -> hash_element x
    | Node (h, _, _) -> h

  let root t = hash_tree t.hash_element t.tree

  let depth t = t.depth

  let merge ~height:_ l r = Hash.hash [| l; r |]

  module MembershipProof = struct
    type t =
      { index : int
      ; path : Hash.t array }

    let ith_bit idx i = (idx lsr i) land 1 = 1

    let path {tree; depth; hash_element} idx =
      let hash = hash_tree hash_element in
      let rec go acc i tree =
        if i < 0 then acc
        else
          match (tree : _ Tree.t) with
          | Element _ ->
              failwith "Sparse_ledger.path: Bad depth at index"
          | Node (_, l, r) ->
              let go_right = ith_bit idx i in
              if go_right then go ((hash l) :: acc) (i - 1) r
              else go ((hash r) :: acc) (i - 1) l
      in
      Array.of_list (go [] (depth - 1) tree)

    let create t index = { index; path= path t index }

    let implied_root entry_hash addr0 path0 =
      let depth = Array.length path0 in
      let path0 = Array.to_list path0 in
      let rec go height acc path =
        let b = (addr0 lsr height) land 1 = 1 in
        match path with
        | [] ->
            acc
        | h :: hs ->
            let l = if b then h else acc and r = if b then acc else h in
            let acc' = merge ~height l r in
            go (height + 1) acc' hs
        | _ ->
            failwith
              "Merkle_tree.Checked.implied_root: address, path length mismatch"
      in
      go 0 entry_hash
        path0

    let check { index; path } root elt_hash =
      Hash.equal (implied_root elt_hash index path) root
  end

  let ceil_pow2 x =
    if x <= 0 then failwith "ceil_pow2: negative" ;
    let x = x - 1 in
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    (* The next line is superfluous on 32-bit architectures, but it's faster to do it
       anyway than to branch *)
    let x = x lor (x lsr 32) in
    x + 1

  let ofArray hash_element default leaves0 =
    let hash_tree = hash_tree hash_element in
    let leaves =
      let n = Array.length leaves0 in
      let padding = ceil_pow2 n - n in
      Array.append
        leaves0 
        (Array.make padding default)
    in
    let pair_up arr f =
      Array.init (Array.length arr / 2) (fun i ->
        f arr.(2 * i) arr.(2 * i + 1))
    in
    let rec go height (trees : _ Tree.t array) =
      if Array.length trees = 1
      then (trees.(0), height)
      else
        let merged =
          pair_up trees (fun l r ->
            Tree0.Node (merge ~height (hash_tree l) (hash_tree r), l, r) )
        in
        go (height + 1) merged
    in
    let tree, height =
      go 0 (Array.map (fun l -> Tree0.Element l) leaves)
    in
    { depth= height
    ; tree
    ; hash_element = hash_element
    }
end

type ('a, 'h) t = ('a, 'h) t0

