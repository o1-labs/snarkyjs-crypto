type 'a t = {mds: 'a array array; round_constants: 'a array array}

let map f {mds; round_constants} =
  let f = Array.map (Array.map f) in
  {mds= f mds; round_constants= f round_constants}

