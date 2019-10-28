const { bn128 } = require('snarkyjs-crypto');

const privKeys = [ ];
for (let i = 0; i < 8; ++i) {
  privKeys.push(bn128.Schnorr.PrivateKey.create());
}

const pubKeys = privKeys.map(bn128.Schnorr.PublicKey.ofPrivateKey);

const dummy = bn128.Schnorr.PublicKey.ofPrivateKey(bn128.Field.ofInt(1));

const hashPubKey = (pk) =>
  bn128.Hash.hash(
    bn128.Group.toAffine(pk));

const tree = bn128.MerkleTree.ofArray(hashPubKey, dummy, pubKeys);

const proof = bn128.MerkleTree.MembershipProof.create(tree, 1);

const root = bn128.MerkleTree.root(tree);

console.log('merkle proof verified?', 
  bn128.MerkleTree.MembershipProof.check(
    proof,
    root,
    hashPubKey(pubKeys[1]))
);

const message = [ root ];

const signature = bn128.Schnorr.sign(
  privKeys[0],
  message );

console.log('signature verified?', 
  bn128.Schnorr.Signature.check(
    signature,
    pubKeys[0],
    message )
);
