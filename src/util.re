let mapPair fn (fst, snd) => (fn fst, fn snd);

let replace wanted replacement x => if (x == wanted) { replacement } else { x };

let joinList = fun sep lst => List.fold_left (fun str x => if (str === "") { x } else { str ^ sep ^ x }) "" lst;
