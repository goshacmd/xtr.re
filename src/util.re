let replace wanted replacement x => if (x == wanted) { replacement } else { x };
let joinList = fun sep lst => List.fold_left (fun str x => if (str === "") { x } else { str ^ sep ^ x }) "" lst;
let isNotEqual x y => x != y;
