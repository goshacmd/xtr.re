type id = int;
type price = float;
type qty = float;
type direction = Buy | Sell;
type order = { id, price, qty, direction };
type orderbook = { orders: list order };

type orderExecution =
  | FullyExecuted id price
  | PartiallyExecuted id price qty
  ;

type executionResult = list orderExecution;

let emptyOrderbook: orderbook = { orders: [] };

let makeOrder id price qty direction => { id, price, qty, direction };
let subQuantity qtyDiff order => makeOrder order.id order.price (order.qty -. qtyDiff) order.direction;

let addOrder order orderbook => { orders: orderbook.orders @ [order] };
let replace wanted replacement x => if (x == wanted) { replacement } else { x };
let isNotEmptyOrder order => order.qty > 0.0;
let replaceOrder sourceOrder newOrder orderbook =>
({ orders: (List.map (replace sourceOrder newOrder) orderbook.orders) });
let cleanEmptyOrders orderbook =>
({ orders: (List.filter isNotEmptyOrder orderbook.orders) });


let joinList = fun sep lst => List.fold_left (fun str x => if (str === "") { x } else { str ^ sep ^ x }) "" lst;
let displayOrder order => (string_of_float order.price) ^ " (qty: " ^ (string_of_float order.qty) ^ ")";

let displayOrderbook orderbook => {
  let (buys, sells) = List.partition (fun order => order.direction == Buy) orderbook.orders;
  let orderCompare order1 order2 => compare order1.price order2.price;
  let sortedBuys = List.sort orderCompare buys;
  let sortedSells = List.sort orderCompare sells;

  let buysDisplay = sortedBuys
    |> List.map displayOrder
    |> joinList "\n";
  let sellsDisplay = sortedSells
    |> List.map displayOrder
    |> joinList "\n";

  "Asks:\n" ^ sellsDisplay ^ "\n----------\n" ^ "Bids:\n" ^ buysDisplay
};

let displayExecution exec => switch exec {
  | FullyExecuted orderId price => "(Filled order #" ^ (string_of_int orderId) ^ ", @ " ^ (string_of_float price) ^ ")"
  | PartiallyExecuted orderId price qty => "(Partially filled order #" ^ (string_of_int orderId) ^ ", " ^ (string_of_float qty) ^ " @ " ^ (string_of_float price) ^ ")"
};
let displayExecutions execs => execs
  |> List.map displayExecution
  |> joinList "\n";


let canFill targetOrder possibleFill => switch targetOrder.direction {
  | Buy => switch possibleFill.direction {
    | Sell => targetOrder.price >= possibleFill.price
    | Buy => false
  }
  | Sell => switch possibleFill.direction {
    | Buy => targetOrder.price <= possibleFill.price
    | Sell => false
  }
};
let executeMutualOrders order fill1 => {
  let qtyToFill = min fill1.qty order.qty;
  let leftFill = subQuantity qtyToFill fill1;
  let leftOrder = subQuantity qtyToFill order;

  let res1 = if (leftOrder.qty == 0.0) {
    FullyExecuted order.id fill1.price
  } else {
    PartiallyExecuted order.id fill1.price qtyToFill
  };

  let res2 = if (leftFill.qty == 0.0) {
    FullyExecuted leftFill.id fill1.price
  } else {
    PartiallyExecuted leftFill.id fill1.price qtyToFill
  };

  let executions = [res2, res1];

  (leftOrder, leftFill, executions);
};
let executeOrder order orderbook => {
  let possibleFills = List.filter (canFill order) orderbook.orders;
  let (newOrderbook, executions, _) = List.fold_left (fun (_orderbook, _executions, _order) fill => {
    let (leftOrder, leftFill, executions) = executeMutualOrders _order fill;

    let newOrderbook = _orderbook
      |> replaceOrder _order leftOrder
      |> replaceOrder fill leftFill
      |> cleanEmptyOrders
      ;

    (newOrderbook, _executions @ executions, leftOrder)
  }) (orderbook, [], order) possibleFills;

  (newOrderbook, executions);
};
let executeOn orderbook => {
  List.fold_left (fun execution order => {
    let (_orderbook, executions) = execution;
    let (new_orderbook, new_executions) = executeOrder order (addOrder order _orderbook);
    (new_orderbook, executions @ new_executions)
  }) ({ orders: [] }, []) orderbook.orders;
};


let expect expectedOrderbook expectedExecutions actualOrderbook actualExecutions => {
  if (actualOrderbook == expectedOrderbook) {
    Js.log "Orderbook OK";
  } else {
    Js.log "Orderbook error, expected:";
    Js.log (displayOrderbook expectedOrderbook);
    Js.log "instead, got:";
    Js.log (displayOrderbook actualOrderbook);
  };
  if (actualExecutions == expectedExecutions) {
    Js.log "Executions OK";
  } else {
    Js.log "Executions error, expected:";
    Js.log (displayExecutions expectedExecutions);
    Js.log "instead, got:";
    Js.log (displayExecutions actualExecutions);
  };
  Js.log "";
};

let test () => {
  let o1 = makeOrder 1 10.0 1.0 Buy;
  let o2 = makeOrder 2 9.0 2.0 Sell;
  let ob1 = emptyOrderbook |> addOrder o1 |> addOrder o2;
  let (ob2, ex1) = executeOn ob1;

  expect ({ orders: [makeOrder 2 9.0 1.0 Sell] }) [(FullyExecuted 1 10.0), (PartiallyExecuted 2 10.0 1.0)] ob2 ex1;

  let o3 = makeOrder 3 9.5 1.0 Sell;
  let o4 = makeOrder 4 10.0 2.0 Buy;
  let ob3 = ob2 |> addOrder o3 |> addOrder o4;
  let (ob4, ex2) = executeOn ob3;

  expect ({ orders: [] }) [(FullyExecuted 2 9.0), (PartiallyExecuted 4 9.0 1.0), (FullyExecuted 3 9.5), (FullyExecuted 4 9.5)] ob4 ex2;
};
test ();
