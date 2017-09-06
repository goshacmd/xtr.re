type id = int;
type price = float;
type qty = float;
type direction = Buy | Sell;
type order = { id, price, qty, direction };
type orderbook = { buys: list order, sells: list order };

type orderFulfilment  =
  | FullFulfilment id price
  | PartialFulfilment id price qty
  ;

type orderExecution = Execution orderFulfilment orderFulfilment;

let emptyOrderbook: orderbook = { buys: [], sells: [] };

let makeOrder id price qty direction => { id, price, qty, direction };
let subQuantity qtyDiff order => makeOrder order.id order.price (order.qty -. qtyDiff) order.direction;

let (mapPair, replace, joinList) = Util.(mapPair, replace, joinList);
let isNotEqual x y => x != y;

let overBuys fn orderbook => ({ ...orderbook, buys: fn orderbook.buys });
let overSells fn orderbook => ({ ...orderbook, sells: fn orderbook.sells });

let addBuyOrder order orderbook => {
  if (order.qty > 0.0) { overBuys (fun x => x @ [order]) orderbook } else { orderbook }
};
let addSellOrder order orderbook => {
  if (order.qty > 0.0) { overSells (fun x => x @ [order]) orderbook } else { orderbook }
};
let addOrder order => order.direction == Buy ? addBuyOrder order : addSellOrder order;

let removeBuyOrder order orderbook => overBuys (List.filter (isNotEqual order)) orderbook;
let removeSellOrder order orderbook => overSells (List.filter (isNotEqual order)) orderbook;
let replaceBuyOrder sourceOrder newOrder orderbook => {
  if (newOrder.qty > 0.0) {
    overBuys (List.map (replace sourceOrder newOrder)) orderbook
  } else {
    removeBuyOrder sourceOrder orderbook
  }
};
let replaceSellOrder sourceOrder newOrder orderbook => {
  if (newOrder.qty > 0.0) {
    overSells (List.map (replace sourceOrder newOrder)) orderbook
  } else {
    removeSellOrder sourceOrder orderbook
  }
};
let replaceOrder sourceOrder => sourceOrder.direction == Buy ? replaceBuyOrder sourceOrder : replaceSellOrder sourceOrder;

let getBuys orderbook => orderbook.buys;
let getSells orderbook => orderbook.sells;
let oppositeOrders order orderbook => order.direction == Buy ? (getSells orderbook) : (getBuys orderbook);

let displayOrder order => (string_of_float order.price) ^ " (qty: " ^ (string_of_float order.qty) ^ ")";

let displayOrderbook orderbook => {
  let buys = getBuys orderbook;
  let sells = getSells orderbook;
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

let displayFulfilment f => switch f {
  | FullFulfilment orderId price => "(Filled order #" ^ (string_of_int orderId) ^ ", @ " ^ (string_of_float price) ^ ")"
  | PartialFulfilment orderId price qty => "(Partially filled order #" ^ (string_of_int orderId) ^ ", " ^ (string_of_float qty) ^ " @ " ^ (string_of_float price) ^ ")"
};
let displayExecution exec => switch exec {
  | Execution bid ask => (displayFulfilment bid) ^ " + " ^ (displayFulfilment ask);
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
  let (leftFill, leftOrder) = (fill1, order) |> mapPair (subQuantity qtyToFill);
  let atPrice = fill1.price;

  let res1 = if (leftOrder.qty == 0.0) {
    FullFulfilment order.id atPrice
  } else {
    PartialFulfilment order.id atPrice qtyToFill
  };

  let res2 = if (leftFill.qty == 0.0) {
    FullFulfilment leftFill.id atPrice
  } else {
    PartialFulfilment leftFill.id atPrice qtyToFill
  };

  let bid = order.direction == Buy ? res1 : res2;
  let ask = order.direction == Sell ? res1 : res2;
  let execution = Execution bid ask;

  (leftOrder, leftFill, execution);
};
let executeOrder order orderbook => {
  let possibleFills = oppositeOrders order orderbook |> List.filter (canFill order);

  let (newOrderbook, executions, leftOrder) = List.fold_left (fun (_orderbook, executions, _order) fill => {
    let (leftOrder, leftFill, execution) = executeMutualOrders _order fill;

    let newOrderbook = _orderbook |> replaceOrder fill leftFill;

    (newOrderbook, executions @ [execution], leftOrder)
  }) (orderbook, [], order) possibleFills;

  (newOrderbook |> addOrder leftOrder, executions);
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

let executeOrders orderbook orders => orders |>
  List.fold_left (fun (_orderbook, executions) order => {
    let (new_orderbook, new_executions) = executeOrder order _orderbook;
    (new_orderbook, executions @ new_executions)
  }) (orderbook, []);

let test () => {
  let o1 = makeOrder 1 10.0 1.0 Buy;
  let o2 = makeOrder 2 9.0 2.0 Sell;
  let (ob1, ex1) = executeOrders emptyOrderbook [o1, o2];

  expect ({ buys: [], sells: [makeOrder 2 9.0 1.0 Sell] }) [Execution (FullFulfilment 1 10.0) (PartialFulfilment 2 10.0 1.0)] ob1 ex1;

  let o3 = makeOrder 3 9.5 1.0 Sell;
  let o4 = makeOrder 4 10.0 2.0 Buy;
  let (ob2, ex2) = executeOrders ob1 [o3, o4];

  expect ({ buys: [], sells: [] }) [Execution (PartialFulfilment 4 9.0 1.0) (FullFulfilment 2 9.0), Execution (FullFulfilment 4 9.5) (FullFulfilment 3 9.5)] ob2 ex2;
};
test ();
