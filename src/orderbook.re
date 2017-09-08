let (replace, joinList, isNotEqual) = Util.(replace, joinList, isNotEqual);

type id = int;
type price = float;
type qty = float;
type direction = Buy | Sell;
type order = { id, price, qty };
type orderbook = { buys: list order, sells: list order };

type orderExecution = Execution id id price qty;

let emptyOrderbook: orderbook = { buys: [], sells: [] };

let makeOrder id price qty => { id, price, qty };
let subQuantity qtyDiff order => makeOrder order.id order.price (order.qty -. qtyDiff);

let overBuys fn orderbook => ({ ...orderbook, buys: fn orderbook.buys });
let overSells fn orderbook => ({ ...orderbook, sells: fn orderbook.sells });

let addBuyOrder order orderbook => {
  if (order.qty > 0.0) { overBuys (fun x => x @ [order]) orderbook } else { orderbook }
};
let addSellOrder order orderbook => {
  if (order.qty > 0.0) { overSells (fun x => x @ [order]) orderbook } else { orderbook }
};

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

let getBuys orderbook => orderbook.buys;
let getSells orderbook => orderbook.sells;
let oppositeOrders direction orderbook => direction == Buy ? (getSells orderbook) : (getBuys orderbook);
let orderCompare order1 order2 => compare order1.price order2.price;

let canFill direction targetOrder possibleFill =>
  if (direction == Buy) { targetOrder.price >= possibleFill.price } else { targetOrder.price <= possibleFill.price };
let bestSortingFor direction =>
  fun a b => (direction == Sell ? -1 : 1) * (orderCompare a b);
let findMatches direction order orderbook => {
  let possibleFills = oppositeOrders direction orderbook
    |> List.filter (canFill direction order)
    |> List.sort (bestSortingFor direction);

  let (matches, _) = List.fold_left (fun (matches, leftToFill) fill => {
    if (leftToFill > 0.0) {
      let qtyToFill = min fill.qty leftToFill;
      let atPrice = fill.price;

      let bid = direction == Buy ? order : fill;
      let ask = direction == Sell ? order : fill;
      let execution = Execution bid.id ask.id atPrice qtyToFill;

      (matches @ [execution], leftToFill -. qtyToFill);
    } else {
      (matches, leftToFill);
    }
  }) ([], order.qty) possibleFills;

  matches
};
let applyMatch _match orderbook => switch _match {
  | Execution bidId askId _ qty => {
    let originalBid = orderbook.buys |> List.find (fun order => order.id == bidId);
    let originalAsk = orderbook.sells |> List.find (fun order => order.id == askId);

    let newBid = subQuantity qty originalBid;
    let newAsk = subQuantity qty originalAsk;

    let processBid = newBid.qty > 0.0 ? (replaceBuyOrder originalBid newBid) : (removeBuyOrder originalBid);
    let processAsk = newAsk.qty > 0.0 ? (replaceSellOrder originalAsk newAsk) : (removeSellOrder originalAsk);

    orderbook |> processBid |> processAsk
  };
};
let applyMatches matches orderbook => matches
  |> List.fold_left (fun _orderbook _match => applyMatch _match _orderbook) orderbook;
let executeOrder direction order orderbook => {
  let matches = findMatches direction order orderbook;
  let addOrder = direction == Buy ? addBuyOrder : addSellOrder;
  let newOrderbook = applyMatches matches (orderbook |> addOrder order);
  (newOrderbook, matches)
};


let displayOrder order => (string_of_float order.price) ^ " (qty: " ^ (string_of_float order.qty) ^ ")";

let displayOrderbook orderbook => {
  let buys = getBuys orderbook;
  let sells = getSells orderbook;
  let sortedBuys = List.sort orderCompare buys;
  let sortedSells = List.sort orderCompare sells;

  let buysDisplay = sortedBuys
    |> List.map displayOrder
    |> joinList "\n";
  let sellsDisplay = sortedSells
    |> List.rev
    |> List.map displayOrder
    |> joinList "\n";

  "Asks:\n" ^ sellsDisplay ^ "\n----------\n" ^ "Bids:\n" ^ buysDisplay
};

let displayExecution exec => switch exec {
  | Execution bidId askId price qty => "Matched bid #" ^ (string_of_int bidId) ^ " with ask #" ^ (string_of_int askId) ^ ", " ^ (string_of_float qty) ^ " @ " ^ (string_of_float price);
};
let displayExecutions execs => execs
  |> List.map displayExecution
  |> joinList "\n";
