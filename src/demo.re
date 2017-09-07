open Orderbook;

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

  expect ({ buys: [], sells: [makeOrder 2 9.0 1.0 Sell] }) [Execution 1 2 10.0 1.0] ob1 ex1;

  let o3 = makeOrder 3 9.5 1.0 Sell;
  let o4 = makeOrder 4 10.0 2.0 Buy;
  let (ob2, ex2) = executeOrders ob1 [o3, o4];

  expect ({ buys: [], sells: [] }) [Execution 4 2 9.0 1.0, Execution 4 3 9.5 1.0] ob2 ex2;

  let o5 = makeOrder 5 10.0 1.0 Sell;
  let o6 = makeOrder 6 9.0 1.0 Sell;
  let o7 = makeOrder 7 10.0 0.5 Buy;
  let (ob3, ex3) = executeOrders emptyOrderbook [o5, o6, o7];

  expect ({ buys: [], sells: [makeOrder 5 10.0 1.0 Sell, makeOrder 6 9.0 0.5 Sell] }) [Execution 7 6 9.0 0.5] ob3 ex3;
};
test ();
