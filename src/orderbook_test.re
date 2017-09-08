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

let executeOrders orderbook orderDirectionPairs => orderDirectionPairs |>
  List.fold_left (fun (_orderbook, executions) (direction, order) => {
    let (new_orderbook, new_executions) = executeOrder direction order _orderbook;
    (new_orderbook, executions @ new_executions)
  }) (orderbook, []);

let test () => {
  let o1 = makeOrder 1 10.0 1.0;
  let o2 = makeOrder 2 9.0 2.0;
  let (ob1, ex1) = executeOrders emptyOrderbook [(Buy, o1), (Sell, o2)];

  expect ({ buys: [], sells: [makeOrder 2 9.0 1.0] }) [Execution 1 2 10.0 1.0] ob1 ex1;

  let o3 = makeOrder 3 9.5 1.0;
  let o4 = makeOrder 4 10.0 2.0;
  let (ob2, ex2) = executeOrders ob1 [(Sell, o3), (Buy, o4)];

  expect ({ buys: [], sells: [] }) [Execution 4 2 9.0 1.0, Execution 4 3 9.5 1.0] ob2 ex2;

  let o5 = makeOrder 5 10.0 1.0;
  let o6 = makeOrder 6 9.0 1.0;
  let o7 = makeOrder 7 10.0 0.5;
  let (ob3, ex3) = executeOrders emptyOrderbook [(Sell, o5), (Sell, o6), (Buy, o7)];

  expect ({ buys: [], sells: [makeOrder 5 10.0 1.0, makeOrder 6 9.0 0.5] }) [Execution 7 6 9.0 0.5] ob3 ex3;
};
test ();
