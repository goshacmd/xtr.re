type id = int;
type instrument = Instrument string;
type amount = float;
type balance = Balance instrument amount;
type account = Account id (list balance);
type marketId = (instrument, instrument);
type market = Market marketId Orderbook.orderbook;
type trade = Trade id id instrument instrument amount amount;
type engine = Engine (list account) (list market) (list trade);

let idOfMarket market => switch market {
  | Market marketId _ => marketId
};
let orderbookOfMarket market => switch market {
  | Market _ orderbook => orderbook
};

let findOrderbook (marketId: marketId) (engine: engine): option Orderbook.orderbook => switch engine {
  | Engine _ markets _ => {
    let matches = markets |> List.filter (fun mkt => (idOfMarket mkt) == marketId);
    if ((List.length matches) > 0) {
      Some (matches |> List.hd |> orderbookOfMarket)
    } else {
      None
    }
  }
};

let placeOrder (marketId: marketId) (accountId: id) (direction: Orderbook.direction) (qty: amount) (price: amount) (engine: engine): engine => {
  let orderbook = findOrderbook marketId engine;
  engine
};

let test () => {
  let btc = Instrument "btc";
  let usd = Instrument "usd";
  let btcusd = Market (btc, usd) Orderbook.emptyOrderbook;
  let acc_usd_1 = Balance usd 100.0;
  let acc_usd_2 = Balance usd 350.0;
  let acc_btc_3 = Balance btc 5.0;
  let acc_btc_4 = Balance btc 10.1;
  let acc_1 = Account 1 [acc_usd_1, acc_btc_4];
  let acc_2 = Account 2 [acc_usd_2, acc_btc_3];
  let engine = Engine [acc_1, acc_2] [btcusd] [];
};
test ();
