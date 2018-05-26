
type mark
  = Spade
  | Club
  | Heart
  | Diam;

type cards = list((int, mark));

type action
  = NoAction
  | Hit
  | Stand;

type state = {
  player: cards,
  dealer: cards,
  last_action: action
};

type status
  = Playing(state)
  | Win(state)
  | Lose(state);

let get_state = status => switch status {
| Playing(state) => state
| Win(state) => state
| Lose(state) => state
};

exception UnexpectedRandomNumber(int);

let draw_card = () => {
  let number =
    switch (Random.int(13)) {
    | 0 => 13
    | n => n
    };
  let mark =
    switch (Random.int(4)) {
    | 0 => Spade
    | 1 => Club
    | 2 => Heart
    | 3 => Diam
    | n => raise(UnexpectedRandomNumber(n))
    };
  (number, mark)
};

let sum_cards = (cards: cards) => {
  let cards' = List.map(((n, _)) => n, cards);

  let (sum, ac) = List.fold_left((acc, n) =>
    switch acc {
    | (s, a) when n == 1 => (s + 1, a + 1)
    | (s, a) when n == 11 || n == 12 || n == 13 => (s + 10, a)
    | (s, a) => (s + n, a)
    }, (0, 0), cards');

  let rec ret = (sum, ac) =>
    switch ac {
    | 0 => sum
    | _ when sum + 10 > 21 => sum
    | _ => ret(sum + 10, ac - 1)
    };

  ret(sum, ac)
};

let is_bust = cards => sum_cards(cards) > 21;

let will_dealer_draw = cards => sum_cards(cards) < 17;

let init = () => {
  Random.self_init();
  {
    player: [draw_card(), draw_card()],
    dealer: [draw_card(), draw_card()],
    last_action: NoAction
  }
};

let next = (action, {player, dealer, last_action}) => {
  let dealer' = will_dealer_draw(dealer) ? [draw_card(), ...dealer] : dealer;
  let (player', last_action') = switch (last_action, action) {
  | (_, Stand | NoAction)
  | (Stand, Hit) => (player, Stand)
  | (_, Hit) => ([draw_card(), ...player], Hit)
  };

  let state = {
    player: player',
    dealer: dealer',
    last_action: last_action'
  };

  switch (is_bust(player'), is_bust(dealer')) {
  | (false, true) => Win(state)
  | (true, _) => Lose(state)
  | _ when last_action' == Stand && !will_dealer_draw(dealer') =>
      sum_cards(player') > sum_cards(dealer') ? Win(state) : Lose(state)
  | _ => Playing(state)
  }
}