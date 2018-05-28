type mark =
  | Spade
  | Club
  | Heart
  | Diam;

type cards = list((int, mark));

type action =
  | NoAction
  | Hit
  | Stand;

type state;

type status =
  | Playing(state)
  | Win(state)
  | Lose(state);

let get_player: status => cards;

let get_dealer: status => cards;

let get_last_action: status => action;

let init: unit => state;

let next: (action, state) => status;