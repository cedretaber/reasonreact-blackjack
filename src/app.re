[%bs.raw {|require('./app.css')|}];

module RR = ReasonReact;
let s = RR.string;

let mark_to_str = str =>
  switch (str) {
  | Game.Spade => {js|♠|js}
  | Game.Club => {js|♣|js}
  | Game.Heart => {js|♥|js}
  | Game.Diam => {js|♦|js}
  };

let is_red = mark =>
  switch (mark) {
  | Game.Heart
  | Game.Diam => true
  | _ => false
  };

let render_card = (number, mark) => {
  let str = mark_to_str(mark);
  let colour = is_red(mark) ? "red" : "";
  <div className="card">
    <div className="front">
      <span className={j|top-left $colour|j}> (s({j|$number$str|j})) </span>
      <span className={j|middle $colour|j}> (s(str)) </span>
      <span className={j|bottom-right $colour|j}>
        (s({j|$number$str|j}))
      </span>
    </div>
  </div>;
};

let show_cards = cards =>
  cards
  |> List.rev
  |> List.map(
       fun
       | (1, mark) => render_card("A", mark)
       | (11, mark) => render_card("J", mark)
       | (12, mark) => render_card("Q", mark)
       | (13, mark) => render_card("K", mark)
       | (n, mark) => render_card(string_of_int(n), mark),
     )
  |> Array.of_list;

let render_cards_table = cards =>
  RR.createDomElement("div", ~props={"className": "play-table"}, cards);

type state = Game.status;

let initialState = () => Game.Playing(Game.init());

type action =
  | Hit
  | Stand
  | Retry;

let reducer = (action, state) =>
  switch (action, state) {
  | (Hit, Game.Playing(game_state)) =>
    RR.Update(Game.next(Game.Hit, game_state))
  | (Stand, Game.Playing(game_state)) =>
    RR.Update(Game.next(Game.Stand, game_state))
  | (Retry, _) => RR.Update(Game.Playing(Game.init()))
  | _ => RR.Update(state)
  };

let component = RR.reducerComponent("App");

let make = _children => {
  ...component,
  initialState,
  reducer,
  render: self => {
    let {Game.player, dealer, last_action} = Game.get_state(self.state);
    let player_cards = show_cards(player);
    let dealer_cards =
      switch (last_action) {
      | Game.NoAction =>
        let cards = show_cards(dealer);
        let hided_cards =
          <div className="card"> <div className="back" /> </div>;
        cards[0] = hided_cards;
        cards;
      | _ => show_cards(dealer)
      };
    let panel =
      switch (self.state) {
      | Game.Playing({last_action: Game.Stand}) =>
        <div className="button-table">
          <button className="disabled" disabled=true> (s("Hit")) </button>
          <button onClick=(_ => self.send(Stand))> (s("Stand")) </button>
        </div>
      | Game.Playing(_) =>
        <div className="button-table">
          <button onClick=(_ => self.send(Hit))> (s("Hit")) </button>
          <button onClick=(_ => self.send(Stand))> (s("Stand")) </button>
        </div>
      | Game.Win(_) =>
        <div className="result-table">
          <div> <span className="result win"> (s("You win!")) </span> </div>
          <div>
            <button onClick=(_ => self.send(Retry))> (s("Retry")) </button>
          </div>
        </div>
      | Game.Lose(_) =>
        <div className="result-table">
          <div> <span className="result lose"> (s("You lose!")) </span> </div>
          <div>
            <button onClick=(_ => self.send(Retry))> (s("Retry")) </button>
          </div>
        </div>
      };
    <div>
      <div className="table-wrapper">
        (render_cards_table(dealer_cards))
        <span className="cast-label"> (s("DEALER")) </span>
      </div>
      <div className="table-wrapper">
        <span className="cast-label"> (s("PLAYER")) </span>
        (render_cards_table(player_cards))
      </div>
      panel
    </div>;
  },
};