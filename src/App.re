[%bs.raw {|require('./App.css')|}];

[@bs.module] external logo : string = "./logo.svg";

let mark_to_str = str => switch str {
| Game.Spade => {js|♠|js}
| Game.Club => {js|♣|js}
| Game.Heart => {js|♥|js}
| Game.Diam => {js|♦|js}
};

let is_red = mark => switch mark {
| Game.Heart | Game.Diam => true
| _ => false
};

let render_card = (number, mark) => {
  let str = mark_to_str(mark);
  let colour = is_red(mark) ? "red" : "";
  <div className="card">
    <div className="front">
      <span className={j|top-left $colour|j}>(ReasonReact.string({j|$number$str|j}))</span>
      <span className={j|middle $colour|j}>(ReasonReact.string(str))</span>
      <span className={j|bottom-right $colour|j}>(ReasonReact.string({j|$number$str|j}))</span>
    </div>
  </div>
};

let show_cards = cards => {
  let cards' = List.map(fun
    | (1, mark) => render_card("A", mark)
    | (11, mark) => render_card("J", mark)
    | (12, mark) => render_card("Q", mark)
    | (13, mark) => render_card("K", mark)
    | (n, mark) => render_card(string_of_int(n), mark)
    , cards);
  ReasonReact.createDomElement("div", ~props={"className": "play-table"}, Array.of_list(cards'))
};

type state = Game.status;

let initialState = () => Game.Playing(Game.init());

type action
  = Hit
  | Stand
  | Retry;

let reducer = (action, state) => switch (action, state) {
| (Hit, Game.Playing(game_state)) =>  ReasonReact.Update(Game.next(Game.Hit, game_state))
| (Stand, Game.Playing(game_state)) => ReasonReact.Update(Game.next(Game.Stand, game_state))
| (Retry, _) => ReasonReact.Update(Game.Playing(Game.init()))
| _ => ReasonReact.Update(state)
};


let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState,
  reducer,
  render: self => {
    let {Game.player, dealer} = Game.get_state(self.state);
    let buttons = switch self.state {
    | Game.Playing({last_action: Game.Stand}) =>
      <div>
        <button disabled=true>(ReasonReact.string("Hit"))</button>
        <button onClick=(_ => self.send(Stand))>(ReasonReact.string("Stand"))</button>
      </div>
    | Game.Playing({last_action: Game.Hit}) =>
      <div>
        <button onClick=(_ => self.send(Hit))>(ReasonReact.string("Hit"))</button>
        <button onClick=(_ => self.send(Stand))>(ReasonReact.string("Stand"))</button>
      </div>
    | Game.Win(_) =>
      <div>
        (ReasonReact.string("You win!"))
        <button onClick=(_ => self.send(Retry))>(ReasonReact.string("Retry"))</button>
      </div>
    | Game.Lose(_) =>
      <div>
        (ReasonReact.string("You lose!"))
        <button onClick=(_ => self.send(Retry))>(ReasonReact.string("Retry"))</button>
      </div>
    };
    <div>
      <div>
        (show_cards(player))
        (show_cards(dealer))
      </div>
      (buttons)
    </div>
  }
};
