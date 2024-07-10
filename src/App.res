let pi = 3.14159265

let gaussian = (mean, stdev) => {
  let u = 1. -. Math.random()
  let v = Math.random()
  let z = Math.sqrt(-2.0 *. Math.log(u)) *. Math.cos(2.0 *. pi *. v)
  z *. stdev +. mean
}

let rec boundGaussian = () => {
  let u = 1. -. Math.random()
  let v = Math.random()
  let z = Math.sqrt(-2.0 *. Math.log(u)) *. Math.cos(2.0 *. pi *. v)
  let scaled = z /. 10.0 +. 0.5
  let resampled = scaled < 0. || scaled > 1. ? boundGaussian() : scaled
  resampled
}

let isWin = (p1, p2) => {
  Math.random() > p1 /. (p1 +. p2)
}

let tournamentConstruction = d => {
  let rec recF = (t, depth) => {
    let level = t->Array.length
    let rank =
      t
      ->Array.map(v => {
        [v, level * 2 - v - 1]
      })
      ->Belt.Array.concatMany

    depth <= 0 ? rank : recF(rank, depth - 1)
  }

  recF([0], d - 1)
}

let genPlayers = () => {
  let players = Array.make(~length=64, 0)->Array.map(_ => boundGaussian())
  Console.log(players)
}

genPlayers()

Console.log(tournamentConstruction(2))

@react.component
let make = () => {
  <div className="p-6" />
}
