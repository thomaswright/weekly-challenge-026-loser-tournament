let arrayRange = n => {
  let a = Array.make(~length=n, 0)->Array.mapWithIndex((_, i) => i)
  a
}

let groupNum = (a, n) => {
  a->Array.reduce(([], []), ((arr, col), cur) => {
    col->Array.length < n - 1 ? (arr, [...col, cur]) : (Array.concat(arr, [...col, cur]), [])
  })
}

let groupPairs = a => {
  let (pairs, _) = a->Array.reduce(([], None), ((arr, col), cur) => {
    switch col {
    | None => (arr, Some(cur))
    | Some(x) => ([...arr, (x, cur)], None)
    }
  })
  pairs
}

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
  Math.random() < p1 /. (p1 +. p2)
}

let getPlacement = d => {
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

let skillByIndex = (i, num) => (1. +. (i + 1)->Int.toFloat /. num->Int.toFloat) ** 5.

// type gameRecord = {
//   p1: int,
//   p2: int,
//   winner: int,
//   round: int,
//   gamePosition: int
// }

type player = {
  id: int,
  skill: float,
}

let getPlayers = num => {
  arrayRange(num)
  ->Array.map(i => skillByIndex(i, num))
  ->Array.toSorted((a, b) => {
    b -. a
  })
  ->Array.mapWithIndex((v, i) => {id: i, skill: v})
}

let runRound = round => {
  let (winners, losers) =
    round
    ->groupPairs
    ->Array.map(((p1, p2)) => {
      isWin(p1.skill, p2.skill) ? (p1, p2) : (p2, p1)
    })
    ->Belt.Array.unzip

  (winners, losers)
}

let runRounds = round1 => {
  let rec recF = (winners, losers) => {
    let (newWinners, newLosers) = winners->runRound
    newWinners->Array.length < 2
      ? (newWinners, Array.concat(losers, newLosers))
      : recF(newWinners, Array.concat(losers, newLosers))
  }

  let (winners, losers) = recF(round1, [])
  let winner = winners->Array.getUnsafe(0)
  (winner, losers)
}

let runRoundsWithLosers = round1 => {
  let rec recF = (playing, sittingOut, comebackRound) => {
    let (newWinners, newLosers) = playing->runRound

    // Console.log2(newWinners, newLosers)

    newWinners->Array.length < 2
      ? (newWinners, Array.concat(sittingOut, newLosers))
      : !comebackRound
      ? {
        let shuffled = sittingOut->Array.toShuffled
        let luckyFew = shuffled->Array.slice(~start=0, ~end=newWinners->Array.length)
        let stillOut = shuffled->Array.sliceToEnd(~start=newWinners->Array.length)
        recF(Array.concat(newWinners, luckyFew), Array.concat(stillOut, newLosers), !comebackRound)
      }
      : recF(newWinners, Array.concat(sittingOut, newLosers), !comebackRound)
  }

  let (winners, losers) = recF(round1, [], false)
  let winner = winners->Array.getUnsafe(0)
  (winner, losers)
}

let runTournament = () => {
  let level = 4
  let num = (2. ** level->Int.toFloat)->Float.toInt
  let placement = getPlacement(level)

  // let players = getPlayers(num)
  // let placement = getPlacement(level)
  // let round1 = placement->Array.map(p => players->Array.getUnsafe(p))

  let winsRegular = arrayRange(1000)->Array.reduce(Array.make(~length=num, 0), (acc, cur) => {
    let players = getPlayers(num)
    let round1 = placement->Array.map(p => players->Array.getUnsafe(p))
    let (winner, losers) = runRounds(round1)

    acc->Array.mapWithIndex((v, i) => winner.id == i ? v + 1 : v)
  })

  let winsLosers = arrayRange(1000)->Array.reduce(Array.make(~length=num, 0), (acc, cur) => {
    let players = getPlayers(num)
    let round1 = placement->Array.map(p => players->Array.getUnsafe(p))
    let (winner, losers) = runRoundsWithLosers(round1)

    acc->Array.mapWithIndex((v, i) => winner.id == i ? v + 1 : v)
  })

  Console.log2(winsRegular, winsLosers)
}

runTournament()

@react.component
let make = () => {
  <div className="p-6" />
}
