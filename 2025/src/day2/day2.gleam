import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

pub fn part1(input: List(String)) -> Int {
  let l =
    list.first(input)
    |> result.unwrap("")
    |> string.split(",")
    |> list.flat_map(fn(r) {
      let bounds =
        string.split(r, "-")
        |> list.map(fn(b) { int.parse(b) |> result.unwrap(0) })
      case bounds {
        [start, end] -> list.range(start, end)
        _ -> []
      }
    })

  io.println("Generated list of length: " <> int.to_string(list.length(l)))

  l
  |> list.fold(0, fn(acc, num) {
    let num_string = int.to_string(num)

    let half = string.length(num_string) / 2
    let window_ranges = list.range(1, half)
    let possible_strings =
      list.any(window_ranges, fn(wr) {
        let strings = string.split(num_string, "") |> list.sized_chunk(wr)

        let leng = list.length(strings)

        let first = list.first(strings) |> result.unwrap([])
        let first_string = string.join(first, "")
        let result =
          list.all(strings, fn(s) {
            let s_string = string.join(s, "")
            s_string == first_string
          })

        result && leng == 2
      })

    case possible_strings {
      True -> acc + num
      False -> acc
    }
  })
}

pub fn part2(input: List(String)) -> Int {
  list.first(input)
  |> result.unwrap("")
  |> string.split(",")
  |> list.flat_map(fn(r) {
    let bounds =
      string.split(r, "-")
      |> list.map(fn(b) { int.parse(b) |> result.unwrap(0) })
    case bounds {
      [start, end] -> list.range(start, end)
      _ -> []
    }
  })
  |> list.fold(0, fn(acc, num) {
    let num_string = int.to_string(num)
    let half = string.length(num_string) / 2
    let window_ranges = list.range(1, half)
    let possible_strings =
      list.any(window_ranges, fn(wr) {
        let strings = string.split(num_string, "") |> list.sized_chunk(wr)

        let first = list.first(strings) |> result.unwrap([])
        let first_string = string.join(first, "")
        let result =
          list.all(list.rest(strings) |> result.unwrap([]), fn(s) {
            let s_string = string.join(s, "")
            s_string == first_string
          })

        result && list.length(strings) >= 2
      })

    case possible_strings {
      True -> acc + num
      False -> acc
    }
  })
}
