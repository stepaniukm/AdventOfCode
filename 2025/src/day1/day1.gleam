import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn part1(input: List(String)) -> Int {
  let value =
    list.fold(input, #(50, 0), fn(acc, line) {
      let #(val, times) = acc

      let dir = string.first(line) |> result.unwrap("")

      let new_val = case dir {
        "L" -> {
          let amount =
            string.slice(line, 1, string.length(line))
            |> int.parse()
            |> result.unwrap(0)
          let n = val - amount % 100

          case n < 0 {
            True -> 100 + n
            False -> n
          }
        }
        "R" -> {
          let amount =
            string.slice(line, 1, string.length(line))
            |> int.parse()
            |> result.unwrap(0)
          let n = val + amount % 100

          n % 100
        }
        _ -> val
      }

      case new_val == 0 {
        True -> #(new_val, times + 1)
        False -> #(new_val, times)
      }
    })

  value.1
}

pub fn part2(input: List(String)) -> Int {
  let value =
    list.fold(input, #(50, 0), fn(acc, line) {
      let #(val, times) = acc

      let dir = string.first(line) |> result.unwrap("")

      let #(new_val, zero_count) = case dir {
        "L" -> {
          let amount =
            string.slice(line, 1, string.length(line))
            |> int.parse()
            |> result.unwrap(0)

          let n = { 100 + val - amount % 100 } % 100

          let count = case val {
            0 -> amount / 100
            _ ->
              case amount >= val {
                True -> 1 + { amount - val } / 100
                False -> 0
              }
          }

          #(n, count)
        }
        "R" -> {
          let amount =
            string.slice(line, 1, string.length(line))
            |> int.parse()
            |> result.unwrap(0)

          let n = { val + amount } % 100

          let steps_to_zero = 100 - val
          let count = case val {
            0 -> amount / 100
            _ ->
              case amount >= steps_to_zero {
                True -> 1 + { amount - steps_to_zero } / 100
                False -> 0
              }
          }

          #(n, count)
        }

        _ -> #(val, 0)
      }

      #(new_val, times + zero_count)
    })

  value.1
}
