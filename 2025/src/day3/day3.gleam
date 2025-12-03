import gleam/bool
import gleam/int
import gleam/list
import gleam/order
import gleam/result
import gleam/string

pub fn part1(input: List(String)) -> Int {
  list.fold(input, 0, fn(acc, line) {
    let linemax =
      line
      |> string.split("")
      |> list.combinations(2)
      |> list.map(fn(pair) {
        string.join(pair, "") |> int.parse() |> result.unwrap(0)
      })
      |> list.reduce(fn(max, num) {
        case num > max {
          True -> num
          False -> max
        }
      })
      |> result.unwrap(0)

    acc + linemax
  })
}

pub fn part2(input: List(String)) -> Int {
  list.fold(input, 0, fn(acc, line) {
    let line_list = string.split(line, "")
    let line_ints_with_indexes =
      list.index_map(line_list, fn(char, index) {
        #(int.parse(char) |> result.unwrap(0), index)
      })

    let max =
      line_ints_with_indexes
      |> list.reduce(fn(acc, curr) {
        let #(num, index) = curr
        let #(acc_num, _) = acc

        case num > acc_num {
          True -> #(num, index)
          False -> acc
        }
      })
      |> result.unwrap(#(0, 0))

    let line_without_max =
      line_ints_with_indexes
      |> list.filter(fn(num) { num != max })

    let found = get_max_num(line_without_max, [max])

    acc + found
  })
}

fn get_max_num(line: List(#(Int, Int)), current_num: List(#(Int, Int))) -> Int {
  case list.length(current_num) {
    12 -> {
      list.sort(current_num, fn(a, b) {
        case a.1 < b.1 {
          True -> order.Lt
          False -> order.Gt
        }
      })
      |> list.fold("", fn(acc, curr) { acc <> int.to_string(curr.0) })
      |> int.parse()
      |> result.unwrap(0)
    }
    _ -> {
      let indexes = line |> list.map(fn(num) { num.1 })
      let first_missing =
        first_missing_index(
          indexes,
          line
            |> list.fold(-1, fn(acc, index) {
              case index.1 > acc {
                True -> index.1
                False -> acc
              }
            }),
        )

      let candidates = line |> list.filter(fn(num) { num.1 >= first_missing })

      let candidates_max =
        candidates
        |> list.reduce(fn(acc, curr) {
          case curr.0 > acc.0 {
            True -> curr
            False -> acc
          }
        })
        |> result.unwrap(#(0, 0))

      let new_line =
        line
        |> list.filter(fn(num) { num != candidates_max })
      let new_current_num = current_num |> list.append([candidates_max])

      get_max_num(new_line, new_current_num)
    }
  }
}

fn first_missing_index(indexes: List(Int), max_index: Int) {
  let full_range = list.range(0, max_index) |> list.reverse

  list.find(full_range, fn(i) { list.contains(indexes, i - 1) |> bool.negate })
  |> result.unwrap(-1)
}
