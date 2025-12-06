import gleam/dict
import gleam/int
import gleam/list
import gleam/order
import gleam/result
import gleam/string

pub fn part1(input: List(String)) -> Int {
  let operators_dict_init: dict.Dict(Int, String) = dict.new()
  let operators =
    list.last(input)
    |> result.unwrap("")
    |> string.split(" ")
    |> list.filter(fn(op) { op != "" })
    |> list.index_fold(operators_dict_init, fn(acc, op, index) {
      let value = string.trim(op)

      dict.insert(acc, index, value)
    })

  let lines = list.reverse(input) |> list.drop(1) |> list.reverse()
  let lines_dict_init: dict.Dict(Int, List(Int)) = dict.new()

  let lines_dict =
    list.fold(lines, lines_dict_init, fn(acc, line) {
      string.split(line, " ")
      |> list.filter(fn(num) { num != "" })
      |> list.index_fold(acc, fn(acc, num, index) {
        let current_list = dict.get(acc, index) |> result.unwrap([])
        let parsed_num = int.parse(string.trim(num)) |> result.unwrap(0)
        dict.insert(acc, index, list.append(current_list, [parsed_num]))
      })
    })

  dict.fold(lines_dict, 0, fn(acc, index, nums) {
    let operator = dict.get(operators, index) |> result.unwrap("+")
    let init = case operator {
      "+" -> 0
      "*" -> 1
      _ -> 0
    }
    let computed_value =
      list.fold(nums, init, fn(num_acc, num) {
        case operator {
          "+" -> num_acc + num
          "*" -> num_acc * num
          _ -> num_acc
        }
      })

    echo computed_value

    acc + computed_value
  })
}

pub fn part2(input: List(String)) -> Int {
  let columns =
    input
    |> list.fold(dict.new(), fn(acc, line) {
      string.split(line, "")
      |> list.index_fold(acc, fn(acc, num, index) {
        let current_list = dict.get(acc, index) |> result.unwrap([])
        let parsed_num = string.trim(num)
        dict.insert(acc, index, list.append(current_list, [parsed_num]))
      })
    })
    |> dict.to_list()
    |> list.sort(fn(a, b) {
      let #(index_a, _) = a
      let #(index_b, _) = b

      case index_a > index_b {
        True -> order.Lt
        False -> order.Gt
      }
    })
    |> list.map(fn(acc) { acc.1 })
    |> list.fold(#(0, []), fn(acc, val) {
      let #(sum, nums) = acc
      let last =
        list.filter(val, fn(v) { v != "" })
        |> list.last
        |> result.unwrap("")

      case last {
        "*" -> {
          let last_num =
            list.reverse(val)
            |> list.drop(1)
            |> list.reverse()
            |> string.join("")
            |> int.parse()
            |> result.unwrap(0)

          let result =
            nums
            |> list.append([last_num])
            |> list.fold(1, fn(acc, num) { acc * num })

          #(sum + result, [])
        }
        "+" -> {
          let last_num =
            list.reverse(val)
            |> list.drop(1)
            |> list.reverse()
            |> string.join("")
            |> int.parse()
            |> result.unwrap(0)

          let result =
            nums
            |> list.append([last_num])
            |> list.fold(0, fn(acc, num) { acc + num })

          #(sum + result, [])
        }
        "" -> #(sum, nums)
        _ -> #(
          sum,
          nums
            |> list.append([
              val |> string.join("") |> int.parse() |> result.unwrap(0),
            ]),
        )
      }
    })

  columns.0
}
