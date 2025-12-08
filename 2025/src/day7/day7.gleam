import gleam/dict
import gleam/list
import gleam/result
import gleam/string

pub fn part1(input: List(String)) -> Int {
  let first_line =
    list.first(input)
    |> result.unwrap("")
  let first_line_length = string.length(first_line)

  let map =
    input
    |> list.index_fold(dict.new(), fn(acc, line, row_index) {
      let chars = string.split(line, "")

      list.index_fold(chars, acc, fn(inner_acc, char, column_index) {
        dict.insert(inner_acc, #(column_index, row_index), char)
      })
    })

  let final_map =
    list.range(0, list.length(input))
    |> list.fold(map, fn(acc, row_index) {
      list.range(0, first_line_length)
      |> list.fold(acc, fn(inner_acc, column_index) {
        let current_pos = #(column_index, row_index)
        let current_value =
          dict.get(inner_acc, current_pos)
          |> result.unwrap("")

        let above_pos = #(column_index, row_index - 1)

        let above_value =
          dict.get(inner_acc, above_pos)
          |> result.unwrap("")

        let new_value = case above_value {
          "S" -> "|"
          "|" -> "|"
          _ -> current_value
        }
        let new_value = case current_value {
          "^" -> "^"
          _ -> new_value
        }

        let left_post = #(column_index - 1, row_index)
        let right_post = #(column_index + 1, row_index)

        let dict = case current_value == "^" && above_value == "|" {
          True -> {
            let inner_dict = case column_index - 1 >= 0 {
              True -> {
                inner_acc |> dict.insert(left_post, "|")
              }
              False -> inner_acc
            }
            case column_index + 1 < first_line_length {
              True -> {
                inner_dict |> dict.insert(right_post, "|")
              }
              False -> inner_dict
            }
          }
          False -> inner_acc
        }

        dict.insert(dict, current_pos, new_value)
      })
    })

  let count =
    dict.fold(final_map, 0, fn(acc, key, value) {
      let above_value =
        dict.get(final_map, #(key.0, key.1 - 1))
        |> result.unwrap("")

      case value == "^" && above_value == "|" {
        True -> acc + 1
        False -> acc
      }
    })

  count
}

pub fn part2(input: List(String)) -> Int {
  let input_length = list.length(input)
  let first_line =
    list.first(input)
    |> result.unwrap("")
  let first_line_length = string.length(first_line)

  let map =
    input
    |> list.index_fold(dict.new(), fn(acc, line, row_index) {
      let chars = string.split(line, "")

      list.index_fold(chars, acc, fn(inner_acc, char, column_index) {
        dict.insert(inner_acc, #(column_index, row_index), char)
      })
    })

  let final_map =
    list.range(0, input_length)
    |> list.fold(map, fn(acc, row_index) {
      list.range(0, first_line_length)
      |> list.fold(acc, fn(inner_acc, column_index) {
        let current_pos = #(column_index, row_index)
        let current_value =
          dict.get(inner_acc, current_pos)
          |> result.unwrap("")

        let above_pos = #(column_index, row_index - 1)

        let above_value =
          dict.get(inner_acc, above_pos)
          |> result.unwrap("")

        let new_value = case above_value {
          "S" -> "|"
          "|" -> "|"
          _ -> current_value
        }
        let new_value = case current_value {
          "^" -> "^"
          _ -> new_value
        }

        let left_post = #(column_index - 1, row_index)
        let right_post = #(column_index + 1, row_index)

        let dict = case current_value == "^" && above_value == "|" {
          True -> {
            let inner_dict = case column_index - 1 >= 0 {
              True -> {
                inner_acc |> dict.insert(left_post, "|")
              }
              False -> inner_acc
            }
            case column_index + 1 < first_line_length {
              True -> {
                inner_dict |> dict.insert(right_post, "|")
              }
              False -> inner_dict
            }
          }
          False -> inner_acc
        }

        dict.insert(dict, current_pos, new_value)
      })
    })

  let start =
    dict.filter(map, fn(_, value) { value == "S" })
    |> dict.keys
    |> list.first
    |> result.unwrap(#(0, 0))

  count_paths(final_map, start, dict.new(), input_length).0
}

type WorkItem {
  Explore(point: #(Int, Int))
  Combine(point: #(Int, Int), left_point: #(Int, Int), right_point: #(Int, Int))
}

fn count_paths(
  map: dict.Dict(#(Int, Int), String),
  start: #(Int, Int),
  cache: dict.Dict(#(Int, Int), Int),
  max: Int,
) -> #(Int, dict.Dict(#(Int, Int), Int)) {
  count_paths_loop(map, [Explore(start)], cache, max, start)
}

fn count_paths_loop(
  map: dict.Dict(#(Int, Int), String),
  stack: List(WorkItem),
  cache: dict.Dict(#(Int, Int), Int),
  max: Int,
  start: #(Int, Int),
) -> #(Int, dict.Dict(#(Int, Int), Int)) {
  case stack {
    [] -> {
      let final_result =
        dict.get(cache, start)
        |> result.unwrap(0)
      #(final_result, cache)
    }
    [first, ..rest] -> {
      case first {
        Explore(point) -> {
          let point_value =
            dict.get(map, point)
            |> result.unwrap("")
          case dict.get(cache, point) {
            Ok(_) -> {
              count_paths_loop(map, rest, cache, max, start)
            }
            Error(_) -> {
              case point.1 >= max - 1 {
                True -> {
                  let new_cache = dict.insert(cache, point, 1)
                  count_paths_loop(map, rest, new_cache, max, start)
                }
                False -> {
                  case point_value == "^" {
                    True -> {
                      let left_point = #(point.0 - 1, point.1)
                      let right_point = #(point.0 + 1, point.1)

                      let left_value =
                        dict.get(map, left_point)
                        |> result.unwrap("")

                      let right_value =
                        dict.get(map, right_point)
                        |> result.unwrap("")

                      let new_stack = [
                        Combine(point, left_point, right_point),
                        ..rest
                      ]

                      let new_stack = case right_value == "|" {
                        True -> [Explore(right_point), ..new_stack]
                        False -> new_stack
                      }

                      let new_stack = case left_value == "|" {
                        True -> [Explore(left_point), ..new_stack]
                        False -> new_stack
                      }

                      count_paths_loop(map, new_stack, cache, max, start)
                    }
                    False -> {
                      let below_point = #(point.0, point.1 + 1)
                      let new_stack = [
                        Explore(below_point),
                        Combine(point, below_point, below_point),
                        ..rest
                      ]
                      count_paths_loop(map, new_stack, cache, max, start)
                    }
                  }
                }
              }
            }
          }
        }
        Combine(point, left_point, right_point) -> {
          let left_val =
            dict.get(cache, left_point)
            |> result.unwrap(0)

          let right_val = case left_point == right_point {
            True -> 0
            False ->
              dict.get(cache, right_point)
              |> result.unwrap(0)
          }

          let sum = left_val + right_val

          let new_cache = dict.insert(cache, point, sum)
          count_paths_loop(map, rest, new_cache, max, start)
        }
      }
    }
  }
}
