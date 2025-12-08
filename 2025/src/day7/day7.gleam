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

  count_paths(final_map)
}

fn count_paths(map: dict.Dict(#(Int, Int), String)) -> Int {
  todo
}
