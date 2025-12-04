import gleam/dict
import gleam/list
import gleam/string

type Position =
  #(Int, Int)

const neighbours_vectors = [
  #(0, -1),
  #(0, 1),
  #(-1, 0),
  #(1, 0),
  #(-1, -1),
  #(1, -1),
  #(-1, 1),
  #(1, 1),
]

pub fn part1(input: List(String)) -> Int {
  let dict: dict.Dict(Position, String) = dict.new()
  let map =
    list.index_fold(input, dict, fn(acc, line, row_index) {
      let chars = string.split(line, "")

      list.index_fold(chars, acc, fn(inner_acc, char, column_index) {
        dict.insert(inner_acc, #(column_index, row_index), char)
      })
    })

  dict.fold(map, 0, fn(acc, position, char) {
    case is_paper(char) {
      True -> {
        let paper_neighbours =
          neighbours_vectors
          |> list.fold(0, fn(neighbour_acc, vector) {
            let neighbour_position = #(
              position.0 + vector.0,
              position.1 + vector.1,
            )
            case dict.get(map, neighbour_position) {
              Ok(neighbour_char) -> {
                case is_paper(neighbour_char) {
                  True -> neighbour_acc + 1
                  False -> neighbour_acc
                }
              }
              _ -> neighbour_acc
            }
          })

        acc
        + {
          case paper_neighbours < 4 {
            True -> 1
            _ -> 0
          }
        }
      }
      False -> acc
    }
  })
}

pub fn part2(input: List(String)) -> Int {
  let dict: dict.Dict(Position, String) = dict.new()
  let map =
    list.index_fold(input, dict, fn(acc, line, row_index) {
      let chars = string.split(line, "")

      list.index_fold(chars, acc, fn(inner_acc, char, column_index) {
        dict.insert(inner_acc, #(column_index, row_index), char)
      })
    })

  keep_removing(map, 0)
}

fn is_paper(char: String) -> Bool {
  char == "@"
}

fn keep_removing(map: dict.Dict(Position, String), current_sum: Int) -> Int {
  let postiions_to_delete =
    dict.fold(map, [], fn(acc, position, char) {
      case is_paper(char) {
        True -> {
          let paper_neighbours =
            neighbours_vectors
            |> list.fold([], fn(neighbour_acc, vector) {
              let neighbour_position = #(
                position.0 + vector.0,
                position.1 + vector.1,
              )
              case dict.get(map, neighbour_position) {
                Ok(neighbour_char) -> {
                  case is_paper(neighbour_char) {
                    True -> neighbour_acc |> list.append([neighbour_position])
                    False -> neighbour_acc
                  }
                }
                _ -> neighbour_acc
              }
            })

          acc
          |> list.append({
            case list.length(paper_neighbours) < 4 {
              True -> [position]
              False -> []
            }
          })
        }
        False -> acc
      }
    })

  case list.is_empty(postiions_to_delete) {
    True -> current_sum
    False -> {
      let new_map =
        list.fold(postiions_to_delete, map, fn(acc, position) {
          dict.delete(acc, position)
        })

      keep_removing(new_map, current_sum + list.length(postiions_to_delete))
    }
  }
}
