import gleam/int
import gleam/list
import gleam/result
import gleam/set
import gleam/string

pub fn part1(input: List(String)) -> Int {
  let ranges =
    list.take_while(input, fn(line) { line != "" })
    |> list.map(fn(range) {
      let parts = string.split(range, "-")
      let start =
        int.parse(list.first(parts) |> result.unwrap("0")) |> result.unwrap(0)
      let end =
        int.parse(list.last(parts) |> result.unwrap("0")) |> result.unwrap(0)
      #(start, end)
    })
  let ingredients =
    list.reverse(input)
    |> list.take_while(fn(line) { line != "" })
    |> list.map(fn(ingredient) { int.parse(ingredient) |> result.unwrap(0) })
    |> list.reverse()

  list.fold(ingredients, 0, fn(acc, ingredient) {
    let found =
      list.any(ranges, fn(range) {
        let #(start, end) = range
        ingredient >= start && ingredient <= end
      })

    case found {
      True -> acc + 1
      False -> acc
    }
  })
}

pub fn part2(input: List(String)) -> Int {
  let ranges =
    list.take_while(input, fn(line) { line != "" })
    |> list.map(fn(range) {
      let parts = string.split(range, "-")
      let start =
        int.parse(list.first(parts) |> result.unwrap("0")) |> result.unwrap(0)
      let end =
        int.parse(list.last(parts) |> result.unwrap("0")) |> result.unwrap(0)
      #(start, end)
    })

  let final_ranges = merge_ranges(ranges, set.new())

  final_ranges
  |> list.fold(0, fn(acc, range) {
    let #(start, end) = range
    acc + { end - start + 1 }
  })
}

fn merge_ranges(
  ranges: List(#(Int, Int)),
  processed_ranges: set.Set(#(Int, Int)),
) -> List(#(Int, Int)) {
  case list.length(ranges) {
    0 -> set.to_list(processed_ranges)
    _ -> {
      let #(start, end) = list.first(ranges) |> result.unwrap(#(0, 0))
      let rest = list.rest(ranges) |> result.unwrap(list.new())

      let #(matched, remaining_ranges) =
        list.fold(rest, #(False, list.new()), fn(acc, range) {
          let #(is_matched, rem_ranges) = acc
          let #(other_start, other_end) = range
          case end < other_start || start > other_end {
            True -> #(is_matched, list.append(rem_ranges, [range]))
            False -> {
              let new_start = int.min(start, other_start)
              let new_end = int.max(end, other_end)
              #(True, list.append(rem_ranges, [#(new_start, new_end)]))
            }
          }
        })

      case matched {
        True -> merge_ranges(remaining_ranges, processed_ranges)
        False -> {
          let new_processed_ranges = set.insert(processed_ranges, #(start, end))
          merge_ranges(remaining_ranges, new_processed_ranges)
        }
      }
    }
  }
}
