import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn part1(input: List(String)) -> Int {
  input
  |> list.fold(0, fn(acc, line) {
    let parts = string.split(line, " ")
    let expected =
      parts
      |> list.first
      |> result.unwrap("")
      |> string.split("")
      |> list.flat_map(fn(s) {
        case s {
          "[" -> []
          "]" -> []
          "." -> [False]
          "#" -> [True]
          _ -> []
        }
      })

    let buttons =
      parts
      |> list.drop(1)
      |> list.take_while(fn(s) { !string.starts_with(s, "{") })
      |> list.map(fn(s) {
        s
        |> string.split("")
        |> list.flat_map(fn(c) {
          case c {
            "(" -> []
            ")" -> []
            "," -> []
            num -> [int.parse(num) |> result.unwrap(0)]
          }
        })
      })

    let num = find_num(expected, buttons, 1)

    acc + num
  })
}

pub fn part2(input: List(String)) -> Int {
  input
  |> list.fold(0, fn(acc, line) {
    let parts = string.split(line, " ")
    let expected =
      parts
      |> list.last
      |> result.unwrap("")
      |> string.replace("{", "")
      |> string.replace("}", "")
      |> string.split(",")
      |> list.filter_map(fn(s) {
        s
        |> string.trim
        |> int.parse
      })

    let buttons =
      parts
      |> list.drop(1)
      |> list.take_while(fn(s) { !string.starts_with(s, "{") })
      |> list.map(fn(s) {
        s
        |> string.replace("(", "")
        |> string.replace(")", "")
        |> string.split(",")
        |> list.filter_map(fn(num_str) {
          num_str
          |> string.trim
          |> int.parse
        })
      })

    acc
  })
}

fn find_num(expected: List(Bool), buttons: List(List(Int)), num: Int) -> Int {
  let initial_state = list.repeat(False, list.length(expected))
  let combinations = list.combinations(buttons, num)

  let any_match =
    combinations
    |> list.find(fn(combination) {
      let state =
        combination
        |> list.fold(initial_state, fn(acc, button) {
          button
          |> list.fold(acc, fn(a, pos) {
            a
            |> list.index_map(fn(v, i) {
              case i == pos {
                True -> !v
                False -> v
              }
            })
          })
        })

      state == expected
    })

  case any_match {
    Ok(_) -> num
    Error(_) -> find_num(expected, buttons, num + 1)
  }
}
