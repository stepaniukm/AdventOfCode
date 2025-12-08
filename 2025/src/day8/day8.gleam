import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/set
import gleam/string

pub fn part1(input: List(String), take: Int) -> Int {
  let input_length = list.length(input)

  let triples_dict =
    input
    |> list.index_fold(dict.new(), fn(acc, curr, index) {
      let nums =
        curr
        |> string.split(",")
        |> list.map(fn(x) { int.parse(x) |> result.unwrap(0) })

      case nums {
        [a, b, c] -> acc |> dict.insert(index, #(a, b, c))
        _ -> acc
      }
    })

  let distances =
    list.range(0, input_length - 1)
    |> list.fold(dict.new(), fn(acc, i) {
      list.range(i + 1, input_length)
      |> list.fold(acc, fn(inner_acc, j) {
        let pos_i = dict.get(triples_dict, i) |> result.unwrap(#(0, 0, 0))
        let pos_j = dict.get(triples_dict, j) |> result.unwrap(#(0, 0, 0))

        let d1 =
          float.power(int.to_float(pos_j.0) -. int.to_float(pos_i.0), 2.0)
          |> result.unwrap(0.0)
        let d2 =
          float.power(int.to_float(pos_j.1) -. int.to_float(pos_i.1), 2.0)
          |> result.unwrap(0.0)
        let d3 =
          float.power(int.to_float(pos_j.2) -. int.to_float(pos_i.2), 2.0)
          |> result.unwrap(0.0)

        let distance = float.square_root(d1 +. d2 +. d3) |> result.unwrap(0.0)

        inner_acc |> dict.insert(#(i, j), distance)
      })
    })

  let distances_sorted =
    distances
    |> dict.to_list()
    |> list.sort(fn(a, b) { float.compare(a.1, b.1) })
    |> list.take(take)

  let list_new: List(set.Set(Int)) = list.new()

  let circuits =
    distances_sorted
    |> list.fold(list_new, fn(acc, curr) {
      let any_match =
        list.find(acc, fn(s) {
          set.contains(s, curr.0.0) || set.contains(s, curr.0.1)
        })

      case any_match {
        Ok(_) -> {
          acc
          |> list.map(fn(s) {
            let is = set.contains(s, curr.0.0) || set.contains(s, curr.0.1)

            case is {
              False -> s
              True -> {
                let new_set = set.insert(set.insert(s, curr.0.0), curr.0.1)
                new_set
              }
            }
          })
        }
        Error(_) -> {
          let new_set =
            set.new()
            |> set.insert(curr.0.0)
            |> set.insert(curr.0.1)

          acc |> list.append([new_set])
        }
      }
    })

  simplify_circuits(circuits)
  |> list.sort(fn(a, b) { int.compare(set.size(b), set.size(a)) })
  |> list.take(3)
  |> list.fold(1, fn(acc, curr) { acc * set.size(curr) })
}

pub fn part2(input: List(String)) -> Int {
  let input_length = list.length(input)

  let triples_dict =
    input
    |> list.index_fold(dict.new(), fn(acc, curr, index) {
      let nums =
        curr
        |> string.split(",")
        |> list.map(fn(x) { int.parse(x) |> result.unwrap(0) })

      case nums {
        [a, b, c] -> acc |> dict.insert(index, #(a, b, c))
        _ -> acc
      }
    })

  let distances =
    list.range(0, input_length - 1)
    |> list.fold(dict.new(), fn(acc, i) {
      list.range(i + 1, input_length)
      |> list.fold(acc, fn(inner_acc, j) {
        let pos_i = dict.get(triples_dict, i) |> result.unwrap(#(0, 0, 0))
        let pos_j = dict.get(triples_dict, j) |> result.unwrap(#(0, 0, 0))

        let d1 =
          float.power(int.to_float(pos_j.0) -. int.to_float(pos_i.0), 2.0)
          |> result.unwrap(0.0)
        let d2 =
          float.power(int.to_float(pos_j.1) -. int.to_float(pos_i.1), 2.0)
          |> result.unwrap(0.0)
        let d3 =
          float.power(int.to_float(pos_j.2) -. int.to_float(pos_i.2), 2.0)
          |> result.unwrap(0.0)

        let distance = float.square_root(d1 +. d2 +. d3) |> result.unwrap(0.0)

        inner_acc |> dict.insert(#(i, j), distance)
      })
    })

  let distances_sorted =
    distances
    |> dict.to_list()
    |> list.sort(fn(a, b) { float.compare(a.1, b.1) })

  let c =
    list.range(2, list.length(distances_sorted))
    |> list.find(fn(x) {
      echo x
      let circuits = get_circuits(distances_sorted |> list.take(x))

      let a = simplify_circuits(circuits)

      list.length(a) == 1
    })
    |> result.unwrap(0)

  let distance =
    distances_sorted
    |> list.take(c)
    |> list.last
    |> result.unwrap(#(#(0, 0), 0.0))

  let first_triple =
    triples_dict |> dict.get(distance.0.0) |> result.unwrap(#(0, 0, 0))
  let second_triple =
    triples_dict |> dict.get(distance.0.1) |> result.unwrap(#(0, 0, 0))

  first_triple.0 * second_triple.0
}

fn simplify_circuits(circuits: List(set.Set(Int))) -> List(set.Set(Int)) {
  let result =
    circuits
    |> list.fold(#(False, list.new()), fn(acc, curr) {
      let any_subset =
        list.find(acc.1, fn(s) { set.intersection(curr, s) |> set.size() > 0 })

      case any_subset {
        Ok(_) -> {
          #(
            True,
            acc.1
              |> list.map(fn(existing_set) {
                let is = set.intersection(curr, existing_set) |> set.size() > 0
                case is {
                  False -> existing_set
                  True -> set.union(curr, existing_set)
                }
              }),
          )
        }
        Error(_) -> #(acc.0, acc.1 |> list.append([curr]))
      }
    })

  case result.0 {
    True -> simplify_circuits(result.1)
    False -> result.1
  }
}

fn get_circuits(
  distances_sorted: List(#(#(Int, Int), Float)),
) -> List(set.Set(Int)) {
  let list_new: List(set.Set(Int)) = list.new()

  distances_sorted
  |> list.fold(list_new, fn(acc, curr) {
    let any_match =
      list.find(acc, fn(s) {
        set.contains(s, curr.0.0) || set.contains(s, curr.0.1)
      })

    case any_match {
      Ok(_) -> {
        acc
        |> list.map(fn(s) {
          let is = set.contains(s, curr.0.0) || set.contains(s, curr.0.1)

          case is {
            False -> s
            True -> {
              let new_set = set.insert(set.insert(s, curr.0.0), curr.0.1)
              new_set
            }
          }
        })
      }
      Error(_) -> {
        let new_set =
          set.new()
          |> set.insert(curr.0.0)
          |> set.insert(curr.0.1)

        acc |> list.append([new_set])
      }
    }
  })
}
