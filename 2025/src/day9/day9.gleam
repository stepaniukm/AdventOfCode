import gleam/dict
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn part1(input: List(String)) -> Int {
  let input_length = list.length(input)
  let points =
    input
    |> list.index_fold(dict.new(), fn(acc, line, index) {
      let nums =
        line
        |> string.split(",")
        |> list.map(fn(n) { n |> int.parse |> result.unwrap(0) })

      case nums {
        [n1, n2] -> acc |> dict.insert(index, #(n1, n2))
        _ -> acc
      }
    })

  let max_area =
    list.range(0, input_length - 2)
    |> list.fold(0, fn(acc, i) {
      list.range(i + 1, input_length - 1)
      |> list.fold(acc, fn(inner_acc, j) {
        let p1 = dict.get(points, i) |> result.unwrap(#(0, 0))
        let p2 = dict.get(points, j) |> result.unwrap(#(0, 0))

        let dist_x = int.absolute_value(p1.0 - p2.0) + 1
        let dist_y = int.absolute_value(p1.1 - p2.1) + 1

        let area = dist_x * dist_y

        case area > inner_acc {
          True -> area
          False -> inner_acc
        }
      })
    })

  max_area
}

pub fn part2(input: List(String)) -> Int {
  let input_length = list.length(input)
  let points =
    input
    |> list.map(fn(line) {
      let nums =
        line
        |> string.split(",")
        |> list.map(fn(n) { n |> int.parse |> result.unwrap(0) })

      case nums {
        [n1, n2] -> #(n1, n2)
        _ -> #(0, 0)
      }
    })

  let lines =
    list.zip(
      points,
      points
        |> list.drop(1)
        |> list.append([list.first(points) |> result.unwrap(#(0, 0))]),
    )

  let vertical_lines =
    lines
    |> list.filter(fn(line) { line.0.0 == line.1.0 })

  let horizontal_lines =
    lines
    |> list.filter(fn(line) { line.0.1 == line.1.1 })

  let edge_cuts_rectangle = fn(
    rect_x_min: Int,
    rect_x_max: Int,
    rect_y_min: Int,
    rect_y_max: Int,
  ) -> Bool {
    let vertical_cuts =
      list.any(vertical_lines, fn(line) {
        let x_line = line.0.0
        let y_min = int.min(line.0.1, line.1.1)
        let y_max = int.max(line.0.1, line.1.1)
        x_line > rect_x_min
        && x_line < rect_x_max
        && y_min < rect_y_max
        && y_max > rect_y_min
      })

    let horizontal_cuts =
      list.any(horizontal_lines, fn(line) {
        let y_line = line.0.1
        let x_min = int.min(line.0.0, line.1.0)
        let x_max = int.max(line.0.0, line.1.0)
        y_line > rect_y_min
        && y_line < rect_y_max
        && x_min < rect_x_max
        && x_max > rect_x_min
      })

    vertical_cuts || horizontal_cuts
  }

  let is_on_boundary = fn(point: #(Int, Int)) -> Bool {
    let is_vertex = list.contains(points, point)

    let is_on_vertical_edge =
      list.any(vertical_lines, fn(line) {
        let x_line = line.0.0
        let y_min = int.min(line.0.1, line.1.1)
        let y_max = int.max(line.0.1, line.1.1)
        point.0 == x_line && point.1 >= y_min && point.1 <= y_max
      })

    let is_on_horizontal_edge =
      list.any(horizontal_lines, fn(line) {
        let y_line = line.0.1
        let x_min = int.min(line.0.0, line.1.0)
        let x_max = int.max(line.0.0, line.1.0)
        point.1 == y_line && point.0 >= x_min && point.0 <= x_max
      })

    is_vertex || is_on_vertical_edge || is_on_horizontal_edge
  }

  let max_area =
    list.range(0, input_length - 2)
    |> list.fold(0, fn(acc, i) {
      list.range(i + 1, input_length - 1)
      |> list.fold(acc, fn(inner_acc, j) {
        let p1 = list.drop(points, i) |> list.first |> result.unwrap(#(0, 0))
        let p2 = list.drop(points, j) |> list.first |> result.unwrap(#(0, 0))
        let p3 = case p1.0 == p2.0 || p1.1 == p2.1 {
          True -> p1
          False -> #(p1.0, p2.1)
        }
        let p4 = case p1.0 == p2.0 || p1.1 == p2.1 {
          True -> p2
          False -> #(p2.0, p1.1)
        }

        let all_points_inside =
          [p1, p2, p3, p4]
          |> list.all(fn(point) {
            case is_on_boundary(point) {
              True -> True
              False -> {
                let crossed_lines_going_to_the_left =
                  list.filter(vertical_lines, fn(line) {
                    let x_line = line.0.0 |> int.to_float
                    let y_min = int.min(line.0.1, line.1.1) |> int.to_float
                    let y_max = int.max(line.0.1, line.1.1) |> int.to_float

                    let point_x = point.0 |> int.to_float
                    let point_y = { point.1 |> int.to_float } +. 0.0001

                    x_line <. point_x && point_y >=. y_min && point_y <=. y_max
                  })
                  |> list.length

                crossed_lines_going_to_the_left % 2 == 1
              }
            }
          })

        let rect_x_min = int.min(p1.0, p2.0)
        let rect_x_max = int.max(p1.0, p2.0)
        let rect_y_min = int.min(p1.1, p2.1)
        let rect_y_max = int.max(p1.1, p2.1)
        let no_edge_cuts =
          !edge_cuts_rectangle(rect_x_min, rect_x_max, rect_y_min, rect_y_max)

        case all_points_inside && no_edge_cuts {
          True -> {
            let dist_x = int.absolute_value(p1.0 - p2.0) + 1
            let dist_y = int.absolute_value(p1.1 - p2.1) + 1

            let area = dist_x * dist_y

            case area > inner_acc {
              True -> area
              False -> inner_acc
            }
          }
          False -> inner_acc
        }
      })
    })

  max_area
}
