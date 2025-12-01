import gleam/result
import gleam/string
import simplifile

pub fn read_lines_from_file(path: String) -> List(String) {
  simplifile.read(path)
  |> result.map(fn(content) { string.split(content, "\n") })
  |> result.unwrap([])
}
