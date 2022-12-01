use std::fs;

pub mod day1;

fn main() {
    let file_content = fs::read_to_string("src/day1/input.txt").expect("This file has to exist");
    let result = day1::solution_a(file_content.as_str());
    let result2 = day1::solution_b(file_content.as_str());

    println!("{} {}", result, result2);
}
