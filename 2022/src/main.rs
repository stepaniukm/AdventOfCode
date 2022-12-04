use std::fs;

pub mod day1;
pub mod day2;
pub mod day3;
pub mod day4;

fn main() {
    let file_content = fs::read_to_string("src/day4/input.txt").expect("This file has to exist");
    let result = day4::solution_a(file_content.as_str());
    let result2 = day4::solution_b(file_content.as_str());

    println!("{} {}", result, result2);
}
