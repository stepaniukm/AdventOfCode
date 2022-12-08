use std::fs;

pub mod day1;
pub mod day2;
pub mod day3;
pub mod day4;
pub mod day5;
pub mod day6;
pub mod day7;
pub mod day8;

fn main() {
    let file_content = fs::read_to_string("src/day8/input.txt").expect("This file has to exist");
    let result = day8::solution_a(file_content.as_str());
    let result2 = day8::solution_b(file_content.as_str());

    println!("{} {}", result, result2);
}
