enum RPC {
    Rock,
    Paper,
    Scissors,
}

#[derive(PartialEq, Copy, Clone)]
enum Result {
    Win = 6,
    Draw = 3,
    Lose = 0,
}

fn letter_to_rpc(letter: &str) -> RPC {
    if letter == "A" || letter == "X" {
        return RPC::Rock;
    } else if letter == "B" || letter == "Y" {
        return RPC::Paper;
    } else if letter == "C" || letter == "Z" {
        return RPC::Scissors;
    }

    panic!("Invalid letter");
}

fn letter_to_outcome(letter: &str) -> Result {
    if letter == "X" {
        return Result::Lose;
    } else if letter == "Y" {
        return Result::Draw;
    } else if letter == "Z" {
        return Result::Win;
    }

    panic!("Invalid letter");
}

fn rpc_to_points(rpc: &RPC) -> u8 {
    match rpc {
        RPC::Rock => 1,
        RPC::Paper => 2,
        RPC::Scissors => 3,
    }
}

fn get_shape_matching_points(opponent: &RPC, score: &Result) -> RPC {
    match opponent {
        RPC::Rock => {
            if score == &Result::Win {
                RPC::Paper
            } else if score == &Result::Draw {
                RPC::Rock
            } else {
                RPC::Scissors
            }
        }
        RPC::Paper => {
            if score == &Result::Win {
                RPC::Scissors
            } else if score == &Result::Draw {
                RPC::Paper
            } else {
                RPC::Rock
            }
        }
        RPC::Scissors => {
            if score == &Result::Win {
                RPC::Rock
            } else if score == &Result::Draw {
                RPC::Scissors
            } else {
                RPC::Paper
            }
        }
    }
}

fn get_points(me: &RPC, opponent: &RPC) -> u8 {
    match me {
        RPC::Rock => match opponent {
            RPC::Rock => {
                return rpc_to_points(me) + Result::Draw as u8;
            }
            RPC::Paper => {
                return rpc_to_points(me) + Result::Lose as u8;
            }
            RPC::Scissors => {
                return rpc_to_points(me) + Result::Win as u8;
            }
        },
        RPC::Paper => match opponent {
            RPC::Rock => {
                return rpc_to_points(me) + Result::Win as u8;
            }
            RPC::Paper => {
                return rpc_to_points(me) + Result::Draw as u8;
            }
            RPC::Scissors => {
                return rpc_to_points(me) + Result::Lose as u8;
            }
        },
        RPC::Scissors => match opponent {
            RPC::Rock => {
                return rpc_to_points(me) + Result::Lose as u8;
            }
            RPC::Paper => {
                return rpc_to_points(me) + Result::Win as u8;
            }
            RPC::Scissors => {
                return rpc_to_points(me) + Result::Draw as u8;
            }
        },
    }
}

pub fn solution_a(input: &str) -> usize {
    return input.lines().fold(0, |acc, curr| {
        let mut split = curr.split_whitespace();
        let opponents_choice = split.next().unwrap();
        let my_choice = split.next().unwrap();

        return acc
            + get_points(&letter_to_rpc(my_choice), &letter_to_rpc(opponents_choice)) as usize;
    });
}

pub fn solution_b(input: &str) -> usize {
    return input.lines().fold(0, |acc, curr| {
        let mut split = curr.split_whitespace();
        let opponents_choice = split.next().unwrap();
        let opponents_choice = letter_to_rpc(opponents_choice);
        let desired_outcome = split.next().unwrap();
        let desired_outcome = letter_to_outcome(desired_outcome);

        let my_choice = get_shape_matching_points(&opponents_choice, &desired_outcome);

        return acc + desired_outcome as usize + rpc_to_points(&my_choice) as usize;
    });
}
