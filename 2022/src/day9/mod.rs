#[derive(Clone, Copy, PartialEq, Debug)]
struct Coord {
    x: isize,
    y: isize,
}

fn is_touching(a: &Coord, b: &Coord) -> bool {
    return isize::abs_diff(a.x, b.x) <= 1 && isize::abs_diff(a.y, b.y) <= 1;
}

fn get_steps(line: &str) -> isize {
    return line
        .chars()
        .skip(2)
        .collect::<String>()
        .parse::<isize>()
        .unwrap();
}

pub fn solution_a(input: &str) -> usize {
    let mut tail_visited: Vec<Coord> = vec![Coord { x: 0, y: 0 }];
    let mut last_head: Coord = Coord { x: 0, y: 0 };
    let mut current_head = Coord { x: 0, y: 0 };
    let mut current_tail = Coord { x: 0, y: 0 };

    input.lines().for_each(|line| {
        let steps = get_steps(line);
        for _ in 0..steps {
            last_head = current_head;
            if line.contains("D") {
                current_head = Coord {
                    x: current_head.x,
                    y: current_head.y - 1,
                };
            } else if line.contains("U") {
                current_head = Coord {
                    x: current_head.x,
                    y: current_head.y + 1,
                };
            } else if line.contains("L") {
                current_head = Coord {
                    x: current_head.x - 1,
                    y: current_head.y,
                };
            } else if line.contains("R") {
                current_head = Coord {
                    x: current_head.x + 1,
                    y: current_head.y,
                };
            }

            if !is_touching(&current_head, &current_tail) {
                current_tail = last_head;
                if !tail_visited.contains(&current_tail) {
                    tail_visited.push(current_tail);
                }
            }
        }
    });

    return tail_visited.len();
}

pub fn solution_b(input: &str) -> usize {
    let mut tail_visited: Vec<Coord> = vec![Coord { x: 0, y: 0 }];
    let mut current_positions: Vec<Coord> = (0..10).map(|_| Coord { x: 0, y: 0 }).collect();

    input.lines().for_each(|line| {
        let steps = get_steps(line);
        for _ in 0..steps {
            let head = current_positions.first_mut().unwrap();
            if line.contains("D") {
                *head = Coord {
                    x: head.x,
                    y: head.y - 1,
                };
            } else if line.contains("U") {
                *head = Coord {
                    x: head.x,
                    y: head.y + 1,
                };
            } else if line.contains("L") {
                *head = Coord {
                    x: head.x - 1,
                    y: head.y,
                };
            } else if line.contains("R") {
                *head = Coord {
                    x: head.x + 1,
                    y: head.y,
                };
            }
            for curr_index in 0..current_positions.len() {
                let current_position = current_positions[curr_index];
                let next_index = curr_index + 1;

                if next_index < current_positions.len() {
                    let next_position = current_positions[next_index];

                    if !is_touching(&current_position, &current_positions[next_index]) {
                        let mut new_x = current_positions[next_index].x;
                        let mut new_y = current_positions[next_index].y;

                        if current_position.x > current_positions[next_index].x {
                            new_x = current_positions[next_index].x + 1;
                        } else if current_position.x < next_position.x {
                            new_x = current_positions[next_index].x - 1;
                        }

                        if current_position.y > current_positions[next_index].y {
                            new_y = current_positions[next_index].y + 1;
                        } else if current_position.y < current_positions[next_index].y {
                            new_y = current_positions[next_index].y - 1;
                        }

                        current_positions[next_index] = Coord { x: new_x, y: new_y };

                        if next_index == current_positions.len() - 1 {
                            if !tail_visited.contains(&current_positions[next_index]) {
                                tail_visited.push(current_positions[next_index]);
                            }
                        }
                    }
                }
            }
        }
    });

    return tail_visited.len();
}
