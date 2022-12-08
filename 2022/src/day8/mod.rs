use std::fmt::Debug;

#[derive(Debug, Clone, Copy, PartialEq)]
struct Entry {
    x: usize,
    y: usize,
    height: usize,
}

fn rotate_90<T: Debug + Clone>(v: Vec<Vec<T>>) -> Vec<Vec<T>> {
    let height = v.len();
    let width = v.first().unwrap().len();
    let mut new_vec: Vec<Vec<T>> = vec![];

    for i in (0..width).rev() {
        let mut inner_vec = vec![];
        for j in 0..height {
            let h = v.get(j).unwrap().get(i).unwrap();
            inner_vec.push((*h).clone());
        }
        new_vec.push(inner_vec);
    }

    return new_vec;
}

fn append_visible(visible: &mut Vec<Entry>, map: &Vec<Vec<Entry>>) {
    (&map).into_iter().for_each(|row| {
        let mut prevs_in_line: Vec<usize> = vec![];
        row.into_iter().for_each(|entry| {
            let all_prev_lower = (&prevs_in_line).into_iter().all(|height| {
                return height < &entry.height;
            });

            let already_visible = visible.into_iter().find(|visible_entry| {
                return visible_entry.x == entry.x && visible_entry.y == entry.y;
            });

            let entry_height = entry.height;

            if all_prev_lower && already_visible.is_none() {
                visible.push(*entry);
            }

            prevs_in_line.push(entry_height);
        });
    });
}

fn get_map(input: &str, map: &mut Vec<Vec<Entry>>) {
    input.lines().enumerate().for_each(|(line_index, line)| {
        let chars_enumerated = line.chars().enumerate();
        let mut inner_vec = vec![];
        chars_enumerated.for_each(|(char_index, height)| {
            let height = height.to_digit(10).unwrap() as usize;
            inner_vec.push(Entry {
                x: char_index,
                y: line_index,
                height,
            });
        });
        map.push(inner_vec);
    });
}

pub fn solution_a(input: &str) -> usize {
    let mut map: Vec<Vec<Entry>> = vec![];
    let mut visible: Vec<Entry> = vec![];

    get_map(input, &mut map);

    append_visible(&mut visible, &map);

    for _ in 0..4 {
        map = rotate_90(map);

        append_visible(&mut visible, &map);
    }

    return visible.len();
}

fn get_trees(entry: &Entry, map: &Vec<Vec<Entry>>) -> Vec<Vec<Entry>> {
    let mut row = map
        .get(entry.y)
        .unwrap()
        .split(|inside_entry| inside_entry.x == entry.x && inside_entry.y == entry.y);

    let mut left = row.next().unwrap().to_vec();
    left.reverse();
    let right = row.next().unwrap().to_vec();

    let mut column: Vec<Entry> = vec![];

    let height = map.len();

    for i in 0..height {
        let inner_entry = map.get(i).unwrap().get(entry.x).unwrap();
        column.push(*inner_entry);
    }

    let mut column =
        column.split(|inside_entry| inside_entry.x == entry.x && inside_entry.y == entry.y);

    let mut top = column.next().unwrap().to_vec();
    top.reverse();
    let bottom = column.next().unwrap().to_vec();

    return vec![left, right, top, bottom];
}

fn calculate_scenic(height: usize, directions: Vec<Vec<Entry>>) -> usize {
    let mut counters = vec![0, 0, 0, 0];
    directions
        .into_iter()
        .enumerate()
        .for_each(|(index, direction)| {
            let mut max_height = None;
            direction.into_iter().for_each(|entry| match max_height {
                None => {
                    max_height = Some(entry.height);
                    *counters.get_mut(index).unwrap() += 1;
                }
                Some(max) => {
                    if max < height {
                        *counters.get_mut(index).unwrap() += 1;
                        max_height = Some(entry.height);
                    }
                }
            });
        });

    return counters.into_iter().product();
}

pub fn solution_b(input: &str) -> usize {
    let mut map: Vec<Vec<Entry>> = vec![];
    get_map(input, &mut map);

    return (&map).into_iter().fold(0, |acc, row| {
        let row_highest = row.into_iter().fold(0, |acc, entry| {
            let directions = get_trees(entry, &map);
            let score = calculate_scenic(entry.height, directions);

            if score > acc {
                return score;
            }
            return acc;
        });

        if row_highest > acc {
            return row_highest;
        }
        return acc;
    });
}
