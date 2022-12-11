use std::collections::HashSet;

#[derive(Debug, Clone)]
struct Monkey {
    starting_numbers: Vec<u128>,
    operation: String,
    test: u8,
    if_true: u8,
    if_false: u8,
}

fn get_operation_from_string(str: String) -> Box<dyn Fn(u128) -> u128> {
    let parts: Vec<String> = str
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect::<String>()
        .split_inclusive(|char| {
            return char == '+' || char == '-' || char == '*' || char == '/';
        })
        .flat_map(|part| {
            let parts: (Vec<char>, Vec<char>) = part.to_owned().chars().partition(|char| {
                return *char == '+' || *char == '-' || *char == '*' || *char == '/';
            });

            let mut v = vec![];
            let part1 = parts.1.into_iter().collect::<String>();
            let part2 = parts.0.into_iter().collect::<String>();

            v.push(part1);

            if part2 != "" {
                v.push(part2);
            }

            return v;
        })
        .collect();

    let a = parts[0].clone();
    let operator = parts[1].clone();
    let b = parts[2].clone();

    return Box::new(move |old| {
        let op_a: u128;
        let op_b: u128;
        if a == "old" {
            op_a = old;
        } else {
            op_a = a.parse::<u128>().unwrap();
        }
        if b == "old" {
            op_b = old;
        } else {
            op_b = b.parse::<u128>().unwrap();
        }
        if operator == "+" {
            return op_a + op_b;
        } else if operator == "*" {
            return op_a * op_b;
        } else if operator == "/" {
            return op_a / op_b;
        } else {
            return op_a - op_b;
        }
    });
}

fn get_monkeys(input: &str) -> Vec<Monkey> {
    return input
        .lines()
        .collect::<Vec<_>>()
        .split(|line| *line == "")
        .map(|el| el.to_vec())
        .collect::<Vec<Vec<&str>>>()
        .into_iter()
        .map(|monkey_lines| {
            let mut lines = monkey_lines.into_iter();
            let _ = lines.next().unwrap();
            let starting_numbers = lines.next().unwrap();
            let starting_numbers = starting_numbers
                .chars()
                .skip(18)
                .collect::<String>()
                .trim()
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect::<String>()
                .split(|el| el == ',')
                .map(|num| {
                    return num.parse::<u128>().unwrap();
                })
                .collect::<Vec<u128>>();
            let operation = lines.next().unwrap();
            let operation = operation.chars().skip(19).collect::<String>();
            let test = lines.next().unwrap().to_owned();
            let test = test
                .chars()
                .skip(21)
                .collect::<String>()
                .parse::<u8>()
                .unwrap();
            let if_true = lines.next().unwrap();
            let if_true = if_true
                .chars()
                .skip(29)
                .collect::<String>()
                .parse::<u8>()
                .unwrap();
            let if_false = lines.next().unwrap();
            let if_false = if_false
                .chars()
                .skip(30)
                .collect::<String>()
                .parse::<u8>()
                .unwrap();

            return Monkey {
                starting_numbers,
                operation,
                test,
                if_true,
                if_false,
            };
        })
        .collect();
}

pub fn solution_a(input: &str) -> usize {
    let mut monkeys: Vec<Monkey> = get_monkeys(input);
    let monkey_len = monkeys.len();
    let mut monkeys_counter: Vec<usize> = (0..monkey_len).map(|_| 0).collect();

    for _ in 0..20 {
        let mut items: Vec<Vec<u128>> = (0..monkey_len).map(|_| vec![]).collect();
        for monkey_index in 0..monkey_len {
            let mut changed_indices: HashSet<usize> = HashSet::new();
            let monkey = &mut monkeys[monkey_index];
            let nums_len = monkey.starting_numbers.len();

            let counter = monkeys_counter.get_mut(monkey_index);

            match counter {
                Some(inner_counter) => {
                    monkeys_counter[monkey_index] = *inner_counter + nums_len;
                }
                None => {
                    monkeys_counter[monkey_index] = nums_len;
                }
            }

            for num_index in 0..nums_len {
                let num = monkey.starting_numbers[num_index];

                let operation = get_operation_from_string(monkey.operation.clone());
                let new_num = operation(num) / 3;
                if new_num % monkey.test as u128 == 0 {
                    changed_indices.insert(monkey.if_true as usize);
                    let monkey = &mut items[monkey.if_true as usize];
                    monkey.push(new_num);
                } else {
                    changed_indices.insert(monkey.if_false as usize);
                    let monkey = &mut items[monkey.if_false as usize];
                    monkey.push(new_num);
                }
            }

            for _ in 0..nums_len {
                monkey.starting_numbers.remove(0);
            }

            drop(monkey);

            for changed_index in changed_indices {
                let additional_items = &mut items[changed_index];

                let additional_items_len = additional_items.len();

                for additional_item_index in 0..additional_items_len {
                    let inner_item = additional_items[additional_item_index];
                    let monkey = &mut monkeys[changed_index];

                    monkey.starting_numbers.push(inner_item);
                }

                for _ in 0..additional_items_len {
                    additional_items.remove(0);
                }
            }
        }
    }

    monkeys_counter.sort_by(|a, b| b.cmp(a));

    return monkeys_counter.into_iter().take(2).product::<usize>();
}

pub fn solution_b(input: &str) -> usize {
    let mut monkeys: Vec<Monkey> = get_monkeys(input);
    let monkey_len = monkeys.len();
    let mut monkeys_counter: Vec<usize> = (0..monkey_len).map(|_| 0).collect();
    let product_of_divisors = (&monkeys)
        .into_iter()
        .map(|m| m.test as u128)
        .product::<u128>();

    for _ in 0..10_000 {
        let mut items: Vec<Vec<u128>> = (0..monkey_len).map(|_| vec![]).collect();
        for monkey_index in 0..monkey_len {
            let mut changed_indices: HashSet<usize> = HashSet::new();
            let monkey = &mut monkeys[monkey_index];
            let nums_len = monkey.starting_numbers.len();

            let counter = monkeys_counter.get_mut(monkey_index);

            match counter {
                Some(inner_counter) => {
                    monkeys_counter[monkey_index] = *inner_counter + nums_len;
                }
                None => {
                    monkeys_counter[monkey_index] = nums_len;
                }
            }

            for num_index in 0..nums_len {
                let num = monkey.starting_numbers[num_index];

                let operation = get_operation_from_string(monkey.operation.clone());
                let new_num = operation(num) % product_of_divisors;
                if new_num % monkey.test as u128 == 0 {
                    changed_indices.insert(monkey.if_true as usize);
                    let m = &mut items[monkey.if_true as usize];
                    m.push(new_num);
                } else {
                    changed_indices.insert(monkey.if_false as usize);
                    let m = &mut items[monkey.if_false as usize];
                    m.push(new_num);
                }
            }

            for _ in 0..nums_len {
                monkey.starting_numbers.remove(0);
            }

            drop(monkey);

            for changed_index in changed_indices {
                let additional_items = &mut items[changed_index];

                let additional_items_len = additional_items.len();

                for additional_item_index in 0..additional_items_len {
                    let inner_item = additional_items[additional_item_index];
                    let monkey = &mut monkeys[changed_index];

                    monkey.starting_numbers.push(inner_item);
                }

                for _ in 0..additional_items_len {
                    additional_items.remove(0);
                }
            }
        }
    }

    monkeys_counter.sort_by(|a, b| b.cmp(a));

    return monkeys_counter.into_iter().take(2).product::<usize>();
}
