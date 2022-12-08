use regex::Regex;

pub fn solution_a(input: &str) -> String {
    let lines_iter = input.lines().collect::<Vec<_>>();
    let mut lines_iter = lines_iter.split(|str| str.is_empty());

    let stack = lines_iter.next().unwrap().into_iter();
    let moves = lines_iter.next().unwrap().into_iter();

    let stacks = stack.clone().last().unwrap().chars().fold(0, |acc, curr| {
        if curr.is_digit(10) {
            let curr = curr.to_digit(10).unwrap();

            if curr > acc {
                return curr;
            }
            return acc;
        }
        return acc;
    });

    let mut init: Vec<Vec<char>> = vec![];

    for i in 0..stacks {
        init.insert(i as usize, vec![]);
    }

    let stack = stack.rev().skip(1).fold(init, |mut acc, curr| {
        let break_length = 1;
        let section_length = 3;

        curr.chars().enumerate().for_each(|(index, curr)| {
            if curr.is_alphabetic() {
                let stack_num = index / (break_length + section_length);
                let stack = acc.get_mut(stack_num).unwrap();

                stack.push(curr);
            }
        });

        return acc;
    });
    let stack = moves.into_iter().fold(stack, |mut stack, one_move| {
        let move_regex = Regex::new(r"^move (\d+) from (\d+) to (\d+)$").unwrap();
        let captures = move_regex.captures(one_move);

        let captures = captures.unwrap();

        let amount = captures.get(1).unwrap().as_str().parse::<usize>().unwrap();
        let from = captures.get(2).unwrap().as_str().parse::<usize>().unwrap() - 1;
        let to = captures.get(3).unwrap().as_str().parse::<usize>().unwrap() - 1;

        for _ in 0..amount {
            let popped = stack.get_mut(from).unwrap().pop().unwrap();
            stack.get_mut(to).unwrap().push(popped);
        }

        return stack;
    });

    return stack
        .into_iter()
        .map(|s| s.last().unwrap().to_string())
        .collect();
}

pub fn solution_b(input: &str) -> String {
    let lines_iter = input.lines().collect::<Vec<_>>();
    let mut lines_iter = lines_iter.split(|str| str.is_empty());

    let stack = lines_iter.next().unwrap().into_iter();
    let moves = lines_iter.next().unwrap().into_iter();

    let stacks = stack.clone().last().unwrap().chars().fold(0, |acc, curr| {
        if curr.is_digit(10) {
            let curr = curr.to_digit(10).unwrap();

            if curr > acc {
                return curr;
            }
            return acc;
        }
        return acc;
    });

    let mut init: Vec<Vec<char>> = vec![];

    for i in 0..stacks {
        init.insert(i as usize, vec![]);
    }

    let stack = stack.rev().skip(1).fold(init, |mut acc, curr| {
        let break_length = 1;
        let section_length = 3;

        curr.chars().enumerate().for_each(|(index, curr)| {
            if curr.is_alphabetic() {
                let stack_num = index / (break_length + section_length);
                let stack = acc.get_mut(stack_num).unwrap();

                stack.push(curr);
            }
        });

        return acc;
    });
    let stack = moves.into_iter().fold(stack, |mut stack, one_move| {
        let move_regex = Regex::new(r"^move (\d+) from (\d+) to (\d+)$").unwrap();
        let captures = move_regex.captures(one_move);

        let captures = captures.unwrap();

        let amount = captures.get(1).unwrap().as_str().parse::<usize>().unwrap();
        let from = captures.get(2).unwrap().as_str().parse::<usize>().unwrap() - 1;
        let to = captures.get(3).unwrap().as_str().parse::<usize>().unwrap() - 1;

        let mut temp_stack = vec![];

        for _ in 0..amount {
            let popped = stack.get_mut(from).unwrap().pop().unwrap();
            temp_stack.push(popped);
        }

        temp_stack.reverse();

        stack.get_mut(to).unwrap().append(&mut temp_stack);

        return stack;
    });

    return stack
        .into_iter()
        .map(|s| s.last().unwrap().to_string())
        .collect();
}
