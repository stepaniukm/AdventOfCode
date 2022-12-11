fn get_register_history(input: &str) -> Vec<isize> {
    return input.lines().fold(vec![1], |mut acc, curr| {
        if curr.contains("addx") {
            let value: String = curr.chars().skip(5).collect();
            let value = value.parse::<isize>().unwrap();

            let last_index: isize = acc.len() as isize - 1;

            let last: isize = acc[last_index as usize];
            acc.push(last);
            acc.push(last + value);
        } else if curr.contains("noop") {
            let last_index: isize = acc.len() as isize - 1;

            if last_index < 0 {
                acc.push(1);
            } else {
                let last: isize = acc[last_index as usize];
                acc.push(last);
            }
        }

        return acc;
    });
}

pub fn solution_a(input: &str) -> isize {
    let register_history = get_register_history(input)
        .into_iter()
        .enumerate()
        .map(|(i, curr)| {
            return (i as isize + 1) * curr;
        })
        .collect::<Vec<isize>>();

    return [20, 60, 100, 140, 180, 220]
        .into_iter()
        .map(|cycle| {
            let value = register_history[cycle - 1];
            return value;
        })
        .sum();
}

pub fn solution_b(input: &str) -> Vec<Vec<String>> {
    let register_history = get_register_history(input);

    return (0..)
        .take(6 * 40)
        .collect::<Vec<_>>()
        .chunks(40)
        .map(|chunk| {
            return chunk
                .into_iter()
                .map(|pixel| {
                    let register_value = register_history[*pixel];
                    if isize::abs_diff(register_value, (*pixel as isize) % 40) <= 1 {
                        return "#".to_owned();
                    } else {
                        return ".".to_owned();
                    }
                })
                .collect();
        })
        .collect();
}
