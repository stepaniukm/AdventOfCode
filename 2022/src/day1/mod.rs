fn get_sums(input: &str) -> Vec<usize> {
    return input.lines().fold(vec![0], |mut acc, curr| {
        if curr.is_empty() {
            acc.push(0);
        } else {
            let num = curr.parse::<usize>().unwrap();
            let current_last = acc.pop().unwrap();

            acc.push(current_last + num);
        }

        return acc;
    });
}

pub fn solution_a(input: &str) -> usize {
    return get_sums(input).into_iter().reduce(usize::max).unwrap();
}

pub fn solution_b(input: &str) -> usize {
    let mut sums = get_sums(input);
    sums.sort_by(|a, b| b.cmp(a));

    return sums.into_iter().take(3).sum();
}
