fn get_sums(input: &str) -> Vec<usize> {
    return input.lines().into_iter().fold(vec![0], |mut acc, curr| {
        if curr.is_empty() {
            acc.push(0);
        } else {
            let num = curr.parse::<usize>().expect("To be a number");
            let current_last = acc.pop().expect("To have at least one number");

            acc.push(current_last + num);
        }

        return acc;
    });
}

pub fn solution_a(input: &str) -> usize {
    return get_sums(input)
        .into_iter()
        .reduce(usize::max)
        .expect("To have max");
}

pub fn solution_b(input: &str) -> usize {
    let mut sums = get_sums(input);
    sums.sort();

    return sums.into_iter().rev().take(3).sum();
}
