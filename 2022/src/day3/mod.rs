fn split_string_into_two_equal(s: &str) -> (&str, &str) {
    let mid = s.len() / 2;
    return s.split_at(mid);
}

fn find_common_letters(a: &str, b: &str) -> String {
    return a.chars().fold("".to_owned(), |acc, curr| {
        if b.contains(curr) && !acc.contains(curr) {
            return acc + &curr.to_string()[..];
        }
        return acc;
    });
}

fn char_to_value(c: char) -> u8 {
    if c.is_lowercase() {
        return c as u8 - 96u8;
    } else {
        return c as u8 - 38u8;
    }
}

fn find_common_letters_in_groups_of_three<'a>(chunk: Vec<String>) -> String {
    return chunk
        .into_iter()
        .reduce(|acc, curr| {
            return find_common_letters(&acc[..], &curr[..]);
        })
        .unwrap()
        .to_string();
}

pub fn solution_a(input: &str) -> usize {
    return input.lines().fold(0, |acc, curr| {
        let (left, right) = split_string_into_two_equal(curr);
        let common_letters = find_common_letters(left, right);
        if common_letters.len() > 1 {
            panic!("There should be only 1 common letter in solution a");
        }
        let common_letter = common_letters.chars().nth(0).unwrap();
        let char_value = char_to_value(common_letter);

        return acc + char_value as usize;
    });
}

pub fn solution_b(input: &str) -> usize {
    return input
        .lines()
        .collect::<Vec<_>>()
        .chunks(3)
        .fold(0, |acc, curr| {
            let common_letters = find_common_letters_in_groups_of_three(
                curr.into_iter()
                    .map(|f| {
                        return f.to_owned().to_owned();
                    })
                    .collect::<Vec<_>>(),
            );
            if common_letters.len() > 1 {
                panic!("In solution b after getting common letter of 3 groups there should be only one that");
            }
            let common_letter = common_letters.chars().nth(0).unwrap();
            return acc + char_to_value(common_letter) as usize;
        });
}
