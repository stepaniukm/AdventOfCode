use std::collections::HashSet;

pub fn solution_a(input: &str) -> usize {
    let chars = input.chars();
    let chars_vec = chars.clone().collect::<Vec<_>>();
    let mut enumerated = chars.enumerate();

    return enumerated
        .find_map(|(index, char)| {
            let next = chars_vec.get(index + 1);
            let next_next = chars_vec.get(index + 2);
            let next_next_next = chars_vec.get(index + 3);

            if next.is_some() && next_next.is_some() && next_next_next.is_some() {
                let mut set = HashSet::new();
                set.insert(char);
                set.insert(*next.unwrap());
                set.insert(*next_next.unwrap());
                set.insert(*next_next_next.unwrap());

                if set.len() == 4 {
                    return Some(index + 4);
                }

                return None;
            } else {
                return None;
            }
        })
        .unwrap();
}

pub fn solution_b(input: &str) -> usize {
    let chars = input.chars();
    let chars_vec = chars.clone().collect::<Vec<_>>();
    let mut enumerated = chars.enumerate();

    return enumerated
        .find_map(|(index, char)| {
            let mut set = HashSet::new();
            set.insert(char);
            for i in 1..14 {
                let next = chars_vec.get(index + i);
                if next.is_some() {
                    set.insert(*next.unwrap());
                }
            }

            if set.len() == 14 {
                return Some(index + 14);
            }

            return None;
        })
        .unwrap();
}
