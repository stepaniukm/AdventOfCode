use regex::Regex;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Clone, Copy, Debug)]
enum Command<'a> {
    Ls,
    Cd(&'a str),
}

fn is_input(line: &str) -> bool {
    return line.starts_with("$");
}

fn parse_command<'a>(line: &'a str) -> Option<Command<'a>> {
    let ls_regex = Regex::new(r"\$ ls").unwrap();
    let cd_regex = Regex::new(r"\$ cd (.+)").unwrap();
    if ls_regex.is_match(line) {
        return Some(Command::Ls);
    } else if cd_regex.is_match(line) {
        let directory = cd_regex.captures(line).unwrap().get(1).unwrap().as_str();

        return Some(Command::Cd(directory));
    } else {
        return None;
    }
}

pub fn solution_a(input: &str) -> usize {
    let mut directory_sizes: HashMap<String, usize> = HashMap::new();
    let mut current_path_origin: Rc<RefCell<Vec<String>>> = Rc::new(RefCell::new(vec![]));
    let mut current_command: Option<Command> = None;
    input.lines().for_each(|line| {
        let current_path = Rc::clone(&current_path_origin);
        let current_path = &mut *current_path.borrow_mut();
        if is_input(line) {
            let command = parse_command(line).unwrap();
            current_command = Some(command);
            match command {
                Command::Ls => {}
                Command::Cd(dir) => {
                    if dir == ".." {
                        current_path.pop();
                    } else if dir == "/" {
                        current_path_origin = Rc::new(RefCell::new(vec!["/".to_owned()]));
                    } else {
                        let last = current_path.last().unwrap();
                        let dir = format!("{last}/{dir}");
                        current_path.push(dir);
                    }
                }
            }
        } else {
            match current_command {
                Some(command) => match command {
                    Command::Ls => {
                        let file_regex = Regex::new(r"(\d+) (.*)").unwrap();
                        if file_regex.is_match(line) {
                            let captures = file_regex.captures(line).unwrap();
                            let size = captures.get(1).unwrap().as_str().parse::<usize>().unwrap();
                            current_path.into_iter().for_each(|path_directory| {
                                let current_size = directory_sizes.get(&path_directory.to_owned());
                                match current_size {
                                    Some(curr) => {
                                        directory_sizes
                                            .insert(path_directory.to_owned(), *curr + size);
                                    }
                                    None => {
                                        directory_sizes.insert(path_directory.to_owned(), size);
                                    }
                                }
                            })
                        }
                    }
                    Command::Cd(_dir) => {}
                },
                None => {}
            }
        }
    });

    return directory_sizes.into_iter().fold(0, |acc, curr| {
        if curr.1 <= 100_000 {
            return acc + curr.1;
        } else {
            return acc;
        }
    });
}

pub fn solution_b(input: &str) -> usize {
    let mut directory_sizes: HashMap<String, usize> = HashMap::new();
    let mut current_path_origin: Rc<RefCell<Vec<String>>> = Rc::new(RefCell::new(vec![]));
    let mut current_command: Option<Command> = None;
    input.lines().for_each(|line| {
        let current_path = Rc::clone(&current_path_origin);
        let current_path = &mut *current_path.borrow_mut();
        if is_input(line) {
            let command = parse_command(line).unwrap();
            current_command = Some(command);
            match command {
                Command::Ls => {}
                Command::Cd(dir) => {
                    if dir == ".." {
                        current_path.pop();
                    } else if dir == "/" {
                        current_path_origin = Rc::new(RefCell::new(vec!["/".to_owned()]));
                    } else {
                        let last = current_path.last().unwrap();
                        let dir = format!("{last}/{dir}");
                        current_path.push(dir);
                    }
                }
            }
        } else {
            match current_command {
                Some(command) => match command {
                    Command::Ls => {
                        let file_regex = Regex::new(r"(\d+) (.*)").unwrap();
                        if file_regex.is_match(line) {
                            let captures = file_regex.captures(line).unwrap();
                            let size = captures.get(1).unwrap().as_str().parse::<usize>().unwrap();
                            current_path.into_iter().for_each(|path_directory| {
                                let current_size = directory_sizes.get(&path_directory.to_owned());
                                match current_size {
                                    Some(curr) => {
                                        directory_sizes
                                            .insert(path_directory.to_owned(), *curr + size);
                                    }
                                    None => {
                                        directory_sizes.insert(path_directory.to_owned(), size);
                                    }
                                }
                            })
                        }
                    }
                    Command::Cd(_dir) => {}
                },
                None => {}
            }
        }
    });

    let unused = 70_000_000 - directory_sizes.get(&"/".to_owned()).unwrap();

    let mut vec = directory_sizes.into_iter().collect::<Vec<_>>();

    vec.sort_by(|a, b| return a.1.cmp(&b.1));

    return vec
        .into_iter()
        .find(|el| {
            if unused + el.1 >= 30_000_000 {
                return true;
            } else {
                return false;
            }
        })
        .unwrap()
        .1;
}
