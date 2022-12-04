fn parse_line(line: &str) -> ((usize, usize), (usize, usize)) {
    let mut split_by_comma = line.split(',');
    let elf1 = split_by_comma.next().unwrap();
    let elf2 = split_by_comma.next().unwrap();

    let mut elf1 = elf1.split("-");
    let elf1_begin = elf1.next().unwrap().parse::<usize>().unwrap();
    let elf1_end = elf1.next().unwrap().parse::<usize>().unwrap();

    let mut elf2 = elf2.split("-");
    let elf2_begin = elf2.next().unwrap().parse::<usize>().unwrap();
    let elf2_end = elf2.next().unwrap().parse::<usize>().unwrap();

    return ((elf1_begin, elf1_end), (elf2_begin, elf2_end));
}

pub fn solution_a(input: &str) -> usize {
    return input.lines().fold(0, |acc, curr| {
        let ((elf1_begin, elf1_end), (elf2_begin, elf2_end)) = parse_line(curr);

        if (elf1_begin >= elf2_begin && elf1_end <= elf2_end)
            || (elf2_begin >= elf1_begin && elf2_end <= elf1_end)
        {
            return acc + 1;
        }

        return acc;
    });
}

pub fn solution_b(input: &str) -> usize {
  return input.lines().fold(0, |acc, curr| {
      let ((elf1_begin, elf1_end), (elf2_begin, elf2_end)) = parse_line(curr);
      
      if (elf1_begin >= elf2_begin && elf1_begin <= elf2_end)
          || (elf2_begin >= elf1_begin && elf2_begin <= elf1_end)
      {
          return acc + 1;
      }

      return acc;
  });
}
