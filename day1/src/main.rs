use std::{
    fs::File,
    io::{BufRead, BufReader},
    panic,
};

fn find_word_digit(line: &Vec<char>, c: char, i: usize) -> Option<u32> {
    match c {
        'o' => match line[i + 1] {
            'n' => match line[i + 2] {
                'e' => Some(1),
                _ => None,
            },
            _ => None,
        },
        't' => match line[i + 1] {
            'w' => match line[i + 2] {
                'o' => Some(2),
                _ => None,
            },
            'h' => match line[i + 2] {
                'r' => match line[i + 3] {
                    'e' => match line[i + 4] {
                        'e' => Some(3),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        },
        'f' => match line[i + 1] {
            'o' => match line[i + 2] {
                'u' => match line[i + 3] {
                    'r' => Some(4),
                    _ => None,
                },
                _ => None,
            },
            'i' => match line[i + 2] {
                'v' => match line[i + 3] {
                    'e' => Some(5),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        },
        's' => match line[i + 1] {
            'i' => match line[i + 2] {
                'x' => Some(6),
                _ => None,
            },
            'e' => match line[i + 2] {
                'v' => match line[i + 3] {
                    'e' => match line[i + 4] {
                        'n' => Some(7),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        },
        'e' => match line[i + 1] {
            'i' => match line[i + 2] {
                'g' => match line[i + 3] {
                    'h' => match line[i + 4] {
                        't' => Some(8),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        },
        'n' => match line[i + 1] {
            'i' => match line[i + 2] {
                'n' => match line[i + 3] {
                    'e' => Some(9),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

fn find_line_val(line: String) -> usize {
    let mut first_set = false;
    let mut first = 0;
    let mut last_set = false;
    let mut last = 0;

    let line_vec = line.clone().chars().collect();

    for (i, c) in line.chars().enumerate() {
        let mut is_digit = false;
        let d = if c.is_digit(10) {
            is_digit = true;
            c.to_digit(10).unwrap()
        } else {
            // not a direct digit
            let parsed = panic::catch_unwind(|| find_word_digit(&line_vec, c, i));
            match parsed {
                Ok(res) => match res {
                    Some(v) => {
                        is_digit = true;
                        v
                    }
                    _ => 0,
                },
                _ => 0,
            }
        };
        if is_digit {
            if !first_set {
                first = d;
                first_set = true;
                continue;
            }
            last = d;
            last_set = true;
        }
    }
    let combined = if last_set {
        format!("{}{}", first, last)
    } else if first_set {
        format!("{}{}", first, first)
    } else {
        "0".to_string()
    };

    let res = combined.parse::<usize>().unwrap();

    return res;
}

fn main() {
    let file = File::open("input").unwrap();
    let reader = BufReader::new(file);

    // Don't print stuff on panics
    panic::set_hook(Box::new(|_| {
        // DO Nothing
    }));

    let sum: usize = reader.lines().map(|l| l.unwrap()).map(find_line_val).sum();

    println!("sum: {sum}");
}
