pub mod utils {
    pub mod collections {
        pub mod find_union;
    }
    pub mod input;
    pub mod itertools;
    pub mod numeric {
        pub mod binsearch;
        pub mod digits;
        pub mod divisors;
        pub mod factorial;
        pub mod sequences {
            pub mod continued_fractions {
                pub mod approximations;
                pub mod e;
                pub mod square_root;
            }
            pub mod fibonacci;
            pub mod heptagonal;
            pub mod hexagonal;
            pub mod octogonal;
            pub mod pentagonal;
            pub mod square;
            pub mod triangular;
        }
    }
    pub mod number_theory {
        pub mod invert_mod;
        pub mod radical;
        pub mod totient;
    }
}
pub mod solutions {
    pub mod euler001;
}

project_euler_rust_macros::make_choose_euler_fun!();

fn ask_for_problem_num() -> usize {
    let mut number_str = String::new();
    println!("Which problem do you want to run?");
    std::io::stdin()
        .read_line(&mut number_str)
        .expect("Failed to read a line");
    number_str.trim().parse().expect("Failed to read a number")
}

fn get_problem_num() -> usize {
    match std::env::args().nth(1) {
        Some(number_str) => match number_str.trim().parse() {
            Ok(num) => {
                println!("Running problem {}", num);
                num
            }
            Err(_) => {
                println!("Unrecognized value passed in argument: {}", number_str);
                ask_for_problem_num()
            }
        },
        None => ask_for_problem_num(),
    }
}

fn main() {
    let fun = choose_euler_fun(get_problem_num());
    let now = std::time::Instant::now();
    let result = fun();
    let time = now.elapsed().as_secs_f64();
    println!("Output: {}", result);
    println!("Execution time: {}s", time);
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    fn read_answer(line: usize) -> String {
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.pop();
        path.push("txt");
        path.push("answers.txt");
        std::fs::read_to_string(path)
            .unwrap()
            .lines()
            .nth(line - 1)
            .unwrap()
            .to_string()
    }

    project_euler_rust_macros::make_tests!();
}
