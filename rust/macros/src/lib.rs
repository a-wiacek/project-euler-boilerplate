extern crate proc_macro;

use proc_macro::TokenStream;
use std::path::PathBuf;

fn solution_numbers() -> Vec<usize> {
    let mut paths = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    paths.pop();
    paths.push("src");
    paths.push("solutions");
    std::fs::read_dir(&paths)
        .expect(&paths.to_str().unwrap())
        .map(|x| {
            x.unwrap().file_name().to_str().unwrap()[5..8]
                .parse()
                .unwrap()
        })
        .collect()
}

#[proc_macro]
pub fn make_choose_euler_fun(_: TokenStream) -> TokenStream {
    format!(
        "fn choose_euler_fun(num: usize) -> impl Fn() -> String {{ match num {{
            {}
            _ => panic!(\"Solution for problem {{}} does not exist yet!\", num),
        }}}}",
        solution_numbers()
            .into_iter()
            .map(|num| format!("{} => solutions::euler{:03}::euler{:03},", num, num, num))
            .collect::<String>()
    )
    .parse()
    .unwrap()
}

#[proc_macro]
pub fn make_tests(_: TokenStream) -> TokenStream {
    solution_numbers()
        .into_iter()
        .map(|num| format!("#[test]
        fn test_euler{:03}() {{
            assert_eq!(crate::solutions::euler{:03}::euler{:03}(), read_answer({}))
        }}", num, num, num, num))
        .collect::<String>()
        .parse()
        .unwrap()
}