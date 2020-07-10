use std::path::PathBuf;

fn get_file(extension: &str, number: usize) -> String {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.pop();
    path.push(extension);
    path.push(format!("Euler{:03}.{}", number, extension));
    std::fs::read_to_string(path).unwrap()
}

pub fn get_input(number: usize) -> String {
    get_file("in", number)
}

pub mod get_solution {
    pub fn from_txt(number: usize) -> String {
        super::get_file("txt", number)
            .lines()
            .next_back()
            .unwrap()
            .to_string()
    }

    pub fn from_tex(number: usize) -> String {
        super::get_file("tex", number)
            .lines()
            .nth_back(1)
            .unwrap()
            .to_string()
    }
}
