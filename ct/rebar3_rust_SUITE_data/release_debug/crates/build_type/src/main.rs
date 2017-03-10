fn main() {
    #[cfg(debug_assertions)]
    print!("debug");

    #[cfg(not(debug_assertions))]
    print!("release");
}
