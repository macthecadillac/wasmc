extern "C" {
    fn func0(a: i32, b: i32, c: i32, d: i32, e: i32) -> i32;
}

fn main() -> Result<(), String> {
    let a = unsafe { func0(1, 2, 3, 4, 5) };
    println!("return: {}", a);
    Ok(())
}
