mod example_module;

use example_module::this_is_a_function;

pub fn add(left: usize, right: usize) -> usize {
    let res = left + right;
    this_is_a_function();
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
