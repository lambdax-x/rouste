use std::char::from_u32;

pub fn percent_decode(data: &str) -> Option<String> {
    let mut buffer: String = String::default();
    let mut iter = data.chars();

    loop {
        let maybe_char = iter.by_ref().next();
        match maybe_char {
            Some('%') => {
                let next_two: String = iter.by_ref().take(2).collect();
                if next_two.len() < 2 {
                    return None;
                }
                let maybe_char: Option<char> = u32::from_str_radix(&next_two, 16)
                                                   .ok()
                                                   .and_then(|ord| from_u32(ord));
                if maybe_char.is_none() {
                    return None;
                }
                buffer.push(maybe_char.unwrap());
            },

            Some(c) => {
                buffer.push(c);
            },

            None => {
                break;
            }
        }
    }
    Some(buffer)
}
