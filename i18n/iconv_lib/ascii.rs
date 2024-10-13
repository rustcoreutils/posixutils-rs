// Convert ASCII to UCS-4
pub fn to_ucs4(input: &[u8], omit_invalid: bool, supress_error: bool) -> (u32, Vec<u32>) {
    let mut result = Vec::new();

    for (i, &code_point) in input.iter().enumerate() {
        if code_point <= 127 {
            result.push(code_point as u32);
        } else if omit_invalid {
            continue;
        } else {
            if !supress_error {
                eprintln!("Error: Invalid input position {i}");
            }
            return (1, result);
        }
    }

    (0, result)
}

/// Convert UCS-4 to ASCII
pub fn from_ucs4(input: &[u32], omit_invalid: bool, supress_error: bool) -> (u32, Vec<u8>) {
    let mut result = Vec::new();

    for (i, &code_point) in input.iter().enumerate() {
        if code_point <= 127 {
            result.push(code_point as u8);
        } else if omit_invalid {
            continue;
        } else {
            if !supress_error {
                eprintln!("Error: Invalid input position {i}");
            }
            return (1, result);
        }
    }
    (0, result)
}
