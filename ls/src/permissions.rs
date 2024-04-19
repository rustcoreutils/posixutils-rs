use std::os::unix::fs::PermissionsExt;

pub fn format_mode(mode: u32) -> String {
    let user = mode_permissions((mode >> 6) & 7);
    let group = mode_permissions((mode >> 3) & 7);
    let other = mode_permissions(mode & 7);
    format!("{}{}{}", user, group, other)
}

pub fn mode_permissions(perm: u32) -> String {
    let mut perms = String::new();
    perms.push(if perm & 4 != 0 { 'r' } else { '-' });
    perms.push(if perm & 2 != 0 { 'w' } else { '-' });
    perms.push(if perm & 1 != 0 { 'x' } else { '-' });
    perms
}
