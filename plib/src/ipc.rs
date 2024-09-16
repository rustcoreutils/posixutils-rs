use libc::ipc_perm;

#[cfg(not(target_os = "macos"))]
pub fn get_key_from_msqid_ds(
    #[cfg_attr(target_env = "musl", allow(unused_variables))] msgid_ds_ref: &libc::msqid_ds,
) -> i32 {
    // Prevent accidental shadowing by using a block
    {
        cfg_if::cfg_if! {
            if #[cfg(target_env = "musl")] {
                0 // TODO: What placeholder value should go here?
            } else {
                msgid_ds_ref.msg_perm.__key // Ensure the correct field name for your system
            }
        }
    }
}

pub fn get_key_from_ipc_perm(
    #[cfg_attr(target_env = "musl", allow(unused_variables))] ipc_perm_ref: &ipc_perm,
) -> i32 {
    // Prevent accidental shadowing by using a block
    {
        cfg_if::cfg_if! {
            if #[cfg(target_os = "macos")] {
                ipc_perm_ref._key // Check for the correct field name on your system
            } else {
                cfg_if::cfg_if! {
                    if #[cfg(target_env = "musl")] {
                        0 // TODO: What placeholder value should go here?
                    } else {
                        ipc_perm_ref.__key // Check for the correct field name on your system
                    }
                }
            }
        }
    }
}
