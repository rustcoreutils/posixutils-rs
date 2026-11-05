//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Writing mailboxes back: quit, migration to mbox, concurrent delivery.

use crate::common::{create_temp_mbox, mailx, mailx_env};
use plib::tmp::NamedTempFile;

/// Mail delivered while mailx is running must survive the rewrite at quit.
///
/// `quit` truncates the mailbox and writes back only the messages it loaded, so
/// anything an MDA appended in the meantime was silently destroyed.
#[test]
fn quit_preserves_mail_delivered_while_running() {
    let sys = create_temp_mbox(
        "From alice@example.com Fri Nov 29 10:00:00 2024\n\
         From: alice@example.com\n\
         Subject: first\n\
         \n\
         body one\n\
         \n",
    );
    let sys_path = sys.path().to_str().unwrap().to_string();

    // `delete 1` forces the rewrite branch. The `!` shell escape appends a new
    // message from a separate process first, exactly as an MDA would.
    let deliver = format!(
        "printf 'From carol@example.com Fri Nov 29 12:00:00 2024\\nFrom: \
         carol@example.com\\nSubject: delivered late\\n\\nbody three\\n\\n' >> {}",
        sys_path
    );

    let output = mailx_env(
        &["-n", "-N"],
        format!("!{}\ndelete 1\nquit\n", deliver),
        &[("MAIL", &sys_path)],
    );
    let after = std::fs::read_to_string(&sys_path).expect("mailbox should still exist");
    assert!(
        after.contains("delivered late"),
        "mail delivered while mailx ran was destroyed by quit: {:?}",
        after
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// The quit rewrite must keep the same inode.
///
/// Unlinking or renaming the spool file breaks an MDA holding it open and
/// discards its mode, owner, and group.
#[test]
fn quit_rewrite_preserves_inode() {
    use std::os::unix::fs::MetadataExt;

    let sys = create_temp_mbox(
        "From alice@example.com Fri Nov 29 10:00:00 2024\n\
         From: alice@example.com\n\
         Subject: first\n\
         \n\
         body one\n\
         \n\
         From bob@example.org Fri Nov 29 11:00:00 2024\n\
         From: bob@example.org\n\
         Subject: second\n\
         \n\
         body two\n\
         \n",
    );
    let sys_path = sys.path().to_str().unwrap().to_string();
    let before = std::fs::metadata(&sys_path).expect("stat before").ino();

    let output = mailx_env(&["-n", "-N"], "delete 1\nquit\n", &[("MAIL", &sys_path)]);
    let after = std::fs::metadata(&sys_path).expect("stat after").ino();
    assert_eq!(
        before, after,
        "quit replaced the mailbox inode instead of rewriting it in place"
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `noappend` places migrated messages at the *start* of mbox (spec 104579).
///
/// It must never truncate mbox: the previously-saved mail has to survive.
#[test]
fn quit_migration_does_not_truncate_mbox() {
    let sys = create_temp_mbox(
        "From alice@example.com Fri Nov 29 10:00:00 2024\n\
         From: alice@example.com\n\
         Subject: fresh mail\n\
         \n\
         body one\n\
         \n",
    );
    let mbox = create_temp_mbox(
        "From older@example.com Fri Nov 22 09:00:00 2024\n\
         From: older@example.com\n\
         Subject: previously saved\n\
         \n\
         old body\n\
         \n",
    );
    let sys_path = sys.path().to_str().unwrap().to_string();
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    // Reading message 1 marks it read, so quit migrates it to mbox.
    let output = mailx_env(
        &["-n", "-N"],
        "print 1\nquit\n",
        &[("MAIL", &sys_path), ("MBOX", &mbox_path)],
    );
    let after = std::fs::read_to_string(&mbox_path).expect("mbox should still exist");
    assert!(
        after.contains("previously saved"),
        "migrating a read message truncated mbox and destroyed saved mail: {:?}",
        after
    );
    assert!(
        after.contains("fresh mail"),
        "the migrated message never reached mbox: {:?}",
        after
    );
    // noappend => the new message goes first.
    let new_at = after.find("fresh mail").unwrap();
    let old_at = after.find("previously saved").unwrap();
    assert!(
        new_at < old_at,
        "noappend must place migrated messages before existing mbox content: {:?}",
        after
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `keepsave` must be honored in a secondary folder, not only the system mailbox.
///
/// The quit partition consulted `keepsave` only when `is_system_mailbox` was
/// true, so a saved message was dropped from a folder even with `keepsave` set.
#[test]
fn keepsave_retains_saved_message_in_folder() {
    let folder = create_temp_mbox(
        "From alice@example.com Fri Nov 29 10:00:00 2024\n\
         From: alice@example.com\n\
         Subject: keep me here\n\
         \n\
         body one\n\
         \n\
         From bob@example.org Fri Nov 29 11:00:00 2024\n\
         From: bob@example.org\n\
         Subject: untouched\n\
         \n\
         body two\n\
         \n",
    );
    let folder_path = folder.path().to_str().unwrap().to_string();
    let out = NamedTempFile::new().expect("out file");
    let out_path = out.path().to_str().unwrap().to_string();

    let output = mailx(
        &["-n", "-N", "-f", &folder_path],
        format!("set keepsave\nsave 1 {}\nquit\n", out_path),
    );
    let saved = std::fs::read_to_string(&out_path).unwrap_or_default();
    assert!(
        saved.contains("keep me here"),
        "save did not write the message to the target file: {:?}",
        saved
    );
    let after = std::fs::read_to_string(&folder_path).expect("folder should still exist");
    assert!(
        after.contains("keep me here"),
        "keepsave was ignored in a secondary folder; the saved message was dropped: {:?}",
        after
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// The rewrite branch (not the unlink branch) must also keep newly delivered mail.
#[test]
fn quit_rewrite_preserves_delivered_tail() {
    let sys = create_temp_mbox(
        "From alice@example.com Fri Nov 29 10:00:00 2024\n\
         From: alice@example.com\n\
         Subject: first\n\
         \n\
         body one\n\
         \n\
         From bob@example.org Fri Nov 29 11:00:00 2024\n\
         From: bob@example.org\n\
         Subject: second\n\
         \n\
         body two\n\
         \n",
    );
    let sys_path = sys.path().to_str().unwrap().to_string();

    let deliver = format!(
        "printf 'From carol@example.com Fri Nov 29 12:00:00 2024\\nFrom: \
         carol@example.com\\nSubject: delivered late\\n\\nbody three\\n\\n' >> {}",
        sys_path
    );

    let output = mailx_env(
        &["-n", "-N"],
        format!("!{}\ndelete 1\nquit\n", deliver),
        &[("MAIL", &sys_path)],
    );
    let after = std::fs::read_to_string(&sys_path).expect("mailbox should still exist");
    assert!(
        after.contains("delivered late"),
        "the rewrite dropped mail delivered while mailx ran: {:?}",
        after
    );
    assert!(
        after.contains("second"),
        "the rewrite dropped the kept message: {:?}",
        after
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// An emptied mailbox is removed, or truncated when `keep` is set (spec 104636).
#[test]
fn emptied_mailbox_removed_unless_keep_is_set() {
    let one = "From alice@example.com Fri Nov 29 10:00:00 2024\n\
               From: alice@example.com\n\
               Subject: only message\n\
               \n\
               body one\n\
               \n";

    let nokeep = create_temp_mbox(one);
    let nokeep_path = nokeep.path().to_str().unwrap().to_string();
    let output = mailx_env(&["-n", "-N"], "delete 1\nquit\n", &[("MAIL", &nokeep_path)]);
    assert!(
        !std::path::Path::new(&nokeep_path).exists(),
        "nokeep should remove an emptied mailbox"
    );
    assert_eq!(output.status.code(), Some(0), "exit status");

    let keep = create_temp_mbox(one);
    let keep_path = keep.path().to_str().unwrap().to_string();
    let output = mailx_env(
        &["-n", "-N"],
        "set keep\ndelete 1\nquit\n",
        &[("MAIL", &keep_path)],
    );
    let meta =
        std::fs::metadata(&keep_path).expect("keep should truncate the mailbox, not remove it");
    assert_eq!(meta.len(), 0, "keep should leave the mailbox zero-length");
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `append` places migrated messages after the existing mbox content (spec 104579).
#[test]
fn append_places_migrated_messages_last() {
    let sys = create_temp_mbox(
        "From alice@example.com Fri Nov 29 10:00:00 2024\n\
         From: alice@example.com\n\
         Subject: fresh mail\n\
         \n\
         body one\n\
         \n",
    );
    let mbox = create_temp_mbox(
        "From older@example.com Fri Nov 22 09:00:00 2024\n\
         From: older@example.com\n\
         Subject: previously saved\n\
         \n\
         old body\n\
         \n",
    );
    let sys_path = sys.path().to_str().unwrap().to_string();
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    let output = mailx_env(
        &["-n", "-N"],
        "set append\nprint 1\nquit\n",
        &[("MAIL", &sys_path), ("MBOX", &mbox_path)],
    );
    let after = std::fs::read_to_string(&mbox_path).expect("mbox should still exist");
    let new_at = after.find("fresh mail").expect("migrated message missing");
    let old_at = after
        .find("previously saved")
        .expect("saved mail destroyed");
    assert!(
        old_at < new_at,
        "append must place migrated messages after existing mbox content: {:?}",
        after
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `file` onto the mailbox already open must not duplicate a fragment of it.
///
/// Reading the new mailbox before `quit` rewrote the old one recorded a byte
/// length taken before the rewrite, so everything `quit` appended looked like
/// mail delivered since and was copied through a second time.
#[test]
fn file_onto_the_current_mailbox_does_not_duplicate_it() {
    let mbox = create_temp_mbox(
        "From a@example.com Fri Nov 29 10:00:00 2024\n\
         From: a@example.com\n\
         Subject: first\n\
         \n\
         one\n\
         \n\
         From c@example.org Fri Nov 29 11:00:00 2024\n\
         From: c@example.org\n\
         Subject: second\n\
         \n\
         two\n\
         \n",
    );
    let path = mbox.path().to_str().unwrap().to_string();

    let output = mailx(
        &["-n", "-N", "-f", &path],
        format!("print 1\nfile {}\nprint 1\nquit\n", path),
    );
    let after = std::fs::read_to_string(&path).expect("mailbox");
    // Byte-exact: a duplicated tail is a few bytes appended past the
    // last message, which a message count or a suffix check misses.
    let expected = "From a@example.com Fri Nov 29 10:00:00 2024\n\
                    From: a@example.com\n\
                    Subject: first\n\
                    Status: RO\n\
                    \n\
                    one\n\
                    \n\
                    From c@example.org Fri Nov 29 11:00:00 2024\n\
                    From: c@example.org\n\
                    Subject: second\n\
                    \n\
                    two\n\
                    \n";
    assert_eq!(
        after, expected,
        "reopening the current mailbox corrupted it"
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}
