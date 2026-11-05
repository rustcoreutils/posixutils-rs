//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Writing mailboxes back: quit, migration to mbox, concurrent delivery.

use crate::common::{copy_test_data, create_temp_mbox, mailx, mailx_env};
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

/// When mbox's directory cannot take a temporary file, read mail is still
/// put ahead of mbox's old contents, in place.
#[test]
fn prepend_to_mbox_in_an_unwritable_directory() {
    use std::os::unix::fs::PermissionsExt;

    let system = copy_test_data("testdata.mbox");
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let locked = dir.path().join("locked");
    std::fs::create_dir(&locked).unwrap();
    let secondary = locked.join("mbox");
    std::fs::write(
        &secondary,
        "From old@x Mon Jan  1 00:00:00 2024\nSubject: old\n\nold body\n\n",
    )
    .unwrap();
    std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o555)).unwrap();

    let output = mailx_env(
        &["-n", "-N"],
        "print 1\nquit\n",
        &[
            ("MAIL", system.path().to_str().unwrap()),
            ("MBOX", secondary.to_str().unwrap()),
        ],
    );
    std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o755)).unwrap();
    let saved = std::fs::read_to_string(&secondary).unwrap();
    assert!(
        saved.starts_with("From alice@example.com "),
        "{}\n{}",
        saved,
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(saved.contains("Subject: old\n\nold body\n"), "{}", saved);
}

/// A recovery file left by an interrupted rewrite is reported at start-up,
/// and kept.
#[test]
fn leftover_recovery_file_is_reported() {
    let mbox = copy_test_data("testdata.mbox");
    let home = plib::tmp::TempDir::new().expect("temp dir");
    let leftover = home.path().join("mailx-recover.spool.AbC123");
    std::fs::write(&leftover, "From a@x Mon Jan  1 00:00:00 2024\n\nbody\n\n").unwrap();
    let output = mailx_env(
        &["-n", "-H", "-f", mbox.path().to_str().unwrap()],
        "",
        &[("HOME", home.path().to_str().unwrap())],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(&leftover.display().to_string()) && stderr.contains("interrupted"),
        "{}",
        stderr
    );
    assert!(leftover.exists(), "the recovery file must not be removed");
}

/// A rewrite that completes leaves no recovery file behind.
#[test]
fn completed_rewrite_leaves_no_recovery_file() {
    let mbox = copy_test_data("testdata.mbox");
    let home = plib::tmp::TempDir::new().expect("temp dir");
    let output = mailx_env(
        &["-n", "-N", "-f", mbox.path().to_str().unwrap()],
        "delete 1\nquit\n",
        &[("HOME", home.path().to_str().unwrap())],
    );
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let after = std::fs::read_to_string(mbox.path()).unwrap();
    assert!(!after.contains("Subject: Meeting Tomorrow\n"), "{}", after);
    assert_eq!(std::fs::read_dir(home.path()).unwrap().count(), 0);
}

/// Quitting the system mailbox reports, as BSD mailx does, what moved to
/// mbox and what was held; quitting a folder reports its rewrite.
#[test]
fn quit_reports_what_it_did() {
    let system = copy_test_data("testdata.mbox");
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let secondary = dir.path().join("mbox");
    let path = system.path().to_str().unwrap();
    let env = [("MAIL", path), ("MBOX", secondary.to_str().unwrap())];

    // Messages 1 and 2 are read: they move to mbox; unread 3 is held.
    let output = mailx_env(&["-n", "-N"], "quit\n", &env);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Saved 2 messages in mbox\n"), "{}", stdout);
    assert!(
        stdout.contains(&format!("Held 1 message in {}\n", path)),
        "{}",
        stdout
    );

    let folder = copy_test_data("testdata.mbox");
    let fpath = folder.path().to_str().unwrap();
    let output = mailx(&["-n", "-N", "-f", fpath], "delete 1\nquit\n");
    assert!(
        String::from_utf8_lossy(&output.stdout).contains(&format!("\"{}\" complete\n", fpath)),
        "{}",
        String::from_utf8_lossy(&output.stdout)
    );
    let output = mailx(&["-n", "-N", "-f", fpath], "delete *\nquit\n");
    assert!(
        String::from_utf8_lossy(&output.stdout).contains(&format!("\"{}\" removed\n", fpath)),
        "{}",
        String::from_utf8_lossy(&output.stdout)
    );
}

/// Mail delivered during the session is taken in by `inc`, or before a
/// prompt with `autoinc`, and is written back once, not twice, at quit.
#[test]
fn inc_and_autoinc_take_in_new_mail() {
    let extra = create_temp_mbox(
        "From late@example.com Fri Nov 29 15:00:00 2024\nFrom: late@example.com\nSubject: late\n\nlate body\n\n",
    );
    let deliver = format!("!cat {} >> ", extra.path().display());

    let mbox = copy_test_data("testdata.mbox");
    let path = mbox.path().to_str().unwrap();
    let output = mailx(
        &["-n", "-N", "-f", path],
        format!("inc\n{deliver}{path}\ninc\nfrom *\nquit\n"),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("No new mail.\n"), "{}", stdout);
    assert!(
        stdout.contains(&format!("\"{}\": 4 messages 1 new", path)),
        "{}",
        stdout
    );
    assert!(stdout.contains("late@example.com"), "{}", stdout);
    let after = std::fs::read_to_string(mbox.path()).unwrap();
    assert_eq!(after.matches("Subject: late\n").count(), 1, "{}", after);

    let mbox = copy_test_data("testdata.mbox");
    let path = mbox.path().to_str().unwrap();
    let output = mailx(
        &["-n", "-N", "-f", path],
        format!("set autoinc\n{deliver}{path}\nfrom *\nexit\n"),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("New mail has arrived.\n"), "{}", stdout);
    assert!(stdout.contains("late@example.com"), "{}", stdout);
}

/// Text that is not a message, past what was read, does not keep `inc`
/// from the mail after it: as BSD mailx's setptr() does, it is passed over,
/// as text before the first message is when a mailbox is read, and the
/// messages after it are taken in.
#[test]
fn inc_passes_over_text_before_new_mail() {
    let extra = create_temp_mbox(
        "rest of a half-written message\n\nFrom late@example.com Fri Nov 29 15:00:00 2024\nSubject: late\n\nlate body\n\n",
    );
    let mbox = copy_test_data("testdata.mbox");
    let path = mbox.path().to_str().unwrap();
    let output = mailx(
        &["-n", "-N", "-f", path],
        format!(
            "!cat {} >> {}\ninc\nfrom $\ndelete 1\nquit\n",
            extra.path().display(),
            path
        ),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(!stdout.contains("No new mail."), "{}", stdout);
    assert!(stdout.contains("late@example.com"), "{}", stdout);
    let after = std::fs::read_to_string(mbox.path()).unwrap();
    assert!(
        !after.contains("rest of a half-written message"),
        "{}",
        after
    );
    assert_eq!(after.matches("Subject: late\n").count(), 1, "{}", after);
}

/// A recovery file another mailx is still writing -- it holds the file's
/// lock -- is not reported as left behind.
#[test]
fn recovery_file_in_use_is_not_reported() {
    use std::os::fd::AsRawFd;

    let mbox = copy_test_data("testdata.mbox");
    let home = plib::tmp::TempDir::new().expect("temp dir");
    let busy = home.path().join("mailx-recover.spool.Busy01");
    let file = std::fs::File::create(&busy).unwrap();
    // SAFETY: the fd is valid for the lifetime of `file`.
    assert_eq!(unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_EX) }, 0);
    let output = mailx_env(
        &["-n", "-H", "-f", mbox.path().to_str().unwrap()],
        "",
        &[("HOME", home.path().to_str().unwrap())],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!stderr.contains("mailx-recover"), "{}", stderr);
}

/// Prepend a read message to an mbox reached through `link`, made by
/// `make_link` beside the real file, returning what each name then holds.
fn prepend_through_link(
    make_link: fn(&std::path::Path, &std::path::Path) -> std::io::Result<()>,
) -> (plib::tmp::TempDir, std::path::PathBuf, std::path::PathBuf) {
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let sys = dir.path().join("spool");
    std::fs::write(
        &sys,
        "From alice@example.com Fri Nov 29 10:00:00 2024\nSubject: fresh mail\n\nbody\n\n",
    )
    .unwrap();
    std::fs::create_dir(dir.path().join("data")).unwrap();
    let real = dir.path().join("data").join("mbox");
    std::fs::write(
        &real,
        "From older@example.com Fri Nov 22 09:00:00 2024\nSubject: previously saved\n\nold\n\n",
    )
    .unwrap();
    let link = dir.path().join("mbox");
    make_link(&real, &link).unwrap();

    let output = mailx_env(
        &["-n", "-N"],
        "print 1\nquit\n",
        &[
            ("MAIL", sys.to_str().unwrap()),
            ("MBOX", link.to_str().unwrap()),
        ],
    );
    assert_eq!(output.status.code(), Some(0), "{:?}", output);
    (dir, real, link)
}

/// The mail now at `path`: the migrated message, then the old.
fn assert_prepended(path: &std::path::Path) {
    let after = std::fs::read_to_string(path).unwrap();
    let new_at = after.find("fresh mail");
    let old_at = after.find("previously saved");
    assert!(
        matches!((new_at, old_at), (Some(n), Some(o)) if n < o),
        "{}: {:?}",
        path.display(),
        after
    );
}

/// An mbox that is a symbolic link stays one: the mail goes to the file it
/// names, not into a regular file put in the link's place.
#[test]
fn prepend_to_mbox_keeps_a_symlink() {
    let (_dir, real, link) =
        prepend_through_link(|real, link| std::os::unix::fs::symlink(real, link));
    assert!(
        std::fs::symlink_metadata(&link)
            .unwrap()
            .file_type()
            .is_symlink(),
        "mbox is no longer a symlink"
    );
    assert_prepended(&real);
}

/// An mbox with other hard links stays one file: every name sees the mail.
#[test]
fn prepend_to_mbox_keeps_hard_links() {
    let (_dir, real, link) = prepend_through_link(|real, link| std::fs::hard_link(real, link));
    assert_prepended(&link);
    assert_prepended(&real);
}
