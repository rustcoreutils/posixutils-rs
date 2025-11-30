//! Command line argument parsing for mailx

#[derive(Debug, Clone)]
pub enum Mode {
    Send,
    CheckMail,
    HeadersOnly,
    Receive,
}

#[derive(Debug, Clone)]
pub struct Args {
    pub mode: Mode,
    pub subject: Option<String>,
    pub addresses: Vec<String>,
    pub file: Option<String>,
    pub read_mbox: bool,
    pub record_to_recipient: bool,
    pub no_header_summary: bool,
    pub ignore_interrupts: bool,
    pub no_init: bool,
    pub user: Option<String>,
}

impl Args {
    pub fn parse(args: Vec<String>) -> Result<Self, String> {
        let mut result = Args {
            mode: Mode::Receive,
            subject: None,
            addresses: Vec::new(),
            file: None,
            read_mbox: false,
            record_to_recipient: false,
            no_header_summary: false,
            ignore_interrupts: false,
            no_init: false,
            user: None,
        };

        let mut i = 0;
        let mut check_mail = false;
        let mut headers_only = false;

        while i < args.len() {
            let arg = &args[i];

            if arg.starts_with('-') && arg.len() > 1 {
                let mut chars = arg[1..].chars().peekable();

                while let Some(c) = chars.next() {
                    match c {
                        'e' => {
                            check_mail = true;
                        }
                        'f' => {
                            result.read_mbox = true;
                            // Check if next argument is a file (not starting with -)
                            if chars.peek().is_none() && i + 1 < args.len() {
                                let next = &args[i + 1];
                                if !next.starts_with('-') {
                                    result.file = Some(next.clone());
                                    i += 1;
                                }
                            }
                        }
                        'F' => {
                            result.record_to_recipient = true;
                        }
                        'H' => {
                            headers_only = true;
                        }
                        'i' => {
                            result.ignore_interrupts = true;
                        }
                        'n' => {
                            result.no_init = true;
                        }
                        'N' => {
                            result.no_header_summary = true;
                        }
                        's' => {
                            // Subject - get the value
                            let rest: String = chars.collect();
                            if !rest.is_empty() {
                                result.subject = Some(rest);
                            } else if i + 1 < args.len() {
                                i += 1;
                                result.subject = Some(args[i].clone());
                            } else {
                                return Err("option requires an argument -- s".to_string());
                            }
                            break;
                        }
                        'u' => {
                            // User mailbox
                            let rest: String = chars.collect();
                            if !rest.is_empty() {
                                result.user = Some(rest);
                            } else if i + 1 < args.len() {
                                i += 1;
                                result.user = Some(args[i].clone());
                            } else {
                                return Err("option requires an argument -- u".to_string());
                            }
                            break;
                        }
                        _ => {
                            return Err(format!("illegal option -- {}", c));
                        }
                    }
                }
            } else {
                // Non-option argument - must be an address
                result.addresses.push(arg.clone());
            }

            i += 1;
        }

        // Determine mode
        if check_mail {
            result.mode = Mode::CheckMail;
        } else if headers_only {
            result.mode = Mode::HeadersOnly;
        } else if !result.addresses.is_empty() {
            result.mode = Mode::Send;
        } else {
            result.mode = Mode::Receive;
        }

        // If -u was specified, set up to read that user's mailbox
        if let Some(ref user) = result.user {
            result.file = Some(format!("/var/mail/{}", user));
        }

        Ok(result)
    }
}
