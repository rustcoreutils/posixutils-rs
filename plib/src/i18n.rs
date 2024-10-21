use std::ffi::CString;

/// Initialize localization catalog.
pub fn initialize_catalog() -> Result<Option<gettext::Catalog>, Box<dyn std::error::Error>> {
    // TODO: load from available mo files
    let available_locales = &["en-US", "en-AU"];

    let mut requested_locales: Vec<String> = sys_locale::get_locales().into_iter().collect();

    let lc_all = match std::env::var("LC_ALL") {
        Ok(var) => Some(var),
        Err(std::env::VarError::NotPresent) => None,
        // TODO: a proper error message
        Err(error) => return Err(error.into()),
    };

    let lc_messages = match std::env::var("LC_MESSAGES") {
        Ok(var) => Some(var),
        Err(std::env::VarError::NotPresent) => None,
        // TODO: a proper error message
        Err(error) => return Err(error.into()),
    };

    let mut insert_highest_priority = |var: &Option<String>| {
        if let Some(var) = &var {
            let mut move_locale = None;
            for (i, locale) in requested_locales.iter().enumerate() {
                if locale == var {
                    if i != 0 {
                        move_locale = Some(i);
                    }
                }
            }
            if let Some(i) = move_locale {
                let locale = requested_locales.remove(i);
                requested_locales.insert(0, locale);
            } else {
                requested_locales.insert(0, var.clone());
            }
        }
    };

    insert_highest_priority(&lc_messages);
    insert_highest_priority(&lc_all);

    let default_locale: fluent_langneg::LanguageIdentifier = "en-US".parse().unwrap();
    let available_locales = fluent_langneg::convert_vec_str_to_langids_lossy(available_locales);
    let locale = fluent_langneg::negotiate_languages(
        &fluent_langneg::convert_vec_str_to_langids_lossy(requested_locales),
        &available_locales,
        Some(&default_locale),
        // Using Lookup strategy because gettext only allows one fallback :'(
        fluent_langneg::NegotiationStrategy::Lookup,
    )
    .into_iter()
    .next()
    .expect("expected at least one locale to be present (at least the default)");

    println!("Using locale {locale}");
    

    if let Some(lc_all) = lc_all {
        let lc_all_c = CString::new(lc_all)?.as_ptr();
        unsafe {
            libc::setlocale(libc::LC_ALL, lc_all_c);
        }
    } else {
        let lc_messages_c = CString::new(locale.to_string())?.as_ptr();
        unsafe {
            libc::setlocale(libc::LC_MESSAGES, lc_messages_c);
        }
    }

    Ok(if locale != &default_locale {
        // TODO: do we need to load if en-US, as it's the fallback?
        let f = std::fs::File::open(format!("messages.{locale}.mo"))?;
        let catalog = gettext::Catalog::parse(f)?;
        Some(catalog)
    } else {
        None
    })
}

/// Macro to initialize 
#[macro_export]
macro_rules! initialize_i18n {
    () => {
        match plib::i18n::initialize_catalog() {
            Ok(Some(catalog)) => {
                tr::set_translator!(catalog);
                Ok(())
            },
            Ok(None) => Ok(()),
            Err(error) => Err(error),
        }
    };
}