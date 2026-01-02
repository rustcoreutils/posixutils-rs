use crate::FormattingSettings;
use regex::Regex;
use std::collections::HashMap;
use std::sync::OnceLock;
use terminfo::Database;

use super::{
    mdoc_macro::{text_production::*, types::*, Macro},
    parser::{trim_quotes, Element, MacroNode, MdocDocument},
};

/// Max Bl -width parameter value
const MAX_INDENT: u8 = 20;

fn regex_unicode() -> &'static Regex {
    static CELL: OnceLock<Regex> = OnceLock::new();
    CELL.get_or_init(|| {
        Regex::new(
            r"(?x)
            (?:
                (?P<unicode_bracket>\\\[u(?P<hex1>[0-9A-F]{4,6})\])      |
                (?P<unicode_c>\\C'u(?P<hex2>[0-9A-F]{4,6})')             |
                (?P<number_n>\\N'(?P<dec1>[0-9]+)')                      |
                (?P<number_char>\\\[char(?P<dec2>[0-9]+)\])
            )
            ",
        )
        .unwrap()
    })
}

fn regex_ns_macro() -> &'static Regex {
    static CELL: OnceLock<Regex> = OnceLock::new();
    CELL.get_or_init(|| Regex::new(r"\s*\\\[nsmacroescape\]\s*").unwrap())
}

fn substitutions() -> &'static HashMap<&'static str, &'static str> {
    static CELL: OnceLock<HashMap<&'static str, &'static str>> = OnceLock::new();
    CELL.get_or_init(|| {
        let mut m = HashMap::with_capacity(410);
        m.insert(r"\[ssindent]", "   ");
        m.insert(r"\[dq]", "\"");
        m.insert(r"\[ti]", "~");
        m.insert(r"\[aq]", "'");
        m.insert(r"\(em", "—");
        m.insert(r"\(en", "–");
        m.insert(r"\(hy", "‐");
        m.insert("\\[pfmacroescape] ", "");
        m.insert("\\[anmacroescape]", "\n");
        // Spaces:
        //m.insert(r"\ ", " "); // unpaddable space
        m.insert(r"\~", " "); // paddable space
        m.insert(r"\0", " "); // digit-width space
        m.insert(r"\|", " "); // one-sixth \(em narrow space
        m.insert(r"\^", " "); // one-twelfth \(em half-narrow space
        m.insert(r"\&", ""); // zero-width space
        m.insert(r"\)", ""); // zero-width space (transparent to end-of-sentence detection)
        m.insert(r"\%", ""); // zero-width space allowing hyphenation
                             //m.insert(r"\:", "");  // zero-width space allowing line break

        // Lines:
        m.insert(r"\(ba", "|"); // bar
        m.insert(r"\(br", "│"); // box rule
        m.insert(r"\(ul", "_"); // underscore
        m.insert(r"\(ru", "_"); // underscore (width 0.5m)
        m.insert(r"\(rn", "‾"); // overline
        m.insert(r"\(bb", "¦"); // broken bar
        m.insert(r"\(sl", "/"); // forward slash
        m.insert(r"\(rs", "\\"); // backward slash
                                 // Text markers:
        m.insert(r"\(ci", "○"); // circle
        m.insert(r"\(bu", "•"); // bullet
        m.insert(r"\(dd", "‡"); // double dagger
        m.insert(r"\(dg", "†"); // dagger
        m.insert(r"\(lz", "◊"); // lozenge
        m.insert(r"\(sq", "□"); // white square
        m.insert(r"\(ps", "¶"); // paragraph
        m.insert(r"\(sc", "§"); // section
        m.insert(r"\(lh", "☜"); // left hand
        m.insert(r"\(rh", "☞"); // right hand
        m.insert(r"\(at", "@"); // at
        m.insert(r"\(sh", "#"); // hash (pound)
        m.insert(r"\(CR", "↵"); // carriage return
        m.insert(r"\(OK", "✓"); // check mark
        m.insert(r"\(CL", "♣"); // club suit
        m.insert(r"\(SP", "♠"); // spade suit
        m.insert(r"\(HE", "♥"); // heart suit
        m.insert(r"\(DI", "♦"); // diamond suit
                                // Legal symbols:
        m.insert(r"\(co", "©"); // copyright
        m.insert(r"\(rg", "®"); // registered
        m.insert(r"\(tm", "™"); // trademarked
                                // Punctuation:
        m.insert(r"\(em", "—"); // em-dash
        m.insert(r"\(en", "–"); // en-dash
        m.insert(r"\(hy", "‐"); // hyphen
        m.insert(r"\e", "\\"); // back-slash
        m.insert(r"\(r!", "¡"); // upside-down exclamation
        m.insert(r"\(r?", "¿"); // upside-down question
                                // Quotes:
        m.insert(r"\(Bq", "„"); // right low double-quote
        m.insert(r"\(bq", "‚"); // right low single-quote
        m.insert(r"\(lq", "“"); // left double-quote
        m.insert(r"\(rq", "”"); // right double-quote
        m.insert(r"\(oq", "‘"); // left single-quote
        m.insert(r"\(cq", "’"); // right single-quote
        m.insert(r"\(aq", "'"); // apostrophe quote (ASCII character)
        m.insert(r"\(dq", "\""); // double quote (ASCII character)
        m.insert(r"\(Fo", "«"); // left guillemet
        m.insert(r"\(Fc", "»"); // right guillemet
        m.insert(r"\(fo", "‹"); // left single guillemet
        m.insert(r"\(fc", "›"); // right single guillemet
                                // Brackets:
        m.insert(r"\(lB", "[");
        m.insert(r"\(rB", "]");
        m.insert(r"\(lC", "{");
        m.insert(r"\(rC", "}");
        m.insert(r"\(la", "⟨");
        m.insert(r"\(ra", "⟩");
        m.insert(r"\(bv", "⎪");
        m.insert(r"\[braceex]", "⎪");
        m.insert(r"\[bracketlefttp]", "⎡");
        m.insert(r"\[bracketleftbt]", "⎣");
        m.insert(r"\[bracketleftex]", "⎢");
        m.insert(r"\[bracketrighttp]", "⎤");
        m.insert(r"\[bracketrightbt]", "⎦");
        m.insert(r"\[bracketrightex]", "⎥");
        m.insert(r"\(lt", "⎧");
        m.insert(r"\[bracelefttp]", "⎧");
        m.insert(r"\(lk", "⎨");
        m.insert(r"\[braceleftmid]", "⎨");
        m.insert(r"\(lb", "⎩");
        m.insert(r"\[braceleftbt]", "⎩");
        m.insert(r"\[braceleftex]", "⎪");
        m.insert(r"\(rt", "⎫");
        m.insert(r"\[bracerighttp]", "⎫");
        m.insert(r"\(rk", "⎬");
        m.insert(r"\[bracerightmid]", "⎬");
        m.insert(r"\(rb", "⎭");
        m.insert(r"\[bracerightbt]", "⎭");
        m.insert(r"\[bracerightex]", "⎪");
        m.insert(r"\[parenlefttp]", "⎛");
        m.insert(r"\[parenleftbt]", "⎝");
        m.insert(r"\[parenleftex]", "⎜");
        m.insert(r"\[parenrighttp]", "⎞");
        m.insert(r"\[parenrightbt]", "⎠");
        m.insert(r"\[parenrightex]", "⎟");
        // Arrows:
        m.insert(r"\(<-", "←");
        m.insert(r"\(->", "→");
        m.insert(r"\(<>", "↔");
        m.insert(r"\(da", "↓");
        m.insert(r"\(ua", "↑");
        m.insert(r"\(va", "↕");
        m.insert(r"\(lA", "⇐");
        m.insert(r"\(rA", "⇒");
        m.insert(r"\(hA", "⇔");
        m.insert(r"\(uA", "⇑");
        m.insert(r"\(dA", "⇓");
        m.insert(r"\(vA", "⇕");
        m.insert(r"\(an", "⎯");
        // Logical:
        m.insert(r"\(AN", "∧");
        m.insert(r"\(OR", "∨");
        m.insert(r"\[tno]", "¬");
        m.insert(r"\(no", "¬");
        m.insert(r"\(te", "∃");
        m.insert(r"\(fa", "∀");
        m.insert(r"\(st", "∋");
        m.insert(r"\(tf", "∴");
        m.insert(r"\(3d", "∴");
        m.insert(r"\(or", "|");
        // Mathematical:
        m.insert(r"\-", "-");
        m.insert(r"\(mi", "−");
        m.insert(r"\+", "+");
        m.insert(r"\(pl", "+");
        m.insert(r"\(-+", "∓");
        m.insert(r"\[t+-]", "±");
        m.insert(r"\(+-", "±");
        m.insert(r"\(pc", "·");
        m.insert(r"\[tmu]", "×");
        m.insert(r"\(mu", "×");
        m.insert(r"\(c*", "⊗");
        m.insert(r"\(c+", "⊕");
        m.insert(r"\[tdi]", "÷");
        m.insert(r"\(di", "÷");
        m.insert(r"\(f/", "⁄");
        m.insert(r"\(**", "∗");
        m.insert(r"\(<=", "≤");
        m.insert(r"\(>=", "≥");
        m.insert(r"\(<<", "≪");
        m.insert(r"\(>>", "≫");
        m.insert(r"\(eq", "=");
        m.insert(r"\(!=", "≠");
        m.insert(r"\(==", "≡");
        m.insert(r"\(ne", "≢");
        m.insert(r"\(ap", "∼");
        m.insert(r"\(|=", "≃");
        m.insert(r"\(=~", "≅");
        m.insert(r"\(~~", "≈");
        m.insert(r"\(~=", "≈");
        m.insert(r"\(pt", "∝");
        m.insert(r"\(es", "∅");
        m.insert(r"\(mo", "∈");
        m.insert(r"\(nm", "∉");
        m.insert(r"\(sb", "⊂");
        m.insert(r"\(nb", "⊄");
        m.insert(r"\(sp", "⊃");
        m.insert(r"\(nc", "⊅");
        m.insert(r"\(ib", "⊆");
        m.insert(r"\(ip", "⊇");
        m.insert(r"\(ca", "∩");
        m.insert(r"\(cu", "∪");
        m.insert(r"\(/_", "∠");
        m.insert(r"\(pp", "⊥");
        m.insert(r"\(is", "∫");
        m.insert(r"\[integral]", "∫");
        m.insert(r"\[sum]", "∑");
        m.insert(r"\[product]", "∏");
        m.insert(r"\[coproduct]", "∐");
        m.insert(r"\(gr", "∇");
        m.insert(r"\(sr", "√");
        m.insert(r"\[sqrt]", "√");
        m.insert(r"\(lc", "⌈");
        m.insert(r"\(rc", "⌉");
        m.insert(r"\(lf", "⌊");
        m.insert(r"\(rf", "⌋");
        m.insert(r"\(if", "∞");
        m.insert(r"\(Ah", "ℵ");
        m.insert(r"\(Im", "ℑ");
        m.insert(r"\(Re", "ℜ");
        m.insert(r"\(wp", "℘");
        m.insert(r"\(pd", "∂");
        m.insert(r"\(-h", "ℏ");
        m.insert(r"\[hbar]", "ℏ");
        m.insert(r"\(12", "½");
        m.insert(r"\(14", "¼");
        m.insert(r"\(34", "¾");
        m.insert(r"\(18", "⅛");
        m.insert(r"\(38", "⅜");
        m.insert(r"\(58", "⅝");
        m.insert(r"\(78", "⅞");
        m.insert(r"\(S1", "¹");
        m.insert(r"\(S2", "²");
        m.insert(r"\(S3", "³");
        // Ligatures:
        m.insert(r"\(ff", "ﬀ");
        m.insert(r"\(fi", "ﬁ");
        m.insert(r"\(fl", "ﬂ");
        m.insert(r"\(Fi", "ﬃ");
        m.insert(r"\(Fl", "ﬄ");
        m.insert(r"\(AE", "Æ");
        m.insert(r"\(ae", "æ");
        m.insert(r"\(OE", "Œ");
        m.insert(r"\(oe", "œ");
        m.insert(r"\(ss", "ß");
        m.insert(r"\(IJ", "Ĳ");
        m.insert(r"\(ij", "ĳ");
        // Accents:
        m.insert(r"\(a-", "¯");
        m.insert(r"\(a.", "˙");
        m.insert(r"\(a^", "^");
        m.insert(r"\(aa", "´");
        m.insert(r"\'", "´");
        m.insert(r"\(ga", "`");
        m.insert(r"\`", "`");
        m.insert(r"\(ab", "˘");
        m.insert(r"\(ac", "¸");
        m.insert(r"\(ad", "¨");
        m.insert(r"\(ah", "ˇ");
        m.insert(r"\(ao", "˚");
        m.insert(r"\(a~", "~");
        m.insert(r"\(ho", "˛");
        m.insert(r"\(ha", "^");
        m.insert(r"\(ti", "~");
        // Accented letters:
        m.insert(r"\('A", "Á");
        m.insert(r"\('E", "É");
        m.insert(r"\('I", "Í");
        m.insert(r"\('O", "Ó");
        m.insert(r"\('U", "Ú");
        m.insert(r"\('Y", "Ý");
        m.insert(r"\('a", "á");
        m.insert(r"\('e", "é");
        m.insert(r"\('i", "í");
        m.insert(r"\('o", "ó");
        m.insert(r"\('u", "ú");
        m.insert(r"\('y", "ý");
        m.insert(r"\(`A", "À");
        m.insert(r"\(`E", "È");
        m.insert(r"\(`I", "Ì");
        m.insert(r"\(`O", "Ò");
        m.insert(r"\(`U", "Ù");
        m.insert(r"\(`a", "à");
        m.insert(r"\(`e", "è");
        m.insert(r"\(`i", "ì");
        m.insert(r"\(`o", "ò");
        m.insert(r"\(`u", "ù");
        m.insert(r"\(~A", "Ã");
        m.insert(r"\(~N", "Ñ");
        m.insert(r"\(~O", "Õ");
        m.insert(r"\(~a", "ã");
        m.insert(r"\(~n", "ñ");
        m.insert(r"\(~o", "õ");
        m.insert(r"\(:A", "Ä");
        m.insert(r"\(:E", "Ë");
        m.insert(r"\(:I", "Ï");
        m.insert(r"\(:O", "Ö");
        m.insert(r"\(:U", "Ü");
        m.insert(r"\(:a", "ä");
        m.insert(r"\(:e", "ë");
        m.insert(r"\(:i", "ï");
        m.insert(r"\(:o", "ö");
        m.insert(r"\(:u", "ü");
        m.insert(r"\(:y", "ÿ");
        m.insert(r"\(^A", "Â");
        m.insert(r"\(^E", "Ê");
        m.insert(r"\(^I", "Î");
        m.insert(r"\(^O", "Ô");
        m.insert(r"\(^U", "Û");
        m.insert(r"\(^a", "â");
        m.insert(r"\(^e", "ê");
        m.insert(r"\(^i", "î");
        m.insert(r"\(^o", "ô");
        m.insert(r"\(^u", "û");
        m.insert(r"\(,C", "Ç");
        m.insert(r"\(,c", "ç");
        m.insert(r"\(/L", "Ł");
        m.insert(r"\(/l", "ł");
        m.insert(r"\(/O", "Ø");
        m.insert(r"\(/o", "ø");
        m.insert(r"\(oA", "Å");
        m.insert(r"\(oa", "å");
        // Special letters:
        m.insert(r"\(-D", "Ð");
        m.insert(r"\(Sd", "ð");
        m.insert(r"\(TP", "Þ");
        m.insert(r"\(Tp", "þ");
        m.insert(r"\(.i", "ı");
        m.insert(r"\(.j", "ȷ");
        // Currency:
        m.insert(r"\(Do", "$");
        m.insert(r"\(ct", "¢");
        m.insert(r"\(Eu", "€");
        m.insert(r"\(eu", "€");
        m.insert(r"\(Ye", "¥");
        m.insert(r"\(Po", "£");
        m.insert(r"\(Cs", "¤");
        m.insert(r"\(Fn", "ƒ");
        // Units:
        m.insert(r"\(de", "°");
        m.insert(r"\(%0", "‰");
        m.insert(r"\(fm", "′");
        m.insert(r"\(sd", "″");
        m.insert(r"\(mc", "µ");
        m.insert(r"\(Of", "ª");
        m.insert(r"\(Om", "º");
        // Greek letters:
        m.insert(r"\(*A", "Α");
        m.insert(r"\(*B", "Β");
        m.insert(r"\(*G", "Γ");
        m.insert(r"\(*D", "Δ");
        m.insert(r"\(*E", "Ε");
        m.insert(r"\(*Z", "Ζ");
        m.insert(r"\(*Y", "Η");
        m.insert(r"\(*H", "Θ");
        m.insert(r"\(*I", "Ι");
        m.insert(r"\(*K", "Κ");
        m.insert(r"\(*L", "Λ");
        m.insert(r"\(*M", "Μ");
        m.insert(r"\(*N", "Ν");
        m.insert(r"\(*C", "Ξ");
        m.insert(r"\(*O", "Ο");
        m.insert(r"\(*P", "Π");
        m.insert(r"\(*R", "Ρ");
        m.insert(r"\(*S", "Σ");
        m.insert(r"\(*T", "Τ");
        m.insert(r"\(*U", "Υ");
        m.insert(r"\(*F", "Φ");
        m.insert(r"\(*X", "Χ");
        m.insert(r"\(*Q", "Ψ");
        m.insert(r"\(*W", "Ω");
        m.insert(r"\(*a", "α");
        m.insert(r"\(*b", "β");
        m.insert(r"\(*g", "γ");
        m.insert(r"\(*d", "δ");
        m.insert(r"\(*e", "ε");
        m.insert(r"\(*z", "ζ");
        m.insert(r"\(*y", "η");
        m.insert(r"\(*h", "θ");
        m.insert(r"\(*i", "ι");
        m.insert(r"\(*k", "κ");
        m.insert(r"\(*l", "λ");
        m.insert(r"\(*m", "μ");
        m.insert(r"\(*n", "ν");
        m.insert(r"\(*c", "ξ");
        m.insert(r"\(*o", "ο");
        m.insert(r"\(*p", "π");
        m.insert(r"\(*r", "ρ");
        m.insert(r"\(*s", "σ");
        m.insert(r"\(*t", "τ");
        m.insert(r"\(*u", "υ");
        m.insert(r"\(*f", "ϕ");
        m.insert(r"\(*x", "χ");
        m.insert(r"\(*q", "ψ");
        m.insert(r"\(*w", "ω");
        m.insert(r"\(+h", "ϑ");
        m.insert(r"\(+f", "φ");
        m.insert(r"\(+p", "ϖ");
        m.insert(r"\(+e", "ϵ");
        m.insert(r"\(ts", "ς");
        // Predefined strings:
        m.insert(r"\*(Ba", "|");
        m.insert(r"\*(Ne", "≠");
        m.insert(r"\*(Ge", "≥");
        m.insert(r"\*(Le", "≤");
        m.insert(r"\*(Gt", ">");
        m.insert(r"\*(Lt", "<");
        m.insert(r"\*(Pm", "±");
        m.insert(r"\*(If", "infinity");
        m.insert(r"\*(Pi", "pi");
        m.insert(r"\*(Na", "NaN");
        m.insert(r"\*(Am", "&");
        m.insert(r"\*R", "®");
        m.insert(r"\*(Tm", "(Tm)");
        m.insert(r"\*q", "\"");
        m.insert(r"\*(Rq", "”");
        m.insert(r"\*(Lq", "“");
        m.insert(r"\*(lp", "(");
        m.insert(r"\*(rp", ")");
        m.insert(r"\*(lq", "“");
        m.insert(r"\*(rq", "”");
        m.insert(r"\*(ua", "↑");
        m.insert(r"\*(va", "↕");
        m.insert(r"\*(<=", "≤");
        m.insert(r"\*(>=", "≥");
        m.insert(r"\*(aa", "´");
        m.insert(r"\*(ga", "`");
        m.insert(r"\*(Px", "POSIX");
        m.insert(r"\*(Ai", "ANSI");
        m
    })
}

fn outer_regex() -> &'static Regex {
    static CELL: OnceLock<Regex> = OnceLock::new();
    CELL.get_or_init(|| {
        let alternation = substitutions()
            .keys()
            .map(|key| regex::escape(key))
            .collect::<Vec<_>>()
            .join("|");

        let pattern = format!(r#"(?P<esc>{})"#, alternation);
        Regex::new(&pattern).unwrap()
    })
}

pub fn replace_escapes(input: &str) -> String {
    let input = outer_regex()
        .replace_all(input, |caps: &regex::Captures| {
            if let Some(esc) = caps.name("esc") {
                substitutions()
                    .get(esc.as_str())
                    .map(|rep| rep.to_string())
                    .unwrap_or_else(|| esc.as_str().to_string())
            } else {
                caps.get(0).unwrap().as_str().to_string()
            }
        })
        .to_string();

    regex_ns_macro().replace_all(&input, "").to_string()
}

/// Formatter state
#[derive(Debug)]
pub struct FormattingState {
    /// Utility name; mdoc title
    first_name: Option<String>,
    /// Header content
    header_text: Option<String>,
    /// Footer content
    footer_text: Option<String>,
    /// Space between adjacent macros
    spacing: String,
    /// Mdoc date for header and footer
    date: String,
    /// Sm split mode
    split_mod: bool,
    /// Indentation of current macros nesting level
    current_indent: usize,
}

impl Default for FormattingState {
    fn default() -> Self {
        Self {
            first_name: None,
            header_text: None,
            footer_text: None,
            spacing: " ".to_string(),
            date: String::default(),
            split_mod: false,
            current_indent: 0,
        }
    }
}

/// Formatter settings and state
#[derive(Debug)]
pub struct MdocFormatter {
    formatting_settings: FormattingSettings,
    formatting_state: FormattingState,
}

// Helper funcitons.
impl MdocFormatter {
    pub fn new(settings: FormattingSettings) -> Self {
        Self {
            formatting_settings: settings,
            formatting_state: FormattingState::default(),
        }
    }

    /// Check if italic is supported for this terminal
    fn _supports_italic(&self) -> bool {
        if let Ok(info) = Database::from_env() {
            return info.raw("sitm").is_some();
        }
        false
    }

    /// Check if bold is supported for this terminal
    fn _supports_bold(&self) -> bool {
        if let Ok(info) = Database::from_env() {
            return info.raw("bold").is_some();
        }
        false
    }

    /// Check if undeline is supported for this terminal
    fn _supports_underline(&self) -> bool {
        if let Ok(info) = Database::from_env() {
            return info.raw("smul").is_some();
        }
        false
    }

    /// Replaces escape sequences in [`text`] [`str`] to true UTF-8 chars
    fn replace_unicode_escapes(&self, text: &str) -> String {
        regex_unicode()
            .replace_all(text, |caps: &regex::Captures| {
                if let Some(hex) = caps
                    .name("hex1")
                    .or_else(|| caps.name("hex2"))
                    .map(|m| m.as_str())
                {
                    if let Ok(codepoint) = u32::from_str_radix(hex, 16) {
                        if codepoint < 0x80 {
                            return "\u{FFFD}".to_string();
                        }
                        if codepoint < 0x10FFFF && !(0xD800..=0xDFFF).contains(&codepoint) {
                            if let Some(ch) = char::from_u32(codepoint) {
                                return ch.to_string();
                            }
                        }
                    }
                } else if let Some(dec) = caps
                    .name("dec1")
                    .or_else(|| caps.name("dec2"))
                    .map(|m| m.as_str())
                {
                    if let Ok(codepoint) = dec.parse::<u32>() {
                        if let Some(ch) = char::from_u32(codepoint) {
                            return ch.to_string();
                        }
                    }
                }
                caps.get(0).unwrap().as_str().to_string()
            })
            .to_string()
    }
}

// Base formatting functions.
impl MdocFormatter {
    /// Append formatted macros on highest mdoc level.
    /// Split lines longer than terminal width and
    /// adds indentation for new lines
    fn append_formatted_text(
        &mut self,
        formatted: &str,
        current_line: &mut String,
        lines: &mut Vec<String>,
    ) {
        let get_indent = |l: &str| {
            l.chars()
                .take_while(|ch| ch.is_whitespace())
                .collect::<String>()
        };

        let is_one_line = !formatted.contains("\n");
        let max_width = self.formatting_settings.width;

        for line in formatted.split("\n") {
            let line = replace_escapes(line);

            if !is_one_line && !current_line.is_empty() {
                lines.push(current_line.trim_end().to_string());
                current_line.clear();
            }

            let line_len = current_line.chars().count() + line.chars().count();
            if line_len > max_width || is_one_line {
                let indent = get_indent(&line);
                let max_width = max_width.saturating_sub(indent.chars().count());

                for word in line.split_whitespace() {
                    let line_len = current_line.chars().count() + word.chars().count();
                    if line_len > max_width {
                        lines.push(indent.clone() + current_line.trim());
                        current_line.clear();
                    }

                    current_line.push_str(word);

                    if !word.chars().all(|ch| ch.is_control()) {
                        current_line.push(' ');
                    }
                }

                let is_not_empty =
                    !(current_line.chars().all(|ch| ch.is_whitespace()) || current_line.is_empty());

                if is_not_empty {
                    *current_line = indent + current_line;
                }
            } else {
                lines.push(line.to_string());
            }
        }
    }

    /// If -h man parameter is enabled this function is used instead
    /// [`format_mdoc`] for displaying only `SINOPSYS` section
    pub fn format_synopsis_section(&mut self, ast: MdocDocument) -> Vec<u8> {
        let mut lines = Vec::new();
        let mut current_line = String::new();

        for node in ast.elements {
            let formatted_node = match node {
                Element::Macro(macro_node) => {
                    if let Macro::Sh { ref title } = macro_node.mdoc_macro {
                        if title.eq_ignore_ascii_case("SYNOPSIS") {
                            self.format_sh_block(title.clone(), macro_node)
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                }
                _ => continue,
            };

            self.append_formatted_text(&formatted_node, &mut current_line, &mut lines);
        }

        if !current_line.is_empty() {
            lines.push(current_line.trim_end().to_string());
        }

        replace_escapes(&lines.join("\n")).into_bytes()
    }

    /// Format full [`MdocDocument`] and returns UTF-8 binary string
    pub fn format_mdoc(&mut self, ast: MdocDocument) -> Vec<u8> {
        let mut lines = Vec::new();
        let mut current_line = String::new();

        for node in ast.elements {
            let mut formatted_node = self.format_node(node.clone());

            if formatted_node.is_empty() {
                continue;
            }

            if let Element::Macro(ref macro_node) = node {
                if !matches!(
                    macro_node.mdoc_macro,
                    Macro::Sh { .. } | Macro::Ss { .. } | Macro::Bd { .. } | Macro::An { .. }
                ) && formatted_node.split("\n").count() > 1
                {
                    formatted_node = formatted_node.trim().to_string();
                }
                if matches!(macro_node.mdoc_macro, Macro::Bd { .. }) {
                    formatted_node.pop();
                    formatted_node.remove(0);
                }
            }

            self.append_formatted_text(&formatted_node, &mut current_line, &mut lines);
        }

        if !current_line.is_empty() {
            lines.push(current_line.trim_end().to_string());
        }

        let first_empty_count = lines
            .iter()
            .take_while(|l| l.chars().all(|ch| ch.is_whitespace()))
            .count();

        lines = lines.split_at(first_empty_count).1.to_vec();

        lines.insert(0, "".to_string());

        lines.insert(
            0,
            self.formatting_state
                .header_text
                .clone()
                .unwrap_or_else(|| self.format_default_header()),
        );

        lines.push(self.format_footer());

        let content = remove_empty_lines(&lines.join("\n"), 2);

        content.into_bytes()
    }

    fn format_default_header(&mut self) -> String {
        self.format_dt(None, "", None);
        self.formatting_state
            .header_text
            .clone()
            .unwrap_or_default()
    }

    fn get_default_footer_text() -> String {
        use std::process::Command;

        let mut footer_text = Command::new("uname")
            .arg("-o")
            .output()
            .map(|o| String::from_utf8(o.stdout).unwrap_or_default())
            .unwrap_or_default()
            .trim()
            .to_string();

        if footer_text.is_empty() {
            footer_text = "()".to_string();
        }

        footer_text
    }

    fn format_footer(&mut self) -> String {
        let footer_text = self
            .formatting_state
            .footer_text
            .clone()
            .unwrap_or(Self::get_default_footer_text());

        if self.formatting_state.date.is_empty() {
            self.formatting_state.date = self.format_dd("$Mdocdate$");
        }

        let mut space_size = self
            .formatting_settings
            .width
            .saturating_sub(2 * footer_text.len() + self.formatting_state.date.len())
            / 2;

        let mut left_footer_text = footer_text.clone();
        let mut right_footer_text = footer_text.clone();

        if space_size <= 1 {
            space_size = self
                .formatting_settings
                .width
                .saturating_sub(self.formatting_state.date.len())
                / 2;

            let space = vec![
                " ";
                self.formatting_settings
                    .width
                    .saturating_sub(footer_text.len())
            ]
            .into_iter()
            .collect::<String>();

            left_footer_text = footer_text.clone() + &space.clone() + "\n";
            right_footer_text = "\n".to_string() + &space.clone() + &footer_text.clone();
        }

        let space = " ".repeat(space_size);

        let mut content = format!(
            "\n{}{}{}{}{}",
            left_footer_text,
            space.clone(),
            self.formatting_state.date,
            space,
            right_footer_text
        );

        let missing_space = self
            .formatting_settings
            .width
            .saturating_sub(content.len() - 1);

        content.insert_str(left_footer_text.len() + 1, &" ".repeat(missing_space));

        content
    }

    /// Convert one [`Element`] AST to [`String`]
    fn format_node(&mut self, node: Element) -> String {
        let result = match node {
            Element::Macro(macro_node) => self.format_macro_node(macro_node),
            Element::Text(text) => self.format_text_node(text.as_str()),
            Element::Eoi => "".to_string(),
        };

        replace_escapes(&result)
    }

    /// Convert one [`MacroNode`] AST to [`String`]
    fn format_macro_node(&mut self, macro_node: MacroNode) -> String {
        match macro_node.clone().mdoc_macro {
            // Block full-explicit
            Macro::Bd {
                block_type,
                offset,
                compact,
            } => self.format_bd_block(block_type, offset, compact, macro_node),
            Macro::Bf(bf_type) => self.format_bf_block(bf_type, macro_node),
            Macro::Bk => self.format_bk_block(macro_node),
            Macro::Bl { .. } => self.format_bl_blocks(macro_node),

            // Special block macro ta formatting
            Macro::Ta => self.format_ta(),

            // Block full-implicit
            Macro::It { head } => self.format_it_block(head, macro_node),
            Macro::Nd => self.format_nd(macro_node),
            Macro::Nm { name } => self.format_nm(name.clone(), macro_node),
            Macro::Sh { title } => self.format_sh_block(title, macro_node),
            Macro::Ss { title } => self.format_ss_block(title, macro_node),

            // Block partial-explicit.
            Macro::Ao => self.format_a_block(macro_node),
            Macro::Bo => self.format_b_block(macro_node),
            Macro::Bro => self.format_br_block(macro_node),
            Macro::Do => self.format_d_block(macro_node),
            Macro::Oo => self.format_o_block(macro_node),
            Macro::Po => self.format_p_block(macro_node),
            Macro::Qo => self.format_q_block(macro_node),
            Macro::Rs => self.format_rs_block(macro_node),
            Macro::So => self.format_s_block(macro_node),
            Macro::Xo => self.format_x_block(macro_node),
            Macro::Eo {
                opening_delimiter,
                closing_delimiter,
            } => self.format_e_block(opening_delimiter, closing_delimiter, macro_node),
            Macro::Fo { ref funcname } => {
                let funcname_copy = funcname.clone();
                self.format_f_block(funcname_copy, macro_node)
            }

            // Block partial-implicit.
            Macro::Aq => self.format_aq(macro_node),
            Macro::Bq => self.format_bq(macro_node),
            Macro::Brq => self.format_brq(macro_node),
            Macro::D1 => self.format_d1(macro_node),
            Macro::Dl => self.format_dl(macro_node),
            Macro::Dq => self.format_dq(macro_node),
            Macro::En => self.format_en(macro_node),
            Macro::Op => self.format_op(macro_node),
            Macro::Pq => self.format_pq(macro_node),
            Macro::Ql => self.format_ql(macro_node),
            Macro::Qq => self.format_qq(macro_node),
            Macro::Sq => self.format_sq(macro_node),
            Macro::Vt => self.format_vt(macro_node),

            // In-line.
            // Rs block macros which can appears outside Rs-Re block.
            Macro::B => self.format_b(macro_node),
            Macro::T => self.format_t(macro_node),
            Macro::U => self.format_u(macro_node),

            // Text production macros.
            Macro::At => self.format_at(macro_node),
            Macro::Bsx => self.format_bsx(macro_node),
            Macro::Bx => self.format_bx(macro_node),
            Macro::Dx => self.format_dx(macro_node),
            Macro::Ad => self.format_ad(macro_node),
            Macro::Ap => self.format_ap(macro_node),
            Macro::Ar => self.format_ar(macro_node),
            Macro::Bt => self.format_bt(),
            Macro::Cd => self.format_cd(macro_node),
            Macro::Cm => self.format_cm(macro_node),
            Macro::Db => self.format_db(),
            Macro::Dv => self.format_dv(macro_node),
            Macro::Em => self.format_em(macro_node),
            Macro::An { author_name_type } => self.format_an(author_name_type, macro_node),
            Macro::Dd => {
                match macro_node.nodes.is_empty() {
                    true => self.formatting_state.date = self.format_dd(""),
                    false => match &macro_node.nodes[0] {
                        Element::Text(l) => self.formatting_state.date = self.format_dd(l.as_str()),
                        _ => unreachable!(),
                    },
                };

                String::new()
            }
            Macro::Dt {
                title,
                section,
                arch,
            } => self.format_dt(title.clone(), section.as_str(), arch.clone()),

            Macro::Er => self.format_er(macro_node),
            Macro::Es {
                opening_delimiter,
                closing_delimiter,
            } => self.format_es(opening_delimiter, closing_delimiter, macro_node),
            Macro::Ev => self.format_ev(macro_node),
            Macro::Ex => self.format_ex(macro_node),
            Macro::Fa => self.format_fa(macro_node),
            Macro::Fd {
                directive,
                arguments,
            } => self.format_fd(directive.as_str(), &arguments),
            Macro::Fl => self.format_fl(macro_node),
            Macro::Fn { funcname } => self.format_fn(funcname.as_str(), macro_node),
            Macro::Fr => self.format_fr(macro_node),
            Macro::Ft => self.format_ft(macro_node),
            Macro::Fx => self.format_fx(macro_node),
            Macro::Hf => self.format_hf(macro_node),
            Macro::Ic => self.format_ic(macro_node),
            Macro::In { filename } => self.format_in(filename.as_str(), macro_node),
            Macro::Lb { lib_name } => self.format_lb(lib_name.as_str(), macro_node),
            Macro::Li => self.format_li(macro_node),
            Macro::Lk { ref uri } => self.format_lk(uri.as_str(), macro_node),
            Macro::Lp => self.format_lp(),
            Macro::Ms => self.format_ms(macro_node),
            Macro::Mt => self.format_mt(macro_node),
            Macro::No => self.format_no(macro_node),
            Macro::Ns => self.format_ns(macro_node),
            Macro::Nx => self.format_nx(macro_node),
            Macro::Os => self.format_os(macro_node),
            Macro::Ox => self.format_ox(macro_node),
            Macro::Pa => self.format_pa(macro_node),
            Macro::Pf { prefix } => self.format_pf(prefix.as_str(), macro_node),
            Macro::Pp => self.format_pp(macro_node),
            Macro::Rv => self.format_rv(macro_node),
            Macro::Sm(sm_mode) => self.format_sm(sm_mode, macro_node),
            Macro::St(st_type) => self.format_st(st_type, macro_node),
            Macro::Sx => self.format_sx(macro_node),
            Macro::Sy => self.format_sy(macro_node),
            Macro::Tg { term } => self.format_tg(term),
            Macro::Tn => self.format_tn(macro_node),
            Macro::Ud => self.format_ud(),
            Macro::Ux => self.format_ux(macro_node),
            Macro::Va => self.format_va(macro_node),
            Macro::Xr { name, section } => {
                self.format_xr(name.as_str(), section.as_str(), macro_node)
            }
            _ => self.format_inline_macro(macro_node),
        }
    }

    /// Convert text node to [`String`]. Escape sequences is converted to true UTF-8 chars
    fn format_text_node(&self, text: &str) -> String {
        self.replace_unicode_escapes(text).trim().to_string()
    }

    /// Special block macro ta formatting
    fn format_ta(&mut self) -> String {
        String::new()
    }
}

/// Split words on lines no longer than [`width`]
fn split_by_width(words: Vec<String>, width: usize) -> Vec<String> {
    if width == 0 {
        return words.iter().map(|s| s.to_string()).collect::<Vec<_>>();
    }

    let mut lines = Vec::new();
    let mut line = String::new();
    let mut i = 0;
    while i < words.len() {
        let word_len = replace_escapes(&words[i]).len();
        let line_len = replace_escapes(&line).len();
        if line.is_empty() || word_len > width {
            lines.extend(
                words[i]
                    .chars()
                    .collect::<Vec<_>>()
                    .chunks(width)
                    .map(|ch| ch.iter().collect::<String>()),
            );
            if let Some(l) = lines.pop() {
                line = l;
            }
            i += 1;
            continue;
        } else if line_len + word_len + 1 > width {
            lines.push(line.clone());
            line.clear();
            continue;
        }
        if !line.is_empty() && line_len < width {
            if let Some(ch) = line.chars().last() {
                if !ch.is_whitespace() {
                    line.push(' ');
                }
            }
        }
        line.push_str(&words[i]);
        i += 1;
    }
    lines.push(line);

    for l in lines.iter_mut() {
        *l = l.trim_end().to_string();
    }

    lines
}

/// Add indentation for every line in [`lines`] according to offset
fn add_indent_to_lines(lines: Vec<String>, width: usize, offset: &OffsetType) -> Vec<String> {
    lines
        .into_iter()
        .map(|line| {
            let mut line_indent = width.saturating_sub(line.len());
            match offset {
                OffsetType::Left => line,
                OffsetType::Right => {
                    let indent = " ".repeat(line_indent);
                    indent + &line
                }
                OffsetType::Center => {
                    line_indent = (line_indent as f32 / 2.0).floor() as usize;
                    let indent = " ".repeat(line_indent);
                    indent.clone() + &line
                }
                _ => unreachable!(),
            }
        })
        .collect::<Vec<_>>()
}

/// Returns list symbol [`String`] according [`list_type`].
/// If [`BlType::Enum`], then this function will return
/// [`String`] with next list number according to [`last_symbol`]
fn get_symbol(last_symbol: &str, list_type: &BlType) -> String {
    match list_type {
        BlType::Bullet => "•".to_string(),
        BlType::Dash => "-".to_string(),
        BlType::Enum => {
            if last_symbol.is_empty() {
                return "0.".to_string();
            }
            let mut symbol = last_symbol.to_string();
            symbol.pop();
            let Ok(number) = symbol.parse::<usize>() else {
                return String::new();
            };
            (number + 1).to_string() + "."
        }
        _ => String::new(),
    }
}

/// Merges adjacent oneline formatted nodes
fn merge_onelined(
    elements: Vec<String>,
    line_width: usize,
    indent_str: &str,
    offset: &OffsetType,
) -> Vec<String> {
    fn merge(
        v: &mut Vec<String>,
        lines: &mut Vec<String>,
        line_width: usize,
        indent_str: &str,
        offset: &OffsetType,
    ) {
        if !v.is_empty() {
            let s = v
                .join(" ")
                .split_whitespace()
                .map(|s| s.to_string())
                .collect::<Vec<_>>();

            let mut content = split_by_width(s, line_width);
            content = add_indent_to_lines(content, line_width, offset);
            for line in content.iter_mut() {
                *line = indent_str.to_string() + line;
            }

            lines.extend(content);
            v.clear();
        }
    }

    let mut lines = Vec::new();
    let mut onelines = Vec::new();

    for el in elements {
        if el.trim().lines().count() > 1 {
            merge(&mut onelines, &mut lines, line_width, indent_str, offset);

            let mut el = el.split("\n").map(|s| s.to_string()).collect::<Vec<_>>();
            if let Some(s) = el.iter_mut().next() {
                if s.is_empty() {
                    *s = indent_str.to_string() + "\\&";
                }
            }

            lines.extend(el);
        } else if el.chars().all(|ch| ch.is_whitespace()) {
            merge(&mut onelines, &mut lines, line_width, indent_str, offset);
            lines.extend(el.lines().map(|_| String::new()));
        } else {
            onelines.push(el);
        }
    }
    merge(&mut onelines, &mut lines, line_width, indent_str, offset);

    if let Some(first) = lines.first() {
        if first.chars().all(|ch| ch.is_whitespace()) {
            lines.remove(0);
        }
    }

    lines
}

/// If Bl has nested Bl macros, then this function
/// convert it to [`Vec<Element>`] flattening super Bl  
fn split_nested_bl(bl: MacroNode) -> Vec<Element> {
    let MacroNode { mdoc_macro, nodes } = bl;

    let super_macros = |nodes: Vec<Element>| {
        Element::Macro(MacroNode {
            mdoc_macro: mdoc_macro.clone(),
            nodes,
        })
    };

    let nested_macros = super_macros(nodes.clone());

    let Macro::Bl {
        list_type: super_list_type,
        ..
    } = mdoc_macro.clone()
    else {
        return vec![nested_macros];
    };
    let is_not_nested = |list_type: &BlType| {
        matches!(
            list_type,
            BlType::Item | BlType::Inset | BlType::Column | BlType::Ohang
        )
    };

    if !is_not_nested(&super_list_type) {
        return vec![nested_macros];
    }

    let mut bl_elements = vec![];
    let mut it_elements = vec![];
    for it in nodes {
        let Element::Macro(MacroNode {
            mdoc_macro: Macro::It { head },
            nodes: it_nodes,
        }) = it
        else {
            if !it_elements.is_empty() {
                bl_elements.push((true, super_macros(it_elements.clone())));
                it_elements.clear();
            }
            bl_elements.push((true, it));
            continue;
        };

        let mut head_elements = vec![];
        for element in head {
            if matches!(
                element,
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bl { .. },
                    ..
                })
            ) {
                if !head_elements.is_empty() {
                    if !it_elements.is_empty() {
                        bl_elements.push((true, super_macros(it_elements.clone())));
                        it_elements.clear();
                    }
                    bl_elements.push((
                        true,
                        super_macros(vec![Element::Macro(MacroNode {
                            mdoc_macro: Macro::It {
                                head: head_elements.clone(),
                            },
                            nodes: vec![],
                        })]),
                    ));
                    head_elements.clear();
                }
                bl_elements.push((false, element));
            } else {
                head_elements.push(element);
            }
        }

        let mut body_elements = vec![];
        for element in it_nodes {
            if matches!(
                element,
                Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bl { .. },
                    ..
                })
            ) {
                if !head_elements.is_empty() || !body_elements.is_empty() {
                    it_elements.push(Element::Macro(MacroNode {
                        mdoc_macro: Macro::It {
                            head: head_elements.clone(),
                        },
                        nodes: body_elements.clone(),
                    }));
                    bl_elements.push((true, super_macros(it_elements.clone())));

                    it_elements.clear();
                    head_elements.clear();
                    body_elements.clear();
                }
                bl_elements.push((false, element));
            } else {
                body_elements.push(element);
            }
        }

        if !head_elements.is_empty() || !body_elements.is_empty() {
            it_elements.push(Element::Macro(MacroNode {
                mdoc_macro: Macro::It {
                    head: head_elements,
                },
                nodes: body_elements,
            }));
        }
    }

    if !it_elements.is_empty() {
        bl_elements.push((true, super_macros(it_elements)));
    }

    if !bl_elements.is_empty() {
        bl_elements
            .into_iter()
            .flat_map(|(checked, bl)| {
                if checked {
                    return vec![bl];
                }
                if let Element::Macro(ref node) = bl {
                    if let MacroNode {
                        mdoc_macro: Macro::Bl { .. },
                        ..
                    } = node
                    {
                        return split_nested_bl(node.clone());
                    }
                }
                vec![]
            })
            .collect::<Vec<_>>()
    } else {
        vec![nested_macros]
    }
}

/// Removes reduntant empty lines or lines that contains only whitespaces from [`input`]
fn remove_empty_lines(input: &str, delimiter_size: usize) -> String {
    let input = input
        .lines()
        .map(|line| {
            if line.chars().all(|ch| ch.is_whitespace()) {
                ""
            } else {
                line
            }
        })
        .collect::<Vec<_>>()
        .join("\n");
    let mut result = String::with_capacity(input.len());
    let mut iter = input.chars().peekable();
    let lines_delimiter_big = "\n".repeat(delimiter_size);
    let mut nl_count = 0;

    while let Some(current_char) = iter.next() {
        if current_char == '\n' {
            if iter.peek() != Some(&'\n') {
                let lines_delimiter = if nl_count > 1 {
                    &lines_delimiter_big.clone()
                } else {
                    "\n"
                };
                result.push_str(lines_delimiter);
                nl_count = 1;
            } else {
                nl_count += 1;
            }
        } else {
            result.push(current_char);
        }
    }

    result
}

// Formatting block full-explicit.
impl MdocFormatter {
    fn get_width_indent(&self, width: &Option<u8>) -> usize {
        let mut width = width.unwrap_or(0).min(MAX_INDENT) as usize;
        if width < 2 {
            width = 2;
        }
        width
    }

    fn get_offset_indent(&self, offset: &Option<OffsetType>) -> usize {
        let Some(offset) = offset else {
            return 0;
        };
        match offset {
            OffsetType::Indent => 6,
            OffsetType::IndentTwo => 6 * 2,
            _ => self.formatting_settings.indent,
        }
    }

    /// Converts [`OffsetType`] to block alignment type ([`OffsetType`]).
    /// This function exists because [`OffsetType::Indent`] and
    /// [`OffsetType::IndentTwo`] exist
    fn get_offset_from_offset_type(&self, offset: &Option<OffsetType>) -> OffsetType {
        let Some(offset) = offset else {
            return OffsetType::Left;
        };
        match offset.clone() {
            OffsetType::Indent | OffsetType::IndentTwo => OffsetType::Left,
            OffsetType::Left | OffsetType::Right | OffsetType::Center => offset.clone(),
        }
    }

    fn format_bd_block(
        &mut self,
        block_type: BdType,
        offset: Option<OffsetType>,
        _compact: bool,
        macro_node: MacroNode,
    ) -> String {
        let indent = self.get_offset_indent(&offset);
        let mut offset = self.get_offset_from_offset_type(&offset);
        if block_type == BdType::Centered {
            offset = OffsetType::Center;
        }

        self.formatting_state.current_indent += indent;

        let current_indent = self.formatting_state.current_indent;
        let line_width = self
            .formatting_settings
            .width
            .saturating_sub(current_indent);

        let formatted_elements = macro_node.nodes.into_iter().map(|el| {
            let is_aligned_macro = if let Element::Macro(MacroNode { mdoc_macro, .. }) = el.clone()
            {
                matches!(mdoc_macro, Macro::Bl { .. }) || matches!(mdoc_macro, Macro::Bd { .. })
            } else {
                false
            };

            let formatted_node = match el {
                Element::Text(text) => text,
                _ => self.format_node(el),
            };

            let content = if is_aligned_macro {
                vec![formatted_node]
            } else {
                formatted_node
                    .split_whitespace()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
            };

            (is_aligned_macro, content)
        });

        if line_width == 0 {
            let content = formatted_elements
                .flat_map(|(_, s)| s)
                .collect::<Vec<_>>()
                .join(" ");

            return content;
        }

        let mut lines = vec![];
        let mut is_last_aligned_macro = false;

        for (is_aligned_macro, content) in formatted_elements {
            if is_aligned_macro {
                lines.extend(content);
            } else {
                let mut content = content;

                if let BdType::Centered | BdType::Filled | BdType::Ragged = block_type {
                    if !is_last_aligned_macro {
                        if let Some(current_line) = lines.last() {
                            let current_line = current_line
                                .split_whitespace()
                                .map(|s| s.to_string())
                                .collect::<Vec<_>>();

                            content = [current_line, content].concat();
                        }
                    }
                }

                let l = split_by_width(content, line_width);
                let l = add_indent_to_lines(l, line_width, &offset)
                    .iter()
                    .map(|line| format!("{}{}", " ".repeat(current_indent), line))
                    .collect::<Vec<_>>();

                lines.extend(l);
            }

            is_last_aligned_macro = is_aligned_macro;
        }

        self.formatting_state.current_indent =
            self.formatting_state.current_indent.saturating_sub(indent);

        let mut content = lines.join("\n");
        content = content.trim_end().to_string();

        "\n\n".to_string() + &content + "\n\n"
    }

    fn format_bf_block(&mut self, bf_type: BfType, macro_node: MacroNode) -> String {
        let _font_change = match bf_type {
            BfType::Emphasis => {
                // if self.supports_italic() {
                //     "\x1b[3m".to_string()
                // } else if self.supports_underline() {
                //     "\x1b[4m".to_string()
                // } else{
                //     String::new()
                // }
                String::new()
            }
            BfType::Literal => String::new(),
            BfType::Symbolic => {
                // if self.supports_bold(){
                //     "\x1b[1m".to_string()
                // }else{
                //     String::new()
                // }
                String::new()
            }
        };

        macro_node
            .nodes
            .into_iter()
            .map(|node| {
                let mut content = self.format_node(node);
                if !content.ends_with('\n') && !content.is_empty() {
                    content.push_str(&self.formatting_state.spacing);
                }
                content
            })
            .filter(|s| !s.is_empty())
            .collect::<Vec<String>>()
            .join("")
    }

    fn format_bk_block(&mut self, macro_node: MacroNode) -> String {
        let indent = self.formatting_state.current_indent;
        let max_width = self.formatting_settings.width - indent;

        let mut content = String::new();
        let mut current_len = indent;

        for node in macro_node.nodes.into_iter() {
            let formatted_node = self.format_node(node);
            let formatted_node_len = formatted_node.chars().count();

            current_len += formatted_node_len;

            if !content.is_empty() && current_len > max_width {
                current_len = indent + formatted_node_len + 1;
                content.push_str(&format!("\n{}", " ".repeat(indent)));
            }

            content.push_str(&format!("{} ", formatted_node.trim()));
        }

        content.trim().to_string()
    }

    fn format_bl_symbol_block(
        &self,
        items: Vec<(String, Vec<String>)>,
        width: Option<u8>,
        offset: Option<OffsetType>,
        list_type: BlType,
        compact: bool,
    ) -> String {
        let indent = self.get_width_indent(&width);
        let offset = self.get_offset_from_offset_type(&offset);
        let origin_indent = self.formatting_state.current_indent;
        let width = self.formatting_settings.width;
        let symbol_range = if let BlType::Enum = list_type {
            items.len().to_string().len() + 1
        } else {
            1
        };
        let mut full_indent = origin_indent + indent;
        if let BlType::Enum = list_type {
            full_indent += symbol_range.saturating_sub(2);
        }
        let line_width = width.saturating_sub(full_indent);
        let indent_str = " ".repeat(full_indent);

        let mut symbol = get_symbol("", &list_type);
        let mut content = String::new();
        for (_, body) in items {
            let mut body = merge_onelined(body, line_width, &indent_str, &offset);

            if let Some(first_line) = body.get_mut(0) {
                symbol = get_symbol(symbol.as_str(), &list_type);
                if first_line.len() > (origin_indent + symbol_range) {
                    first_line
                        .replace_range(origin_indent..(origin_indent + symbol_range), &symbol);
                }
            }

            content.push_str(&(body.join("\n") + "\n"));

            if !compact {
                content.push('\n');
            }
        }

        content
    }

    fn format_bl_item_block(
        &self,
        items: Vec<(String, Vec<String>)>,
        offset: Option<OffsetType>,
        compact: bool,
    ) -> String {
        let indent = self.formatting_settings.indent;
        let offset = self.get_offset_from_offset_type(&offset);
        let origin_indent = self.formatting_state.current_indent;
        let width = self.formatting_settings.width;
        let line_width = width.saturating_sub(origin_indent + indent);
        let origin_indent_str = " ".repeat(origin_indent);
        let delimiter = if compact { "\n" } else { "\n\n" };

        let mut content = String::new();
        for (_, body) in items {
            let body = body.join(" ");
            let mut body = split_by_width(
                body.split_whitespace()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>(),
                line_width + indent,
            );
            body = add_indent_to_lines(body, line_width + indent, &offset);
            for line in body.iter_mut() {
                *line = origin_indent_str.clone() + line;
            }
            content.push_str(&(body.join(delimiter) + delimiter));
        }

        content
    }

    fn format_bl_ohang_block(
        &self,
        items: Vec<(String, Vec<String>)>,
        offset: Option<OffsetType>,
        compact: bool,
    ) -> String {
        let indent = self.formatting_settings.indent;
        let offset = self.get_offset_from_offset_type(&offset);
        let origin_indent = self.formatting_state.current_indent;
        let width = self.formatting_settings.width;
        let line_width = width.saturating_sub(origin_indent + indent);
        let origin_indent_str = " ".repeat(origin_indent);

        let items = items
            .into_iter()
            .map(|(head, body)| (head, body.join(" ")))
            .collect::<Vec<_>>();

        let delimiter = if compact { "\n" } else { "\n\n" };

        let mut content = String::new();
        for (head, body) in items {
            let mut h = split_by_width(
                head.split_whitespace()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>(),
                line_width + indent,
            );
            let mut body = split_by_width(
                body.split_whitespace()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>(),
                line_width + indent,
            );
            h.extend(body);
            body = h;
            body = add_indent_to_lines(body, line_width + indent, &offset);
            for line in body.iter_mut() {
                *line = origin_indent_str.clone() + line;
            }
            content.push_str(&(body.join(delimiter).trim_end().to_string() + "\n"));
        }

        content
    }

    fn format_bl_inset_block(
        &self,
        items: Vec<(String, Vec<String>)>,
        offset: Option<OffsetType>,
        compact: bool,
        list_type: BlType,
    ) -> String {
        let head_space = match list_type {
            BlType::Inset => " ",
            BlType::Diag => "  ",
            _ => " ",
        };
        let indent = self.formatting_settings.indent;
        let offset = self.get_offset_from_offset_type(&offset);
        let origin_indent = self.formatting_state.current_indent;
        let width = self.formatting_settings.width;
        let line_width = width.saturating_sub(origin_indent + indent);
        let origin_indent_str = " ".repeat(origin_indent);

        let items = items
            .into_iter()
            .map(|(head, body)| (head, body.join(" ")))
            .collect::<Vec<_>>();

        let get_words = |s: &str| {
            s.split_whitespace()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        };

        let mut content = String::new();
        for (head, body) in items {
            let mut head = get_words(&head);
            let mut body = get_words(&body);
            if let Some(word) = head.last_mut() {
                *word += head_space;
            }

            body = split_by_width([head, body].concat(), line_width + indent);

            body = add_indent_to_lines(body, line_width + indent, &offset);
            for line in body.iter_mut() {
                *line = origin_indent_str.clone() + line;
            }
            content.push_str(&(body.join("\n") + "\n"));
            if !compact {
                content.push('\n');
            }
        }

        content
    }

    fn format_bl_column_block(&self, items: Vec<Vec<String>>, mut columns: Vec<String>) -> String {
        fn split_cells(table: Vec<Vec<String>>, col_widths: &[usize]) -> Vec<Vec<String>> {
            let mut splitted_rows_table = vec![];
            for row in table {
                let mut splitted_row = vec![];
                for (i, cell) in row.iter().enumerate() {
                    if i >= col_widths.len() {
                        break;
                    }
                    splitted_row.push(split_by_width(
                        cell.split_whitespace()
                            .map(|w| w.to_string())
                            .collect::<Vec<_>>(),
                        col_widths[i],
                    ));
                }
                splitted_rows_table.push(splitted_row);
            }
            let mut new_table = vec![];
            for row in splitted_rows_table {
                let height = row.iter().map(|c| c.len()).max().unwrap_or(0);
                for i in 0..height {
                    let mut new_row = vec![];
                    for cell in &row {
                        new_row.push(if i >= cell.len() {
                            "".to_string()
                        } else {
                            cell[i].clone()
                        });
                    }
                    new_table.push(new_row);
                }
            }
            new_table
        }

        /// Merges last row cells for rows with length bigger then [`col_count`]
        fn merge_row_ends(table: &mut [Vec<String>], col_count: usize) -> Option<(usize, usize)> {
            let mut row_len_range: Option<(usize, usize)> = None;
            table
                .iter_mut()
                .for_each(|row| match row.len().cmp(&col_count) {
                    std::cmp::Ordering::Less => row.resize(col_count, "".to_string()),
                    std::cmp::Ordering::Greater => {
                        if row_len_range.is_none() {
                            row_len_range = Some((usize::MAX, 0));
                        }
                        let end = row.split_off(col_count).join(" ");
                        let end_len = end.len();
                        row_len_range = row_len_range.map(|r| {
                            if end_len == 0 {
                                return (r.0, r.1.max(end_len));
                            }
                            (r.0.min(end_len), r.1.max(end_len))
                        });
                        row.push(trim_quotes(end.trim().to_string()));
                    }
                    _ => {}
                });

            row_len_range
        }

        fn calculate_col_widths(
            table: &Vec<Vec<String>>,
            total_width: &mut usize,
            columns: Vec<String>,
            mut row_len_range: Option<(usize, usize)>,
            max_line_width: usize,
        ) -> (Vec<usize>, bool) {
            let col_count = columns.len();
            let mut bigger_row_len = None;
            if let Some((min, max)) = row_len_range.as_mut() {
                let columns_total_width =
                    max_line_width.saturating_sub(columns.iter().map(|c| c.len()).sum::<usize>());
                bigger_row_len = if *max < columns_total_width {
                    Some(*max)
                } else if *min == usize::MAX {
                    None
                } else {
                    Some(*min)
                }
            };

            if let Some(bigger_row_len) = bigger_row_len {
                *total_width += bigger_row_len;
            }
            let columns_suit_by_width = *total_width < max_line_width;
            let mut col_widths = vec![0; col_count];

            if columns_suit_by_width {
                for (i, col) in columns.iter().enumerate() {
                    col_widths[i] = col.len();
                }
                if let Some(bigger_row_len) = bigger_row_len {
                    col_widths.push(bigger_row_len);
                } else if let Some(last_col_width) = col_widths.last_mut() {
                    *last_col_width += max_line_width - *total_width;
                }
            } else {
                for row in table {
                    for (i, cell) in row.iter().take(col_count).enumerate() {
                        col_widths[i] = col_widths[i].max(cell.len());
                    }
                }
                if let Some(bigger_row_len) = bigger_row_len {
                    col_widths.push(bigger_row_len);
                }
            }

            (col_widths, columns_suit_by_width)
        }

        fn format_table(
            mut table: Vec<Vec<String>>,
            columns: Vec<String>,
            max_line_width: usize,
        ) -> String {
            if table.is_empty() {
                return String::new();
            }

            let col_count = columns.len();
            let mut total_width: usize =
                columns.iter().map(|c| c.len()).sum::<usize>() + 2 * (col_count - 1);
            let row_len_range = merge_row_ends(&mut table, col_count);
            let (col_widths, columns_suit_by_width) = calculate_col_widths(
                &table,
                &mut total_width,
                columns,
                row_len_range,
                max_line_width,
            );
            if columns_suit_by_width {
                table = split_cells(table, &col_widths);
            }

            let mut result = String::new();
            for row in table {
                let mut offset = 0;
                let indent_step = 8;

                let items_to_print = col_widths.len().min(row.len());
                if !columns_suit_by_width {
                    for (i, cell) in row.iter().take(items_to_print).enumerate() {
                        result.push_str(&" ".repeat(offset));
                        result.push_str(&format!("{:<width$}", cell, width = col_widths[i]));
                        result = result.trim_end().to_string();
                        result.push('\n');
                        offset += indent_step;
                    }
                } else {
                    let mut line_width = 0;
                    for (i, cell) in row.iter().take(items_to_print).enumerate() {
                        let cell_width = col_widths[i] + 1;
                        if line_width + cell_width > max_line_width {
                            result.push('\n');
                            offset += indent_step;
                            line_width = offset;
                            result.push_str(&" ".repeat(offset));
                        }
                        result.push_str(&format!("{:<width$}  ", cell, width = col_widths[i]));
                        line_width += cell_width;
                    }
                    result = result.trim_end().to_string();
                    result.push('\n');
                }
            }
            result
        }

        let origin_indent = self.formatting_state.current_indent;
        let width = self.formatting_settings.width;
        let line_width = width.saturating_sub(origin_indent);

        columns.iter_mut().for_each(|c| {
            *c = trim_quotes(c.clone());
        });

        let mut content = format_table(items, columns, line_width);

        content = content
            .lines()
            .map(|line| " ".repeat(origin_indent) + line)
            .collect::<Vec<_>>()
            .join("\n");

        if !content.ends_with("\n") {
            content.push('\n');
        }

        content
    }

    fn format_bl_tag_block(
        &self,
        items: Vec<(String, Vec<String>)>,
        width: Option<u8>,
        offset: Option<OffsetType>,
        compact: bool,
    ) -> String {
        let indent = self.get_width_indent(&width);
        let offset = self.get_offset_from_offset_type(&offset);
        let origin_indent = self.formatting_state.current_indent;
        let width = self.formatting_settings.width;
        let line_width = width.saturating_sub(origin_indent + indent);
        let indent_str = " ".repeat(origin_indent + indent);
        let origin_indent_str = " ".repeat(origin_indent);

        let mut content = String::new();
        for (head, body) in items {
            let mut body = merge_onelined(body, line_width, &indent_str, &offset);
            let head = head.trim().to_string();
            let space = if head.len() < indent.saturating_sub(1) {
                if let Some(line) = body.first_mut() {
                    *line = line.trim_start().to_string();
                }
                " ".repeat(indent - head.len())
            } else {
                "\n".to_string()
            };

            content.push_str(&(origin_indent_str.clone() + &head + &space + &body.join("\n")));
            if !body.is_empty() || head.len() < indent.saturating_sub(1) {
                content.push('\n');
            }
            if !compact {
                content.push('\n');
            }
        }

        content
    }

    fn format_bl_hang_block(
        &self,
        items: Vec<(String, Vec<String>)>,
        is_first_block: Vec<bool>,
        width: Option<u8>,
        offset: Option<OffsetType>,
        compact: bool,
    ) -> String {
        let indent = self.get_width_indent(&width);
        let offset = self.get_offset_from_offset_type(&offset);
        let origin_indent = self.formatting_state.current_indent;
        let width = self.formatting_settings.width;
        let line_width = width.saturating_sub(origin_indent + indent);
        let indent_str = " ".repeat(origin_indent + indent);
        let origin_indent_str = " ".repeat(origin_indent);
        let mut content = String::new();

        for (i, (head, body)) in items.into_iter().enumerate() {
            let mut body = body;
            let mut head = head.clone();

            if !is_first_block[i] {
                let first_line = body
                    .first()
                    .cloned()
                    .unwrap_or_default()
                    .split_whitespace()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>();
                let mut i = 0;
                let mut j = 0;
                if head.len() > indent.saturating_sub(1) {
                    while head.len() < line_width + indent && i < first_line.len() {
                        if head.len() + first_line[i].len() >= line_width + indent {
                            break;
                        }
                        head.push_str(&(" ".to_string() + &first_line[i]));
                        j += first_line[i].len() + 1;
                        i += 1;
                    }
                }
                if let Some(line) = body.get_mut(0) {
                    line.replace_range(0..j, "");
                }
            }

            let mut body = merge_onelined(body, line_width, &indent_str, &offset);

            if head.len() < indent {
                if let Some(line) = body.first_mut() {
                    *line = line.trim_start().to_string();
                }
                let space = if is_first_block[i] {
                    "\n".to_string() + &origin_indent_str.clone() + &" ".repeat(indent)
                } else {
                    " ".repeat(indent - head.len())
                };
                content.push_str(
                    &(origin_indent_str.clone()
                        + &head
                        + &space
                        + body.join("\n").trim_end()
                        + "\n"),
                );
            } else {
                content.push_str(
                    &(origin_indent_str.clone()
                        + head.trim_end()
                        + "\n"
                        + body.join("\n").trim_end()
                        + "\n"),
                );
            }
            if !compact {
                content.push('\n');
            }
        }

        content
    }

    /// Extract head from It macro and format every element in it
    fn get_heads(&mut self, macro_node: MacroNode, list_type: &BlType) -> Vec<String> {
        macro_node
            .nodes
            .into_iter()
            .filter_map(|el| {
                let Element::Macro(MacroNode {
                    mdoc_macro: Macro::It { head },
                    ..
                }) = el
                else {
                    return None;
                };

                if list_type == &BlType::Column {
                    None
                } else {
                    let content = head
                        .iter()
                        .map(|element| self.format_node(element.clone()))
                        .collect::<Vec<_>>()
                        .join(" ")
                        .trim()
                        .to_string();

                    Some(content)
                }
            })
            .collect::<Vec<_>>()
    }

    /// Extract head from each It macro of Bl macro and format
    /// every element in them. Then return [`Vec<String>`] of
    /// all formatted It macro heads
    fn prepare_rows(&mut self, elements: Vec<Element>) -> Vec<String> {
        elements
            .split(|el| {
                matches!(
                    el,
                    Element::Macro(MacroNode {
                        mdoc_macro: Macro::Ta,
                        ..
                    })
                )
            })
            .map(|elements| {
                elements
                    .iter()
                    .map(|el| self.format_node(el.clone()))
                    .collect::<Vec<_>>()
                    .join(" ")
            })
            .collect::<Vec<_>>()
    }

    /// Extract body from each It macro of Bl macro and format
    /// every element in them. Then return [`Vec<String>`] of
    /// all formatted It macro bodies
    fn get_bodies(&mut self, macro_node: MacroNode, list_type: &BlType) -> Vec<Vec<String>> {
        macro_node
            .nodes
            .into_iter()
            .filter_map(|el| {
                let Element::Macro(MacroNode {
                    mdoc_macro: Macro::It { head },
                    nodes,
                }) = el
                else {
                    return None;
                };

                if list_type == &BlType::Column {
                    Some(self.prepare_rows([head, nodes].concat()))
                } else {
                    Some(
                        nodes
                            .iter()
                            .filter(|el| {
                                !matches!(
                                    el,
                                    Element::Macro(MacroNode {
                                        mdoc_macro: Macro::Ta,
                                        ..
                                    })
                                )
                            })
                            .map(|element| self.format_node(element.clone()))
                            .collect::<Vec<_>>(),
                    )
                }
            })
            .collect::<Vec<_>>()
    }

    fn format_bl_block(
        &mut self,
        list_type: BlType,
        width: Option<u8>,
        offset: Option<OffsetType>,
        compact: bool,
        columns: Vec<String>,
        macro_node: MacroNode,
    ) -> String {
        fn get_symbol_width(list_type: &BlType, macro_node: &MacroNode) -> usize {
            if !matches!(list_type, BlType::Bullet | BlType::Dash | BlType::Enum) {
                return 0;
            }
            let it_count = macro_node
                .nodes
                .iter()
                .filter(|n| {
                    matches!(
                        n,
                        Element::Macro(MacroNode {
                            mdoc_macro: Macro::It { .. },
                            ..
                        })
                    )
                })
                .count();
            if let BlType::Enum = list_type {
                it_count.to_string().len()
            } else {
                0
            }
        }

        fn strip_empty_between_nested(
            formatted_its: &mut [Vec<String>],
            macro_node: &MacroNode,
            max_nl: usize,
        ) {
            for (i, it_node) in macro_node.nodes.iter().enumerate() {
                let Element::Macro(MacroNode {
                    mdoc_macro: Macro::It { .. },
                    nodes,
                }) = it_node
                else {
                    continue;
                };
                let Some(Element::Macro(MacroNode {
                    mdoc_macro: Macro::Bl { .. },
                    ..
                })) = nodes.first()
                else {
                    continue;
                };
                let Some(element) = formatted_its.get_mut(i) else {
                    continue;
                };
                let Some(first) = element.first_mut() else {
                    continue;
                };
                let leading_nl_count = first
                    .chars()
                    .take_while(|ch| ch.is_whitespace())
                    .filter(|ch| *ch == '\n')
                    .count();
                if leading_nl_count >= max_nl {
                    *first = first
                        .lines()
                        .skip(max_nl + 1)
                        .collect::<Vec<_>>()
                        .join("\n")
                        .to_string();
                }
            }
        }

        let heads = self.get_heads(macro_node.clone(), &list_type);
        let width_indent = self.get_width_indent(&width);
        let offset_indent = self.get_offset_indent(&offset);

        self.formatting_state.current_indent += offset_indent + width_indent;
        let symbol_width = get_symbol_width(&list_type, &macro_node);
        let is_symbol = matches!(list_type, BlType::Bullet | BlType::Dash | BlType::Enum);
        if is_symbol {
            self.formatting_state.current_indent += symbol_width;
        }

        let mut bodies = self.get_bodies(macro_node.clone(), &list_type);

        self.formatting_state.current_indent = self
            .formatting_state
            .current_indent
            .saturating_sub(width_indent);
        if is_symbol {
            self.formatting_state.current_indent = self
                .formatting_state
                .current_indent
                .saturating_sub(symbol_width);
        }

        let max_nl = if matches!(list_type, BlType::Hang) {
            1
        } else {
            0
        };
        strip_empty_between_nested(&mut bodies, &macro_node, max_nl);

        let items: Vec<(String, Vec<String>)> = if heads.is_empty() {
            bodies
                .clone()
                .into_iter()
                .map(|body| ("".to_string(), body))
                .collect()
        } else {
            heads.into_iter().zip(bodies.clone()).collect()
        };

        let mut content = match list_type {
            BlType::Bullet | BlType::Dash | BlType::Enum => {
                self.format_bl_symbol_block(items, width, offset, list_type, compact)
            }
            BlType::Item => self.format_bl_item_block(items, offset, compact),
            BlType::Ohang => self.format_bl_ohang_block(items, offset, compact),
            BlType::Inset | BlType::Diag => {
                self.format_bl_inset_block(items, offset, compact, list_type)
            }
            BlType::Column => self.format_bl_column_block(bodies, columns),
            BlType::Tag => self.format_bl_tag_block(items, width, offset, compact),
            BlType::Hang => {
                let MacroNode { nodes, .. } = macro_node;
                let is_first_block = nodes
                    .iter()
                    .map(|el| {
                        if let Element::Macro(MacroNode { nodes, .. }) = el {
                            if let Some(Element::Macro(MacroNode { mdoc_macro, .. })) =
                                nodes.first()
                            {
                                return matches!(mdoc_macro, &Macro::Bl { .. } | &Macro::Bd { .. });
                            }
                        }
                        false
                    })
                    .collect::<Vec<_>>();
                self.format_bl_hang_block(items, is_first_block, width, offset, compact)
            }
        };

        self.formatting_state.current_indent = self
            .formatting_state
            .current_indent
            .saturating_sub(offset_indent);

        content = "\n\n".to_string() + &content + "\n";

        content
    }

    fn format_bl_blocks(&mut self, macro_node: MacroNode) -> String {
        split_nested_bl(macro_node)
            .into_iter()
            .map(|element| {
                let Element::Macro(ref macro_node) = element else {
                    return self.format_node(element);
                };
                let MacroNode { mdoc_macro, .. } = macro_node.clone();
                let Macro::Bl {
                    list_type,
                    width,
                    offset,
                    compact,
                    columns,
                } = mdoc_macro.clone()
                else {
                    return self.format_node(element);
                };
                self.format_bl_block(
                    list_type,
                    width,
                    offset,
                    compact,
                    columns,
                    macro_node.clone(),
                )
            })
            .collect::<Vec<_>>()
            .join("")
    }
}

// Formatting block full-implicit.
impl MdocFormatter {
    fn format_it_block(&mut self, _head: Vec<Element>, _macro_node: MacroNode) -> String {
        String::new()
    }

    fn format_nd(&mut self, macro_node: MacroNode) -> String {
        let content = macro_node
            .nodes
            .into_iter()
            .map(|node| {
                let mut content = self.format_node(node);
                if !content.ends_with('\n') && !content.is_empty() {
                    content.push_str(&self.formatting_state.spacing);
                }
                content
            })
            .filter(|s| !s.is_empty())
            .collect::<Vec<String>>()
            .join("");

        format!("– {}", content)
    }

    fn format_nm(&mut self, name: Option<String>, macro_node: MacroNode) -> String {
        let content = self.format_inline_macro(macro_node);

        if self.formatting_state.first_name.is_none() {
            self.formatting_state.first_name = name;
            let first_name = match self.formatting_state.first_name.as_ref() {
                Some(name) => name.clone(),
                None => "".to_string(),
            };

            if is_first_char_delimiter(&content) {
                format!("{}{}", first_name.trim(), content.trim())
            } else {
                format!("{} {}", first_name.trim(), content.trim())
            }
        } else {
            let provided_name = match name {
                Some(name) => name,
                None => self.formatting_state.first_name.clone().unwrap(),
            };

            let separator = if is_first_char_delimiter(&content) {
                ""
            } else {
                " "
            };

            format!("{}{}{}", provided_name.trim(), separator, content.trim())
        }
    }

    /// If line don't have enought indentation according
    /// to [`self.formatting_settings.indent`], then
    /// indent is added to this line
    fn add_missing_indent(&self, content: &mut String) {
        *content = content
            .split("\n")
            .map(|line| {
                let indent_is_small = line.chars().take_while(|ch| ch.is_whitespace()).count()
                    < self.formatting_settings.indent;

                let is_not_empty = !(line.chars().all(|ch| ch.is_whitespace()) || line.is_empty());
                let line = if indent_is_small && is_not_empty && !line.starts_with("\\[ssindent]") {
                    " ".repeat(self.formatting_settings.indent) + line.trim_start()
                } else {
                    line.to_string()
                };

                line
            })
            .collect::<Vec<_>>()
            .join("\n");
    }

    fn format_sh_block(&mut self, title: String, macro_node: MacroNode) -> String {
        fn append_formatted_node(
            content: &mut String,
            formatted_node: &str,
            current_len: &mut usize,
            indent: usize,
            max_width: usize,
        ) {
            let formatted_node_len = formatted_node.chars().count();
            *current_len += formatted_node_len;

            if !content.is_empty() && *current_len > max_width {
                *current_len = indent + formatted_node_len + 1;
                content.push_str(&format!("\n{}", " ".repeat(indent)));
            }

            content.push_str(&format!("{} ", formatted_node.trim_end()));
        }

        let mut prev_node = Macro::Soi;
        let mut is_first_an_in_authors_block = true;

        self.formatting_state.current_indent += self.formatting_settings.indent;

        let mut content = if title.eq_ignore_ascii_case("SYNOPSIS") {
            let first_name_len = self
                .formatting_state
                .first_name
                .clone()
                .unwrap_or_default()
                .len();
            let indent = self.formatting_state.current_indent + first_name_len + 1;

            let max_width = self.formatting_settings.width;

            let mut content = String::new();
            let mut current_len = indent;

            for node in macro_node.nodes.into_iter() {
                let formatted_node = match &node {
                    Element::Macro(macro_node) => {
                        let formatted = match &macro_node.mdoc_macro {
                            Macro::Vt => self.format_vt_synopsis(macro_node.clone()),
                            Macro::Nm { name } => {
                                let formatted_node =
                                    self.format_nm(name.clone(), macro_node.clone());

                                current_len = indent;

                                format!("\n{}", formatted_node.trim_end())
                            }
                            Macro::Ft => {
                                let formatted_node = self.format_ft_synopsis(macro_node.clone());
                                content.push_str(&formatted_node);

                                current_len = indent;

                                continue;
                            }
                            Macro::In { ref filename } => {
                                let formatted_node = self.format_in_synopsis(
                                    filename.as_str(),
                                    macro_node.clone(),
                                    &prev_node,
                                );
                                content.push_str(&formatted_node);

                                current_len = indent;

                                continue;
                            }
                            Macro::Fd {
                                directive,
                                arguments,
                            } => {
                                let formatted_node = self.format_fd_synopsis(directive, arguments);
                                content.push_str(&formatted_node);

                                current_len = indent;

                                continue;
                            }
                            Macro::Fn { funcname } => {
                                let formatted_node =
                                    self.format_fn_synopsis(funcname, macro_node.clone());
                                content.push_str(&formatted_node);

                                current_len = indent;

                                continue;
                            }
                            Macro::Bk => {
                                for node in macro_node.nodes.clone().into_iter() {
                                    let formatted_node = self.format_node(node);
                                    append_formatted_node(
                                        &mut content,
                                        &formatted_node,
                                        &mut current_len,
                                        indent,
                                        max_width,
                                    );
                                    continue;
                                }

                                String::new()
                            }
                            _ => self.format_macro_node(macro_node.clone()),
                        };

                        prev_node = macro_node.mdoc_macro.clone();
                        formatted
                    }
                    Element::Text(text) => self.format_text_node(text),
                    Element::Eoi => "".to_string(),
                };

                append_formatted_node(
                    &mut content,
                    &formatted_node,
                    &mut current_len,
                    indent,
                    max_width,
                );
            }

            content.trim().to_string()
        } else {
            macro_node
                .nodes
                .into_iter()
                .map(|node| match node {
                    Element::Macro(ref macro_node) => {
                        if title.eq_ignore_ascii_case("AUTHORS") {
                            match &macro_node.mdoc_macro {
                                Macro::An { author_name_type } => {
                                    if is_first_an_in_authors_block {
                                        self.formatting_state.split_mod = false;
                                        is_first_an_in_authors_block = false;
                                    } else {
                                        self.formatting_state.split_mod = true;
                                    }

                                    self.format_an_authors(
                                        author_name_type.clone(),
                                        macro_node.clone(),
                                    )
                                }
                                _ => self.format_macro_node(macro_node.clone()),
                            }
                        } else if title.eq_ignore_ascii_case("SEE ALSO") {
                            match &macro_node.mdoc_macro {
                                Macro::Rs => self.format_rs_see_also(macro_node.clone()),
                                _ => self.format_macro_node(macro_node.clone()),
                            }
                        } else {
                            self.format_macro_node(macro_node.clone())
                        }
                    }
                    Element::Text(ref text) => self.format_text_node(text),
                    Element::Eoi => String::new(),
                })
                .filter(|s| !s.is_empty())
                .collect::<Vec<_>>()
                .join(&self.formatting_state.spacing)
        };

        self.add_missing_indent(&mut content);

        self.formatting_state.current_indent = 0;

        let content = if content.starts_with('\n') {
            content.strip_prefix("\n").unwrap().to_string()
        } else {
            content
        };

        format!("\n{}\n{}", title.to_uppercase(), content.trim_end())
    }

    fn format_ss_block(&mut self, title: String, macro_node: MacroNode) -> String {
        if self.formatting_state.current_indent == 0 {
            self.formatting_state.current_indent += self.formatting_settings.indent;
        }
        let mut content = macro_node
            .nodes
            .into_iter()
            .map(|node| {
                let mut content = self.format_node(node);
                if !content.ends_with('\n') && !content.is_empty() {
                    content.push_str(&self.formatting_state.spacing);
                }
                content
            })
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>()
            .join("");

        self.add_missing_indent(&mut content);
        self.formatting_state.current_indent = 0;

        format!("\n\n\\[ssindent]{title}\n\n{content}\n")
    }
}

// Formatting block partial-explicit.
impl MdocFormatter {
    fn format_partial_explicit_block(&mut self, iter: impl Iterator<Item = Element>) -> String {
        let mut result = String::new();
        let mut prev_was_open = false;
        let mut is_first_node = true;

        for node in iter {
            match node {
                Element::Text(text) => match text.as_str() {
                    "(" | "[" => {
                        result.push_str(&text);
                        prev_was_open = true;
                    }
                    ")" | "]" | "." | "," | ":" | ";" | "!" | "?" => {
                        result.push_str(&text);
                        prev_was_open = false;
                    }
                    _ => {
                        if prev_was_open {
                            result.push_str(&self.format_text_node(&text));
                        } else {
                            let offset = if is_first_node {
                                ""
                            } else {
                                self.formatting_state.spacing.as_str()
                            };
                            result.push_str(&format!("{}{}", offset, self.format_text_node(&text)));
                        }
                        prev_was_open = false;
                    }
                },
                _ => {
                    let mut content = self.format_node(node);
                    if !content.ends_with('\n') && !content.is_empty() {
                        content.push_str(&self.formatting_state.spacing);
                    }
                    result.push_str(&content);
                    prev_was_open = false;
                }
            }

            if is_first_node {
                is_first_node = false;
            }
        }

        result.trim().to_string()
    }

    fn format_a_block(&mut self, macro_node: MacroNode) -> String {
        let iter = macro_node.nodes.into_iter();
        let body = iter.clone().take_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Ac)
            } else {
                false
            }
        });

        let tail = iter.skip_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Ac)
            } else {
                false
            }
        });

        let formatted_body = self.format_partial_explicit_block(body);
        let formatted_tail = self.format_partial_explicit_block(tail);

        if is_first_word_delimiter(&formatted_tail) {
            return format!("⟨{}⟩{}", formatted_body, formatted_tail);
        }

        format!("⟨{}⟩ {}", formatted_body, formatted_tail)
    }

    fn format_b_block(&mut self, macro_node: MacroNode) -> String {
        let iter = macro_node.nodes.into_iter();
        let body = iter.clone().take_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Bc)
            } else {
                false
            }
        });

        let tail = iter.skip_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Bc)
            } else {
                false
            }
        });

        let formatted_body = self.format_partial_explicit_block(body);
        let formatted_tail = self.format_partial_explicit_block(tail);

        if is_first_word_delimiter(&formatted_tail) {
            return format!("[{}]{}", formatted_body.trim(), formatted_tail.trim());
        }

        format!("[{}] {}", formatted_body.trim(), formatted_tail.trim())
    }

    fn format_br_block(&mut self, macro_node: MacroNode) -> String {
        let iter = macro_node.nodes.into_iter();
        let body = iter.clone().take_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Brc)
            } else {
                false
            }
        });

        let tail = iter.skip_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Brc)
            } else {
                false
            }
        });

        let formatted_body = self.format_partial_explicit_block(body);
        let formatted_tail = self.format_partial_explicit_block(tail);

        if is_first_word_delimiter(&formatted_tail) {
            return format!("{{{}}}{}", formatted_body.trim(), formatted_tail.trim());
        }

        format!("{{{}}} {}", formatted_body, formatted_tail.trim())
    }

    fn format_d_block(&mut self, macro_node: MacroNode) -> String {
        let iter = macro_node.nodes.into_iter();
        let body = iter.clone().take_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Dc)
            } else {
                false
            }
        });

        let tail = iter.skip_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Dc)
            } else {
                false
            }
        });

        let formatted_body = self.format_partial_explicit_block(body);
        let formatted_tail = self.format_partial_explicit_block(tail);

        if is_first_word_delimiter(&formatted_tail) {
            return format!("“{}”{}", formatted_body.trim(), formatted_tail.trim());
        }

        format!("“{}” {}", formatted_body.trim(), formatted_tail.trim())
    }

    fn format_e_block(
        &mut self,
        opening_delimiter: Option<char>,
        closing_delimiter: Option<char>,
        macro_node: MacroNode,
    ) -> String {
        let iter = macro_node.nodes.into_iter();
        let body = iter.clone().take_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Dc)
            } else {
                false
            }
        });

        let tail = iter.skip_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Dc)
            } else {
                false
            }
        });

        let formatted_body = self.format_partial_explicit_block(body);
        let formatted_tail = self.format_partial_explicit_block(tail);

        match (opening_delimiter, closing_delimiter) {
            (Some(open), Some(close)) => {
                format!(
                    "{}{}{} {} ",
                    open,
                    formatted_body.trim(),
                    close,
                    formatted_tail.trim()
                )
            }
            (Some(open), None) => {
                format!(
                    "{}{} {} ",
                    open,
                    formatted_body.trim(),
                    formatted_tail.trim()
                )
            }
            (None, Some(close)) => {
                format!(
                    "{}{} {} ",
                    formatted_body.trim(),
                    close,
                    formatted_tail.trim()
                )
            }
            (None, None) => format!("{} {}", formatted_body.trim(), formatted_tail.trim()),
        }
    }

    fn format_f_block(&mut self, funcname: String, macro_node: MacroNode) -> String {
        let mut body = macro_node
            .nodes
            .into_iter()
            .map(|node| {
                let mut content = self.format_node(node);
                if !content.ends_with('\n') && !content.is_empty() {
                    content.push_str(&format!(",{}", self.formatting_state.spacing));
                }
                content
            })
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>()
            .join("");

        body.pop();
        body.pop();

        format!("{}({});", funcname, body)
    }

    fn format_o_block(&mut self, macro_node: MacroNode) -> String {
        let iter = macro_node.nodes.into_iter();
        let body = iter.clone().take_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Oc)
            } else {
                false
            }
        });

        let tail = iter.skip_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Oc)
            } else {
                false
            }
        });

        let formatted_body = self.format_partial_explicit_block(body);
        let formatted_tail = self.format_partial_explicit_block(tail);

        if is_first_word_delimiter(&formatted_tail) {
            return format!("[{}]{}", formatted_body, formatted_tail);
        }

        format!("[{}] {}", formatted_body, formatted_tail)
    }

    fn format_p_block(&mut self, macro_node: MacroNode) -> String {
        let iter = macro_node.nodes.into_iter();
        let body = iter.clone().take_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Pc)
            } else {
                false
            }
        });

        let tail = iter.skip_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Pc)
            } else {
                false
            }
        });

        let formatted_body = self.format_partial_explicit_block(body);
        let formatted_tail = self.format_partial_explicit_block(tail);

        if is_first_word_delimiter(&formatted_tail) {
            return format!("({}){}", formatted_body, formatted_tail);
        }

        format!("({}) {} ", formatted_body.trim(), formatted_tail.trim())
    }

    fn format_q_block(&mut self, macro_node: MacroNode) -> String {
        let iter = macro_node.nodes.into_iter();
        let body = iter.clone().take_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Qc)
            } else {
                false
            }
        });

        let tail = iter.skip_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Qc)
            } else {
                false
            }
        });

        let formatted_body = self.format_partial_explicit_block(body);
        let formatted_tail = self.format_partial_explicit_block(tail);

        if is_first_word_delimiter(&formatted_tail) {
            return format!("\"{}\"{} ", formatted_body, formatted_tail);
        }

        format!("\"{}\" {} ", formatted_body.trim(), formatted_tail.trim())
    }

    fn format_s_block(&mut self, macro_node: MacroNode) -> String {
        let iter = macro_node.nodes.into_iter();
        let body = iter.clone().take_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Sc)
            } else {
                false
            }
        });

        let tail = iter.skip_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Sc)
            } else {
                false
            }
        });

        let formatted_body = self.format_partial_explicit_block(body);
        let formatted_tail = self.format_partial_explicit_block(tail);

        if is_first_word_delimiter(&formatted_tail) {
            return format!("'{}'{} ", formatted_body, formatted_tail);
        }

        format!("'{}' {} ", formatted_body, formatted_tail)
    }

    fn format_x_block(&mut self, macro_node: MacroNode) -> String {
        let iter = macro_node.nodes.into_iter();
        let body = iter.clone().take_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Xc)
            } else {
                false
            }
        });

        let tail = iter.skip_while(|p| {
            if let Element::Macro(ref macro_node) = p {
                !matches!(macro_node.mdoc_macro, Macro::Xc)
            } else {
                false
            }
        });

        let formatted_body = self.format_partial_explicit_block(body);
        let formatted_tail = self.format_partial_explicit_block(tail);

        if is_first_word_delimiter(&formatted_tail) {
            return format!("{}{} ", formatted_body, formatted_tail);
        }

        format!("{} {} ", formatted_body, formatted_tail)
    }
}

// Formatting Rs-Re bloock. Can contain only %* macros
impl MdocFormatter {
    fn format_rs_block(&self, macro_node: MacroNode) -> String {
        let mut iter = macro_node.nodes.into_iter().peekable();

        let mut items = Vec::new();
        while let Some(el) = iter.peek() {
            if let Element::Macro(node) = el {
                if node.mdoc_macro == Macro::A {
                    let el = iter.next().unwrap();
                    if let Element::Macro(node) = el {
                        items.push(self.format_a(node));
                    }
                } else {
                    break;
                }
            } else {
                unreachable!("Unexpected rule!");
            }
        }

        let formatted_a = match items.len() {
            0 => "".to_string(),
            1 => items[0].clone(),
            2 => format!("{} and {}", items[0], items[1]),
            _ => {
                let last = items.last().unwrap();
                let all_but_last = &items[..items.len() - 1];
                format!("{}, and {}", all_but_last.join(", "), last)
            }
        };

        let formatted_all = iter
            .map(|el| match el {
                Element::Macro(node) => match node.mdoc_macro {
                    Macro::B => self.format_b(node),
                    Macro::C => self.format_c(node),
                    Macro::D => self.format_d(node),
                    Macro::I => self.format_i(node),
                    Macro::J => self.format_j(node),
                    Macro::N => self.format_n(node),
                    Macro::O => self.format_o(node),
                    Macro::P => self.format_p(node),
                    Macro::Q => self.format_q(node),
                    Macro::R => self.format_r(node),
                    Macro::T => self.format_t(node),
                    Macro::U => self.format_u(node),
                    Macro::V => self.format_v(node),
                    _ => unreachable!("Rs can not contain macro: {:?}", node),
                },
                _ => unreachable!("Unexpected element type!"),
            })
            .collect::<Vec<_>>()
            .join(", ");

        match (formatted_a.is_empty(), formatted_all.is_empty()) {
            (true, true) => "".to_string(),
            (true, false) => format!("{}.\n", formatted_all),
            (false, true) => format!("{}.\n", formatted_a),
            (false, false) => format!("{}, {}.\n", formatted_a, formatted_all),
        }
    }

    fn format_rs_see_also(&self, macro_node: MacroNode) -> String {
        let c = self.format_rs_block(macro_node);

        format!("\n{}", c)
    }

    fn format_a(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_b(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_c(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_d(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_i(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_j(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_n(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_o(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_p(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_q(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_r(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_t(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_u(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_v(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }
}

// Formatting block partial-implicit.
impl MdocFormatter {
    fn format_partial_implicit_block(
        &mut self,
        macro_node: &mut MacroNode,
        open_char: &str,
        close_char: &str,
    ) -> String {
        fn is_closing_delimiter(s: &str) -> bool {
            matches!(s, ")" | "]" | "." | "," | ":" | ";" | "!" | "?")
        }

        fn extract_trailing_delims(node: &mut MacroNode) -> String {
            let mut delim_sequence = String::new();
            loop {
                if node.nodes.is_empty() {
                    break;
                }
                let last_index = node.nodes.len() - 1;
                match &mut node.nodes[last_index] {
                    Element::Text(ref text) if is_closing_delimiter(text) => {
                        if let Some(Element::Text(delim)) = node.nodes.pop() {
                            delim_sequence = format!("{}{}", delim, delim_sequence);
                        } else {
                            break;
                        }
                    }
                    Element::Macro(ref mut inner_macro_node) => {
                        let inner_delims = extract_trailing_delims(inner_macro_node);
                        if inner_delims.is_empty() {
                            break;
                        }
                        delim_sequence = format!("{}{}", inner_delims, delim_sequence);
                        if inner_macro_node.nodes.is_empty() {
                            node.nodes.pop();
                        }
                    }
                    _ => break,
                }
            }
            delim_sequence
        }

        let trailing_punctuation = extract_trailing_delims(macro_node);

        let mut result = open_char.to_string();
        let mut prev_was_open = false;
        let mut is_first_node = true;

        for node in &macro_node.nodes {
            let content = match node {
                Element::Text(text) => match text.as_str() {
                    "(" | "[" => {
                        prev_was_open = true;
                        text.clone()
                    }
                    ")" | "]" => {
                        prev_was_open = false;
                        text.clone()
                    }
                    "." | "," | ":" | ";" | "!" | "?" => {
                        prev_was_open = false;
                        String::new()
                    }
                    _ => {
                        let formatted_text = self.format_text_node(text);
                        let offset = if is_first_node || prev_was_open {
                            ""
                        } else {
                            self.formatting_state.spacing.as_str()
                        };
                        prev_was_open = false;
                        format!("{}{}", offset, formatted_text)
                    }
                },
                other => {
                    let mut s = self.format_node(other.clone());
                    if !s.is_empty() && !s.ends_with('\n') {
                        s.push_str(&self.formatting_state.spacing);
                    }
                    s
                }
            };

            if !content.is_empty() {
                result.push_str(&content);
            }
            is_first_node = false;
        }

        result = result.trim().to_string();

        format!("{}{}{}", result, close_char, trailing_punctuation)
    }

    fn format_aq(&mut self, mut macro_node: MacroNode) -> String {
        self.format_partial_implicit_block(&mut macro_node, "⟨", "⟩")
    }

    fn format_bq(&mut self, mut macro_node: MacroNode) -> String {
        self.format_partial_implicit_block(&mut macro_node, "[", "]")
    }

    fn format_brq(&mut self, mut macro_node: MacroNode) -> String {
        self.format_partial_implicit_block(&mut macro_node, "{{", "}}")
    }

    fn format_d1(&mut self, mut macro_node: MacroNode) -> String {
        let spaces = " ".repeat(self.formatting_settings.indent);
        self.format_partial_implicit_block(&mut macro_node, &spaces, "")
    }

    fn format_dl(&mut self, mut macro_node: MacroNode) -> String {
        let content = self.format_partial_implicit_block(&mut macro_node, "", "");
        let spaces =
            " ".repeat(self.formatting_state.current_indent + self.formatting_settings.indent);

        format!("\n{}{}\n", spaces, content)
    }

    fn format_dq(&mut self, mut macro_node: MacroNode) -> String {
        self.format_partial_implicit_block(&mut macro_node, "“", "”")
    }

    fn format_en(&mut self, mut macro_node: MacroNode) -> String {
        self.format_partial_implicit_block(&mut macro_node, "", "")
            .trim()
            .to_string()
    }

    fn format_op(&mut self, mut macro_node: MacroNode) -> String {
        self.format_partial_implicit_block(&mut macro_node, "[", "]")
    }

    fn format_pq(&mut self, mut macro_node: MacroNode) -> String {
        self.format_partial_implicit_block(&mut macro_node, "(", ")")
    }

    fn format_ql(&mut self, mut macro_node: MacroNode) -> String {
        self.format_partial_implicit_block(&mut macro_node, "‘", "’")
    }

    fn format_qq(&mut self, mut macro_node: MacroNode) -> String {
        self.format_partial_implicit_block(&mut macro_node, "\"", "\"")
    }

    fn format_sq(&mut self, mut macro_node: MacroNode) -> String {
        self.format_partial_implicit_block(&mut macro_node, "\'", "\'")
    }

    fn format_vt(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_vt_synopsis(&mut self, mut macro_node: MacroNode) -> String {
        self.format_partial_implicit_block(&mut macro_node, "", "")
            .trim()
            .to_string()
    }
}

// Format other in-line macros.
impl MdocFormatter {
    fn format_inline_macro(&self, macro_node: MacroNode) -> String {
        let mut result = String::new();
        let mut prev_was_open = false;
        let mut is_first_node = true;

        for node in macro_node.nodes {
            match node {
                Element::Text(text) => match text.as_str() {
                    "(" | "[" => {
                        result.push_str(&text);
                        prev_was_open = true;
                    }
                    ")" | "]" | "." | "," | ":" | ";" | "!" | "?" => {
                        result.push_str(&text);
                        prev_was_open = false;
                    }
                    _ => {
                        match prev_was_open {
                            true => result.push_str(&self.format_text_node(&text)),
                            false => {
                                let offset = if is_first_node {
                                    ""
                                } else {
                                    self.formatting_state.spacing.as_str()
                                };
                                let formatted_node =
                                    format!("{}{}", offset, self.format_text_node(&text));
                                result.push_str(&formatted_node);
                            }
                        }
                        prev_was_open = false;
                    }
                },
                _ => unreachable!("macro can't contain macro node or EOI!"),
            }

            if is_first_node {
                is_first_node = false;
            }
        }

        result.trim().to_string()
    }

    fn format_ad(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_ap(&self, macro_node: MacroNode) -> String {
        let content = self.format_inline_macro(macro_node);
        format!("'{}", content)
    }

    fn format_an(&mut self, an_type: AnType, macro_node: MacroNode) -> String {
        match an_type {
            AnType::NoSplit => {
                self.formatting_state.split_mod = false;
                String::new()
            }
            AnType::Split => {
                self.formatting_state.split_mod = true;
                String::new()
            }
            AnType::Name => {
                let content = self.format_inline_macro(macro_node);
                match self.formatting_state.split_mod {
                    true => format!("\n{}", content),
                    false => content,
                }
            }
        }
    }

    fn format_an_authors(&mut self, an_type: AnType, macro_node: MacroNode) -> String {
        match an_type {
            AnType::NoSplit => String::new(),
            AnType::Split => String::new(),
            AnType::Name => {
                let content = self.format_inline_macro(macro_node);
                match self.formatting_state.split_mod {
                    true => format!(
                        "\n{}{}",
                        " ".repeat(self.formatting_settings.indent),
                        content
                    ),
                    false => format!("{}{}", " ".repeat(self.formatting_settings.indent), content),
                }
            }
        }
    }

    fn format_ar(&self, macro_node: MacroNode) -> String {
        if macro_node.nodes.is_empty() {
            return "file ...".to_string();
        }

        self.format_inline_macro(macro_node)
    }

    fn format_bt(&self) -> String {
        "is currently in beta test.".to_string()
    }

    fn format_cd(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_cm(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_db(&self) -> String {
        "".to_string()
    }

    fn format_dv(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_em(&self, macro_node: MacroNode) -> String {
        // let line = self.format_inline_macro(macro_node);

        // if self.supports_italic() {
        //     format!("\x1b[3m{line}\x1b[0m")
        // } else if self.supports_underline() {
        //     format!("\x1b[4m{line}\x1b[0m")
        // } else {
        //     line
        // }
        self.format_inline_macro(macro_node)
    }

    fn format_dt(&mut self, title: Option<String>, section: &str, arch: Option<String>) -> String {
        let title = match title {
            Some(name) => format!("{name}({section})"),
            None if section.is_empty() => "UNTITLED".to_string(),
            _ => format!("UNTITLED({section})"),
        };

        let section = match section {
            "1" => "General Commands Manual",
            "2" => "System Calls Manual",
            "3" => "Library Functions Manual",
            "4" => "Device Drivers Manual",
            "5" => "File Formats Manual",
            "6" => "Games Manual",
            "7" => "Miscellaneous Information Manual",
            "8" => "System Manager's Manual",
            "9" => "Kernel Developer's Manual",
            _ if section.is_empty() => "LOCAL",
            _ => section,
        };

        let section = if let Some(val) = arch {
            format!("{section} ({val})")
        } else {
            section.to_string()
        };

        let side_len = title.len();
        let center_len = section.len();

        let center_start = (self.formatting_settings.width / 2).saturating_sub(center_len / 2);

        let right_start = self.formatting_settings.width.saturating_sub(side_len);

        let mut line = String::with_capacity(self.formatting_settings.width);

        line.push_str(&title);

        if center_start > side_len {
            line.push_str(&" ".repeat(center_start - side_len));
        }
        line.push_str(&section);

        let current_len = line.len();
        if right_start > current_len {
            line.push_str(&" ".repeat(right_start - current_len));
        }
        line.push_str(&title);

        let final_len = line.len();
        if final_len < self.formatting_settings.width {
            line.push_str(&" ".repeat(self.formatting_settings.width - final_len));
        }

        self.formatting_state.header_text = Some(line + "\n");
        String::new()
    }

    fn format_dx(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_dd(&mut self, line: &str) -> String {
        fn parse_month_name(month: &str) -> Option<u32> {
            let mut months = HashMap::new();
            months.insert("january", 1);
            months.insert("february", 2);
            months.insert("march", 3);
            months.insert("april", 4);
            months.insert("may", 5);
            months.insert("june", 6);
            months.insert("july", 7);
            months.insert("august", 8);
            months.insert("september", 9);
            months.insert("october", 10);
            months.insert("november", 11);
            months.insert("december", 12);

            months.get(&month.to_lowercase()[..]).copied()
        }

        let trimmed = line.trim();

        if trimmed == "$Mdocdate$" {
            return chrono::Utc::now().format("%B %-d, %Y").to_string();
        }

        let prefix = "$Mdocdate: ";
        if let Some(remainder) = trimmed.strip_prefix(prefix) {
            let mut parts = remainder.split_whitespace();

            let Some(month_str) = parts.next() else {
                return line.to_string();
            };
            let Some(day_str) = parts.next() else {
                return line.to_string();
            };
            let Some(year_str) = parts.next() else {
                return line.to_string();
            };

            let Some(month_num) = parse_month_name(month_str) else {
                return line.to_string();
            };

            let Ok(day_num) = day_str.parse::<u32>() else {
                return line.to_string();
            };
            let Ok(year_num) = year_str.parse::<i32>() else {
                return line.to_string();
            };

            let Some(date) = chrono::NaiveDate::from_ymd_opt(year_num, month_num, day_num) else {
                return line.to_string();
            };

            return date.format("%B %-d, %Y").to_string();
        }

        line.to_string()
    }

    fn format_bx(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_bsx(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_at(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_er(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_es(
        &self,
        opening_delimiter: char,
        closing_delimiter: char,
        macro_node: MacroNode,
    ) -> String {
        let c = self.format_inline_macro(macro_node);

        format!("{}{} {}", opening_delimiter, closing_delimiter, c)
    }

    fn format_ev(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_ex(&mut self, macro_node: MacroNode) -> String {
        let mut content = macro_node
            .nodes
            .clone()
            .into_iter()
            .map(|node| self.format_node(node))
            .filter(|s| !s.is_empty())
            .collect::<Vec<String>>()
            .join(", ");

        if macro_node.nodes.is_empty() {
            content = self.formatting_state.first_name.clone().unwrap_or_default();
        }

        if let Some(pos) = content.rfind(",") {
            content.replace_range(pos..(pos + 1), " and");
        }

        let ending = if macro_node.nodes.len() <= 1 {
            "y"
        } else {
            "ies"
        };

        if !content.is_empty() {
            format!("The {content} utilit{ending} exits 0 on success, and >0 if an error occurs.")
        } else {
            String::new()
        }
    }

    fn format_fa(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_fd(&self, directive: &str, arguments: &[String]) -> String {
        format!(
            "{directive} {}",
            arguments.join(&self.formatting_state.spacing)
        )
    }

    fn format_fd_synopsis(&self, directive: &str, arguments: &[String]) -> String {
        format!("{}\n", self.format_fd(directive, arguments))
    }

    fn format_fl(&mut self, macro_node: MacroNode) -> String {
        if macro_node.nodes.is_empty() {
            return "-\\[nsmacroescape]".to_string();
        }

        let mut result = String::new();
        let mut prev_was_open = false;
        let mut is_first_node = true;

        for node in macro_node.nodes {
            match node {
                Element::Text(text) => match text.as_str() {
                    "(" | "[" => {
                        result.push_str(&text);
                        prev_was_open = true;
                    }
                    ")" | "]" | "." | "," | ":" | ";" | "!" | "?" => {
                        result.push_str(&text);
                        prev_was_open = false;
                    }
                    _ => {
                        let fmtd = self.format_text_node(&text);
                        let fmtd = match is_first_char_alnum(&fmtd) {
                            true => format!("-{}", fmtd),
                            false => fmtd,
                        };

                        match prev_was_open {
                            true => result.push_str(&fmtd),
                            false => {
                                let offset = if is_first_node {
                                    ""
                                } else {
                                    self.formatting_state.spacing.as_str()
                                };
                                result.push_str(&format!("{}{}", offset, fmtd));
                            }
                        }
                        prev_was_open = false;
                    }
                },
                _ => unreachable!("macro can't contain macro node or EOI!"),
            }

            if is_first_node {
                is_first_node = false;
            }
        }

        result
    }

    fn format_fn(&mut self, funcname: &str, macro_node: MacroNode) -> String {
        let mut result = format!("{funcname}(");
        let mut iter = macro_node.nodes.iter();

        if let Some(node) = iter.next() {
            match node {
                Element::Text(arg) => match arg.as_str() {
                    "(" | "[" | ")" | "]" | "." | "," | ":" | ";" | "!" | "?" => {
                        let c = iter
                            .map(|n| self.format_node(n.clone()))
                            .collect::<String>();
                        return format!("{}){} {}", result, arg, c);
                    }
                    _ => {
                        let mut prev_was_open = false;
                        let mut is_first_node = true;

                        for node in macro_node.nodes {
                            match node {
                                Element::Text(text) => match text.as_str() {
                                    "(" | "[" => {
                                        result.push_str(&text);
                                        prev_was_open = true;
                                    }
                                    ")" | "]" | "." | "," | ":" | ";" | "!" | "?" => {
                                        result.push_str(&text);
                                        prev_was_open = false;
                                    }
                                    _ => {
                                        match prev_was_open {
                                            true => result.push_str(&self.format_text_node(&text)),
                                            false => {
                                                let offset = if is_first_node {
                                                    ""
                                                } else {
                                                    self.formatting_state.spacing.as_str()
                                                };
                                                let formatted_node = format!(
                                                    "{}{},",
                                                    offset,
                                                    self.format_text_node(&text)
                                                );
                                                result.push_str(&formatted_node);
                                            }
                                        }
                                        prev_was_open = false;
                                    }
                                },
                                _ => unreachable!("macro can't contain macro node or EOI!"),
                            }

                            if is_first_node {
                                is_first_node = false;
                            }
                        }

                        if result.ends_with(",") {
                            result.pop();
                        }

                        result.push(')');

                        return result;
                    }
                },
                _ => unreachable!(),
            }
        };

        let c = iter
            .map(|n| self.format_node(n.clone()))
            .collect::<String>();
        format!("{}){}", result, c.trim())
    }

    fn format_fn_synopsis(&mut self, funcname: &str, macro_node: MacroNode) -> String {
        format!("{};\n", &self.format_fn(funcname, macro_node))
    }

    fn format_fr(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_ft(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_ft_synopsis(&mut self, macro_node: MacroNode) -> String {
        let content = self.format_inline_macro(macro_node);

        format!("\n{}\n", content)
    }

    fn format_fx(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_hf(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_ic(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_in(&self, filename: &str, macro_node: MacroNode) -> String {
        let mut result = if is_first_char_delimiter(filename) {
            let mut filename = filename.to_string();
            let del = filename.remove(0);
            format!("{}<{}>", del, filename)
        } else {
            format!("<{}>", filename)
        };

        if let Some(node) = macro_node.nodes.into_iter().next() {
            match node {
                Element::Text(close_del) => result.push_str(close_del.as_str()),
                _ => unreachable!(),
            }
        }

        result
    }

    fn format_in_synopsis(
        &self,
        filename: &str,
        macro_node: MacroNode,
        prev_node: &Macro,
    ) -> String {
        let in_formatted = self.format_in(filename, macro_node);
        let start = match prev_node {
            Macro::Fn { .. } => "\n#include".to_string(),
            _ => "#include".to_string(),
        };

        format!("{} {}\n", start, in_formatted)
    }

    fn format_lb(&self, lib_name: &str, macro_node: MacroNode) -> String {
        let mut result = String::new();
        let mut iter = macro_node.nodes.into_iter();

        if let Some(node) = iter.next() {
            match node {
                Element::Text(open_del) => result.push_str(open_del.as_str()),
                _ => unreachable!(),
            }
        }

        result.push_str(&format!("library “{lib_name}”"));

        if let Some(node) = iter.next() {
            match node {
                Element::Text(close_del) => result.push_str(close_del.as_str()),
                _ => unreachable!(),
            }
        }

        result
    }

    fn format_li(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_lk(&mut self, uri: &str, macro_node: MacroNode) -> String {
        let content = macro_node
            .nodes
            .clone()
            .into_iter()
            .map(|node| self.format_node(node))
            .collect::<Vec<String>>()
            .join(&self.formatting_state.spacing);

        format!("{content}: {uri}")
    }

    fn format_lp(&self) -> String {
        "\n\n".to_string()
    }

    fn format_ms(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_mt(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_no(&mut self, macro_node: MacroNode) -> String {
        // self.formatting_state.suppress_space = false;
        self.format_inline_macro(macro_node)
    }

    fn format_ns(&mut self, macro_node: MacroNode) -> String {
        let content = self.format_inline_macro(macro_node);

        format!("\\[nsmacroescape]{}", content)
    }

    fn format_nx(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_os(&mut self, macro_node: MacroNode) -> String {
        let content = macro_node
            .nodes
            .into_iter()
            .map(|node| self.format_node(node))
            .collect::<Vec<String>>()
            .join(&self.formatting_state.spacing);

        if !content.is_empty() {
            self.formatting_state.footer_text = Some(content);
        }
        String::new()
    }

    fn format_ox(&self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_pa(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_pf(&mut self, prefix: &str, macro_node: MacroNode) -> String {
        format!(
            "{}\\[pfmacroescape]{}",
            prefix,
            &self.format_inline_macro(macro_node)
        )
    }

    fn format_pp(&self, _macro_node: MacroNode) -> String {
        "\n\n".to_string()
    }

    fn format_rv(&mut self, macro_node: MacroNode) -> String {
        let mut content = macro_node
            .nodes
            .clone()
            .into_iter()
            .take(macro_node.nodes.len().saturating_sub(1))
            .map(|node| self.format_node(node))
            .filter(|s| !s.is_empty())
            .collect::<Vec<String>>()
            .join("(), ");

        if macro_node.nodes.is_empty() {
            content = self.formatting_state.first_name.clone().unwrap_or_default();
        } else if let Some(formatted_node) = macro_node.nodes.iter().last() {
            let formatted_node = self.format_node(formatted_node.clone());
            if macro_node.nodes.len() == 1 {
                content = format!("{formatted_node}()");
            } else {
                content.push_str(&format!("(), and {formatted_node}()"));
            }
        }

        let ending_1 = if macro_node.nodes.len() <= 1 { "" } else { "s" };

        let ending_2 = if macro_node.nodes.len() <= 1 { "s" } else { "" };

        format!("The {content} function{ending_1} return{ending_2} the value 0 if successful; otherwise the value -1 is returned and the global variable errno is set to indicate the error.")
    }

    fn format_sm(&mut self, sm_mode: Option<SmMode>, macro_node: MacroNode) -> String {
        self.formatting_state.spacing = match sm_mode {
            Some(SmMode::On) => " ".to_string(),
            Some(SmMode::Off) => "".to_string(),
            None => match self.formatting_state.spacing.as_str() {
                "" => "".to_string(),
                " " => "".to_string(),
                _ => " ".to_string(),
            },
        };

        let c = self.format_inline_macro(macro_node);

        format!("{}{}", c, self.formatting_state.spacing)
    }

    fn format_st(&self, st_type: StType, macro_node: MacroNode) -> String {
        let content = self.format_inline_macro(macro_node);

        if is_first_char_delimiter(&content) {
            return format!("{}{}", st_type, content);
        }

        format!("{} {}", st_type, content)
    }

    fn format_sx(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_sy(&mut self, macro_node: MacroNode) -> String {
        // let line = self.format_inline_macro(macro_node);

        // if self.supports_bold() {
        //     format!("\x1b[1m{line}\x1b[0m")
        // } else {
        //     line
        // }
        self.format_inline_macro(macro_node)
    }

    fn format_tg(&self, _term: Option<String>) -> String {
        String::new()
    }

    fn format_tn(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_ud(&self) -> String {
        "currently under development.".to_string()
    }

    fn format_ux(&self, macro_node: MacroNode) -> String {
        let content = self.format_inline_macro(macro_node);

        if is_first_char_delimiter(&content) {
            return format!("UNIX{content}");
        }

        format!("UNIX {content}")
    }

    fn format_va(&mut self, macro_node: MacroNode) -> String {
        self.format_inline_macro(macro_node)
    }

    fn format_xr(&self, name: &str, section: &str, macro_node: MacroNode) -> String {
        let content = self.format_inline_macro(macro_node);

        if is_first_char_delimiter(&content) {
            return format!("{name}({section}){content}");
        }

        format!("{}({}) {}", name, section, content.trim())
    }
}

/// Check if first char of [`s`] string is ASCII digit or letter
fn is_first_char_alnum(s: &str) -> bool {
    s.chars()
        .next()
        .map(|c| c.is_ascii_alphanumeric())
        .unwrap_or(false)
}

fn is_first_char_delimiter(s: &str) -> bool {
    s.chars()
        .next()
        .map(|c| matches!(c, '(' | '[' | ')' | ']' | '.' | ',' | ':' | ';' | '!' | '?'))
        .unwrap_or(false)
}

fn is_first_word_delimiter(s: &str) -> bool {
    s.split_whitespace()
        .next()
        .map(|c| matches!(c, "(" | "[" | ")" | "]" | "." | "," | ":" | ";" | "!" | "?"))
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use crate::{man_util::formatter::MdocDocument, FormattingSettings, MdocFormatter, MdocParser};

    /// Test settings
    const FORMATTING_SETTINGS: FormattingSettings = FormattingSettings {
        width: 78,
        indent: 5,
    };

    /// Parse [`input`] into AST
    fn get_ast(input: &str) -> MdocDocument {
        MdocParser::parse_mdoc(input).unwrap()
    }

    /// Universal function for all tests
    pub fn test_formatting(input: &str, output: &str) {
        let ast = get_ast(input);
        let mut formatter = MdocFormatter::new(FORMATTING_SETTINGS);
        let result = String::from_utf8(formatter.format_mdoc(ast)).unwrap();
        println!(
            "Formatted document:\nTarget:\n{}\n{}\nReal:\n{}\n",
            output,
            vec!['-'; formatter.formatting_settings.width]
                .iter()
                .collect::<String>(),
            result
        );
        assert_eq!(output, result);
    }

    mod special_chars {
        use crate::man_util::formatter::tests::test_formatting;

        #[test]
        fn spaces() {
            let input = r".Dd January 1, 1970
.Os footer text
\~\0\|\^\&\)\%"; //not used: "\ ", "\:"
            let output = r"UNTITLED                             LOCAL                            UNTITLED

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn lines() {
            let input = r".Dd January 1, 1970
.Os footer text
\(ba \(br \(ul \(ru \(rn \(bb \(sl \(rs";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

| │ _ _ ‾ ¦ / \

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn text_markers() {
            let input = r".Dd January 1, 1970
.Os footer text
\(ci \(bu \(dd \(dg \(lz \(sq \(ps \(sc \(lh \(rh \(at \(sh \(CR \(OK \(CL \(SP \(HE \(DI";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

○ • ‡ † ◊ □ ¶ § ☜ ☞ @ # ↵ ✓ ♣ ♠ ♥ ♦

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn legal_symbols() {
            let input = r".Dd January 1, 1970
.Os footer text
\(co \(rg \(tm";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

© ® ™

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn punctuation() {
            let input = r".Dd January 1, 1970
.Os footer text
\(em \(en \(hy \e \(r! \(r?";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

— – ‐ \ ¡ ¿

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn quotes() {
            let input = r".Dd January 1, 1970
.Os footer text
\(Bq \(bq \(lq \(rq \(oq \(cq \(aq \(dq \(Fo \(Fc \(fo \(fc";
            let output =
                "UNTITLED                             LOCAL                            UNTITLED

„ ‚ “ ” ‘ ’ ' \" « » ‹ ›

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn brackets() {
            let input = r".Dd January 1, 1970
.Os footer text
\(lB \(rB \(lC \(rC \(la \(ra \(bv \[braceex] \[bracketlefttp] \[bracketleftbt]
\[bracketleftex] \[bracketrighttp] \[bracketrightbt] \[bracketrightex]
\(lt \[bracelefttp] \(lk \[braceleftmid] \(lb \[braceleftbt] \[braceleftex]
\(rt \[bracerighttp] \(rk \[bracerightmid] \(rb \[bracerightbt] \[bracerightex]
\[parenlefttp] \[parenleftbt] \[parenleftex] \[parenrighttp] \[parenrightbt] \[parenrightex]
";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

[ ] { } ⟨ ⟩ ⎪ ⎪ ⎡ ⎣ ⎢ ⎤ ⎦ ⎥ ⎧ ⎧ ⎨ ⎨ ⎩ ⎩ ⎪ ⎫ ⎫ ⎬ ⎬ ⎭ ⎭ ⎪ ⎛ ⎝ ⎜ ⎞ ⎠ ⎟

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn arrows() {
            let input = r".Dd January 1, 1970
.Os footer text
\(<- \(-> \(<> \(da \(ua \(va \(lA \(rA \(hA \(uA \(dA \(vA \(an";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

← → ↔ ↓ ↑ ↕ ⇐ ⇒ ⇔ ⇑ ⇓ ⇕ ⎯

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn logical() {
            let input = r".Dd January 1, 1970
.Os footer text
\(AN \(OR \[tno] \(no \(te \(fa \(st \(tf \(3d \(or";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

∧ ∨ ¬ ¬ ∃ ∀ ∋ ∴ ∴ |

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn mathematical() {
            let input = r".Dd January 1, 1970
.Os footer text
\- \(mi \+ \(pl \(-+ \[t+-] \(+- \(pc \[tmu]
\(mu \(c* \(c+ \[tdi] \(di \(f/ \(** \(<= \(>= \(<< \(>> \(eq \(!= \(==
\(ne \(ap \(|= \(=~ \(~~ \(~= \(pt \(es \(mo \(nm \(sb \(nb \(sp
\(nc \(ib \(ip \(ca \(cu \(/_ \(pp \(is \[integral] \[sum] \[product]
\[coproduct] \(gr \(sr \[sqrt] \(lc \(rc \(lf \(rf \(if \(Ah \(Im \(Re
\(wp \(pd \(-h \[hbar] \(12 \(14 \(34 \(18 \(38 \(58 \(78 \(S1 \(S2 \(S3
";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

- − + + ∓ ± ± · × × ⊗ ⊕ ÷ ÷ ⁄ ∗ ≤ ≥ ≪ ≫ = ≠ ≡ ≢ ∼ ≃ ≅ ≈ ≈ ∝ ∅ ∈ ∉ ⊂ ⊄ ⊃ ⊅ ⊆ ⊇
∩ ∪ ∠ ⊥ ∫ ∫ ∑ ∏ ∐ ∇ √ √ ⌈ ⌉ ⌊ ⌋ ∞ ℵ ℑ ℜ ℘ ∂ ℏ ℏ ½ ¼ ¾ ⅛ ⅜ ⅝ ⅞ ¹ ² ³

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn ligatures() {
            let input = r".Dd January 1, 1970
.Os footer text
\(ff \(fi \(fl \(Fi \(Fl \(AE \(ae \(OE \(oe \(ss \(IJ \(ij";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

ﬀ ﬁ ﬂ ﬃ ﬄ Æ æ Œ œ ß Ĳ ĳ

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn accents() {
            let input = ".Dd January 1, 1970
.Os footer text
\\(a- \\(a. \\(a^ \\(aa \\\' \\(ga \\` \\(ab \\(ac \\(ad \\(ah \\(ao \\(a~ \\(ho \\(ha \\(ti";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

¯ ˙ ^ ´ ´ ` ` ˘ ¸ ¨ ˇ ˚ ~ ˛ ^ ~

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn accented_letters() {
            let input = r".Dd January 1, 1970
.Os footer text
\('A \('E \('I \('O \('U \('Y \('a \('e
\('i \('o \('u \('y \(`A \(`E \(`I \(`O \(`U \(`a \(`e \(`i \(`o \(`u
\(~A \(~N \(~O \(~a \(~n \(~o \(:A \(:E \(:I \(:O \(:U \(:a \(:e \(:i
\(:o \(:u \(:y \(^A \(^E \(^I \(^O \(^U \(^a \(^e \(^i \(^o \(^u \(,C
\(,c \(/L \(/l \(/O \(/o \(oA \(oa
";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

Á É Í Ó Ú Ý á é í ó ú ý À È Ì Ò Ù à è ì ò ù Ã Ñ Õ ã ñ õ Ä Ë Ï Ö Ü ä ë ï ö ü ÿ
Â Ê Î Ô Û â ê î ô û Ç ç Ł ł Ø ø Å å

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn special_letters() {
            let input = r".Dd January 1, 1970
.Os footer text
\(-D \(Sd \(TP \(Tp \(.i \(.j";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

Ð ð Þ þ ı ȷ

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn currency() {
            let input = r".Dd January 1, 1970
.Os footer text
\(Do \(ct \(Eu \(eu \(Ye \(Po \(Cs \(Fn";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

$ ¢ € € ¥ £ ¤ ƒ

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn units() {
            let input = r".Dd January 1, 1970
.Os footer text
\(de \(%0 \(fm \(sd \(mc \(Of \(Om";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

° ‰ ′ ″ µ ª º

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn greek_leters() {
            let input = r".Dd January 1, 1970
.Os footer text
\(*A \(*B \(*G \(*D \(*E \(*Z
\(*Y \(*H \(*I \(*K \(*L \(*M \(*N \(*C \(*O \(*P \(*R \(*S
\(*T \(*U \(*F \(*X \(*Q \(*W \(*a \(*b \(*g \(*d \(*e \(*z
\(*y \(*h \(*i \(*k \(*l \(*m \(*n \(*c \(*o \(*p \(*r \(*s
\(*t \(*u \(*f \(*x \(*q \(*w \(+h \(+f \(+p \(+e \(ts
";
            let output = r"UNTITLED                             LOCAL                            UNTITLED

Α Β Γ Δ Ε Ζ Η Θ Ι Κ Λ Μ Ν Ξ Ο Π Ρ Σ Τ Υ Φ Χ Ψ Ω α β γ δ ε ζ η θ ι κ λ μ ν ξ ο
π ρ σ τ υ ϕ χ ψ ω ϑ φ ϖ ϵ ς

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn predefined_strings() {
            let input = r".Dd January 1, 1970
.Os footer text
\*(Ba \*(Ne \*(Ge \*(Le \*(Gt \*(Lt \*(Pm \*(If \*(Pi \*(Na \*(Am \*R \*(Tm \*q \*(Rq \*(Lq \*(lp \*(rp \*(lq \*(rq \*(ua \*(va \*(<= \*(>= \*(aa \*(ga \*(Px \*(Ai";
            let output =
                "UNTITLED                             LOCAL                            UNTITLED

| ≠ ≥ ≤ > < ± infinity pi NaN & ® (Tm) \" ” “ ( ) “ ” ↑ ↕ ≤ ≥ ´ ` POSIX ANSI

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn unicode() {
            let input = r".Dd January 1, 1970
.Os footer text
\[u0100] \C'u01230' \[u025600]";
            let output =
                "UNTITLED                             LOCAL                            UNTITLED

Ā ሰ 𥘀

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn numbered() {
            let input = r".Dd January 1, 1970
.Os footer text
\N'34' \[char43]";
            let output =
                "UNTITLED                             LOCAL                            UNTITLED

\" +

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }
    }

    mod full_explicit {
        use crate::man_util::formatter::tests::test_formatting;

        mod bd {
            use crate::man_util::formatter::tests::test_formatting;

            #[test]
            fn bd_filled() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bd -filled -offset indent
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
.Ed";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua.
      tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
      veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
      commodo consequat.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bd_unfilled() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bd -unfilled -offset indent
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
.Ed";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua.
      Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
      ut aliquip ex ea commodo consequat.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bd_centered() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bd -centered -offset indent
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
.Ed";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
                tempor incididunt ut labore et dolore magna aliqua.
        tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
      veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
                                 commodo consequat.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bd_offset_right() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bd -filled -offset right
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
.Ed";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

       Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
                           tempor incididunt ut labore et dolore magna aliqua.
          tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
       veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
                                                            commodo consequat.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bd_compact() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bd -literal -offset indent -compact
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
.Ed";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua.
      Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
      ut aliquip ex ea commodo consequat.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bd_nested_blocks() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bd -unfilled -offset indent
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
.Ed
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>
.Sh DESCRIPTION
.Ss SUBSECTION
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bd -unfilled -offset indent
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
.Bd -unfilled -offset indent
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
.Bd -unfilled -offset indent
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
.Bd -unfilled -offset indent
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
.Ed
.Ed
.Ed
.Ed
Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Adssdf sdfmsdpf sdfm sdfmsdpf <alpha>

      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua.
      Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
      ut aliquip ex ea commodo consequat.

Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg
g wefwefwer werwe rwe r wer <alpha>

DESCRIPTION

   SUBSECTION

     Adssdf sdfmsdpf  sdfm sdfmsdpf <alpha> 

           Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
           eiusmod tempor incididunt ut labore et dolore magna aliqua.
           Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
           nisi ut aliquip ex ea commodo consequat.

                 Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed
                 do eiusmod tempor incididunt ut labore et dolore magna
                 aliqua.
                 Ut enim ad minim veniam, quis nostrud exercitation ullamco
                 laboris nisi ut aliquip ex ea commodo consequat.

                       Lorem ipsum dolor sit amet, consectetur adipiscing
                       elit, sed do eiusmod tempor incididunt ut labore et
                       dolore magna aliqua.
                       Ut enim ad minim veniam, quis nostrud exercitation
                       ullamco laboris nisi ut aliquip ex ea commodo
                       consequat.

                             Lorem ipsum dolor sit amet, consectetur
                             adipiscing elit, sed do eiusmod tempor incididunt
                             ut labore et dolore magna aliqua.
                             Ut enim ad minim veniam, quis nostrud
                             exercitation ullamco laboris nisi ut aliquip ex
                             ea commodo consequat.

     Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg
     dfg g wefwefwer werwe rwe r wer <alpha>

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }
        }

        #[test]
        fn bf() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bf -emphasis
Line 1
Line 2
.Ef";
            let output =
                "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Line 1 Line 2

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn bf_macro() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bf Em
Line 1
Line 2
.Ef";
            let output =
                "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Line 1 Line 2

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn bk() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bk -words
Line 1
Line 2
.Ek";
            let output =
                "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Line 1 Line 2

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        mod bl {
            use crate::man_util::formatter::tests::test_formatting;

            #[test]
            fn bl_bullet() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bl -bullet -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

•       Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
        eiusmod tempor incididunt ut labore et dolore magna aliqua.
•       Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
        nisi ut aliquip ex ea commodo consequat.
•       Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
        dolore eu fugiat nulla pariatur.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_column() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1  
.Os footer text
.Bl -column -width 8 -compact \"long  column1\" \"long  column2\" \"long  column3\"
.It Cell 1 Ta Cell 2 Ta Cell 3
Line 1
.It Cell 4 Ta Cell 5 Ta Cell 6
Line 2
.It Cell 7 Ta Cell 8 Ta Cell 9
Line 3
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Cell 1         Cell 2         Cell 3 Line 1
Cell 4         Cell 5         Cell 6 Line 2
Cell 7         Cell 8         Cell 9 Line 3

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_column_long_content() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bl -column -width 8 -compact \"very big super long column1\" \"very big super long column2\" \"very big super long column3\"
.It AAAAAA AAAAAAAAAAAA AAAAA Ta BBBBBB BBBBBBBBB BBBBBB Ta CCCCCC CCCCCCCCCC CCCCCCC
Line 1
.It DDDDDD DDDDDDDDDDDD DDDDD Ta EEEEEE EEEEEEEEE EEEEEE Ta FFFFFF FFFFFFFFFF FFFFFFF
Line 2
.It RRRRRR RRRRRRRRRRRR RRRRR Ta VVVVVV VVVVVVVVV VVVVVV Ta WWWWWW WWWWWWWWWW WWWWWWW
Line 3
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

AAAAAA AAAAAAAAAAAA AAAAA
        BBBBBB BBBBBBBBB BBBBBB
                CCCCCC CCCCCCCCCC CCCCCCC Line 1
DDDDDD DDDDDDDDDDDD DDDDD
        EEEEEE EEEEEEEEE EEEEEE
                FFFFFF FFFFFFFFFF FFFFFFF Line 2
RRRRRR RRRRRRRRRRRR RRRRR
        VVVVVV VVVVVVVVV VVVVVV
                WWWWWW WWWWWWWWWW WWWWWWW Line 3

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_dash() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bl -dash -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

-       Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
        eiusmod tempor incididunt ut labore et dolore magna aliqua.
-       Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
        nisi ut aliquip ex ea commodo consequat.
-       Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
        dolore eu fugiat nulla pariatur.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_diag() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bl -diag -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

head1  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
eiusmod tempor incididunt ut labore et dolore magna aliqua.
head2  Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
nisi ut aliquip ex ea commodo consequat.
head3  Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
dolore eu fugiat nulla pariatur.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_enum() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bl -enum -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

1.      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
        eiusmod tempor incididunt ut labore et dolore magna aliqua.
2.      Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
        nisi ut aliquip ex ea commodo consequat.
3.      Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
        dolore eu fugiat nulla pariatur.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_item() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bl -item -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua.
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
eu fugiat nulla pariatur.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_hang() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bl -hang -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

head1   Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
        eiusmod tempor incididunt ut labore et dolore magna aliqua.
head2   Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
        nisi ut aliquip ex ea commodo consequat.
head3   Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
        dolore eu fugiat nulla pariatur.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_inset() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bl -inset -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

head1 Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua.
head2 Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
ut aliquip ex ea commodo consequat.
head3 Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
dolore eu fugiat nulla pariatur.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_ohang() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bl -ohang -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua.
head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
aliquip ex ea commodo consequat.
head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
eu fugiat nulla pariatur.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_tag() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bl -tag -width 12 -compact
.It head1 
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

head1       Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
            eiusmod tempor incididunt ut labore et dolore magna aliqua.
head2       Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
            nisi ut aliquip ex ea commodo consequat.
head3       Duis aute irure dolor in reprehenderit in voluptate velit esse
            cillum dolore eu fugiat nulla pariatur.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_hang_long_head() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bl -hang -width 8 -compact
.It Item head title1 
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It Item head title2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It Item head title3 
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Item head title1 Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed
        do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Item head title2 Ut enim ad minim veniam, quis nostrud exercitation ullamco
        laboris nisi ut aliquip ex ea commodo consequat.
Item head title3 Duis aute irure dolor in reprehenderit in voluptate velit
        esse cillum dolore eu fugiat nulla pariatur.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_inset_long_head() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bl -inset -width 8 -compact
.It Item head title1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It Item head title2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It Item head title3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Item head title1 Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed
do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Item head title2 Ut enim ad minim veniam, quis nostrud exercitation ullamco
laboris nisi ut aliquip ex ea commodo consequat.
Item head title3 Duis aute irure dolor in reprehenderit in voluptate velit
esse cillum dolore eu fugiat nulla pariatur.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_ohang_long_head() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bl -ohang -width 8 -compact
.It Item head title1 
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It Item head title2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It Item head title3 
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Item head title1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua.
Item head title2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
aliquip ex ea commodo consequat.
Item head title3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
eu fugiat nulla pariatur.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_tag_long_head() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
.Bl -tag -width 8 -compact
.It Item head title1 
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It Item head title2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It Item head title3 
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Item head title1
        Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
        eiusmod tempor incididunt ut labore et dolore magna aliqua.
Item head title2
        Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
        nisi ut aliquip ex ea commodo consequat.
Item head title3
        Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
        dolore eu fugiat nulla pariatur.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_symbol_nested_lists() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -bullet -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>
.Sh DESCRIPTION
.Ss SUBSECTION
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -bullet -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.It head4
.Bl -bullet -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.It head4
.Bl -bullet -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El
.El
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Adssdf sdfmsdpf sdfm sdfmsdpf <alpha>
•       Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
        eiusmod tempor incididunt ut labore et dolore magna aliqua.
•       Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
        nisi ut aliquip ex ea commodo consequat.
•       Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
        dolore eu fugiat nulla pariatur.
Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg
g wefwefwer werwe rwe r wer <alpha>

DESCRIPTION

   SUBSECTION

     Adssdf sdfmsdpf  sdfm sdfmsdpf <alpha> 

     •       Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
             eiusmod tempor incididunt ut labore et dolore magna aliqua.
     •       Ut enim ad minim veniam, quis nostrud exercitation ullamco
             laboris nisi ut aliquip ex ea commodo consequat.
     •       Duis aute irure dolor in reprehenderit in voluptate velit esse
             cillum dolore eu fugiat nulla pariatur.
     •       
             •       Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                     sed do eiusmod tempor incididunt ut labore et dolore
                     magna aliqua.
             •       Ut enim ad minim veniam, quis nostrud exercitation
                     ullamco laboris nisi ut aliquip ex ea commodo consequat.
             •       Duis aute irure dolor in reprehenderit in voluptate velit
                     esse cillum dolore eu fugiat nulla pariatur.
             •       
                     •       Lorem ipsum dolor sit amet, consectetur
                             adipiscing elit, sed do eiusmod tempor incididunt
                             ut labore et dolore magna aliqua.
                     •       Ut enim ad minim veniam, quis nostrud
                             exercitation ullamco laboris nisi ut aliquip ex
                             ea commodo consequat.
                     •       Duis aute irure dolor in reprehenderit in
                             voluptate velit esse cillum dolore eu fugiat
                             nulla pariatur.

     Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg
     dfg g wefwefwer werwe rwe r wer <alpha>

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_item_nested_lists() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -item -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>
.Sh DESCRIPTION
.Ss SUBSECTION
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -item -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.It head4
.Bl -item -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.It head4
.Bl -item -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El
.El
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Adssdf sdfmsdpf sdfm sdfmsdpf <alpha>
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua.
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
eu fugiat nulla pariatur.
Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg
g wefwefwer werwe rwe r wer <alpha>

DESCRIPTION

   SUBSECTION

     Adssdf sdfmsdpf  sdfm sdfmsdpf <alpha> 

     Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
     tempor incididunt ut labore et dolore magna aliqua.
     Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
     ut aliquip ex ea commodo consequat.
     Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
     dolore eu fugiat nulla pariatur.

     Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
     tempor incididunt ut labore et dolore magna aliqua.
     Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
     ut aliquip ex ea commodo consequat.
     Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
     dolore eu fugiat nulla pariatur.

     Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
     tempor incididunt ut labore et dolore magna aliqua.
     Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
     ut aliquip ex ea commodo consequat.
     Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
     dolore eu fugiat nulla pariatur.

     Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg
     dfg g wefwefwer werwe rwe r wer <alpha>

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_ohang_nested_lists() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -ohang -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>
.Sh DESCRIPTION
.Ss SUBSECTION
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -ohang -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.It head4
.Bl -ohang -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.It head4
.Bl -ohang -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El
.El
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Adssdf sdfmsdpf sdfm sdfmsdpf <alpha>
head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua.
head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
aliquip ex ea commodo consequat.
head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
eu fugiat nulla pariatur.
Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg
g wefwefwer werwe rwe r wer <alpha>

DESCRIPTION

   SUBSECTION

     Adssdf sdfmsdpf  sdfm sdfmsdpf <alpha> 

     head1
     Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
     tempor incididunt ut labore et dolore magna aliqua.
     head2
     Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
     ut aliquip ex ea commodo consequat.
     head3
     Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
     dolore eu fugiat nulla pariatur.
     head4

     head1
     Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
     tempor incididunt ut labore et dolore magna aliqua.
     head2
     Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
     ut aliquip ex ea commodo consequat.
     head3
     Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
     dolore eu fugiat nulla pariatur.
     head4

     head1
     Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
     tempor incididunt ut labore et dolore magna aliqua.
     head2
     Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
     ut aliquip ex ea commodo consequat.
     head3
     Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
     dolore eu fugiat nulla pariatur.

     Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg
     dfg g wefwefwer werwe rwe r wer <alpha>

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_inset_nested_lists() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -inset -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>
.Sh DESCRIPTION
.Ss SUBSECTION
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -inset -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.It head4
.Bl -inset -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.It head4
.Bl -inset -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El
.El
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Adssdf sdfmsdpf sdfm sdfmsdpf <alpha>
head1 Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua.
head2 Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
ut aliquip ex ea commodo consequat.
head3 Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
dolore eu fugiat nulla pariatur.
Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg
g wefwefwer werwe rwe r wer <alpha>

DESCRIPTION

   SUBSECTION

     Adssdf sdfmsdpf  sdfm sdfmsdpf <alpha> 

     head1 Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
     eiusmod tempor incididunt ut labore et dolore magna aliqua.
     head2 Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
     nisi ut aliquip ex ea commodo consequat.
     head3 Duis aute irure dolor in reprehenderit in voluptate velit esse
     cillum dolore eu fugiat nulla pariatur.
     head4

     head1 Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
     eiusmod tempor incididunt ut labore et dolore magna aliqua.
     head2 Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
     nisi ut aliquip ex ea commodo consequat.
     head3 Duis aute irure dolor in reprehenderit in voluptate velit esse
     cillum dolore eu fugiat nulla pariatur.
     head4

     head1 Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
     eiusmod tempor incididunt ut labore et dolore magna aliqua.
     head2 Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
     nisi ut aliquip ex ea commodo consequat.
     head3 Duis aute irure dolor in reprehenderit in voluptate velit esse
     cillum dolore eu fugiat nulla pariatur.

     Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg
     dfg g wefwefwer werwe rwe r wer <alpha>

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_column_nested_lists() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -column -width 8 -compact \"col1_ _ _ _ _ _ col1\" \"col2_ _ _ _ _ _ col2\" \"col3_ _ _ _ _ _ col3\" \"col4_ _ _ _ _ _ col4\"
.It head1 Ta Lorem ipsum dolor sit amet, Ta consectetur adipiscing elit, Ta sed do eiusmod tempor incididunt ut Ta labore et dolore magna aliqua. 
.It head2 Ta Ut enim ad minim veniam, Ta quis nostrud exercitation ullamco Ta laboris nisi ut aliquip ex Ta ea commodo consequat. 
.It head3 Ta Duis aute irure dolor in Ta reprehenderit in voluptate velit Ta esse cillum dolore eu Ta fugiat nulla pariatur. 
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>
.Sh DESCRIPTION
.Ss SUBSECTION
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -column -width 8 -compact col1 col2 col3 col4
.It head1 Ta Lorem ipsum dolor sit amet, Ta consectetur adipiscing elit, Ta sed do eiusmod tempor incididunt ut Ta labore et dolore magna aliqua. 
.It head2 Ta Ut enim ad minim veniam, Ta quis nostrud exercitation ullamco Ta laboris nisi ut aliquip ex Ta ea commodo consequat. 
.It head3 Ta Duis aute irure dolor in Ta reprehenderit in voluptate velit Ta esse cillum dolore eu Ta fugiat nulla pariatur. 
.It head4
.Bl -column -width 8 -compact \"col1_ _ _ _ _ _ col1\" \"col2_ _ _ _ _ _ col2\" \"col3_ _ _ _ _ _ col3\" \"col4_ _ _ _ _ _ col4\"
.It head1 Ta Lorem ipsum dolor sit amet, Ta consectetur adipiscing elit, Ta sed do eiusmod tempor incididunt ut Ta labore et dolore magna aliqua. 
.It head2 Ta Ut enim ad minim veniam, Ta quis nostrud exercitation ullamco Ta laboris nisi ut aliquip ex Ta ea commodo consequat. 
.It head3 Ta Duis aute irure dolor in Ta reprehenderit in voluptate velit Ta esse cillum dolore eu Ta fugiat nulla pariatur. 
.It head4
.Bl -column -width 8 -compact col1 col2 col3 col4
.It head1 Ta Lorem ipsum dolor sit amet, Ta consectetur adipiscing elit, Ta sed do eiusmod tempor incididunt ut Ta labore et dolore magna aliqua. 
.It head2 Ta Ut enim ad minim veniam, Ta quis nostrud exercitation ullamco Ta laboris nisi ut aliquip ex Ta ea commodo consequat. 
.It head3 Ta Duis aute irure dolor in Ta reprehenderit in voluptate velit Ta esse cillum dolore eu Ta fugiat nulla pariatur. 
.El
.El
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Adssdf sdfmsdpf sdfm sdfmsdpf <alpha>
head1
        Lorem ipsum dolor sit amet,
                consectetur adipiscing elit,
                        sed do eiusmod tempor incididunt ut
                                labore et dolore magna aliqua.
head2
        Ut enim ad minim veniam,
                quis nostrud exercitation ullamco
                        laboris nisi ut aliquip ex
                                ea commodo consequat.
head3
        Duis aute irure dolor in
                reprehenderit in voluptate velit
                        esse cillum dolore eu
                                fugiat nulla pariatur.
Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg
g wefwefwer werwe rwe r wer <alpha>

DESCRIPTION

   SUBSECTION

     Adssdf sdfmsdpf  sdfm sdfmsdpf <alpha> 

     head  Lore  cons  sed   labore et dolore magna aliqua.
     1     ipsu  ecte  eius
           dolo  adip  temp
           r     isci  inci
           amet  elit  didu
           ,     ,     nt
                       ut
     head  Ut    nost  labo  ea commodo consequat.
     2     enim  exer  ris
           mini  cita  nisi
           veni  ulla  aliq
           am,   mco   uip
                       ex
     head  Duis  repr  cill  fugiat nulla pariatur.
     3     irur  ehen  dolo
           dolo  deri  re
           r in  volu  eu
                 ptat
                 veli
                 t
     head
     4

     head1
             Lorem ipsum dolor sit amet,
                     consectetur adipiscing elit,
                             sed do eiusmod tempor incididunt ut
                                     labore et dolore magna aliqua.
     head2
             Ut enim ad minim veniam,
                     quis nostrud exercitation ullamco
                             laboris nisi ut aliquip ex
                                     ea commodo consequat.
     head3
             Duis aute irure dolor in
                     reprehenderit in voluptate velit
                             esse cillum dolore eu
                                     fugiat nulla pariatur.
     head4

     head  Lore  cons  sed   labore et dolore magna aliqua.
     1     ipsu  ecte  eius
           dolo  adip  temp
           r     isci  inci
           amet  elit  didu
           ,     ,     nt
                       ut
     head  Ut    nost  labo  ea commodo consequat.
     2     enim  exer  ris
           mini  cita  nisi
           veni  ulla  aliq
           am,   mco   uip
                       ex
     head  Duis  repr  cill  fugiat nulla pariatur.
     3     irur  ehen  dolo
           dolo  deri  re
           r in  volu  eu
                 ptat
                 veli
                 t

     Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg
     dfg g wefwefwer werwe rwe r wer <alpha>

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_tag_nested_lists() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -tag -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>
.Sh DESCRIPTION
.Ss SUBSECTION
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -tag -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.It head4
.Bl -tag -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.It head4
.Bl -tag -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El
.El
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Adssdf sdfmsdpf sdfm sdfmsdpf <alpha>
head1   Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
        eiusmod tempor incididunt ut labore et dolore magna aliqua.
head2   Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
        nisi ut aliquip ex ea commodo consequat.
head3   Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
        dolore eu fugiat nulla pariatur.
Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg
g wefwefwer werwe rwe r wer <alpha>

DESCRIPTION

   SUBSECTION

     Adssdf sdfmsdpf  sdfm sdfmsdpf <alpha> 

     head1   Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
             eiusmod tempor incididunt ut labore et dolore magna aliqua.
     head2   Ut enim ad minim veniam, quis nostrud exercitation ullamco
             laboris nisi ut aliquip ex ea commodo consequat.
     head3   Duis aute irure dolor in reprehenderit in voluptate velit esse
             cillum dolore eu fugiat nulla pariatur.
     head4   
             head1   Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                     sed do eiusmod tempor incididunt ut labore et dolore
                     magna aliqua.
             head2   Ut enim ad minim veniam, quis nostrud exercitation
                     ullamco laboris nisi ut aliquip ex ea commodo consequat.
             head3   Duis aute irure dolor in reprehenderit in voluptate velit
                     esse cillum dolore eu fugiat nulla pariatur.
             head4   
                     head1   Lorem ipsum dolor sit amet, consectetur
                             adipiscing elit, sed do eiusmod tempor incididunt
                             ut labore et dolore magna aliqua.
                     head2   Ut enim ad minim veniam, quis nostrud
                             exercitation ullamco laboris nisi ut aliquip ex
                             ea commodo consequat.
                     head3   Duis aute irure dolor in reprehenderit in
                             voluptate velit esse cillum dolore eu fugiat
                             nulla pariatur.

     Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg
     dfg g wefwefwer werwe rwe r wer <alpha>

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_hang_nested_lists() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -hang -width 8 -compact
.It Item head title1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It Item head title2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It Item head title3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>
.Sh DESCRIPTION
.Ss SUBSECTION
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -hang -width 8 -compact
.It Item head title1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It Item head title2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It Item head title3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.It Item head title4
.Bl -hang -width 8 -compact
.It Item head title1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It Item head title2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It Item head title3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.It Item head title4
.Bl -hang -width 8 -compact
.It Item head title1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It Item head title2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It Item head title3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El
.El
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Adssdf sdfmsdpf sdfm sdfmsdpf <alpha>
Item head title1 Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed
        do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Item head title2 Ut enim ad minim veniam, quis nostrud exercitation ullamco
        laboris nisi ut aliquip ex ea commodo consequat.
Item head title3 Duis aute irure dolor in reprehenderit in voluptate velit
        esse cillum dolore eu fugiat nulla pariatur.
Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg
g wefwefwer werwe rwe r wer <alpha>

DESCRIPTION

   SUBSECTION

     Adssdf sdfmsdpf  sdfm sdfmsdpf <alpha> 

     Item head title1 Lorem ipsum dolor sit amet, consectetur adipiscing elit,
             sed do eiusmod tempor incididunt ut labore et dolore magna
             aliqua.
     Item head title2 Ut enim ad minim veniam, quis nostrud exercitation
             ullamco laboris nisi ut aliquip ex ea commodo consequat.
     Item head title3 Duis aute irure dolor in reprehenderit in voluptate
             velit esse cillum dolore eu fugiat nulla pariatur.
     Item head title4
             Item head title1 Lorem ipsum dolor sit amet, consectetur
                     adipiscing elit, sed do eiusmod tempor incididunt ut
                     labore et dolore magna aliqua.
             Item head title2 Ut enim ad minim veniam, quis nostrud
                     exercitation ullamco laboris nisi ut aliquip ex ea
                     commodo consequat.
             Item head title3 Duis aute irure dolor in reprehenderit in
                     voluptate velit esse cillum dolore eu fugiat nulla
                     pariatur.
             Item head title4
                     Item head title1 Lorem ipsum dolor sit amet, consectetur
                             adipiscing elit, sed do eiusmod tempor incididunt
                             ut labore et dolore magna aliqua.
                     Item head title2 Ut enim ad minim veniam, quis nostrud
                             exercitation ullamco laboris nisi ut aliquip ex
                             ea commodo consequat.
                     Item head title3 Duis aute irure dolor in reprehenderit
                             in voluptate velit esse cillum dolore eu fugiat
                             nulla pariatur.

     Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg
     dfg g wefwefwer werwe rwe r wer <alpha>

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn bl_mixed_nested_lists() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME 1
.Os footer text
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -bullet -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>
.Sh DESCRIPTION
.Ss SUBSECTION
Adssdf sdfmsdpf  sdfm sdfmsdpf
.Ms <alpha>
.Bl -bullet -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.It head4
.Bl -hang -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.It head4
.Bl -tag -width 8 -compact
.It head1
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
.It head2
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
.It head3
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
.El
.El
.El
Adssdf sdfmsdpf  sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg g wefwefwer werwe rwe r wer 
.Ms <alpha>";
                let output =
                    "PROGNAME(1)                 General Commands Manual                PROGNAME(1)

Adssdf sdfmsdpf sdfm sdfmsdpf <alpha>
•       Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
        eiusmod tempor incididunt ut labore et dolore magna aliqua.
•       Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
        nisi ut aliquip ex ea commodo consequat.
•       Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
        dolore eu fugiat nulla pariatur.
Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg dfg
g wefwefwer werwe rwe r wer <alpha>

DESCRIPTION

   SUBSECTION

     Adssdf sdfmsdpf  sdfm sdfmsdpf <alpha> 

     •       Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
             eiusmod tempor incididunt ut labore et dolore magna aliqua.
     •       Ut enim ad minim veniam, quis nostrud exercitation ullamco
             laboris nisi ut aliquip ex ea commodo consequat.
     •       Duis aute irure dolor in reprehenderit in voluptate velit esse
             cillum dolore eu fugiat nulla pariatur.
     •       
             head1   Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                     sed do eiusmod tempor incididunt ut labore et dolore
                     magna aliqua.
             head2   Ut enim ad minim veniam, quis nostrud exercitation
                     ullamco laboris nisi ut aliquip ex ea commodo consequat.
             head3   Duis aute irure dolor in reprehenderit in voluptate velit
                     esse cillum dolore eu fugiat nulla pariatur.
             head4
                     head1   Lorem ipsum dolor sit amet, consectetur
                             adipiscing elit, sed do eiusmod tempor incididunt
                             ut labore et dolore magna aliqua.
                     head2   Ut enim ad minim veniam, quis nostrud
                             exercitation ullamco laboris nisi ut aliquip ex
                             ea commodo consequat.
                     head3   Duis aute irure dolor in reprehenderit in
                             voluptate velit esse cillum dolore eu fugiat
                             nulla pariatur.

     Adssdf sdfmsdpf sdfm sdfmsdpf sgsdgsdg sdfg sdfg sdfg fdsg d gdfg df gdfg
     dfg g wefwefwer werwe rwe r wer <alpha>

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }
        }
    }

    mod full_implicit {
        use crate::man_util::formatter::tests::test_formatting;

        #[test]
        fn it() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Bl -bullet
.It 
Line 1
.It 
Line 2
.El";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

• Line 1

• Line 2

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn nd() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Nd short description of the manual";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

– short description of the manual

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn nm() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Nm command_name";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

 command_name

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn sh() {
            let input = ".Dd $Mdocdate: October 28 2016 $
.Dt REV 1
.Os footer text
.Sh NAME
.Nm rev
.Nd reverse lines of a file
.Sh SYNOPSIS
.Nm rev
.Op Ar
.Sh DESCRIPTION
The
.Nm rev
utility copies the specified files to the standard output, reversing the
order of characters in every line.
If no files are specified, the standard input is read.";
            let output =
                "REV(1)                      General Commands Manual                     REV(1)

NAME
     rev  – reverse lines of a file

SYNOPSIS
     rev [file ...]

DESCRIPTION
     The rev utility copies the specified files to the standard output,
     reversing the order of characters in every line. If no files are
     specified, the standard input is read.

footer text                    October 28, 2016                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn ss() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Ss Options
These are the available options.";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

   Options

     These are the available options. 

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }
    }

    #[test]
    fn ta() {
        let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Bl -column \"A col\" \"B col\"
.It item1 Ta item2
.It item1 Ta item2
.El";
        let output =
            "PROGNAME(section)                   section                  PROGNAME(section)

item1  item2
item1  item2

footer text                     January 1, 1970                    footer text";
        test_formatting(input, output);
    }

    mod inline {
        use crate::man_util::formatter::tests::test_formatting;

        mod rs_submacro {
            use super::*;

            #[test]
            fn a() {
                let input = r".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rs
.%A author name
.Re
.Rs
.%A author name1
.%A author name2
.Re
.Rs
.%A author name1
.%A author name2
.%A author name3
.Re
.Rs
.%A ( author ) name1
.%A author , name2
.%A author name3 !
.Re";
                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

author name. author name1 and author name2. author name1, author name2, and
author name3. (author) name1, author, name2, and author name3!.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn b() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rs
.%B book title
.Re
.Rs
.%B book title
.%B book title
.Re
.Rs
.%B ( book ) title
.%B book , title
.%B book title !
.Re";
                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

book title. book title, book title. (book) title, book, title, book title!.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn c() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rs
.%C Publication city
.Re
.Rs
.%C Publication city
.%C Publication city
.Re
.Rs
.%C ( Publication ) city
.%C Publication , city
.%C Publication city !
.Re";
                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

Publication city. Publication city, Publication city. (Publication) city,
Publication, city, Publication city!.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn d() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rs
.%D January 1, 1970
.Re
.Rs
.%D January 1 1970
.%D first january 1970
.Re
.Rs
.%D ( March ) 1189
.%D 12 , 1900
.%D 12 of March, 1970 !
.Re";
                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

January 1, 1970. January 1 1970, first january 1970. (March) 1189, 12, 1900,
12 of March, 1970!.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn i() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rs
.%I issuer name
.Re
.Rs
.%I issuer name
.%I issuer name
.Re
.Rs
.%I ( issuer ) name
.%I issuer , name
.%I issuer name !
.Re";
                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

issuer name. issuer name, issuer name. (issuer) name, issuer, name, issuer
name!.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn j() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rs
.%J Journal name
.Re
.Rs
.%J Journal name
.%J Journal name
.Re
.Rs
.%J ( Journal ) name
.%J Journal , name
.%J Journal name !
.Re";
                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

Journal name. Journal name, Journal name. (Journal) name, Journal, name,
Journal name!.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn n() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rs
.%N Issue number
.Re
.Rs
.%N Issue number
.%N Issue number
.Re
.Rs
.%N ( Issue ) number
.%N Issue , number
.%N Issue number !
.Re";
                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

Issue number. Issue number, Issue number. (Issue) number, Issue, number, Issue
number!.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn o() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rs
.%O Optional information
.Re
.Rs
.%O Optional information
.%O Optional information
.Re
.Rs
.%O ( Optional ) information
.%O Optional , information
.%O Optional information !
.Re";
                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

Optional information. Optional information, Optional information. (Optional)
information, Optional, information, Optional information!.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn p() {
                let input = r".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rs
.%P pp. 42\(en47
.Re
.Rs
.%P pp. 42\(en47
.%P p. 42
.Re
.Rs
.%P ( p. 42 ) p. 43
.%P pp. 42 , 47
.%P pp. 42\(en47 !
.Re";
                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

pp. 42–47. pp. 42–47, p. 42. (p. 42) p. 43, pp. 42, 47, pp. 42–47!.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn q() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rs
.%Q Institutional author
.Re
.Rs
.%Q Institutional author
.%Q Institutional author
.Re
.Rs
.%Q ( Institutional ) author
.%Q Institutional , author
.%Q Institutional author !
.Re";
                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

Institutional author. Institutional author, Institutional author.
(Institutional) author, Institutional, author, Institutional author!.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn r() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rs
.%R Technical report
.Re
.Rs
.%R Technical report
.%R Technical report
.Re
.Rs
.%R ( Technical report ) Technical report
.%R Technical report , Technical report
.%R Technical report !
.Re";
                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

Technical report. Technical report, Technical report. (Technical report)
Technical report, Technical report, Technical report, Technical report!.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn t() {
                let input = r".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rs
.%T Article title
.Re
.Rs
.%T Article title
.%T Article title
.Re
.Rs
.%T ( Article title ) Article title
.%T Article title , Article title
.%T Article title !
.Re";
                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

Article title. Article title, Article title. (Article title) Article title,
Article title, Article title, Article title!.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn u() {
                let input = r".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rs
.%U Article title
.Re
.Rs
.%U Article title
.%U Article title
.Re
.Rs
.%U ( Article title ) Article title
.%U Article title , Article title
.%U Article title !
.Re";
                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

Article title. Article title, Article title. (Article title) Article title,
Article title, Article title, Article title!.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }

            #[test]
            fn v() {
                let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rs
.%V Volume number
.Re
.Rs
.%V Volume number
.%V Volume number
.Re
.Rs
.%V ( Volume number ) Volume number
.%V Volume number , Volume number
.%V Volume number !
.Re";
                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

Volume number. Volume number, Volume number. (Volume number) Volume number,
Volume number, Volume number, Volume number!.

footer text                     January 1, 1970                    footer text";
                test_formatting(input, output);
            }
        }

        #[test]
        fn ad() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Ad [0,$]
.Ad 0x00000000
.Ad [ 0,$ ]";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

[0,$] 0x00000000 [0,$]

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn ap() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Ap Text Line";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

'Text Line

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn ar() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Ar
.Ar arg1 , arg2 .";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

file ... arg1, arg2.

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn at() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.At
.At III
.At V.1
.At ( V.1 )
.At ( V.1 ) subnode Ad ( addr )";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

AT&T UNIX AT&T System III UNIX AT&T System V Release 1 UNIX (AT&T System V
Release 1 UNIX) (AT&T System V Release 1 UNIX) subnode (addr)

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn bsx() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Bsx 1.0
.Bsx
.Bsx ( 1.0 )";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

BSD/OS 1.0 BSD/OS (BSD/OS 1.0)

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn bt() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Bt";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

is currently in beta test.

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn bx() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Bx 4.3 Tahoe
.Bx 4.4
.Bx
.Bx ( 4.3 Tahoe )";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

4.3BSD-Tahoe 4.4BSD BSD (4.3BSD-Tahoe)

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn cd() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Cd device le0 at scode?";

            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

device le0 at scode?

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn cm() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Cm file bind";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

file bind

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn db() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Db
";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn dd() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn dt() {
            let input = ".Dd January 1, 1970
.Dt TITLE 7 arch
.Os footer text";
            let output =
                "TITLE(7)            Miscellaneous Information Manual (arch)           TITLE(7)

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn dv() {
            let input = ".Dd January 1, 1970
.Dt TITLE 7 arch
.Os footer text
.Dv NULL
.Dv BUFSIZ
.Dv STDOUT_FILEnmo";
            let output =
                "TITLE(7)            Miscellaneous Information Manual (arch)           TITLE(7)

NULL BUFSIZ STDOUT_FILEnmo

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn dx() {
            let input = ".Dd January 1, 1970
.Dt TITLE 7 arch
.Os footer text
.Dx 2.4.1
.Dx ( 2.4.1 )
";
            let output =
                "TITLE(7)            Miscellaneous Information Manual (arch)           TITLE(7)

DragonFly 2.4.1 (DragonFly 2.4.1)

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn em() {
            let input = ".Dd January 1, 1970
.Dt TITLE 7 arch
.Os footer text
Selected lines are those
.Em not
matching any of the specified patterns.
Some of the functions use a
.Em hold space
to save the pattern space for subsequent retrieval.";
            let output =
                "TITLE(7)            Miscellaneous Information Manual (arch)           TITLE(7)

Selected lines are those not matching any of the specified patterns. Some of
the functions use a hold space to save the pattern space for subsequent
retrieval.

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn er() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Er ERROR ERROR2";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

ERROR ERROR2

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn es() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Es ( )";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

()

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn ev() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Ev DISPLAY";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

DISPLAY

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn ex() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Ex -std grep";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

The grep utility exits 0 on success, and >0 if an error occurs.

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn fa() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Fa funcname Ft const char *";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

funcname const char *

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn fd() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Fd #define sa_handler __sigaction_u.__sa_handler";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

#define sa_handler __sigaction_u.__sa_handler

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn fl() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Fl H | L | P inet";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

-H | -L | -P -inet

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[allow(non_snake_case)]
        #[test]
        fn Fn() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Fn funcname arg arg2 arg3";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

funcname(arg, arg2, arg3)

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn fr() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Fr 32";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

32

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn ft() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Ft int32 void";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

int32 void

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn fx() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Fx 1.0";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

FreeBSD 1.0

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn hf() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Hf file/path file2/path";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

file/path file2/path

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn ic() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Ic :wq";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

:wq

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[allow(non_snake_case)]
        #[test]
        fn In() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.In stdatomic.h";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

<stdatomic.h>

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn lb() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Lb libname";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

library “libname”

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn li() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Li Book Antiqua";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

Book Antiqua

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn lk() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Lk https://bsd.lv The BSD.lv Project";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

The BSD.lv Project: https://bsd.lv

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn ms() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Ms alpha beta";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

alpha beta

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn mt() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Mt abc@gmail.com abc@gmail.com";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

abc@gmail.com abc@gmail.com

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn no() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.No a b c";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

a b c

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn nx() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Nx Version 1.0";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

NetBSD Version 1.0

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn os() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn ot() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Ot functype";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

functype

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn ox() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Ox Version 1.0";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

OpenBSD Version 1.0

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn pa() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Pa name1 name2";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

name1 name2

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn rv() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rv -std f1 f2 Ar value";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

The f1(), f2(), Ar(), and value() functions return the value 0 if successful;
otherwise the value -1 is returned and the global variable errno is set to
indicate the error.

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn rv_std() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Rv -std";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

The function returns the value 0 if successful; otherwise the value -1 is
returned and the global variable errno is set to indicate the error.

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn sm() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Sm off A B C D
.Sm on A B C D";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

ABCD A B C D

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn st() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.St -ansiC word
.St -iso9945-1-96";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

ANSI X3.159-1989 (“ANSI C89”) word ISO/IEC 9945-1:1996 (“POSIX.1”)

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn sx() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Sx MANUAL STRUCTURE";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

MANUAL STRUCTURE

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn sy() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Sy word1 word2";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

word1 word2

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn tn() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Tn word1 word2";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

word1 word2

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn ud() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Ud";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

currently under development.

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn ux() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Ux";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

UNIX

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn va() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Va const char *bar";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

const char *bar

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn xr() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Xr mandoc 1";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

mandoc(1)

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }
    }

    mod partial_implicit {
        use crate::man_util::formatter::tests::test_formatting;

        #[test]
        fn block_empty() {
            let input = r#".Dd January 1, 1970
.Os footer text
.Aq"#;
            let output =
                "UNTITLED                             LOCAL                            UNTITLED

⟨⟩

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn block_single_line() {
            let input = r#".Dd January 1, 1970
.Os footer text
.Aq Ad addr addr Ad addr Ad addr"#;
            let output =
                "UNTITLED                             LOCAL                            UNTITLED

⟨addr addr addr addr⟩

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }
    }

    mod partial_explicit {
        use crate::man_util::formatter::tests::test_formatting;

        #[test]
        fn block_empty() {
            let input = r#".Dd January 1, 1970
.Os footer text
.Ao
.Ac"#;
            let output =
                "UNTITLED                             LOCAL                            UNTITLED

⟨⟩

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn block_single_line() {
            let input = r#".Dd January 1, 1970
.Os footer text
.Ao
.Ad addr addr
.Ad addr 
.Ad addr 
.Ac"#;
            let output =
                "UNTITLED                             LOCAL                            UNTITLED

⟨addr addr addr addr⟩

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn multi_line() {
            let input = r#".Dd January 1, 1970
.Os footer text
.Ao
.Ad addr 
.Ad addr 
.Ad addr 
Text loooooooong line
Text loooooooong line
Text loooooooong line
Text loooooooong line
Text loooooooong line
Text loooooooong line
.Ac"#;
            let output = r#"UNTITLED                             LOCAL                            UNTITLED

⟨addr addr addr⟩ Text loooooooong line Text loooooooong line Text loooooooong
line Text loooooooong line Text loooooooong line Text loooooooong line

footer text                     January 1, 1970                    footer text"#;
            test_formatting(input, output);
        }

        #[test]
        fn block_overlong_line() {
            let input = r#".Dd January 1, 1970
.Os Debian
.Aq Ad addr Ad addr Ad addr Text looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong line"#;
            let output = r#"UNTITLED                             LOCAL                            UNTITLED

⟨addr addr addr Text
looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
line⟩

Debian                          January 1, 1970                         Debian"#;
            test_formatting(input, output);
        }

        #[test]
        fn rs_block() {
            let input = ".Dd January 1, 1970
.Dt TITLE 7 arch
.Os footer text
.Rs
.%A J. E. Hopcroft
.%A J. D. Ullman 
.%B Introduction to Automata Theory, Languages, and Computation
.%I Addison-Wesley
.%C Reading, Massachusetts
.%D 1979
.Re";
            let output =
                "TITLE(7)            Miscellaneous Information Manual (arch)           TITLE(7)

J. E. Hopcroft and J. D. Ullman, Introduction to Automata Theory, Languages,
and Computation, Addison-Wesley, Reading, Massachusetts, 1979.

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }
    }

    #[test]
    fn zero_width() {
        let input = r".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Xr mandoc 1 \&Ns \&( s \&) behaviour
Text Line \&Ns \&( s \&) behaviour";
        let output =
            "PROGNAME(section)                   section                  PROGNAME(section)

mandoc(1) Ns ( s ) behaviour Text Line Ns ( s ) behaviour

footer text                     January 1, 1970                    footer text";
        test_formatting(input, output);
    }

    mod delimiters {
        use super::*;

        #[test]
        fn delimiters_inline_common() {
            fn test(macro_str: &str) {
                let input = vec![
                    format!(".Dd January 1, 1970\n.Dt PROGNAME section\n.Os footer text"),
                    format!(".{} {} text {}", macro_str, "(", ")"),
                    format!(".{} {} text {}", macro_str, "[", "]"),
                    format!(".{} text {}", macro_str, "."),
                    format!(".{} text {}", macro_str, ","),
                    format!(".{} text {}", macro_str, "?"),
                    format!(".{} text {}", macro_str, "!"),
                    format!(".{} text {}", macro_str, ":"),
                    format!(".{} text {}", macro_str, ";"),
                ]
                .join("\n");

                let output =
                    "PROGNAME(section)                   section                  PROGNAME(section)

(text) [text] text. text, text? text! text: text;

footer text                     January 1, 1970                    footer text";

                test_formatting(&input, &output);
            }

            let inline_macros = vec![
                "Ad", "An", "Ar", "Cd", "Cm", "Dv", "Er", "Ev", "Fa", "Fr", "Ft", "Hf", "Ic", "Li",
                "Ms", "Mt", "No", "Ot", "Pa", "Sx", "Tn", "Va",
            ];

            for macro_str in inline_macros {
                println!("Macro: {macro_str}");

                test(macro_str);
            }
        }

        #[test]
        fn delimiters_text_production() {
            fn test(macro_str: &str) {
                let placeholder = match macro_str {
                    "At" => "AT&T UNIX",
                    "Bsx" => "BSD/OS",
                    "Dx" => "DragonFly",
                    "Fx" => "FreeBSD",
                    "Nx" => "NetBSD",
                    "Ox" => "OpenBSD",
                    _ => unreachable!(),
                };

                let input = vec![
                    format!(".Dd January 1, 1970\n.Dt PROGNAME section\n.Os footer text"),
                    format!(".{} {} text {}", macro_str, "(", ")"),
                    format!(".{} {} text {}", macro_str, "[", "]"),
                    format!(".{} text {}", macro_str, "."),
                ]
                .join("\n");

                let output = format!(
                    "PROGNAME(section)                   section                  PROGNAME(section)

({placeholder} text) [{placeholder} text] {placeholder} text.

footer text                     January 1, 1970                    footer text",
                );
                test_formatting(&input, &output);
            }

            let macros = vec!["At", "Bsx", "Ox", "Dx", "Fx", "Nx"];

            for macro_str in macros {
                println!("Macro: {}", macro_str);

                test(macro_str)
            }
        }

        #[test]
        fn delimiters_bx() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Bx ( random )
.Bx random !";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

(randomBSD) randomBSD!

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn delimiters_em() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Em ( random ) text !";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

(random) text!

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn delimiters_fn() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Fn ( random ) text !";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

(random()) text!

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn delimiters_sy() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Sy ( random ) text !";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

(random) text!

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn delimiters_fl() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Fl ( random ) text !";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

(-random) -text!

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn delimiters_in() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.In ( random )";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

(<random>)

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn delimiters_lb() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Lb ( random )";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

(library “random”)

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }

        #[test]
        fn delimiters_vt() {
            let input = ".Dd January 1, 1970
.Dt PROGNAME section
.Os footer text
.Vt ( random ) text !";
            let output =
                "PROGNAME(section)                   section                  PROGNAME(section)

(random) text!

footer text                     January 1, 1970                    footer text";
            test_formatting(input, output);
        }
    }
}

/*
/// Use this mod for testing whole mdoc files.
/// Test results may differ from original mdoc
/// formatter. So this tests will not pass
/// successfully. This is expected behavior
#[cfg(test)]
mod test_mdoc {
    use crate::man_util::formatter::tests::test_formatting;
    use std::process::Command;
    use rstest::rstest;

    #[rstest]
    // Small
    #[case("./test_files/mdoc/rev.1")]
    #[case("./test_files/mdoc/adjfreq.2")]
    #[case("./test_files/mdoc/getgroups.2")]
    #[case("./test_files/mdoc/sigreturn.2")]
    #[case("./test_files/mdoc/size.1")]
    #[case("./test_files/mdoc/fgen.1")]
    #[case("./test_files/mdoc/getrtable.2")]
    #[case("./test_files/mdoc/wall.1")]
    #[case("./test_files/mdoc/getsid.2")]
    #[case("./test_files/mdoc/ypconnect.2")]
    #[case("./test_files/mdoc/closefrom.2")]
    #[case("./test_files/mdoc/moptrace.1")]

    //Other
    #[case("./test_files/mdoc/rlog.1")]
    #[case("./test_files/mdoc/access.2")]
    #[case("./test_files/mdoc/munmap.2")]
    #[case("./test_files/mdoc/ipcs.1")]
    #[case("./test_files/mdoc/atq.1")]
    #[case("./test_files/mdoc/brk.2")]
    #[case("./test_files/mdoc/cal.1")]
    #[case("./test_files/mdoc/minherit.2")]
    #[case("./test_files/mdoc/cat.1")]
    #[case("./test_files/mdoc/file.1")]
    #[case("./test_files/mdoc/mkdir.1")]
    #[case("./test_files/mdoc/getsockname.2")]
    #[case("./test_files/mdoc/mlockall.2")]
    #[case("./test_files/mdoc/cut.1")]

    //without bl
    #[case("./test_files/mdoc/umask.2")]
    #[case("./test_files/mdoc/sched_yield.2")]
    #[case("./test_files/mdoc/sigsuspend.2")]
    #[case("./test_files/mdoc/mopa.out.1")]
    #[case("./test_files/mdoc/fsync.2")]
    #[case("./test_files/mdoc/shar.1")]
    #[case("./test_files/mdoc/sysarch.2")]

    //word as macro
    #[case("./test_files/mdoc/fork.2")]
    #[case("./test_files/mdoc/symlink.2")]
    #[case("./test_files/mdoc/sync.2")]
    #[case("./test_files/mdoc/futex.2")]
    #[case("./test_files/mdoc/reboot.2")]
    #[case("./test_files/mdoc/id.1")]
    #[case("./test_files/mdoc/rename.2")]
    #[case("./test_files/mdoc/cu.1")]
    #[case("./test_files/mdoc/getfh.2")]
    #[case("./test_files/mdoc/ioctl.2")]
    #[case("./test_files/mdoc/dup.2")]
    #[case("./test_files/mdoc/getpeername.2")]
    #[case("./test_files/mdoc/lpq.1")]
    #[case("./test_files/mdoc/nm.1")]
    #[case("./test_files/mdoc/truncate.2")]
    #[case("./test_files/mdoc/chdir.2")]
    #[case("./test_files/mdoc/mkfifo.2")]
    #[case("./test_files/mdoc/quotactl.2")]
    #[case("./test_files/mdoc/send.2")]
    #[case("./test_files/mdoc/getpriority.2")]
    #[case("./test_files/mdoc/select.2")]
    #[case("./test_files/mdoc/w.1")]
    #[case("./test_files/mdoc/chflags.2")]
    #[case("./test_files/mdoc/flock.2")]

    // Bl -column
    #[case("./test_files/mdoc/shutdown.2")]
    #[case("./test_files/mdoc/tmux.1")]
    #[case("./test_files/mdoc/nl.1")]
    #[case("./test_files/mdoc/bc.1")]
    #[case("./test_files/mdoc/mg.1")]
    #[case("./test_files/mdoc/snmp.1")]
    #[case("./test_files/mdoc/rdist.1")]

    //Block 1
    #[case("./test_files/mdoc/chmod.2")]
    #[case("./test_files/mdoc/cvs.1")]
    #[case("./test_files/mdoc/dc.1")]
    #[case("./test_files/mdoc/flex.1")]
    #[case("./test_files/mdoc/getdents.2")]
    #[case("./test_files/mdoc/getitimer.2")]
    #[case("./test_files/mdoc/getrusage.2")]
    #[case("./test_files/mdoc/getsockopt.2")]

    #[case("./test_files/mdoc/gettimeofday.2")]
    #[case("./test_files/mdoc/ktrace.2")]
    #[case("./test_files/mdoc/msgrcv.2")]
    #[case("./test_files/mdoc/msgsnd.2")]
    #[case("./test_files/mdoc/mv.1")]
    #[case("./test_files/mdoc/poll.2")]
    #[case("./test_files/mdoc/profil.2")]
    #[case("./test_files/mdoc/rcs.1")]
    #[case("./test_files/mdoc/read.2")]
    #[case("./test_files/mdoc/rup.1")]
    #[case("./test_files/mdoc/semget.2")]
    #[case("./test_files/mdoc/shmctl.2")]
    #[case("./test_files/mdoc/signify.1")]
    #[case("./test_files/mdoc/statfs.2")]
    #[case("./test_files/mdoc/t11.2")]
    #[case("./test_files/mdoc/talk.1")]
    #[case("./test_files/mdoc/write.2")]

    #[case("./test_files/mdoc/diff.1")]
    #[case("./test_files/mdoc/top.1")]
    #[case("./test_files/mdoc/execve.2")]
    #[case("./test_files/mdoc/open.2")]
    #[case("./test_files/mdoc/scp.1")]
    #[case("./test_files/mdoc/socket.2")]
    #[case("./test_files/mdoc/socketpair.2")]
    #[case("./test_files/mdoc/setuid.2")]
    #[case("./test_files/mdoc/shmget.2")]
    #[case("./test_files/mdoc/sftp.1")]
    #[case("./test_files/mdoc/grep.1")]
    fn format_mdoc_file(#[case] path: &str){
        let input = std::fs::read_to_string(path).unwrap();
        let output = Command::new("mandoc")
            .args(["-T", "locale", path])
            .output()
            .unwrap()
            .stdout;
        let output = String::from_utf8(output).unwrap();
        println!("Current path: {}", path);
        test_formatting(&input, &output);
    }
}
*/
