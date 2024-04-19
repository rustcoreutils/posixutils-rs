use std::ffi::OsString;

pub fn is_printable(c:char)->bool{
    return  c.is_ascii_graphic() || c==' ';
 }
 pub  fn sanitize_filename(name:OsString) -> String{
     return     name.to_string_lossy().chars().map(|c| if is_printable(c){c} else {'?'}).collect();
     }
 
     