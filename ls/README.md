### rust-ls
An implementation of the unix command ls using Rust.

## Roadmap## Roadmap
| Command   | Coverage                             | Test |
|-----------|--------------------------------------|------|
| ls        | 100%                                 | Yes  |
| ls path   | 100%                                 | Yes  |
| ls -a     | 100%                                 | Yes  |
| ls -A     | 100%                                 | Yes  |
| ls -R     | 100%                                 | Yes  |
| ls -l     | 100%                                 | Yes  |
| ls -h     | 80% (need to exclude if -H and -L)  |      |
| ls -H     | TODO                                 |      |
| ls -L     | TODO                                 |      |
| ls -S     | 100%                                 | Yes  |
| ls -c     | Differs on macOS                     |      |
| ls -d     | 100%                                 |      |
| ls -f     | TODO                                 |      |
| ls -g     | TODO                                 |      |
| ls -i     | 100%                                 | Yes  |
| ls -k     | TODO                                 |      |
| ls -l     | TODO                                 |      |  <!-- Duplicate row, possibly needs correction or removal -->
| ls -m     | 100%                                 | Yes  |
| ls -n     | 100%                                 | Yes  |
| ls -o     | 100%                                 |      |
| ls -p     | 100%                                 |      |
| ls -q     | 100%                                 |      |
| ls -r     | 100%                                 |      |
| ls -s     | 100%                                 |      |
| ls -t     | 100%                                 |      |
| ls -u     | TODO                                 |      |
| ls -x     | TODO                                 |      |
| ls -1     | TODO                                 |      |
```
ls -l                                             
total 384
-rw-r--r--  1 jd  staff  12091 Apr 19 10:44 Cargo.lock
-rw-r--r--  1 jd  staff    276 Apr 19 10:44 Cargo.toml
-rw-r--r--  1 jd  staff    287 Apr 19 10:42 README.md
-rw-r--r--  1 jd  staff  32797 Apr 19 08:26 lsaR.unix
-rw-r--r--  1 jd  staff  86815 Apr 19 08:26 lsaRl.unix
-rw-r--r--  1 jd  staff  22248 Apr 18 22:54 rust.out
lrwxr-xr-x  1 jd  staff      8 Apr 19 09:12 rust.out.symlink -> rust.out
drwxr-xr-x  3 jd  staff     96 Apr 18 18:42 src
drwxr-xr-x@ 5 jd  staff    160 Apr 18 18:42 target
-rw-r--r--  1 jd  staff  22248 Apr 18 22:54 unix.out


./target/debug/rust_ls -l  
total 384
-rw-r--r--  1 jd  staff  12091 Apr 19 10:44 Cargo.lock
-rw-r--r--  1 jd  staff    276 Apr 19 10:44 Cargo.toml
-rw-r--r--  1 jd  staff  32797 Apr 19 08:26 lsaR.unix
-rw-r--r--  1 jd  staff  86815 Apr 19 08:26 lsaRl.unix
-rw-r--r--  1 jd  staff    287 Apr 19 10:42 README.md
-rw-r--r--  1 jd  staff  22248 Apr 18 22:54 rust.out
lrwxr-xr-x  1 jd  staff      8 Apr 19 09:12 rust.out.symlink
drwxr-xr-x  3 jd  staff     96 Apr 18 18:42 src
drwxr-xr-x@ 5 jd  staff    160 Apr 18 18:42 target
-rw-r--r--  1 jd  staff  22248 Apr 18 22:54 unix.out
```
