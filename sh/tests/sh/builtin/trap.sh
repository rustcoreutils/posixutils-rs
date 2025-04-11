trap
trap -p
trap 'a' INT
trap 'b' EXIT 6 15
trap
trap - INT
trap 'c' BUS
trap
trap 0 ABRT 15 BUS
trap
trap -- 'd' TERM ALRM
trap
