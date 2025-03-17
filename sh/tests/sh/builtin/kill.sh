kill -l

trap 'echo received SIGTERM' TERM
kill $$
kill -- $$
kill -TERM $$
kill -TERM -- $$
kill -s TERM $$
kill -s TERM -- $$
kill -0 $$

