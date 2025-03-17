kill -l

trap 'echo received SIGTERM' TERM
kill $$
kill -- $$
kill -TERM $$
kill -TERM -- $$
kill -s TERM $$
kill -s TERM -- $$
kill -0 $$

kill -l 1
kill -l -- 2
kill -l 129