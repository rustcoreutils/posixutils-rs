mkdir -p tests/write_dir
cd tests/write_dir

echo test >ouput_redirection.txt
cat ouput_redirection.txt
rm ouput_redirection.txt

echo test 1>ouput_redirection.txt
cat ouput_redirection.txt
rm ouput_redirection.txt

echo test >|ouput_redirection.txt
cat ouput_redirection.txt
rm ouput_redirection.txt

echo test 1>|ouput_redirection.txt
cat ouput_redirection.txt
rm ouput_redirection.txt

