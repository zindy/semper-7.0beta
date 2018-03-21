rm Debug_Log
./configure --enable-debug
make clean
make 2>&1 | tee -a Debug_Log
