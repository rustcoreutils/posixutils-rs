dnl Currently ignored because it seems like GNU m4 treats this incorrectly.
dnl We will need to solve this if it becomes a problem later.
define(`x',`')define(`y',x() hello)y
define(`x',`')define(`y',`x() hello')y