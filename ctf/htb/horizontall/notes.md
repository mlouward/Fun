# Horizontall

1. Nmap

result = ports 22 and 80 (nginx).
Added address to /etc/hosts


2. Visit website

Nothing interactive, no buttons.

Tried to run
`gobuster dir -u horizontall.htb -w /usr/share/seclists/Discovery/Web-Content/raft-small-directories.txt` but nothing came up.

Explored page source, found a function called `getReviews` with an url **http://api-prod.horizontall.htb/reviews**

Added **api-prod.horizontall.htb** to `/etc/hosts`

3. Prod API exploration

Ran gobuster again. 3 results: **/admin**, **/users** and **/reviews**.

We can access **/reviews** (3 reviews) and admin portal but need to login.
