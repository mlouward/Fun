## 1. Find webserver

Use gobuster to enumerate directories

```bash
gobuster dir -u previse.htb -w /usr/share/seclists/Discovery/Web-Content/directory-list-2.3-small.txt -x php
```

Find logs.php, intercept with burp (proxy, intercept on, right click and "**do intercept response**").

Change response from **302** to **200 OK**

Access admin panel, create account, login and view php sourcecode.

We find a list of potential users in out.log, config.php and a call to "exec" => potential vulnerability

> config.php

```php
function connectDB(){
    $host = 'localhost';
    $user = 'root';
    $passwd = 'mySQL_p@ssw0rd!:)';
    $db = 'previse';
    $mycon = new mysqli($host, $user, $passwd, $db);
    return $mycon;
}
```
