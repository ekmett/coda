#!/usr/bin/env python

import os, random, socket, ssl, sys, time, base64, binascii

protected_vars = vars().keys()

for key, value in os.environ.items():
    if key.lower() not in protected_vars and key.lower() != 'vars':
        vars()[key.lower()] = value

for key, value in os.environ.items():
    if key.startswith('APPVEYOR_'):
        trimmed_key = key[9:].lower()
        if trimmed_key not in vars() and trimmed_key != 'vars':
            if trimmed_key != 'vars':
                vars()[trimmed_key] = value
    if key.startswith('TRAVIS_'):
        trimmed_key = key[7:].lower()
        if trimmed_key not in vars() and trimmed_key != 'vars':
            vars()[trimmed_key] = value

if 'repo_commit' in vars():
    commit = repo_commit

if 'repo_branch' in vars():
    branch = repo_branch

short_commit = commit[0:7]
color_code = "\x03"
color_off = "\x0f"
underline = "\x1f"

if 'TRAVIS' in os.environ:
    project_url = "https://travis-ci.org/{repo_slug}".format(**vars())
    build_url = "{project_url}/builds/{build_id}".format(**vars())
    job_url = "{project_url}/jobs/{job_id}".format(**vars())
    repo_url = "https://github.com/{repo_slug}".format(**vars()).lower()
    commit_url = "{repo_url}/commit/{short_commit}".format(**vars())
    username = "coda-travis"
else:    
    project_url = "{appveyor_url}/project/{account_name}/{project_name}".format(**vars())
    build_url = "{project_url}/build/{build_version}".format(**vars())
    job_url = "{build_url}/job/{job_id}".format(**vars())
    repo_url = "https://{repo_provider}.com/{repo_name}".format(**vars()).lower()
    commit_url = "{repo_url}/commit/{short_commit}".format(**vars())
    username = "{project_name}-appveyor".format(**vars())

messages = []
for msg in ' '.join(sys.argv[1:]).split(','):
    messages.append(msg.format(**vars()).strip())

s = ssl.wrap_socket(socket.socket(socket.AF_INET,socket.SOCK_STREAM))
s.connect((socket.gethostbyname("chat.freenode.net"), 6697))

f = s.makefile()

def send(msg):
    print("> {}".format(msg))
    s.send("{}\r\n".format(msg).encode())

def login(username):
    send("NICK {0}".format(username))
    send("USER {0} * 0 :{0}".format(username))

if "password" in vars():
    send("CAP REQ :sasl")
else:
    login(username)

while f:
    line = f.readline()
    if line == "":
        sys.exit()
    print("< {}".format(line.rstrip()))
    w = line.split()
    if len(w) >= 1 and w[0] == "PING":
        send("PONG {}".format(w[1]))
    elif len(w) >= 4 and w[1] == "CAP" and w[3] == "ACK":
        send("AUTHENTICATE PLAIN")
    elif len(w) >= 2 and w[0] == "AUTHENTICATE" and w[1] == "+":
        print("> AUTHENTICATE <sensitive>")
        s.send("AUTHENTICATE {}\r\n".format(base64.b64encode("{username}\0{username}\0{password}".format(**vars()))).encode())
        send("CAP END")
    elif len(w) >= 2 and w[1] == "903":
        login(username)
    elif len(w) >= 2 and w[1] == "433":
        send("NICK {}{}".format(username, str(random.randint(1,9999))))
    elif len(w) >= 2 and w[1] == "001":
        time.sleep(2)
        for msg in messages:
            send("PRIVMSG ##coda :{}\r\n".format(msg))
        time.sleep(5)
        sys.exit()
