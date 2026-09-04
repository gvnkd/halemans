# pwstore-fast makePassword replica (pbkdf1, strength 17) for SQL-only user
# seeding where no Haskell runtime is available (dev seed + sandboxed smoke).
# Format: sha256|17|base64(salt)|base64(hash); the salt bytes fed into pbkdf1
# are the base64 TEXT (pwstore-fast's SaltBS), not the raw salt.
import base64
import hashlib
import os
import sys

def hash_password(password: str) -> str:
    salt_b64 = base64.b64encode(os.urandom(16))
    h = hashlib.sha256(password.encode() + salt_b64).digest()
    for _ in range(2**17 + 1):
        h = hashlib.sha256(h).digest()
    return "sha256|17|%s|%s" % (salt_b64.decode(), base64.b64encode(h).decode())

if __name__ == "__main__":
    print(hash_password(sys.argv[1]))
