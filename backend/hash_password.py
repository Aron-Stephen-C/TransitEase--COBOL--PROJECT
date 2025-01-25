import hashlib
import sys

def hash_password(password):
    hashed_password = hashlib.sha256(password.encode()).hexdigest()

    with open('data/hashpassword.txt', 'w') as file:
        file.write(hashed_password) 

if __name__ == "__main__":
    password = sys.argv[1]
    hash_password(password)

