import hashlib
import sys

def hash_password(password):
    hashed_password = hashlib.sha256(password.encode()).hexdigest()

    with open('hashpassword.txt')

if __name__ == "__main__":
    password = sys.argv[1]
    print(hash_password(password)) 
