import sys
import random
import smtplib
import pickle
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart

def send_otp_for_email_validation(user_email_address):
        
        otp = f"{random.randint(0, 999999):06d}"

        sender_email = "TransitEase2025@gmail.com"

        with open('data/app_password.pkl', 'rb') as file:
            sender_password = pickle.load(file)

        subject = "Email Verification"
        body = f"Here is your one time password: \n{otp}"
        to_email = user_email_address
    
        msg = MIMEMultipart()
        msg['From'] = sender_email
        msg['To'] = to_email
        msg['Subject'] = subject
        msg.attach(MIMEText(body, 'plain'))

        try:
            with smtplib.SMTP("smtp.gmail.com", 587) as server:
                server.starttls()
                server.login(sender_email, sender_password)
                server.sendmail(sender_email, to_email, msg.as_string())

        except Exception as e:
            print(f"Error sending email: {e}")

        with open('data/otp.txt', 'w') as file:
            file.write(otp)           

if __name__ == "__main__":
    recipient_email = sys.argv[1]
    send_otp_for_email_validation(recipient_email)
