import os
from fpdf import FPDF
import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.mime.base import MIMEBase
from email import encoders
import pickle
import sys

def txt_to_pdf(input_file, output_file):
    """Convert a text file to a PDF."""
    pdf = FPDF()
    pdf.set_auto_page_break(auto=True, margin=15)
    pdf.add_page()
    pdf.set_font("Arial", size=12)

    try:
        with open(input_file, 'r') as file:
            for line in file:
                pdf.cell(0, 10, txt=line.strip(), ln=True)

        pdf.output(output_file)
        print(f"PDF generated: {output_file}")
    except FileNotFoundError:
        print(f"Error: {input_file} not found.")


def send_email(sender_email, sender_password, recipient_email, subject, body, attachment_path):
    """Send an email with an attachment."""
    try:
        # Set up the MIME
        message = MIMEMultipart()
        message['From'] = sender_email
        message['To'] = recipient_email
        message['Subject'] = subject

        # Add body to the email
        message.attach(MIMEText(body, 'plain'))

        # Attach the file
        with open(attachment_path, "rb") as attachment:
            mime_base = MIMEBase('application', 'octet-stream')
            mime_base.set_payload(attachment.read())
            encoders.encode_base64(mime_base)
            mime_base.add_header('Content-Disposition', f'attachment; filename={os.path.basename(attachment_path)}')
            message.attach(mime_base)

        # Connect to the server and send the email
        with smtplib.SMTP('smtp.gmail.com', 587) as server:
            server.starttls()
            server.login(sender_email, sender_password)
            server.send_message(message)

        print(f"Email sent to {recipient_email}")
    except Exception as e:
        print(f"Failed to send email: {e}")


if __name__ == "__main__":
    # File paths
    txt_file = "data/booking_details.txt"
    pdf_file = "data/booking_details.pdf"

    # Email credentials and recipient
    sender_email = "TransitEase2025@gmail.com"  # Replace with your email
    with open ('data/app_password.pkl', 'rb') as file:
        sender_password = pickle.load(file)

    recipient_email = sys.argv[1]

    subject = "Your Booking Details"
    body = "Dear Customer,\n\nPlease find attached your booking details in PDF format.\n\nBest regards,\nTransitEase2025 Team"

    # Convert TXT to PDF
    txt_to_pdf(txt_file, pdf_file)

    # Send email with the PDF attachment
    send_email(sender_email, sender_password, recipient_email, subject, body, pdf_file)
