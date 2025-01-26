import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.mime.base import MIMEBase
from email import encoders
import pickle
from fpdf import FPDF

def txt_to_pdf(txt_file, pdf_file):
    pdf = FPDF()
    pdf.set_auto_page_break(auto=True, margin=15)
    pdf.add_page()
    pdf.set_font("Arial", size=12)
   
    with open(txt_file, 'r') as file:
        for line in file:
            pdf.cell(0, 10, txt=line.strip(), ln=True)
   
    pdf.output(pdf_file)
    print(f"PDF generated: {pdf_file}")

def send_email(to_email, subject, body, attachment_path):

    from_email = "TransitEase2025@gmail.com"  

    with open ('data/app_password.pkl', 'rb') as file:
        from_password = pickle.load(file)   

    print(from_password)

    msg = MIMEMultipart()
    msg['From'] = from_email
    msg['To'] = to_email
    msg['Subject'] = subject

    msg.attach(MIMEText(body, 'plain'))

    with open(attachment_path, "rb") as attachment:
        part = MIMEBase('application', 'octet-stream')
        part.set_payload(attachment.read())
        encoders.encode_base64(part)
        part.add_header('Content-Disposition', f"attachment; filename={attachment_path}")
        msg.attach(part)

    with smtplib.SMTP('smtp.gmail.com', 587) as server:
        server.starttls()
        server.login(from_email, from_password)
        server.send_message(msg)

    print(f"Email sent to {to_email}")

if __name__ == "__main__":
    txt_file = "INVOICE.TXT"
    pdf_file = "INVOICE.PDF"
    recipient_email = input("Enter recipient email: ")

    # Convert TXT to PDF
    txt_to_pdf(txt_file, pdf_file)

    # Send email with the generated PDF
    email_subject = "Your Invoice"
    email_body = "Please find your invoice attached."
    send_email(recipient_email, email_subject, email_body, pdf_file)

