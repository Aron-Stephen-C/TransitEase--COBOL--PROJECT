      * This is the module that Manages user records
       IDENTIFICATION DIVISION.
       PROGRAM-ID. customer_profile_management.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT 
           FS-PASSENGER-FILE ASSIGN TO 'data/passenger_file.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM.

           SELECT 
           FS-ADMIN-FILE ASSIGN TO 'data/admin_file.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM.


       
       DATA DIVISION.
       FILE SECTION.
       FD  FS-PASSENGER-FILE.
      *01  FS-PASSENGER-RECORD.
       FD  FS-ADMIN-FILE.
      
       WORKING-STORAGE SECTION.
           
       LINKAGE SECTION.

       PROCEDURE DIVISION.
           STOP RUN.

           