;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |15|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct student (id name major))
;; the structure is called student and holds 3 values
;; id refers to the student id issued by the university
;; name refers to the name of the student
;; major refers to the students major according to the university
;; Example: (make-student 1 2 3)
