;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |14|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; define-struct : Values -> Values
;; GIVEN: Values
;; RETURNS: Creates a structure
;; Examples:
;; (make-student 1 2 3) => Creates a structure with id name and major
(define-struct student (id name major))