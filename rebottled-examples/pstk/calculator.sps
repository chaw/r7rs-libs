;; Calculator example
;; Translated from Python source into Scheme
;; http://www.techinfected.net/2016/02/make-gui-calculator-in-python-windows-linux.html

(import (scheme base)
        (rebottled pstk))

; def getandreplace(self):
;  """replace x with * and ÷ with /"""
;  
;  self.expression = self.e.get()
;  self.newtext=self.expression.replace(self.newdiv,'/')
;  self.newtext=self.newtext.replace('x','*')

; def equals(self):
;  """when the equal button is pressed"""
;
;  self.getandreplace()
;  try: 
;   self.value= eval(self.newtext) #evaluate the expression using the eval function
;  except SyntaxError or NameErrror:
;   self.e.delete(0,END)
;   self.e.insert(0,'Invalid Input!')
;  else:
;   self.e.delete(0,END)
;   self.e.insert(0,self.value)
 
; def squareroot(self):
;  """squareroot method"""
;  
;  self.getandreplace()
;  try: 
;   self.value= eval(self.newtext) #evaluate the expression using the eval function
;  except SyntaxError or NameErrror:
;   self.e.delete(0,END)
;   self.e.insert(0,'Invalid Input!')
;  else:
;   self.sqrtval=math.sqrt(self.value)
;   self.e.delete(0,END)
;   self.e.insert(0,self.sqrtval)

; def square(self):
;  """square method"""
;  
;  self.getandreplace()
;  try: 
;   self.value= eval(self.newtext) #evaluate the expression using the eval function
;  except SyntaxError or NameErrror:
;   self.e.delete(0,END)
;   self.e.insert(0,'Invalid Input!')
;  else:
;   self.sqval=math.pow(self.value,2)
;   self.e.delete(0,END)
;   self.e.insert(0,self.sqval)
 
; def clear1(self):
;  self.txt=self.e.get()[:-1]
;  self.e.delete(0,END)
;  self.e.insert(0,self.txt)

(define (button tk text command row column)
  (tk/grid (tk 'create-widget 'button
               'text: text 'width: 3
               'command: command)
           'row: row 'column: column))

(define (create-calculator tk)
  (let* ((entry (tk 'create-widget 'entry))
         (action (lambda (i) (entry 'insert (string-length (entry 'get)) i)))
         (clear-all (lambda () (entry 'delete 0 (string-length (entry 'get)))))
         (clear-1 (lambda () ())) ; delete a single character
         (equals (lambda () ())) ; evaluate
         (square-root (lambda () ())) ; squareroot the value
         (square (lambda () ()))) ; square the value
    (tk/wm 'title tk "Calculator")
    (tk/grid entry 'row: 0 'column: 0 'columnspan: 6 'pady: 3)
    (tk/focus entry)
    ; Generate buttons
    (tk/grid (tk 'create-widget 'button
                 'text: "=" 'width: 10
                 'command: equals)
             'row: 4 'column: 4 'columnspan: 2)
    (button tk "AC" clear-all 1 4)
    (button tk "C" clear-1 1 5)
    (button tk "+" (lambda () (action "+")) 4 3)
    (button tk "*" (lambda () (action "*")) 2 3)
    (button tk "-" (lambda () (action "-")) 3 3)
    (button tk "/" (lambda () (action "/")) 1 3)
    (button tk "%" (lambda () (action "%")) 4 2)
    (button tk "7" (lambda () (action "7")) 1 0)
    (button tk "8" (lambda () (action "8")) 1 1)
    (button tk "9" (lambda () (action "9")) 1 2)
    (button tk "4" (lambda () (action "4")) 2 0)
    (button tk "5" (lambda () (action "5")) 2 1)
    (button tk "6" (lambda () (action "6")) 2 2)
    (button tk "1" (lambda () (action "1")) 3 0)
    (button tk "2" (lambda () (action "2")) 3 1)
    (button tk "3" (lambda () (action "3")) 3 2)
    (button tk "0" (lambda () (action "0")) 4 0)
    (button tk "." (lambda () (action ".")) 4 1)
    (button tk "(" (lambda () (action "(")) 2 4)
    (button tk ")" (lambda () (action ")")) 2 5)
    (button tk "√" square-root 3 4)
    (button tk "x²" square 3 5)))

(let ((tk (tk-start)))
  (create-calculator tk)
  (tk-event-loop tk))
