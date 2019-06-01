;; MIT License
;;
;; Copyright (c) 2019 Aldn
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(setf cube-size 3)

;;
;; stores the moves allowed by the puzzle
;;
;; In other words this rules describes the relation ship between
;; the smaller cubes.
;;
(setf puzzle-rules
	  '(
		(1 h) ;; cube 0 has only one contraint, with cube 1
		(2 h h) ;; cube 1 has two contraints, with cube 0 and cube 2
		(2 h p) ;; cube 2 has two contraints, with cube 1 and cube 3, they are perpendicular 'p'
		(2 h h)
		(2 h p)
		(2 h h)
		(2 h p)
		(2 h h)
		(2 h p)
		(2 h p)
		(2 h p)
		(2 h p)
		(2 h h)
		(2 h p)
		(2 h h)
		(2 h p)
		(2 h p)
		(2 h p)
		(2 h h)
		(2 h p)
		(2 h p)
		(2 h h)
		(2 h p)
		(2 h p)
		(2 h p)
		(2 h h)
		(1 h)
		)
	  )

(setf previous-move nil)

;;
;; verifies if an element exists in a list
;; 
;; (contains '(1 2 3) '((0 1 0) (1 2 3) (1 2 0)))
;;  --> True
;;
(defun contains (pattern list)
  ;; TODO this function lacks robustness:
  ;; assumes that pattern is always a list of 3 elements
  ;; assumes that car of list is always a list of 3 elements
  (if (null list)
	  nil
	(if (and (= (car pattern) (caar list))
			 (= (cadr pattern) (cadar list))
			 (= (caddr pattern) (caddar list))
			 )
		T
	  (contains pattern (cdr list))
	  )
	)
  )

;;
;; this function prevents the explorer from visting a case outside of the
;; 3x3 cube
;; it's also preventing from visiting the same case twice
;;
(defun is-safe-move(x y z visit)
  ;; check boundaries,
  (if (or (< x 0) (< y 0) (< z 0)
		  (>= x cube-size) (>= y cube-size) (>= z cube-size)
		  )
	  nil
	;; check already visited
	(if (contains (list x y z) visit)
		nil
	  T
	  )
	)
  )


;;
;; This fuction determines if a move is allowed based on the puzzle
;; rules.
;; the are 3 possible cases
;;
;;case 1: only one constraint
;;  ____   ____
;; /   /| /   /|
;;/___/ |/___/ |
;;|   |--|   | /
;;|___|/ |___|/
;;
;;case 2: 2 contraints
;;  ____   ____   ____
;; /   /| /   /| /   /|
;;/___/ |/___/ |/___/ |
;;| --|--| * |--|-- | /
;;|___|/ |___|/ |___|/
;;
;;case 3: 2 contraints one perpendicular to the other.
;;         ____
;;        /   /|
;;       /___/ |
;;  ____ |   | /
;; /   /||_|_|/|
;;/___/ |/_|_/ |
;;| --|--|-* | /
;;|___|/ |___|/
;;
(defun is-puzzle-allowed-move (prev current index)
  (let ((cur-rule (nth index puzzle-rules)))
	(if (= (car cur-rule) 1)
		T
	  (cond
	   ((and (equal (cadr cur-rule) 'h) (equal (caddr cur-rule) 'h))
		(if (equalp prev current)
			T
		  nil
		  )
		)
	   ((and (equalp (cadr cur-rule) 'h) (equalp (caddr cur-rule) 'p))
		(if (equalp prev current)
			nil
		  T
		  )
		)
	   (t
		nil)
	   )
	  )
	)
  )

(defun explore-cube (x y z index prev visit res)

  ;;(print index)
  (if (= (length visit) 27)
	  (print (reverse (cons (list x y z) res))) ;; print final result when all cases are visited
	(progn
	  ;; x varaition exploring
	  ;; x+1, x-1
	  (if (is-safe-move (+ x 1) y z visit)
		  (if (is-puzzle-allowed-move prev 'x index)
			  (progn
				;;(print (list x y z))
				(explore-cube (+ x 1) y z (+ index 1) 'x (cons (list x y z) visit)
							  (cons (list x y z) res))
				;;(print '--)
				)
			)
		)
	  (if (is-safe-move (- x 1) y z visit)
		  (if (is-puzzle-allowed-move prev 'x index)
			  (progn
				;;(print (list x y z))
				(explore-cube (- x 1) y z (+ index 1) 'x (cons (list x y z) visit)
							  (cons (list x y z) res))
				;;(print '--)
				)
			)
		)
	  ;; y variation exploring
	  ;; y+1, y-1
	  (if (is-safe-move  x (+ y 1) z visit)
		  (if (is-puzzle-allowed-move prev 'y index)
			  (progn
				;;(print (list x y z))
				(explore-cube  x (+ y 1) z (+ index 1) 'y (cons (list x y z) visit)
							   (cons (list x y z) res))
				;;(print '--)
				)
			)
		)
	  (if (is-safe-move  x (- y 1) z visit)
		  (if (is-puzzle-allowed-move prev 'y index)
			  (progn
				;;(print (list x y z))
				(explore-cube  x (- y 1) z (+ index 1) 'y (cons (list x y z) visit)
							   (cons (list x y z) res))
				;;(print '--)
				)
			)
		)

	  ;; z variation exploring
	  ;; z+1, z-1
	  (if (is-safe-move  x y (+ z 1) visit)
		  (if (is-puzzle-allowed-move prev 'z index)
			  (progn
				;;(print (list x y z))
				(explore-cube  x y (+ z 1) (+ index 1) 'z (cons (list x y z) visit)
							   (cons (list x y z) res))
				;;(print '--)
				)
			)
		)
	  (if (is-safe-move x y (- z 1) visit)
		  (if (is-puzzle-allowed-move prev 'z index)
			  (progn
				;;(print (list x y z))
				(explore-cube  x y (- z 1) (+ index 1) 'z (cons (list x y z) visit)
							   (cons (list x y z) res))
				;;(print '--)
				)

			)
		)))
  )



(defun run-resolver (x y z)
  (print (explore-cube x y z 0 nil (list (list x y z))  '()))
  )

(run-resolver 0 0 0)
