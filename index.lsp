(defparameter *coefficients* (make-array '(2 2) :initial-element 0))
(defparameter *constants* (make-array 2 :initial-element 0))

(setf (aref *coefficients* 0 0) 2)
(setf (aref *coefficients* 0 1) 3)
(setf (aref *constants* 0) 5)

(setf (aref *coefficients* 1 0) 4)
(setf (aref *coefficients* 1 1) 1)
(setf (aref *constants* 1) 11)

(defun gaussian-elimination (coefficients constants)
  (let* ((n (array-dimension coefficients 0))
         (m (array-dimension coefficients 1)))
    (loop for i from 0 to (1- n) do
      (let ((factor (aref coefficients i i)))
        (loop for j from i to (1- m) do
          (setf (aref coefficients i j) (/ (aref coefficients i j) factor)))
        (setf (aref constants i) (/ (aref constants i) factor)))
      (loop for k from (1+ i) to (1- n) do
        (let ((multiplier (aref coefficients k i)))
          (loop for j from i to (1- m) do
            (setf (aref coefficients k j) (- (aref coefficients k j) (* multiplier (aref coefficients i j)))))
          (setf (aref constants k) (- (aref constants k) (* multiplier (aref constants i)))))))
    (let ((solution (make-array n :initial-element 0)))
      (loop for i from (1- n) downto 0 do
        (setf (aref solution i) (aref constants i))
        (loop for j from (1+ i) to (1- n) do
          (setf (aref solution i) (- (aref solution i) (* (aref coefficients i j) (aref solution j))))))
      solution)))

(defparameter *solution* (gaussian-elimination *coefficients* *constants*))

(format t "Решение: x = ~a, y = ~a~%" (aref *solution* 0) (aref *solution* 1))
