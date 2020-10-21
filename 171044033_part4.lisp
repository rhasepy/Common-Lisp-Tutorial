(defstruct minHeapNode
  data
  freq
  left
  right
)

(defstruct minHeap
  size
  capacity
  arr
)

(defun newNode (dataParam freqParam)

  (setq tempNode (make-minHeapNode
    :data dataParam
    :freq freqParam
    :left nil
    :right nil))

  tempNode)

(defun createMinHeap (capacityParam)

  (setq tempHeap (make-minHeap
    :size 0
    :capacity capacityParam
    :arr '()
    ))
  tempHeap)

(defun swapOnList (nodeList i j)

  (rotatef (nth i nodeList) (nth j nodeList))
  nodeList)

(defun minHeapify (minHeap_param index)

  (setq small index)
  (setq left (+ 1 (* 2 index)))
  (setq right (+ 2 (* 2 index)))

  (if 
    (and
      (< left (minHeap-size minHeap_param))

      (<
        (minHeapNode-freq (nth left (minHeap-arr minHeap_param)))
        (minHeapNode-freq (nth small (minHeap-arr minHeap_param)))
      )
    )
    (setq small left)   
  )

  (if 
    (and
      (< right (minHeap-size minHeap_param))

      (<
        (minHeapNode-freq (nth left (minHeap-arr minHeap_param)))
        (minHeapNode-freq (nth small (minHeap-arr minHeap_param)))
      )
    )
    (setq small right)   
  )

  (if (not (= small index))
    (setq (minHeap-arr minHeap_param) (swapOnList (minHeap-arr minHeap_param) small index))
  )

  (if (not (= small index))
    (minHeapify minHeap_param small)
  )
  minHeap_param)

(defun isSizeOne (minHeap_param)
  (= 1 (minHeap-size minHeap_param)))

(defun extractMin (minHeap_param)

  (setq minHeapNode_temp (nth 0 (minHeap-arr minHeap_param)))
  (setq (nth 0 (minHeap-arr minHeap_param)) (nth (minHeap-size minHeap_param) (minHeap-arr minHeap_param)))

  (decf (minHeap-size minHeap_param))
  (setq minHeap_param (minHeapify minHeap_param 0))

  ; 0th min heap node temp
  ; 1th min heap
  (setq returnList '(minHeapNode_temp minHeap_param))

  returnList)

(defun insertMinHeap (minHeap_param minHeapNode_param)

  (incf (minHeap-size minHeapNode_param))
  (setq i (- (minHeap-size) 1))

  (loop while 
      (and
        (= 0 i)
        (< 
          (minHeapNode-freq minHeapNode_param)
          (minHeapNode-freq (nth (/ (- i 1) 2) (minHeap-arr minHeap_param)))
        )
      )
    do
      (setq (nth i (minHeap-arr minHeap_param)) (nth (/ (- i 1) 2) minHeap-arr minHeap_param))
      (setq i (/ (- i 1) 2))
  )
    
    (setq (nth i (minHeap-arr minHeap_param)) minHeapNode_param)
    (setq returnList '(minHeapNode_param minHeap_param))

  returnList)

(defun buildMinHeap (minHeap_param)

  (setq n (- (minHeap-size minHeap_param) 1))
  (setq i (/ (- n 1) 2))

  (loop while (>= i 0)
    do
      (setq minHeap_param (minHeapify minHeap_param i))
      (decf i))

  minHeap_param)

(defun isLeaf (minHeapNode_param)
  (and 
    (= (minHeapNode-right minHeapNode_param) 0) 
    (= (minHeapNode-left minHeapNode_param) 0)))

(defun createAndBuildMinHeap (datas freqs size)


  (setq minHeap_var (createMinHeap size))

  (loop for i from 1 to size do
      (setq (nth i (minHeap-arr minHeapNode_var)) (newNode (nth i datas) (nth i freqs))))

  (setq (minHeap-size minHeap_var) size)
  (setq minHeap_var (buildMinHeap minHeap_var))

  minHeap_var)

(defun buildHuffman (datas freqs size)

  (setq minHeap_var (createAndBuildMinHeap datas freqs size))

  (loop while (not (isSizeOne minHeap_var))
    do
      (setq list1 (extractMin minHeap_var)) ; 0 node 1 heap
      (setq minHeap_var (nth 1 list1))
      (setq left (nth 0 list1))

      (setq list2 (extractMin minHeap_var)) ; 0 node 1 heap
      (setq minHeap_var (nth 1 list2))
      (setq right (nth 0 list2))

      (setq top (newNode "$" (+ (minHeapNode-freq left) (minHeapNode-freq right))))
      (setq (minHeapNode-right top) left)
      (setq (minHeapNode-right top) right)

      (setq list3 (insertMinHeap minHeap_var top))
      (setq minHeap_var (nth 1 list3))
      (setq top (nth 0 list3))
  )

  (setq retunList (extractMin minHeap_var)) ; 0 node 1 heap

  (nth 0 retunList))

(defun printArr (arr n)

  (loop for i from 1 to n
    do
    (format t "~d" (aref arr i)))

  (terpri))

(defun printCodes (root arr top)

  (if (not (= 0 (minHeapNode-left root)))
    (setf (aref arr top) 0))

  (if (not (= 0 (minHeapNode-left root)))
    (printCodes (minHeapNode-left root) arr (+ 1 top)))

  (if (not (= 0 (minHeapNode-right root)))
   (setf (aref arr top) 1))

  (if (not (= 0 (minHeapNode-right root)))
    (printCodes (minHeapNode-right root) arr (+ 1 top)))

  (if (isLeaf root)
    (format t "~c: " (minHeapNode-data root)))

  (if (isLeaf root)
    (printArr arr top)))

(defun HuffmanCodes (datas freqs size)

  (setq minHeapNode_root (buildHuffman datas freqs size))
  (setf arr (make-array '(100)))
  (setq top 0)
  (printCodes minHeapNode_root arr top))

(defun main()

  (setf arrData (make-array '(6)))
  (setf arrFreq (make-array '(6)))
  (setq size 6)

  (setf (aref arrData 0) "A")
  (setf (aref arrData 1) "B")
  (setf (aref arrData 2) "C")
  (setf (aref arrData 3) "D")
  (setf (aref arrData 4) "E")
  (setf (aref arrData 5) "F")

  (setf (aref arrFreq 0) 5)
  (setf (aref arrFreq 1) 9)
  (setf (aref arrFreq 2) 12)
  (setf (aref arrFreq 3) 13)
  (setf (aref arrFreq 4) 16)
  (setf (aref arrFreq 5) 45)

  (HuffmanCodes arrData arrFreq size))

(main)