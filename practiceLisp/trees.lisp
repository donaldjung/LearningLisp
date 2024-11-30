(defstruct treenode
  value
  left
  right)

;; Create an instance of the tree
(defvar z (make-treenode
              :value 1
              :left (make-treenode
                      :value 2
                      :left (make-treenode :value 4)
                      :right (make-treenode :value 5))
              :right (make-treenode
                       :value 3
                       :left (make-treenode :value 6)
                       :right (make-treenode :value 7))))
;   1
; 2   3
;4 5 6 7

(defun inorder (x)
  (when x
    (inorder (treenode-left x))
    (print (treenode-value x))
    (inorder (treenode-right x)))
  )

(defun preorder (x)
  (when x
    (print (treenode-value x))
    (preorder (treenode-left x))
    (preorder (treenode-right x))))

(defun postorder (x)
  (when x
    (postorder (treenode-left x))
    (postorder (treenode-right x))
    (print (treenode-value x))))
