;;; graph-test.el --- test graph drawing

;; Copyright (C) 2017 by David ZUBER

;; Author: David ZUBER <zuber.david@gmx.de>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)

(when (require 'undercover nil t)
  (require 'edebug)
  (def-edebug-spec setf (&rest [place form]))
  (undercover "*.el"))

(require 'graph)

(defalias 'mgs 'make-graph-shape)
(defalias 'mgt 'make-graph-treen)

(defun mgbt (ind width text &optional left right)
  "Create a binary tree node with IND WIDTH TEXT RIGHT and LEFT."
  (make-graph-btreen :ind ind :width width :text text :left left :right right))

(ert-deftest let-shape ()
  "Test binding of shape slots."
  (should (= (let-shape (x y) (make-graph-shape :x 2 :y 5) (+ x y)) 7)))

(ert-deftest let-treen ()
  "Test binding of tree node slots."
  (should (= (let-treen (x y) (make-graph-treen :x 2 :y 5) (+ x y)) 7)))

(ert-deftest let-btreen ()
  "Test binding of tree node slots."
  (should (= (let-btreen (ind width) (make-graph-btreen :ind 2 :width 5) (+ ind width)) 7)))

(ert-deftest half ()
  "Test dividing things by two."
  (should (equal (/ 3 2.0) (graph-half 3))))

(ert-deftest fill ()
  "Test fill."
  (should (equal "a" (graph-fill ?a)))
  (should (equal "aaa" (graph-fill ?a 3)))
  (should (equal "" (graph-fill ?a 0)))
  (should (equal "" (graph-fill ?a -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest integer-shapes ()
  "Test rounding off shapes."
  (let ((shapes (list
                 (mgs :x 0 :y 0 :width 10 :height 20 :type "type")
                 (mgs :x 3.2 :y 1.6 :width 4.7 :height 9.5 :type "foo")))
        (expected (list
                   (mgs :x 0 :y 0 :width 10 :height 20 :type "type")
                   (mgs :x 3 :y 1 :width 4 :height 10 :type "foo"))))
    (should (equal expected (graph-integer-shapes shapes)))))

(ert-deftest rect-relation ()
  "Test calculating rectangle relation."
  (should (equal 'on (graph-rect-relation 3 3 10)))
  (should (equal 'on (graph-rect-relation 7 3 5)))
  (should (equal 'in (graph-rect-relation 4 3 5)))
  (should (equal nil (graph-rect-relation 1 3 5)))
  (should (equal nil (graph-rect-relation 10 3 5))))

(ert-deftest shapes-height ()
  "Test calculating the maximum shape height."
  (let ((shapes (list
                 (mgs :y 10 :height 3)
                 (mgs :y 1 :height 6))))
    (should (equal 13 (graph-shapes-height shapes)))))

(ert-deftest shapes-width ()
  "Test calculating the maximum shape width."
  (let ((shapes (list
                 (mgs :x 10 :width 3)
                 (mgs :x 1 :width 6))))
    (should (equal 13 (graph-shapes-width shapes)))))

(ert-deftest numbered ()
  "Test generating numbered sequences."
  (should (equal '((0 . "a") (1 . "b") (2 . "c") (3 . "d"))
                 (graph-numbered '("a" "b" "c" "d")))))

(ert-deftest iter-height ()
  "Test iterating over ypos."
  (should (equal '(0 1 2 3 4)
                 (graph-iter-height (list (mgs :y 2 :height 3))))))

(ert-deftest filter-shapes-at-ypos ()
  "Test filtering shapes that don't cover ypos."
  (let* ((shapes (list
                  (mgs :y 3 :height 4)
                  (mgs :y 1 :height 5)
                  (mgs :y 1 :height 100)))
         (expected (list (nth 0 shapes) (nth 2 shapes))))
    (should (equal expected (graph-filter-shapes-at-ypos shapes 6)))))

(ert-deftest x-sort-shapes ()
  "Test sorting shapes."
  (let ((assert (lambda (expected input)
                  (should (equal expected (graph-x-sort-shapes input)))
                  (should (equal expected (graph-x-sort-shapes (reverse input))))
                  (should (equal expected (graph-x-sort-shapes expected))))))
    (let ((a (mgs :x 3))
          (b (mgs :x 1)))
      (assert (list b a) (list a b)))
    (let ((a (mgs :x 1 :width 3))
          (b (mgs :x 1 :width 1)))
      (assert (list b a) (list a b)))
    (let ((a (mgs :x 1 :width 1 :height 1))
          (b (mgs :x 1 :width 1 :height 10)))
      (assert (list b a) (list a b)))
    (let ((a (mgs :x 1 :width 1 :height 10))
          (b (mgs :x 1 :width 1 :height 1)))
      (assert (list a b) (list a b)))
    (let ((a (mgs :x 1 :width 1 :height 1))
          (b (mgs :x 1 :width 1 :height 1)))
      (assert (list a b) (list a b)))))

(ert-deftest new-xcur ()
  "Test moving the xcursor"
  (should (equal 8 (graph-new-xcur 5 2 "123456")))
  (should (equal 8 (graph-new-xcur 8 2 "12")))
  (should (equal 8 (graph-new-xcur 8 2 "123456"))))

(ert-deftest crop-already-drawn ()
  "Test cropping a string so it doesn't overlap with what we've already drawn."
  (should (equal "3456" (graph-crop-already-drawn 5 3 "123456")))
  (should (equal "123456" (graph-crop-already-drawn 2 5 "123456"))))

(ert-deftest get-next-shapes-to-draw ()
  "Test getting the next shapes."
  (should (equal '(1 0 2 3) (graph-get-next-shapes-to-draw t 0 '(1 2 3))))
  (should (equal '(1 2 3) (graph-get-next-shapes-to-draw nil 0 '(1 2 3)))))

(ert-deftest draw-border ()
  (should (equal ">" (graph-draw-border 'arrow 'right 1)))
  (should (equal "<" (graph-draw-border 'arrow 'left 1)))
  (should (equal "<+" (graph-draw-border 'arrow 'left 2)))
  (should (equal "<----+" (graph-draw-border 'arrow 'left 6)))
  (should (equal "^" (graph-draw-border 'arrow 'up 1)))
  (should (equal "V" (graph-draw-border 'arrow 'down 1)))
  (should (equal "*" (graph-draw-border 'arrow nil 1)))
  (should (equal "-" (graph-draw-border 'cap 'right 1)))
  (should (equal "-" (graph-draw-border 'cap 'left 1)))
  (should (equal "-+" (graph-draw-border 'cap 'left 2)))
  (should (equal "-----+" (graph-draw-border 'cap 'left 6)))
  (should (equal "|" (graph-draw-border 'cap 'up 1)))
  (should (equal "|" (graph-draw-border 'cap 'down 1)))
  (should (equal "+" (graph-draw-border 'box nil 1)))
  (should (equal "++" (graph-draw-border 'box nil 2)))
  (should (equal "+-+" (graph-draw-border 'box nil 3)))
  (should (equal "+----+" (graph-draw-border 'box nil 6))))

(ert-deftest draw-body-line ()
  (should (equal "|    |" (graph-draw-body-line 7 2 6 (graph-wrap-fn "asdf" 6 2))))
  (should (equal "|  bar  |" (graph-draw-body-line 5 2 9 (graph-wrap-fn "foo bar spam" 9 9))))
  (should (equal "| d|" (graph-draw-body-line 4 1 3 (graph-wrap-fn "asdf" 3 2))))
  (should (equal "| |" (graph-draw-body-line 4 -10 3 (graph-wrap-fn "asdf" 3 2))))
  (should (equal "||" (graph-draw-body-line 7 2 2 (graph-wrap-fn "asdf" 3 2)))))

(ert-deftest draw-at-ypos ()
  (should (equal "+----+" (graph-draw-at-ypos 7 (mgs :y 7 :height 6 :width 6 :text '("asdf")))))
  (should (equal "+----+" (graph-draw-at-ypos 7 (mgs :y 2 :height 6 :width 6 :text '("asdf")))))
  (should (equal "|asdf|" (graph-draw-at-ypos 7 (mgs :y 3 :height 6 :width 6 :text '("" "" "" "asdf"))))))

(ert-deftest positions ()
  "Test getting positions"
  (should (equal '(3 4 5) (graph-positions (lambda (x) (> x 2)) '(0 1 2 3 4 5)))))

(ert-deftest wrap ()
  "Test wrapping text"
  (should (equal '("foo" "spam" "longg" "g") (graph-wrap "foo spam longgg" 5)))
  (should (equal nil (graph-wrap "" 5)))
  (should (equal '("f" "o" "o") (graph-wrap "foo" 1)))
  (should (equal '("foo spam" "bar") (graph-wrap "foo spam bar" 9)))
  (should (equal '("foo" "spam" "bar") (graph-wrap "foo spam bar" 4)))
  (should (equal '("foo" "spam" "bar") (graph-wrap "foo spam bar" 5))))

(ert-deftest wrap-fn ()
  "Test wrapping text again"
  (should (equal '(" f" " o" " o" " s" " p" " a" " m" " b" " a" " r") (graph-wrap-fn "foo spam bar" 4 10)))
  (should (equal '(" foo" " spam" " bar") (graph-wrap-fn "foo spam bar" 8 6)))
  (let ((graph-ascii-wrap-threshold 5))
    (should (equal '(" foo" " spam" " bar") (graph-wrap-fn "foo spam bar")))))

(ert-deftest center ()
  "Test centering text"
  (should (equal '("" "foo") (graph-center '("foo") 3 3)))
  (should (equal '("foo") (graph-center '("foo") 3 2)))
  (should (equal '("foo") (graph-center '("foo") 3 1)))
  (should (equal '(" foo" "spam" " bar") (graph-center '("foo" "spam" "bar") 5 3)))
  (should (equal '("foo" "bar") (graph-center '("foo" "bar") 3 3)))
  (should (equal '("" "" "foo" "bar") (graph-center '("foo" "bar") 3 6)))
  (should (equal '("" "   foo" "  spam") (graph-center '("foo" "spam") 9 5))))

(ert-deftest draw-shapes ()
  "Test drawing shapes"
  (should (equal
           (mapconcat
            'identity
            '("   |        |"
              "   |        |"
              "   +--------+"
              "   +--------+"
              "   | 1231   |"
              "   +--------+"
              "   | asdfas |"
              "   |   df   |"
              "   |        |---+"
              "   |        |fas|"
              "   +--------+f  |-------+"
              "        |       |asdfas |"
              "        |       |  df   |"
              "        +-------+       |"
              "               |        |"
              "               +--------+"
              "")
            "\n")
           (graph-draw-shapes
            (list
             (mgs :x 3 :y -3 :height 6 :width 10 :text (graph-wrap-fn "123123" 8 6) :on-top nil)
             (mgs :x 3 :y 3 :height 6 :width 10 :text (graph-wrap-fn "123123" 8 6) :on-top nil)
             (mgs :x 3 :y 5 :height 6 :width 10 :text (graph-wrap-fn "asdfasdf" 10 6) :on-top t)
             (mgs :x 8 :y 8 :height 6 :width 9 :text (graph-wrap-fn "asdfasdf" 10 6) :on-top t)
             (mgs :x 15 :y 10 :height 6 :width 10 :text (graph-wrap-fn "asdfasdf" 10 6) :on-top nil)))))
  (should (equal
           (mapconcat
            'identity
            '(""
              ""
              "  +----+---+"
              "  |    +   |"
              "  |        |"
              "  |        |"
              "  |        | +------+"
              "  | asdf   |"
              "  |        |"
              "  |        |"
              "  |        |"
              "  +----+---+"
              "       |"
              "       |"
              "       +"
              "")
            "\n")
           (graph-draw-shapes
            (list
             (mgs :x 7 :y 3 :width 1 :height 0 :type 'rect)
             (mgs :x 2 :y 2 :width 10 :height 10 :type 'rect :text '(" " " " " " " " " asdf"))
             (mgs :x 13 :y 6 :width 8 :height 1 :type 'rect)
             (mgs :x 7 :y 11 :width 1 :height 4 :type 'rect))))))

(ert-deftest draw-custom ()
  "Test drawing of a shape with custom data"
  (let ((graph-draw-arrow-fn (lambda (dir data) (let ((text "<"))
                                                  (put-text-property 0 (length text) 'data data text)
                                                  text))))
    (should (equal 'foobar
                   (get-text-property 0 'data (graph-draw-shapes
                                               (list (mgs :x 0 :y 0 :width 1 :height 1
                                                          :type 'arrow :dir 'left :data 'foobar))))))))

(ert-deftest draw-custom-wrap ()
  "Test using a simple wrapper for all drawings."
  (let ((graph-draw-custom-fn (lambda (drawn data)
                                   (put-text-property 0 (length drawn) 'data data drawn)
                                   drawn)))
    (should (equal 'foobar
                   (get-text-property 0 'data (graph-draw-shapes
                                               (list (mgs :x 0 :y 0 :width 3 :height 3
                                                          :type 'rect :data 'foobar))))))))

(ert-deftest label-text ()
  "Test the special symbol case."
  (should (equal "asdf" (graph-label-text "asdf")))
  (should (equal "oak tree" (graph-label-text 'oak-tree)))
  (should (equal "oaktree" (graph-label-text 'oaktree)))
  (should (equal "" (graph-label-text nil)))
  (should (equal "5" (graph-label-text 5))))

(ert-deftest horizontal ()
  "Test horizontal function"
  (should (eq 'left (graph-horizontal 'left)))
  (should (eq 'right (graph-horizontal 'right)))
  (should (eq nil (graph-horizontal 'up)))
  (should (eq nil (graph-horizontal 'down)))
  (should (eq nil (graph-horizontal nil))))

(ert-deftest vertical ()
  "Test vertical function"
  (should (eq 'up (graph-vertical 'up)))
  (should (eq 'down (graph-vertical 'down)))
  (should (eq nil (graph-vertical 'left)))
  (should (eq nil (graph-vertical 'right)))
  (should (eq nil (graph-vertical nil))))

(ert-deftest scan-add ()
  "Test adding a scan."
  (should (equal '((0 5) (6 12) (9 10))
                 (graph-scan-add
                  '((0 5) (7 10)) 6 12 3))))

(ert-deftest scan-lowest-y ()
  "Test finding the lowest y to prevent intersection."
  (should (equal 5 (graph-scan-lowest-y '((0 5) (7 10)) 3 3)))
  (should (equal 10 (graph-scan-lowest-y '((0 5) (7 10)) 3 5)))
  (should (equal 10 (graph-scan-lowest-y '((0 5) (7 10)) 20 5)))
  (should (equal 6 (graph-scan-lowest-y '((0 17) (9 13) (10 17) (21 13) (22 14) (31 6) (32 10) (41 6) (56.5 0))
                                         42 8)))
  (should (equal 6 (graph-scan-lowest-y '((0 0) (8.5 5) (12.0 11) (19.0 5) (20.0 10) (31.0 6) (46.75 0))
                    41.25 10))))

(ert-deftest height-fn ()
  "Test calculating height of a box."
  (should (equal 3 (graph-height-fn "asdf")))
  (should (equal 4 (graph-height-fn "1234567890 1231246")))
  (should (equal 2 (graph-height-fn ""))))

(ert-deftest width-fn ()
  "Test calculating height of a box."
  (should (equal 4 (graph-width-fn "")))
  (should (equal (+ 4 graph-ascii-wrap-threshold) (graph-width-fn "1234567890 1231246")))
  (should (equal 8 (graph-width-fn "asdf"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest -tree-to-shapes ()
  "Test converting to shapes."
  (let ((tree (list
               (mgt :text "asdf" :x 2 :y 6 :width 10 :height 10
                                 :line-right 15 :line-left 10 :line-ypos 30
                                 :leaf nil :parent-line-y 2 :data 'foobar))))
    (should (equal (list
                    (mgs :x 6.5 :y 2 :height 5 :width 1 :type 'rect)
                    (mgs :x 2 :y 6 :height 10 :width 10 :type 'rect :text '(" " " " " " "  asdf") :data 'foobar)
                    (mgs :x 10 :y 30 :height 1 :width 5 :type 'rect)
                    (mgs :x 6.5 :y 15 :height 16 :width 1 :type 'rect))
                   (graph--tree-to-shapes tree)))))

(ert-deftest make-rows ()
  "Test converting a tree into rows."
  (should (equal (list
                  (list (mgt :id 1 :text "asdf" :leaf nil)
                        (mgt :id 4 :text "4" :leaf nil))
                  (list (mgt :id 2 :text "123" :leaf t :parent 1)
                        (mgt :id 3 :text "3" :leaf t :parent 1 :data 'foobar)
                        (mgt :id 5 :text "5" :leaf t :parent 4)
                        (mgt :id 6 :text "6" :leaf t :parent 4)))
                 (graph-make-rows
                  (list
                   (mgt :id 1 :text "asdf"
                                     :children (list
                                                (mgt :id 2 :text "123")
                                                (mgt :id 3 :text "3" :data 'foobar)))
                   (mgt :id 4 :text "4"
                                          :children (list
                                                     (mgt :id 5 :text "5")
                                                     (mgt :id 6 :text "6"))))))))

(ert-deftest wrap-text ()
  "Test wrapping the text of a tree's rows."
  (should (equal (list
                  (list (mgt :id 1 :height 4 :text "1234567890 asdfsadf"
                                          :wrapped-text (list " 1234567890" " asdfsadf")))
                  (list (mgt :id 2 :height 3 :text "123" :wrapped-text (list " 123"))
                        (mgt :id 6 :height 5 :text "1234567890 asdfsadf 1231231231"
                                          :wrapped-text (list " 1234567890" "  asdfsadf" " 1231231231"))))
                 (graph-wrap-text
                  (list
                   (list (mgt :id 1 :text "1234567890 asdfsadf"))
                   (list (mgt :id 2 :text "123")
                         (mgt :id 6 :text "1234567890 asdfsadf 1231231231")))))))

(ert-deftest row-pos ()
  "Test calculating x of a tree's row."
  (should (equal
           (list (mgt :id 1 :x 0 :width 14 :height 4 :text "asdfsadfa 13213125")
                 (mgt :id 2 :x 15 :width 7 :height 3 :text "123"))
           (graph-row-pos
            (list (mgt :id 1 :text "asdfsadfa 13213125")
                  (mgt :id 2 :text "123"))
            0))))

(ert-deftest parent-child-tests ()
  "Test parent child testing."
  (let ((a (mgt :id 1 :parent 2))
        (b (mgt :id 2)))
    (should (graph-parent-p a b))
    (should (not (graph-parent-p b a)))
    (should (graph-child-p b a))
    (should (not (graph-child-p a b)))))

(ert-deftest space-row ()
  "Test spacing a row."
  (should
   (equal
    (list (mgt :id 0 :x 10.0 :width 14 :height 4 :text "north america" :wrapped-text '(" north america"))
          (mgt :id 6 :x 33.875 :width 11 :height 3 :text "europe" :wrapped-text '(" europe")))
    (graph-space-row
     'graph-parent-p 68
     (list (mgt :id 1 :x 13.0 :y nil :width 8 :height 3 :text "usa"
                :wrapped-text '(" :usa") :leaf nil :parent 0)
           (mgt :id 7 :x 22.0 :width 12 :height 3 :text "germany"
                :wrapped-text '(" :germany") :leaf t :parent 6)
           (mgt :id 8 :x 45.25 :width 11 :height 3 :text "france"
                :wrapped-text '(" :france") :leaf nil :parent 6))
     (list (mgt :id 0 :x 0 :width 14 :height 4 :text "north america" :wrapped-text '(" north america"))
           (mgt :id 6 :x 15 :width 11 :height 3 :text "europe" :wrapped-text '(" europe")))
     27))))

(ert-deftest space ()
  "Test spacing rows."
  (should
   (equal
   (list (list (mgt :id 0 :x 10.0 :width 14 :height 4 :text "north america" :wrapped-text '(" north america"))
               (mgt :id 6 :x 33.875 :width  11 :height 3 :text "europe" :wrapped-text '(" europe"))))
    (graph-space
     'graph-parent-p 68
     (list (mgt :id 1 :x 13.0 :width 8 :height 3 :text "usa" :wrapped-text '(" usa") :parent 0)
           (mgt :id 7 :x 22.0 :width 12 :height 3 :text "germany" :wrapped-text '(" germany") :leaf t :parent 6)
           (mgt :id 8 :x 45.25 :width 11 :height 3 :text "france" :wrapped-text '(" france") :parent 6))
     (list (list (list (mgt :id 0 :x 0 :width 14 :height 4 :text "north america" :wrapped-text '(" north america"))
                       (mgt :id 6 :x 15 :width 11 :height 3 :text "europe" :wrapped-text '(" europe"))) 27))))))

(ert-deftest bounds ()
  "Test getting bounds of a node."
  (should (equal 15.0 (graph-bounds (mgt :x 12 :width 6)))))

(ert-deftest horz-lines ()
  "Test calculating horizontal line that leads to children."
  (should (equal (list
                  (list (mgt :id 1 :text "asdf" :x 3 :width 6 :line-left 2.0 :line-right 8.0)
                        (mgt :id 4 :text "4" :x 6 :width 3 :line-left 7.0 :line-right 17.0))
                  (list (mgt :id 2 :text "123" :leaf t :parent 1 :x 0 :width 5
                                          :line-left 2.0 :line-right 3.0)
                        (mgt :id 3 :text "3" :leaf t :parent 1 :x 6 :width 3
                                          :line-left 7.0 :line-right 8.0)
                        (mgt :id 5 :text "5" :leaf t :parent 4 :x 11 :width 3
                                          :line-left 12.0 :line-right 13.0)
                        (mgt :id 6 :text "6" :leaf t :parent 4 :x 15 :width 3
                                          :line-left 16.0 :line-right 17.0)))
                 (graph-horz-lines
                  (list
                   (list (mgt :id 1 :text "asdf" :x 3 :width 6)
                         (mgt :id 4 :text "4" :x 6 :width 3))
                   (list (mgt :id 2 :text "123" :leaf t :parent 1 :x 0 :width 5)
                         (mgt :id 3 :text "3" :leaf t :parent 1 :x 6 :width 3)
                         (mgt :id 5 :text "5" :leaf t :parent 4 :x 11 :width 3)
                         (mgt :id 6 :text "6" :leaf t :parent 4 :x 15 :width 3)))))))

(ert-deftest level-lines ()
  "Test stacking horizontal lines."
  (should
   (equal
    (list
     (list
      (mgt :id 0 :x 10.0 :width 14 :height 4 :text "north america"
           :wrapped-text '(" north america") :line-right 17.5 :line-left 16.5 :line-y 0)
      (mgt :id 6 :x 33.875 :width 11 :height 3 :text "europe"
           :wrapped-text '(" europe") :line-right 51.25 :line-left 27.5 :line-y 0))
     (list
      (mgt :id 1 :x 13.0 :width 8 :height 3 :text "usa"
           :wrapped-text '(" usa") :line-right 29.5 :line-left 4.5 :line-y 0 :parent 0)
      (mgt :id 7 :x 22.0 :width 12 :height 3 :text "germany"
           :wrapped-text '(" germany") :line-right 28.5 :line-left 27.5 :line-y 0 :leaf t :parent 6)
      (mgt :id 8 :x 45.25 :width 11 :height 3 :text "france"
           :wrapped-text '(" france") :line-right 62.0 :line-left 39.5 :line-y 0 :parent 6)))
    (graph-level-lines
     (list
      (list
       (mgt :id 0 :x 10.0 :width 14 :height 4 :text "north america"
            :wrapped-text '(" north america") :line-right 17.5 :line-left 16.5)
       (mgt :id 6 :x 33.875 :width 11 :height 3 :text "europe"
            :wrapped-text '(" europe") :line-right 51.25 :line-left 27.5))
      (list
       (mgt :id 1 :x 13.0 :width 8 :height 3 :text "usa"
            :wrapped-text '(" usa") :line-right 29.5 :line-left 4.5 :parent 0)
       (mgt :id 7 :x 22.0 :width 12 :height 3 :text "germany"
            :wrapped-text '(" germany") :line-right 28.5 :line-left 27.5 :leaf t :parent 6)
       (mgt :id 8 :x 45.25 :width 11 :height 3 :text "france"
            :wrapped-text '(" france") :line-right 62.0 :line-left 39.5 :parent 6)))))))

(ert-deftest lev-children ()
  "Test updating parent-line-y."
  (should
   (equal
    (list
     (list
      (mgt :id 0 :x 10.0 :y 0 :width 14 :height 4 :text "north america"
           :wrapped-text '(" north america") :line-right 17.5 :line-left 16.5 :line-y 0 :line-ypos 1)
      (mgt :id 6 :x 33.875 :y 0 :width 11 :height 3 :text "europe"
           :wrapped-text '(" europe") :line-right 51.25 :line-left 27.5 :line-y 0 :line-ypos 4))
     (list
      (mgt :id 1 :x 13.0 :y 7 :width 8 :height 3 :text "usa" :wrapped-text '(" usa")
           :line-right 29.5 :line-left 4.5 :line-y 0 :parent 0 :parent-line-y 1)
      (mgt :id 7 :x 22.0 :y 6 :width 12 :height 3 :text "germany" :wrapped-text '(" germany")
           :line-right 28.5 :line-left 27.5 :line-y 0 :leaf t :parent 6 :parent-line-y 4)
      (mgt :id 8 :x 45.25 :y 6 :width 11 :height 3 :text "france" :wrapped-text '(" france")
           :line-right 62.0 :line-left 39.5 :line-y 0 :parent 6 :parent-line-y 4)))
    (graph-lev-children
     (list
      (list
       (mgt :id 0 :x 10.0 :y 0 :width 14 :height 4 :text "north america"
            :wrapped-text '(" north america") :line-right 17.5 :line-left 16.5 :line-y 0 :line-ypos 1)
       (mgt :id 6 :x 33.875 :y 0 :width 11 :height 3 :text "europe"
            :wrapped-text '(" europe") :line-right 51.25 :line-left 27.5 :line-y 0 :line-ypos 4))
      (list
       (mgt :id 1 :x 13.0 :y 7 :width 8 :height 3 :text "usa"
            :wrapped-text '(" usa") :line-right 29.5 :line-left 4.5 :line-y 0 :parent 0)
       (mgt :id 7 :x 22.0 :y 6 :width 12 :height 3 :text "germany"
            :wrapped-text '(" germany") :line-right 28.5 :line-left 27.5 :line-y 0 :leaf t :parent 6)
       (mgt :id 8 :x 45.25 :y 6 :width 11 :height 3 :text "france"
            :wrapped-text '(" france") :line-right 62.0 :line-left 39.5 :line-y 0 :parent 6)))))))

(ert-deftest place-boxes ()
  "Test placing boxes as high as possible."
  (should
   (equal
    (list (list (mgt :id 5 :x 24.0 :y 19 :width 10 :height 3 :text "boise" :wrapped-text '(" boise")
                     :line-right 29.5 :line-left 28.5 :line-y 0 :leaf t :parent 4))
          '((0 17) (10 13) (11 17) (23 13) (24.0 23) (34.0 6) (35 16) (45 12) (46 16) (55 12) (56 16) (67 0)))
    (graph-place-boxes
     '((0 17) (10 13) (11 17) (23 13) (24 17) (28.5 19) (29.5 17)
       (34 6) (35 16) (45 12) (46 16) (55 12) (56 16) (67 0)) nil
     (list (mgt :id 5 :x 24.0 :width 10 :height 3 :text "boise" :wrapped-text '(" boise")
                :line-right 29.5 :line-left 28.5 :line-y 0 :leaf t :parent 4))))))

(ert-deftest place-lines ()
  "Test placing lines as high as possible."
  (should
   (equal
    (list
     (list (mgt :id 1 :x 13.0 :y 7 :width 8 :height 3 :text "usa" :wrapped-text '(" usa")
                :line-right 29.5 :line-left 4.5 :line-y 0 :line-ypos 11 :parent 0)
           (mgt :id 7 :x 22.0 :y 6 :width 12 :height 3 :text "germany" :wrapped-text '(" germany")
                :line-right 28.5 :line-left 27.5 :line-y 0 :leaf t :parent 6)
           (mgt :id 8 :x 45.25 :y 6 :width 11 :height 3 :text "france" :wrapped-text '(" france")
                :line-right 62.0 :line-left 39.5 :line-y 0 :line-ypos 10 :parent 6))
     '((0 0) (4.5 13) (29.5 10) (34.0 6) (39.5 12) (62.0 0)))
    (graph-place-lines
     '((0 0) (10.0 5) (13.0 11) (21.0 5) (22.0 10) (34.0 6) (45.25 10) (56.25 0)) nil
     (list (mgt :id 1 :x 13.0 :y 7 :width 8 :height 3 :text "usa" :wrapped-text '(" usa")
                :line-right 29.5 :line-left 4.5 :line-y 0 :parent 0)
           (mgt :id 7 :x 22.0 :y 6 :width 12 :height 3 :text "germany" :wrapped-text '(" germany")
                :line-right 28.5 :line-left 27.5 :line-y 0 :leaf t :parent 6)
           (mgt :id 8 :x 45.25 :y 6 :width 11 :height 3 :text "france" :wrapped-text '(" france")
                :line-right 62.0 :line-left 39.5 :line-y 0 :parent 6))))))

(ert-deftest tree-row-wid ()
  "Test caluclating the row width."
  (should (equal 25
                 (graph-tree-row-wid
                  (list (mgt :text "adfaasdfas aldsfjsladfjlsjdf")
                        (mgt :text "assdf"))))))

(ert-deftest pack-tree ()
  "Test packing a tree."
  (should (equal
           (list
            (list
             (mgt :id 0 :x 10.0 :y 0 :width 14 :height 4 :text "north america" :wrapped-text '(" north  america")
                  :line-right 17.5 :line-left 16.5 :line-y 0 :line-ypos 5)
             (mgt :id 6 :x 33.875 :y 0 :width 11 :height 3 :text "europe" :wrapped-text '(" europe")
                  :line-right 51.25 :line-left 27.5 :line-y 0 :line-ypos 4))
            (list
             (mgt :id 1 :x 13.0 :y 7 :width 8 :height 3 :text "usa" :wrapped-text '(" usa")
                  :line-right 29.5 :line-left 4.5 :line-y 0 :line-ypos 11 :parent 0)
             (mgt :id 7 :x 22.0 :y 6 :width 12 :height 3 :text "germany" :wrapped-text '(" germany")
                  :line-right 28.5 :line-left 27.5 :line-y 0 :line-ypos 13 :parent 6)
             (mgt :id 8 :x 45.25 :y 6 :width 11 :height 3 :text "france" :wrapped-text '(" france")
                  :line-right 62.0 :line-left 39.5 :line-y 0 :line-ypos 10 :parent 6)))
           (graph-pack-tree
            (list
             (list
              (mgt :id 0 :x 10.0 :width 14 :height 4 :text "north america" :wrapped-text '(" north  america")
                   :line-right 17.5 :line-left 16.5 :line-y 0)
              (mgt :id 6 :x 33.875 :width 11 :height 3 :text "europe" :wrapped-text '(" europe")
                   :line-right 51.25 :line-left 27.5 :line-y 0))
             (list
              (mgt :id 1 :x 13.0 :width 8 :height 3 :text "usa" :wrapped-text '(" usa")
                   :line-right 29.5 :line-left 4.5 :line-y 0 :parent 0)
              (mgt :id 7 :x 22.0 :width 12 :height 3 :text "germany" :wrapped-text '(" germany")
                   :line-right 28.5 :line-left 27.5 :line-y 0 :parent 6)
              (mgt :id 8 :x 45.25 :width 11 :height 3 :text "france" :wrapped-text '(" france")
                   :line-right 62.0 :line-left 39.5 :line-y 0 :parent 6)))))))


(defun mkn (id text &optional children data)
  (mgt :id id :text text :children children :data data))

(ert-deftest idtree ()
  "Test the initial conversion into nodes."
;(draw-tree [[:north-america [:usa [:miami] [:seattle] [:idaho [:boise]]]] [:europe [:germany] [:france [:paris] [:lyon] [:cannes]]]])
  (should (equal (list
                  (mkn 0 "north america"
                           (list (mkn 1 "usa"
                                          (list (mkn 2 "miami")
                                                (mkn 3 "seattle")
                                                (mkn 4 "idaho"
                                                         (list (mkn 5 "boise")))))))
                  (mkn 6 "europe"
                           (list (mkn 7 "germany")
                                 (mkn 8 "france"
                                          (list (mkn 9 "paris")
                                                (mkn 10 "lyon")
                                                (mkn 11 "cannes"))))))
                 (graph-idtree
                  '((north-america (usa
                                    (miami) (seattle) (idaho
                                                       (boise))))
                    (europe (germany) (france
                                       (paris) (lyon) (cannes)))))))
  (should (equal (list (mkn 0 "north america" nil 'foobar))
                 (graph-idtree '(((north-america . foobar)))))))

(ert-deftest draw-tree ()
  "Test drawing a tree."
  (should
   (equal
"          +------------+         +---------+
          |   :north   |         | :europe |
          |  america   |         +----+----+
          +-----+------+              |
                |          +----------+-----------+
                +          |                      |
                |     +----+-----+           +----+----+
             +--+---+ | :germany |           | :france |
             | :usa | +----------+           +----+----+
             +--+---+                             |
                |                      +----------+----------+
    +-----------+-----------+          |          |          |
    |           |           |      +---+----+ +---+---+ +----+----+
+---+----+ +----+-----+ +---+----+ | :paris | | :lyon | | :cannes |
| :miami | | :seattle | | :idaho | +--------+ +-------+ +---------+
+--------+ +----------+ +---+----+
                            |
                            +
                            |
                        +---+----+
                        | :boise |
                        +--------+
"
    (graph-draw-tree '((:north-america (:usa (:miami) (:seattle) (:idaho (:boise)))) (:europe (:germany) (:france (:paris) (:lyon) (:cannes))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest get-side ()
  "Test calculating the length of a side of a graph."
  (should (equal 1 (graph-get-side '(0))))
  (should (equal 2 (graph-get-side '(0 1))))
  (should (equal 3 (graph-get-side (number-sequence 0 4))))
  (should (equal 4 (graph-get-side (number-sequence 0 14)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binary trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest line-info-btree ()
  "Test arringing lines between two rows."
  (should (equal '((space . 12) (nop) (lbottom) (line . 0)
                   (ltop) (space . 5) (nop) (rtop) (line . 0)
                   (rbottom) (space . 5) (nop))
                 (graph-line-info-btree
                  (list (mgbt 14 5 1 t t))
                  (list (mgbt 7 5 2 t t) (mgbt 9 5 5))))))

(ert-deftest btree-row-wid ()
  "Calculate the width of a row."
  (should (equal 38 (graph-btree-row-wid (list (mgbt 5 10 nil) (mgbt 20 3 nil))))))

(ert-deftest layout-btree ()
  "Test laying out a binary tree."
  (should
   (equal (list (list (mgbt 14 5 1 t t))
            (list (mgbt 7 5 2 t t) (mgbt 9 5 5 nil nil))
            (list (mgbt 0 5 3 nil nil) (mgbt 9 5 4 nil nil)))
          (graph-layout-btree '(1 (2 (3) (4)) (5)))))
  (should
   (equal (list (list (mgbt 29 13 'organisms t t))
                (list (mgbt 12 15 'prokaryotes t nil) (mgbt 58 14 'eukaryotes t t))
                (list (mgbt 0 10 'e-coli nil nil) (mgbt 63 10 'plants t t) (mgbt 49 12 'animalia t t))
                (list (mgbt 56 15 'seed-plants t t) (mgbt 15 9 'ferns nil nil)
                      (mgbt 18 17 'invertebrates t t) (mgbt 27 11 'mammals t t))
                (list (mgbt 44 10 'carrot nil nil) (mgbt 19 12 'oak-tree nil nil) (mgbt 16 10 'sponge nil nil)
                      (mgbt 21 8 'worm nil nil) (mgbt 6 9 'mouse nil nil) (mgbt 31 8 'apes t t))
                (list (mgbt 170 14 'chimpanzee nil nil) (mgbt 12 9 'human nil nil)))
          (graph-layout-btree
           '(organisms (prokaryotes (e-coli))
                       (eukaryotes (plants (seed-plants (carrot) (oak-tree))
                                           (ferns))
                                   (animalia (invertebrates (sponge)
                                                            (worm))
                                             (mammals (mouse)
                                                      (apes (chimpanzee)
                                                            (human))))))))))

(ert-deftest sp ()
  "Test spacing."
  (should (equal "   " (graph-sp 3)))
  (should (equal " " (graph-sp))))

(ert-deftest draw-btree ()
  "Test drawing a binary tree."
  (should
   (equal
"              +---+
              | 1 |
              +---+
             /     \\      
            /       \\     
       +---+         +---+
       | 2 |         | 5 |
       +---+         +---+
      /     \\             
     /       \\            
+---+         +---+
| 3 |         | 4 |
+---+         +---+
"
    (graph-draw-binary-tree '(1 (2 (3) (4)) (5))))))

(provide 'graph-test)

;;; graph-test.el ends here
