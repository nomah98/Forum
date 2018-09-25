;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |7a forum|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/string)


;;Exercise 7a : Forum Project

;;Stage 1 - New World State Definitions

;; A World is one of
;; - EditviewPosts
;; - SearchPosts
;; INTERPRETATION: Represents two different "views" in your program.

;; A EditviewPosts is a (make-editview Edit History Search)
;; INTERPRETATION: Means the user is viewing all posts and
;; potentially typing in a new one.
(define-struct editview [edit history search])

;; A SearchPosts   is a (make-search Edit History Search)
;; INTERPRETATION: Means the user is trying to view only a subset
;; of the existing messages.
(define-struct search [edit history search])

(define (editview-temp ev)
  (... (editview-edit ev) ... (editview-history ev) ... (editview-search ev) ...))

(define (search-temp s)
  (... (search-edit s) ... (search-history s) ... (search-search s) ...))

(define (world-temp w)
  (cond
    [(editview? w)
     (... (editview-temp w) ...)]
    [(search? w)
     (... (search-temp w) ...)]))

;; A Edit is a String
;; INTERPRETATION: the contents of the post the user is currently
;; editing.
;; A History is a List of Strings
;; INTERPRETATION: the prior posts received from the server.
;; A Search is a String
;; INTERPRETATION: the current search term the user is looking for

(define World1 (make-editview "" '() ""))
(define World2 (make-editview "let's go fundies!" '() ""))
(define World3 (make-editview "hello" (cons "world" '()) "halp"))
(define World4 (make-search "" '() ""))
(define World5 (make-search "" (cons "aaa" '()) "4a"))
(define World6 (make-search "halfpost" (cons "fullpost" (cons "oldpost" '())) "post"))

(define WORLD-WIDTH 1000)
(define WORLD-HEIGHT 700)

(define EMPTY-WORLD
  (rectangle WORLD-WIDTH WORLD-HEIGHT "solid" "AliceBlue"))

;; simple-net-forum : World -> World

;; Main Function

(define SERVER-NAME "dictionary.ccs.neu.edu")
(define PORT-NAME 10003)
(define USERNAME "scherer.e:0137")

(define (simple-net-forum InitialWorld)
  (big-bang InitialWorld
            [name USERNAME]
            [register SERVER-NAME]
            [port PORT-NAME]
            [on-key key-type]
            [to-draw display-text]
            [on-receive receive-post]))

;; On-key function
;; key-type: World String -> World
;;Interpretation: pressing a key results in a string which updates the current post
;;Specifically, "\b" will remove a letter, "\r" sends a post, and other single
; characters are typed into the current post
#;(define (key-type Editviewposts KeyEvent)
    ...)

(check-expect (key-type World1 "h") (make-editview "h" '() ""))
(check-expect (key-type World1 "\b") World1)
(check-expect (key-type World1 "\r") World1)
(check-expect (key-type World2 "\b")
              (make-editview "let's go fundies" '() ""))
(check-expect (key-type World2 "\r")
              (make-package
               (make-editview "" '() "")
               "let's go fundies!"))
(check-expect (key-type World3 "right") World3)
(check-expect (key-type World3 "6")
              (make-editview "hello6" (cons "world" '()) "halp"))
(check-expect (key-type World4 "h") (make-search "" '() "h"))
(check-expect (key-type World4 "\b") World4)
(check-expect (key-type World5 "a") (make-search "" (cons "aaa" '()) "4aa"))
(check-expect (key-type World5 "\b") (make-search "" (cons "aaa" '()) "4"))
(check-expect (key-type World6 "a") (make-search "halfpost"
                                                 (cons "fullpost" (cons "oldpost" '()))
                                                 "posta"))
(check-expect (key-type World6 "\b") (make-search "halfpost"
                                                  (cons "fullpost"
                                                        (cons "oldpost" '()))
                                                  "pos"))
(check-expect (key-type World5 "\r") (make-search "" (cons "aaa" '()) "4a"))
(check-expect (key-type (make-search "" (cons "fundies" (cons "is" (cons "fun" '()))) "fu") "n")
              (make-search "" (cons "fundies" (cons "is" (cons "fun" '()))) "fun"))


(define (key-type world ke)
  (cond
    [(editview? world) (key-type-editview world ke)]
    [(search? world) (key-type-search world ke)]))

;;Helper Function: key-type-editview

;; key-type-editview: EditviewPosts String -> EditviewPosts
;;Interpretation: runs the key-type function when in Edit Mode

(check-expect (key-type-editview World1 "h") (make-editview "h" '() ""))
(check-expect (key-type-editview World1 "\b") World1)
(check-expect (key-type-editview World1 "\r") World1)
(check-expect (key-type-editview World2 "\b")
              (make-editview "let's go fundies" '() ""))
(check-expect (key-type-editview World2 "\r")
              (make-package
               (make-editview "" '() "")
               "let's go fundies!"))
(check-expect (key-type-editview World3 "right") World3)
(check-expect (key-type-editview World3 "6")
              (make-editview "hello6" (cons "world" '()) "halp"))
(check-expect (key-type-editview World1 "f1") World1)
(check-expect (key-type-editview World1 "f2") (make-search "" '() ""))
(check-expect (key-type-editview World2 "f2") (make-search "let's go fundies!" '() ""))
(check-expect (key-type-editview World3 "f1") World3)
(check-expect (key-type-editview World3 "f2") (make-search "hello" (cons "world" '()) "halp"))


(define (key-type-editview world ke)
  (cond
    [(string=? ke "\b")
     (if
      (string=? "" (editview-edit world))
      (make-editview (editview-edit world) (editview-history world) (editview-search world))
      (make-editview (substring (editview-edit world)
                                0 (- (string-length (editview-edit world)) 1))
                     (editview-history world) (editview-search world)))]
    [(string=? ke "\r")
     (submit-post world)]
    [(and (= (string-length ke) 1)
          (not (string=? ke "u\007F")))
     (make-editview
      (string-append (editview-edit world) ke)
      (editview-history world) (editview-search world))]
    [(or (string=? ke "f1") (string=? ke "f2"))
     (swip-swap-editview world ke)]
    [else
     (make-editview (editview-edit world) (editview-history world) (editview-search world))]))

;;Helper Function: key-type-search

;; key-type-search : Search String -> Search
;; Interpretation: runs the key-type function when in Search mode

(check-expect (key-type-search World4 "h") (make-search "" '() "h"))
(check-expect (key-type-search World4 "\b") World4)
(check-expect (key-type-search World5 "a") (make-search "" (cons "aaa" '()) "4aa"))
(check-expect (key-type-search World5 "\b") (make-search "" (cons "aaa" '()) "4"))
(check-expect (key-type-search World6 "a") (make-search "halfpost"
                                                        (cons "fullpost" (cons "oldpost" '()))
                                                        "posta"))
(check-expect (key-type-search World6 "\b") (make-search "halfpost"
                                                         (cons "fullpost"
                                                               (cons "oldpost" '()))
                                                         "pos"))
(check-expect (key-type-search World5 "\r") (make-search "" (cons "aaa" '()) "4a"))
(check-expect (key-type-search (make-search ""
                                            (cons "fundies" (cons "is" (cons "fun" '()))) "fu") "n")
              (make-search "" (cons "fundies" (cons "is" (cons "fun" '()))) "fun"))
(check-expect (key-type-search World4 "f1") (make-editview "" '() ""))
(check-expect (key-type-search World4 "f2") World4)
(check-expect (key-type-search World5 "f1") (make-editview "" (cons "aaa" '()) "4a"))
(check-expect (key-type-search World5 "f2") World5)
(check-expect (key-type-search World6 "f1") (make-editview "halfpost"
                                                           (cons "fullpost"
                                                                 (cons "oldpost" '()))
                                                           "post"))
(check-expect (key-type-search World4 "left") World4)


(define (key-type-search world ke)
  (cond
    [(string=? ke "\b")
     (if
      (string=? "" (search-search world))
      (make-search (search-edit world) (search-history world) (search-search world))
      (make-search (search-edit world) (search-history world)
                   (substring (search-search world)
                              0 (- (string-length (search-search world)) 1))))]
    [(string=? ke "\r")
     (make-search (search-edit world) (search-history world) (search-search world))]
    [(and (= (string-length ke) 1)
          (not (string=? ke "u\007F")))
     (make-search
      (search-edit world)
      (search-history world)
      (string-append (search-search world) ke))]
    [(or (string=? ke "f1") (string=? ke "f2"))
     (swip-swap-search world ke)]
    [else
     (make-search (search-edit world) (search-history world) (search-search world))]))


;;Helper Function
;;submit-post: World -> World
;;Interpretation: sends a finished post to the server, and clears current post

(check-expect (submit-post World1) World1)
(check-expect (submit-post World2) (make-package (make-editview "" '() "") "let's go fundies!"))
(check-expect (submit-post World3) (make-package (make-editview "" (cons "world" '()) "halp")
                                                 "hello"))

(define (submit-post world)
  (if
   (string=? "" (editview-edit world))
   world
   (make-package (make-editview "" (editview-history world) (editview-search world))
                 (editview-edit world))))

;;Helper function: swip-swap-editview
;;swip-swap-editview: EditviewPosts KeyEvent -> World
;; Interpretation: Switches the world between editing and searching mode on a keypress
; F1 will switch to EditviewPosts mode, and F2 will switch to SearchPosts mode

(check-expect (swip-swap-editview World1 "f1") World1)
(check-expect (swip-swap-editview World1 "f2") (make-search "" '() ""))
(check-expect (swip-swap-editview World2 "f2") (make-search "let's go fundies!" '() ""))
(check-expect (swip-swap-editview World3 "f1") World3)
(check-expect (swip-swap-editview World3 "f2") (make-search "hello" (cons "world" '()) "halp"))

(define (swip-swap-editview world ke)
  (cond
    [(string=? ke "f1")
     (make-editview (editview-edit world)
                    (editview-history world)
                    (editview-search world))]
    [(string=? ke "f2")
     (make-search (editview-edit world)
                  (editview-history world)
                  (editview-search world))]))

;;Helper function: swip-swap-search
;;swip-swap-search: SearchPosts KeyEvent -> World
;; Interpretation: Switches the world between editing and searching mode on a keypress
; F1 will switch to EditviewPosts mode, and F2 will switch to SearchPosts mode

(check-expect (swip-swap-search World4 "f1") (make-editview "" '() ""))
(check-expect (swip-swap-search World4 "f2") World4)
(check-expect (swip-swap-search World5 "f1") (make-editview "" (cons "aaa" '()) "4a"))
(check-expect (swip-swap-search World5 "f2") World5)
(check-expect (swip-swap-search World6 "f1") (make-editview "halfpost"
                                                            (cons "fullpost"
                                                                  (cons "oldpost" '()))
                                                            "post"))

(define (swip-swap-search world ke)
  (cond
    [(string=? ke "f1")
     (make-editview (search-edit world)
                    (search-history world)
                    (search-search world))]
    [(string=? ke "f2")
     (make-search (search-edit world)
                  (search-history world)
                  (search-search world))]))


;;To-draw function
;;display-text: World -> Image
;;Interpretation: Draws the current world
#; (define (display-text World)
     ...)

(check-expect (display-text World1)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (beside (display-current "")
                                                  (display-current ""))
                                          empty-image)
                             EMPTY-WORLD))
(check-expect (display-text World2)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (beside (display-current
                                                   "let's go fundies!")
                                                  (display-current ""))
                                          empty-image)
                             EMPTY-WORLD))
(check-expect (display-text World3)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (beside (display-current "hello")
                                                  (display-current "halp"))
                                          empty-image)
                             EMPTY-WORLD))

(check-expect (display-text World4)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (beside (display-current "")
                                                  (display-current ""))
                                          empty-image)
                             EMPTY-WORLD))

(check-expect (display-text World5)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (beside (display-current "")
                                                  (display-current "4a"))
                                          empty-image)
                             EMPTY-WORLD))

(check-expect (display-text World6)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (beside (display-current "halfpost")
                                                  (display-current "post"))
                                          (text "fullpost" 18 "black")
                                          (text "oldpost" 18 "black"))
                             EMPTY-WORLD))

(define (display-text world)
  (cond
    [(editview? world) (display-text-editview world)]
    [(search? world) (display-text-search world)]))


      
;; Helper function: display-text-editview
;; display-text-editview: EditviewPosts -> Image
;; Interpretation: Draws the current world when it is an editview

(check-expect (display-text-editview World1)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (beside (display-current "")
                                                  (display-current ""))
                                          empty-image)
                             EMPTY-WORLD))
(check-expect (display-text-editview World2)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (beside (display-current
                                                   "let's go fundies!")
                                                  (display-current ""))
                                          empty-image)
                             EMPTY-WORLD))
(check-expect (display-text-editview World3)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (beside (display-current "hello")
                                                  (display-current "halp"))
                                          empty-image)
                             EMPTY-WORLD))


(define (display-text-editview world)
  (overlay/align "left" "top"
                 (above/align "left"
                              (beside (display-current (editview-edit world))
                                      (display-current (editview-search world)))
                              (draw-history (search-for-string
                                             (editview-search world)
                                             (editview-history world))))
                 EMPTY-WORLD))

;; Helper function: display-text-search
;; display-text-search: SearchPosts -> Image
;; Interpretation: Draws the current world when it is a search

(check-expect (display-text-search World4)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (beside (display-current "")
                                                  (display-current ""))
                                          empty-image)
                             EMPTY-WORLD))

(check-expect (display-text-search World5)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (beside (display-current "")
                                                  (display-current "4a"))
                                          empty-image)
                             EMPTY-WORLD))

(check-expect (display-text-search World6)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (beside (display-current "halfpost")
                                                  (display-current "post"))
                                          (text "fullpost" 18 "black")
                                          (text "oldpost" 18 "black"))
                             EMPTY-WORLD))

(define (display-text-search world)
  (overlay/align "left" "top"
                 (above/align "left"
                              (beside (display-current (search-edit world))
                                      (display-current (search-search world)))
                              (draw-history (search-for-string
                                             (search-search world)
                                             (search-history world))))
                 EMPTY-WORLD))

;;Helper function: display-current
;;display-current: String -> Image
;;Draws the current post that the user is editing

(check-expect (display-current "")
              (frame (rectangle (/ WORLD-WIDTH 2) 40 "solid" "white")))
(check-expect (display-current "hello")
              (frame
               (overlay
                (text "hello" 18 "black")
                (rectangle (/ WORLD-WIDTH 2) 40 "solid" "white"))))

(define (display-current str)
  (frame
   (overlay
    (text str 18 "black")
    (rectangle (/ WORLD-WIDTH 2) 40 "solid" "white"))))

;;Helper function: draw-history
;;draw-history: LoS -> Image
;;Draws all previously submitted posts
#; (define (draw-history los)
     ...)

(check-expect (draw-history (editview-history World1)) empty-image)
(check-expect (draw-history (editview-history World2)) empty-image)
(check-expect (draw-history (editview-history World3)) (text "world" 18 "black"))

(define (draw-history los)
  (local
    ((define (text-of-history str img)
       (above/align "left" (text-color str) img)))
     (foldr text-of-history empty-image los)))

#;(define (draw-history los)
  (cond
    [(empty? los) empty-image]
    [(cons? los)
     (above/align "left"
                  (text-color (first los))
                  (draw-history (rest los)))]))

;;Helper function: search-for-string
;;search-for-string: String LoS -> LoS
;;Interpretation: Returns a list of strings with only the strings in a given los
;;that contain a given string

(define los1 '())
(define los2 (cons "cat" '()))
(define los3 (cons "catch" (cons "doggy" (cons "cattle" '()))))

(check-expect (search-for-string "" los1) '())
(check-expect (search-for-string "cat" los2) (cons "cat" '()))
(check-expect (search-for-string "kitty" los2) '())
(check-expect (search-for-string "cat" los3) (cons "catch" (cons "cattle" '())))

(define (search-for-string str los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (if
      (string-contains? (first los) str)
      (cons (first los)
            (search-for-string str (rest los)))
      (search-for-string str (rest los)))]))

;;Helper function: text-color
;;text-color: String -> Image
;;Takes the message received and creates a text image of a certain color and size
#; (define (text-color str)
     ...)

(check-expect (text-color "") (text "" 18 "black"))
(check-expect (text-color "hello") (text "hello" 18 "black"))
(check-expect (text-color "ERROR: you done messed up")
              (text "ERROR: you done messed up" 36 "red"))

(define (text-color str)
  (if
   (string-contains? str "ERROR")
   (text str 36 "red")
   (text str 18 "black")))


;;on-receive function:
;;receive-post: World String -> World
;;Interpretation: takes in the current world and the message the user submitted,
; and adds the message to the post history
#; (define (receive-post World str)
     ...)

(check-expect (receive-post World1 "0:scherer.e:hey")
              (make-editview "" (cons "0:scherer.e:hey" '()) ""))
(check-expect (receive-post World2 "0:scherer.e:sup")
              (make-editview "let's go fundies!"
                             (cons "0:scherer.e:sup" '()) ""))
(check-expect (receive-post World3 "1:scherer.e:heyo")
              (make-editview "hello" (cons "1:scherer.e:heyo" (cons "world" '())) "halp"))
(check-expect (receive-post (make-editview "what is wrong" '() "") "ERROR: you done messed up")
              (make-editview "what is wrong" (cons "ERROR: you done messed up" '()) ""))

(define (receive-post World str)
  (cond
    [(editview? World)
     (make-editview (editview-edit World)
                    (cons str (editview-history World)) (editview-search World))]
    [(search? World)
     (make-search (search-edit World)
                  (cons str (search-history World)) (search-search World))]))
