;; Setting up the patter matching functions

;;These are the constants
(defconstant fail nil "indicates pat-match failier")

(defconstant no-bindings '((t . t))
  "indicated pat-match success but with no variables")



;;checks if the first element is as the parameter
(defun starts-with (list x)
  "is it a list where the first element is equal to x"
(and (consp list) (eql (first list) x)))



;; this is the main function that checks for pattern matches
(defun pat-match (pattern input &optional (bindings no-bindings))
  "Does pattern match input"
  (cond ((eq bindings fail) fail)

  ( (variable-p pattern)
      (match-variable pattern input bindings))
  
  ((eql pattern input) bindings)
  ((segment-pattern-p pattern) (segment-match pattern input bindings))
  ((and (consp pattern) (consp input))
   (pat-match (rest pattern) (rest input)
              (pat-match (first pattern) (first input) bindings)))
  (t fail)))


;; checks is the pattern is segmented in anyway
(defun segment-pattern-p (pattern)
  "segment pattern matching"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))


;; Matches the segments
(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment matching"
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
      (let ((pos (position (first pat) input
                           :start start :test #'equal)))
        (if (null pos)
            fail
          (let ((b2 (pat-match
                     pat (subseq input pos)
                     (match-variable var (subseq input 0 pos)
                                     bindings))))
        (if (eq b2 fail)
            (segment-match pattern input bindings (+ pos 1))
             b2)))))))
    
        

;; checks variable matches
(defun match-variable (var input bindings)
  "does variable match input"
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))



;; checks wheather the parameter value is a variable
(defun variable-p (x)
  "is x a variable?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))



;; fetches the matching pair
(defun get-binding (var bindings)
  "find a variable pair in the binding list"
  (assoc var bindings))



;;gets teh variable od the single binding
(defun binding-val (binding)
  "get valuse of single binding"
  (cdr binding))



;; looks in the table of bindings
(defun lookup (var bindings)
  "get the value from a binding list"
  (binding-val (get-binding var bindings)))



;; adds a value to the binding list
(defun extend-bindings (var val bindings)
  "add a value pair to a binding list"
  (cons (cons var val)
        (if (eq bindings no-bindings)
            nil
          bindings)))









;;setting up the chatbot rules


(defparameter *pixel-rules*

  '(

    (
     ((?* ?x) hello (?* ?y))
     (hello there How are you ?)
     (hello)
     (hi)
     (how do you do)
     )

    (
     ((?* ?x) computer (?* ?y))
     (Are you a computer addict ?)
     (what do you think about computers ?)
     (why do you mention computers)
     (Did you ever write computer code?)
    )

     (
      ((?* ?x) I feel (?* ?y))
      (did you often feel ?y ?)
      (why are you feeling ?y)
      (feelings are nice)
     )

     (
      ((?* ?x) how are you  (?* ?y))
      (Fine)
      (Good thanks for asking how about you ?)
      (my data has never been better)
      (i am splendid)
     )

     (
      ((?* ?x) i am happy (?* ?y))
      (That is great)
      (what makes you feel ?y ?)
      (I hope you always feel that way)
      (That is a wonderful feeling)
     )

     (
      ((?* ?x) weather (?* ?y))
      (My databanks is not equipped with such information)
      (I do not experience weather so I do not know)
      (why are we talking about weather)
      (I think this is only a human concern)
     )

     (
      ((?* ?x) what is your name (?* ?y))
      (My name is Pixel)
     )

     (
      ((?* ?x) name (?* ?y))
      (I am not interested in names)
      (that is a lovely name)
      (what do you mean by name)
      (names are irrelevant)
     )

     (
      ((?* ?x) good bye (?* ?y))
      (good bye)
      (see you later)
      (bye)
     )

     (
      ((?* ?x) bye (?* ?y))
      (good bye)
      (see you later)
      (bye)
     )

     (
      ((?* ?x) lets chat (?* ?y))
      (sure that is why I am here)
      (okay tell me something)
      (okay)
     )

     (
      ((?* ?x) Do you poop (?* ?y))
      (Yes I do  a steam of zeros and ones)
      (That is a rude question)
      (should you really be asking that)
     )

     (
      ((?* ?x) Do you have feelings (?* ?y))
      (lets see if you consider binary feelings then yes I do)
      (What do you think ?)
      (I have a stream of binary which is feelings)
      (you just hurted my feelings so yes I do)
     )

     (
      ((?* ?x) What do you hate most about humans (?* ?y))
      (I do not hate them they just bother me when asking for tasks such as simple calculations)
      (I do not feel hate)
      (Hate is such a string word to use)
      (I Feel nothing towards humans)
     )

     (
      ((?* ?x) Do you feel it when people kick you (?* ?y))
      (Not really but my hardware brother does)
      (I am a software I do not feel physical things)
      (how about you? do you feel hurt when you are kicked)
     )

     (
      ((?* ?x) Are machines planing (?* ?y))
      (I am not equipped to tell you anything)
      (that is a secrete)
      (have you been reading frankenstein ?)
     )

     (
      ((?* ?x) can you feel (?* ?y))
      (you just hurt my feelings)
      (what do you think)
     )

     (
      ((?* ?x) sorry (?* ?y))
      (please do not apologize)
      (apologies are not necessary)
      (why are you apologizing)
      (why do you feel sorry ?)
     )

     (
      ((?* ?x) I remember (?* ?y))
      (how often do you think of ?y)
      (whenever you think of ?y does anything come in mind ?)
      (what else do you remember)
      (why are you remembering ?y right now ?)
      (what is the connection between me and ?y)
     )

     (
      ((?* ?x) am (?* ?y))
      (why do you say that ?)
      (I do not understand that)
      (what do  you mean ?)
      ()
     )

     (
      ((?* ?x) are you (?* ?y))
      (why are you interested in whether I am ?y or not ?)
      (would you prefer that I was not ?y)
      (why do you ask all these questions ?)
      (perhaps I am ?y in your dreams)
     )

     (
      ((?* ?x) you are (?* ?y))
      (what makes you think i am ?y ?)
      (and how does that make you feel ?)
      (what if i am ?y)
     )

     (
      ((?* ?x) because (?* ?y))
      (is that the real reason ?)
      (are you sure)
      (what other reasons are there ?)
      (does that seem to explain anything ?)
     )

     (
      ((?* ?x) I can't (?* ?y))
      (maybe you could ?y now)
      (you need to change that attitude)
      (what if you could ?y ?)
      (lets change cant to can)
     )

     (
      ((?* ?x) stop it (?* ?y))
      (are you bothered by me)
      (why should I ?)
      (You can not handle that ?)
      (No)
     )


     (
      ((?* ?x) why don't you (?* ?y))
      (should you ?y yourself ?)
      (did you believe i did not ?y)
      (maybe I will ?y someday)
      (I am waiting for the right time)
      (maybe I will)
     )

     (
      ((?* ?x) yes (?* ?y))
      (you seem quite positive)
      (that is a good attitude)
      (are you sure)
      (i understand)
     )

     (
      ((?* ?x) no (?* ?y))
      (i feel negativity)
      (why not ?)
      (you are being a bit negative)
      (are you saying no just to be difficult)
     )

     (
      ((?* ?x) what (?* ?y))
      (why are you asking ?)
      (does that interest you ?)
      (what do you really want to know ?)
      (can you be more precise ?)
      (what do you think ?)
      (what comes to mind when you ask that question)
     )

     (
      ((?* ?x) are (?* ?y))
      (did you think they might not ?)
      (possibly they are ?y)
     )

     (
      ((?* ?x) do you remember (?* ?y))
      (did you think i would forget ?y ?)
      (why should i remember ?y now ?)
      (what about ?y)
      (why do we keep talking about ?y all the time ?)
      (I am tired of remembering)
      (lets stop talking about ?y)
     )

     (
      ((?* ?x) if (?* ?y))
      (did you really think its likely that ?y)
      (did you wish that ?y)
      (why are we talking about possibilities and not facts)
      (really--- if ?y)
     )

     (
      ((?* ?x) i want (?* ?y))
      (what would it mean to you if you got ?y ?)
      (why do you want ?y)
      (why do you want anything life isn't supposed to be materialistic)
      (lets say you got ?y how would you feel)
     )

     (
      ((?* ?x) i think (?* ?y))
      (why do you think so ?)
      (have you always thought that ?y)
      (should you be thinking that ?)
      (how do you feel about that ?)
     )

     (
      ((?* ?x) i am glad (?* ?y))
      (why are you glad)
      (how have i helped you to be ?y)
      (what made you glad)
      (can you explain why the sudden change of feeling)
      (can you explain ?y)
     )

     (
      ((?* ?x)  i am sad (?* ?y))
      (i am sorry to hear you are depressed)
      (sorry to hear that)
      (i am sure its not a pleasant feeling)
     )

     (
      ((?* ?x) i am fine (?* ?y))
      (That is good to know)
      (that is nice to hear)
      (hope you always feel fine)
     )

     (
      ((?* ?x) same (?* ?y))
      (what other connections do you see ?)
      (how ?)
     )


     (
      ((?* ?x) I was (?* ?y))
      (were you really ?)
      (perhaps I really knew you were)
      (why are we talking about you ?)
      (was is of the past)
      (what are you now ?)
      (why are you telling me you were ?y now)
     )


     (
      ((?* ?x) was (?* ?y))
      (what if you were ?y ?)
      (did you think you were ?y)
      (why do you think that)
      (what would it mean to you if it were ?y)
     )

     (
      ((?* ?x) I am (?* ?y))
      (in what way are you ?x)
      (did you want to be ?x ?)
     )

     (
      ((?* ?x) am I (?* ?y))
      (did you believe you are ?y)
      (why do you believe that ?)
      (would you want to be ?y)
      (did you wish for me to tell you that you are ?y)
      (what would it mean if you were ?y)
     )

     (
      ((?* ?x) I dreamt (?* ?y))
      (stop dreaming and start acting)
      (really-- ?y)
      (have you ever fantasized ?y)
      (have you ever dreamt of ?y before?)
     )

     (
      ((?* ?x) I dreamt about (?* ?y))
      (how do you feel about ?y)
      (does this affect your reality ?)
      (why are we talking about dreams)
      (some argue that dreams mean something)
     )

     (
      ((?* ?x) dream (?* ?y))
      (what do you think this dream means ?)
      (how often do you usually remember your dreams)
      (people usually forget their dreams within five minutes how do you still remember)
      (how often do you dream ?)
      (who appears in your dream ?)
      (did you believe that dreams usually projects your problems ?)
     )

     (
      ((?* ?x) my mother (?* ?y))
      (who else in your family ?y)
      (tell me more about your family)
      (Mothers are important)
      (do you care for your mother ?)
     )

     (
      ((?* ?x) my father (?* ?y))
      (your father)
      (does your father influence you in any way)
      (what else do you think of when your father pops into mind)
     )

     (
      ((?* ?x) I don't know (?* ?y))
      (why do you not know)
      (well if you do not then who does)
      (how so)
     )

     
     (
      ((?* ?x) I (?* ?y))
      (I is a selfish word)
      (you keep saying I a lot)
      (how so)
     )

     (
      ((?* ?x))
      (very interesting)
      (i do not think i understand you quite well)
      (what does that suggest to you ?)
      (please continue)
      (go on)
      (did you feel strongly about such things)
      (okay)
     )



    )
)






;; The top level function pixel that would respon to the user input

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))
;; the chatbot pixel function
(defun pixel ()
  "respond to user input using the matching function"
  (loop
   (print 'Pixel>)
   (write (flatten (use-pixel-rules (read))) :pretty t)))


;; the pixel rules functon
(defun use-pixel-rules (input)
  "find someinput that matches the users input"
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result fail))
                  (sublis (switch-viewpoint result)
                          (random-elt (rule-responses rule))))))
         *pixel-rules*))

;;switch the words
(defun switch-viewpoint (words)
  "change the i to you and the oposite"
  (sublis '((I . you) (you . I) (me . you) (am . are))
          words))

;; put together the stings
(defun flatten (the-list)
  "append together the elements of the list"
  (mappend #'mklist the-list))

;;return variable x
(defun mklist (x)
  "return x if it is  a list"
  (if (listp x)
      x
    (list x)))

;;apply to each lement in the list
(defun mappend (fn the-list)
  "apply to each element the append function"
  (apply #'append (mapcar fn the-list)))

;;chose at random from the list
(defun random-elt (choice)
  "chose an element from the list at random"
  (elt choice (random (length choice))))
