(reset-document)
(add-child-and-enter (make-instance '<block-element> :name "subsection"
                                                     :title "Array elements"))
(add-child-and-enter (make-instance '<block-element> :name "subsubsection"
                                                     :title "Array elements index"))
(add-child-and-enter (make-instance '<paragraph>
                                    :children '("this is a sample text")))
(close-child-and-go-up nil nil)
(close-child-and-go-up "subsubsection" "Array elements index")
(close-child-and-go-up "subsection" "Array elements")
