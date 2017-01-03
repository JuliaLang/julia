;;;; FemtoLisp Documentation System


(define (docstring f)
    "
        (docstring f)

    Retrieve and display function `f` docstring.

    # Usage

    ```
    > (docstring help)
    \"\n        (help f)\n\n    Displays documentation for function `f`.\n\n    # Usage\n\n    ```\n    > (help docstring)\n\n           (docstring f)\n        Retrieve and display function `f` docstring.\n\n    #t\n\n    > (help princ)\n\n        No documentation found for `princ`.\n\n    #t\n\n    > (help 'foo)\n    type error: function:name: expected function, got foo\n    #0 (help foo)\n    ```\n    \"

    > (docstring princ)
    \"\"

    > (docstring 'foo)
    type error: function:vals: expected function, got foo
    #0 (docstring foo)
    ```
    "
    (let* ((function-values (function:vals f))
           (first-value     (aref function-values 0))
           (doc-string      (if (string? first-value)
                                first-value
                                "")))

          doc-string))


(define (help f)
    "
        (help f)

    Displays documentation for function `f`.

    # Usage

    ```
    > (help docstring)

            (docstring f)

        Retrieve and display function `f` docstring.

        # Usage

        ```
        > (docstring help)
        \"
            (help f)

        Displays documentation for function `f`.
        \"

        > (docstring princ)
        \"\"

        > (docstring 'foo)
        type error: function:vals: expected function, got foo
        #0 (docstring foo)
        ```

    #t

    > (help princ)

        No documentation found for `princ`.

    #t

    > (help 'foo)
    type error: function:name: expected function, got foo
    #0 (help foo)
    ```
    "
    (let* ((function-name     (function:name f))
           (doc-string        (docstring f))
           (docstring-message (if (eq? doc-string "")
               (string "\n    No documentation found for `"
                              function-name
                              "`.\n\n")
               (string doc-string "\n"))))

          (with-output-to *output-stream* (princ docstring-message))))
