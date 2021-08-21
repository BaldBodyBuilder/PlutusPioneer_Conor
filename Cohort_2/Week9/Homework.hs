When
    [Case
        (Deposit
            (Role "Charlie")
            (Role "Charlie")
            (Token "" "")
            (AddValue
                (Constant 10)
                (Constant 10)
            )
        )
        (When
            [Case
                (Deposit
                    (Role "Alice")
                    (Role "Alice")
                    (Token "" "")
                    (Constant 10)
                )
                (When
                    [Case
                        (Deposit
                            (Role "Bob")
                            (Role "Bob")
                            (Token "" "")
                            (Constant 10)
                        )
                        (When
                            [Case
                                (Choice
                                    (ChoiceId
                                        "Winner"
                                        (Role "Charlie")
                                    )
                                    [Bound 1 2]
                                )
                                (If
                                    (ValueEQ
                                        (ChoiceValue
                                            (ChoiceId
                                                "Winner"
                                                (Role "Charlie")
                                            ))
                                        (Constant 1)
                                    )
                                    (Pay
                                        (Role "Charlie")
                                        (Account (Role "Alice"))
                                        (Token "" "")
                                        (Constant 10)
                                        (Pay
                                            (Role "Bob")
                                            (Account (Role "Charlie"))
                                            (Token "" "")
                                            (Constant 10)
                                            Close 
                                        )
                                    )
                                    (If
                                        (ValueEQ
                                            (ChoiceValue
                                                (ChoiceId
                                                    "Winner"
                                                    (Role "Charlie")
                                                ))
                                            (Constant 2)
                                        )
                                        (Pay
                                            (Role "Alice")
                                            (Account (Role "Charlie"))
                                            (Token "" "")
                                            (Constant 10)
                                            (Pay
                                                (Role "Charlie")
                                                (Account (Role "Bob"))
                                                (Token "" "")
                                                (Constant 10)
                                                Close 
                                            )
                                        )
                                        Close 
                                    )
                                )]
                            30
                            (Pay
                                (Role "Charlie")
                                (Account (Role "Alice"))
                                (Token "" "")
                                (Constant 10)
                                (Pay
                                    (Role "Charlie")
                                    (Account (Role "Bob"))
                                    (Token "" "")
                                    (Constant 10)
                                    Close 
                                )
                            )
                        )]
                    20 Close 
                )]
            10 Close 
        )]
    1 Close 