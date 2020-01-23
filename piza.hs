

main :: IO ()
main =
    putStrLn "What is the size of pizza 1"
        >>  getLine
        >>= (\size1 ->
                putStrLn "What is the cost of pizza 1"
                    >>  getLine
                    >>= (\cost1 ->
                            putStrLn "What is the size of pizza 2"
                                >>  getLine
                                >>= (\size2 ->
                                        putStrLn "What is the cost of pizza 2"
                                            >>  getLine
                                            >>= (\cost2 ->
                                                    (\pizza1 ->
                                                            (\pizza2 ->
                                                                    (\betterPizza ->
                                                                            putStrLn
                                                                                (describePizza
                                                                                    betterPizza
                                                                                )
                                                                        )
                                                                        (comparePizzas
                                                                            pizza1
                                                                            pizza2
                                                                        )
                                                                )
                                                                ( read size2
                                                                , read cost2
                                                                )
                                                        )
                                                        (read size1, read cost1)
                                                )
                                    )
                        )
            )
