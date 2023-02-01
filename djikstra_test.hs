import Djikstra

-- a simple test case
testGraph = (Graph [(Node "a"), (Node "b"), (Node "c"), (Node "d"), (Node "e")] 
                        [(Edge (Node "a") (Node "b") (3)),
                         (Edge (Node "a") (Node "c") (2)),
                         (Edge (Node "b") (Node "e") (4)),
                         (Edge (Node "c") (Node "e") (4)),
                         (Edge (Node "c") (Node "d") (2)),
                         (Edge (Node "d") (Node "e") (1))])
testStart = (Node "a")
testFinish = (Node "e")
                
main :: IO()
main = do
        print $ run testGraph (initGraph testGraph testStart) testFinish
        print $ djikstraShortestPathFromTo testGraph testStart testFinish
        print $ djikstraShortestPathFrom testGraph testStart
