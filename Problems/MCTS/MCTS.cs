namespace MCTS
{
    public class MCTS
    {
        public static readonly int WIN_SCORE = 1;
        public static long ExploredNodes { get; set; }

        /// <summary>
        /// Core of the MCTS algorithm. Fins the next best move
        /// in given time for the board in parameter
        /// </summary>
        /// <param name="b"> Current board </param>
        /// <param name="playerNo"> The player's turn </param>
        /// <param name="searchTime"> The time (in seconds) to explore the tree for each move </param>
        /// <returns></returns>
        public static Board FindNextMove(Board b, int playerNo, int searchTime = 5)
        {
            ExploredNodes = 0;
            int end = Environment.TickCount + searchTime * 1000;

            int maxDepth = 0;
            int opponent = 3 - playerNo;
            Tree tree = new Tree();
            Node rootNode = tree.Root;
            rootNode.State.Board = b;
            rootNode.State.PlayerNo = opponent;
            ExpandNode(rootNode);

            while (Environment.TickCount < end)
            {
                // Selection (Child with highest UTC score)
                Node candidate = SelectCandidate(rootNode);
                // Expansion (Add children to selected node)
                if (candidate.State.Board.CheckStatus() == Board.IN_PROGRESS)
                    ExpandNode(candidate);
                // Simulation
                Node nodeToExplore = candidate;
                if (candidate.ChildrenList.Count > 0)
                    nodeToExplore = candidate.GetRandomChildNode();
                int playoutResult = SimulateRandomPlayout(nodeToExplore);
                // Update
                BackPropagation(nodeToExplore, playoutResult);
                //maxDepth = Math.Max(maxDepth, MaximumDepth(rootNode));
            }
            //Console.WriteLine("Max depth: " + maxDepth);
            Node winnerNode = rootNode.GetChildWithMaxScore();
            tree.Root = winnerNode;
            Console.WriteLine($"Winrate for {(winnerNode.State.PlayerNo == 1 ? "O" : "X")}: {100 * winnerNode.State.WinScore / ExploredNodes:F2}%");

            return winnerNode.State.Board;
        }

        /// <summary>
        /// Finds the most promising child of the current node, using UCT formula.
        /// </summary>
        /// <param name="rootNode"> Starting node </param>
        /// <returns></returns>
        public static Node SelectCandidate(Node rootNode)
        {
            Node node = rootNode;
            while (node.ChildrenList.Count != 0)
                node = UCT.FindBestNodeWithUCT(node);
            return node;
        }

        /// <summary>
        /// Get list of children of the node.
        /// </summary>
        /// <param name="candidate"> Starting node </param>
        public static void ExpandNode(Node candidate)
        {
            ExploredNodes++; //?
            candidate.State.GetAllPossibleStates().ForEach(s =>
            {
                Node newNode = new Node(s) { Parent = candidate };
                newNode.State.PlayerNo = candidate.State.GetOpponent();
                candidate.ChildrenList.Add(newNode);
            });
        }

        /// <summary>
        /// Plays the game from current node to the end with random moves.
        /// </summary>
        /// <param name="nodeToExplore"> Starting node </param>
        /// <returns>Result of the game (cf CheckStatus())</returns>
        public static int SimulateRandomPlayout(Node nodeToExplore)
        {
            Node tempNode = new Node(nodeToExplore); // Clone node to explore
            State tempState = tempNode.State;
            int boardStatus = tempState.Board.CheckStatus();

            while (boardStatus == Board.IN_PROGRESS) // If game not over, play randomly til the end
            {
                tempState.TogglePlayer();
                tempState.RandomPlay();
                boardStatus = tempState.Board.CheckStatus();
                //ExploredNodes++; //?
            }
            if (boardStatus == tempState.GetOpponent()) // If we lose, return opponent and store winscore as minvalue.
            {
                if (tempNode.Parent != null)
                    tempNode.Parent.State.WinScore = int.MinValue;
                return boardStatus;
            }
            return boardStatus; // else, return 0 (draw) or P1/P2
        }

        /// <summary>
        /// Propagates the game's result back to the parent nodes and updates the visit count.
        /// </summary>
        /// <param name="nodeToExplore"> Starting node </param>
        /// <param name="playoutResult"> Final result for starting node </param>
        public static void BackPropagation(Node nodeToExplore, int playoutResult)
        {
            Node? tempNode = nodeToExplore;
            while (tempNode != null)
            {
                ExploredNodes++; //?
                tempNode.State.VisitCount++;
                if (tempNode.State.PlayerNo == playoutResult) // If winner is last player, add score to state
                    tempNode.State.AddScore(WIN_SCORE);
                tempNode = tempNode.Parent;
            }
        }

        private static int MaximumDepth(Node root)
        {
            int answer = 0;
            if (root == null)
            {
                return 0;
            }
            foreach (var child in root.ChildrenList)
            {
                answer = Math.Max(answer, MaximumDepth(child));
            }
            return answer + 1;
        }
    }
}