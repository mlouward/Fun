using System;
using System.Collections.Generic;
using System.Linq;

namespace MCTS
{
    public class Node
    {
        public State State { get; set; }
        public Node? Parent { get; set; }
        public List<Node> ChildrenList { get; set; }

        public Node()
        {
            State = new State();
            ChildrenList = new List<Node>();
        }

        public Node(State state)
        {
            State = state;
            ChildrenList = new List<Node>();
        }

        public Node(Node node)
        {
            ChildrenList = new List<Node>();
            State = new State(node.State);
            if (node.Parent != null)
                Parent = node.Parent;
            List<Node> childArray = ChildrenList;
            foreach (Node child in childArray)
            {
                ChildrenList.Add(new Node(child));
            }
        }

        public Node(State state, Node parent, List<Node> childrenList)
        {
            State = state;
            Parent = parent;
            ChildrenList = childrenList;
        }

        internal Node GetRandomChildNode()
        {
            Random rd = new Random();
            return ChildrenList[rd.Next(ChildrenList.Count)];
        }

        /// <summary>
        /// Add heuristic to choose a better node.
        /// </summary>
        /// <returns></returns>
        internal Node GetPromisingChildNode()
        {
            return ChildrenList.Aggregate((x, y) => HeuristicValue(x) > HeuristicValue(y) ? x : y);
        }

        private static int HeuristicValue(Node n)
        {
            int boardSize = Board.DEFAULT_BOARD_LENGTH;
            int maxIndex = boardSize - 1;
            int[] diag1 = new int[boardSize];
            int[] diag2 = new int[boardSize];
            int val = 0;
            int player = n.State.PlayerNo;
            int opp = n.State.GetOpponent();

            for (int i = 0; i < boardSize; i++)
            {
                int[] row = n.State.Board.boardValues[i];
                int[] col = new int[boardSize];
                for (int j = 0; j < boardSize; j++)
                {
                    col[j] = n.State.Board.boardValues[j][i];
                }

                if (row.Count(x => x == player) == 2 && row.Count(x => x == opp) == 0) val++;
                if (col.Count(x => x == player) == 2 && col.Count(x => x == opp) == 0) val++;

                diag1[i] = n.State.Board.boardValues[i][i];
                diag2[i] = n.State.Board.boardValues[maxIndex - i][i];
            }

            if (diag1.Count(x => x == player) == 2 && diag1.Count(x => x == opp) == 0) val++;
            if (diag2.Count(x => x == player) == 2 && diag2.Count(x => x == opp) == 0) val++;

            return val;
        }

        internal Node GetChildWithMaxScore()
        {
            return ChildrenList.Aggregate((x, y) => x.State.WinScore > y.State.WinScore ? x : y);
        }
    }
}